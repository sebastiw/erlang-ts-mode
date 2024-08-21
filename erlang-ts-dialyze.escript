#!/usr/bin/env escript

-mode(compile).

%% We consider a set of apps. We group them into subsets;
%% 1) OTP apps
%% 2) Project apps
%% 2.1) External apps (dependencies)
%% 2.2) Internal apps
%% 2.2.1) Generated apps
%% 2.2.2) Interesting apps
%%
%% We want to generate 4 plts;
%% # otp.plt (from item 1)
%% # ext.plt (from item 2.1)
%% # gen.plt (from item 2.2.1)
%% # int.plt (from item 2.2.2)
%%
%% We expect our caller to give us ARGS as a list of "KEY=VALUE" strings.

main(Args) ->
    pipe(Args,
         [fun config/1,
          fun check_plts/1,
          fun analyze/1,
          fun warnings/1,
          fun print/1]).

check_plts(Cfg) ->
    maps:update_with(plts, mk_check_and_build_plts(Cfg), Cfg).

mk_check_and_build_plts(Cfg) ->
    fun(PLTs) -> check_and_build_plts(PLTs, Cfg) end.

check_and_build_plts(PLTs, Cfg) ->
    progress(ok, "checking ~w PLTs.~n", [length(PLTs)]),
    lists:filtermap(fun filter_plt/1, map_reduce(mk_map(PLTs, Cfg))).

filter_plt(skip) -> false;
filter_plt({error, Error}) -> error(Error);
filter_plt(_) -> true.

mk_map(PLTs, Cfg) ->
    fun() -> lists:map(mk_check_and_build_plt(Cfg), PLTs) end.

mk_check_and_build_plt(Cfg) ->
    fun(PLT) -> {filename:basename(PLT), mk_check_and_build_plt(PLT, Cfg)} end.

mk_check_and_build_plt(PLT, Cfg) ->
    fun() -> check_and_build_plt(PLT, Cfg) end.

check_and_build_plt(PLT, Cfg) ->
    case check_plt(PLT) of
        true -> PLT;
        false -> build_plt(PLT, Cfg)
    end.

check_plt(PLT) ->
    filelib:is_regular(PLT) andalso [] =:= dialyzer(check_plt, PLT).

build_plt(PLT, Cfg) ->
    case plt_apps(PLT, Cfg) of
        [] -> skip;
        {From, Apps} -> build_plt(PLT, From, Apps)
    end.

build_plt(PLT, From, Apps) ->
    [ok = file:delete(PLT) || filelib:is_regular(PLT)],
    case dialyzer(build_plt, {PLT, From, Apps}) of
        [] -> PLT;
        Err -> {error, {PLT, Err}}
    end.

plt_apps(PLT, Cfg) ->
    case filename:basename(PLT, ".plt") of
        "otp" -> {apps, maps:get(otp_apps, Cfg, [])};
        "ext" -> {files_rec, maps:get(ext_apps, Cfg, [])};
        "int" -> {files_rec, maps:get(int_apps, Cfg, [])};
        "gen" -> {files_rec, maps:get(gen_apps, Cfg, [])}
    end.

analyze(#{target := []}) -> 
    progress([], "analyze (no targets): ~n", []);
analyze(#{target := Apps, plts := PLTs}) ->
    progress(ok, "analyze: ~p...", [Apps]),
    case analyze(Apps, PLTs) of
        [] -> progress([], ok);
        Ws -> progress(Ws, nok)
    end.

analyze(Apps, PLTs) ->
    dialyzer(analyze, {Apps, PLTs}).

warnings(Ws) ->
    lists:map(fun dialyzer:format_warning/1, Ws).

print(Lines) ->
    lists:foreach(fun(L) -> io:fwrite("~s~n", [L]) end, Lines).

dialyzer(What, How) ->
    dialyzer:run(dialyzer_opts(What, How)).

dialyzer_opts(check_plt, PLT) ->
    [{analysis_type, plt_check},
     {init_plt, PLT}];
dialyzer_opts(build_plt, {PLT, From, Apps}) ->
    [{analysis_type, plt_build},
     {From, Apps},
     {warnings, [no_unknown]},
     {output_plt, PLT}];
dialyzer_opts(analyze, {Apps, PLTs}) ->
    [{files_rec, Apps},
     {from, byte_code},
     {plts, PLTs}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% make configs from command line args

config(RawArgs) ->
    Args = raw_args_to_map(RawArgs),
    pipe(#{},
         [mk_cfg(base_dir, Args),
          mk_cfg(otp_apps, Args),
          mk_cfg(ext_apps, Args),
          mk_cfg(gen_apps, Args),
          mk_cfg(int_apps, Args),
          mk_cfg(plts, Args),
          mk_cfg(target, Args)]).

raw_args_to_map(Args) ->
    pipe(Args,
         [fun(As) -> [re:split(A, "=|\\s|\\n", [{return, list}]) || A <- As] end,
          fun(As) -> lists:map(fun([K|V]) -> {K, V} end, As) end,
          fun maps:from_list/1]).

mk_cfg(K, Args) ->
    fun(Cfg) -> cfg(K, Cfg, Args) end.

cfg(K, Cfg, As) ->
    try Cfg#{K => get_arg(K, Cfg, As)}
    catch C:R -> error({bad_arg, {C, R, K, As}})
    end.

get_arg(target, Cfg, As) ->
    %% we default to interesting_apps
    case maps:get("target", As, [[]]) of
        [[]] -> maps:get(int_apps, Cfg);
        Apps -> filter_apps(Apps, Cfg)
    end;
get_arg(base_dir, _, As) ->
    %% base_dir
    pipe(As,
         [fun(X) -> maps:get("base_dir", X) end,
          fun lists:flatten/1,
          fun(X) -> true = filelib:is_dir(X), X end]);
get_arg(otp_apps, _, As) ->
    %% otp_apps
    [list_to_atom(App) || App <- maps:get("otp_apps", As)];
get_arg(ext_apps, Cfg, As) ->
    %% external_apps = project_apps - internal_apps
    filter_apps(maps:get("project_apps", As) -- maps:get("internal_apps", As), Cfg);
get_arg(gen_apps, Cfg, As) ->
    %% generated_apps
    filter_apps(maps:get("generated_apps", As), Cfg);
get_arg(int_apps, Cfg, As) ->
    %% interesting_apps = internal_apps - generated_apps
    filter_apps(maps:get("internal_apps", As), Cfg) -- maps:get(gen_apps, Cfg);
get_arg(plts, #{base_dir := BaseDir}, _) ->
    [plt_name(BaseDir, P) || P <- ["otp", "ext", "gen", "int"]].

plt_name(Dir, Base) ->
    filename:join([Dir, Base++".plt"]).

filter_apps(Apps, #{base_dir := BaseDir}) ->
    lists:filtermap(mk_filter_app(BaseDir), Apps).

mk_filter_app(BaseDir) ->
    fun(App) -> filter_app(filename:join([BaseDir, App])) end.

%% filter out dirs that are not apps
filter_app(AppDir) ->
    case filelib:wildcard(filename:join([AppDir, 'ebin', '*.app'])) of
        [_] -> {true, AppDir};
        _ -> false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% first arg is the return value, the others specify a progress
%% message.

progress(X, nl) ->
    progress(X, "~n", []);
progress(X, ok) ->
    progress(X, " ok.~n", []);
progress(X, nok) ->
    progress(X, " failed.~n", []).

progress(X, F, As) ->
    io:fwrite(F, As),
    X.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Map is a fun() -> [fun()]. 
map_reduce(Map) ->
    reduce(reduce_init(map(Map))).

map(Map) ->
    lists:foldl(fun map_spawn/2, #{}, Map()).

map_spawn({Tag, F0}, O) ->
    O#{erlang:spawn_monitor(fun() -> exit(F0()) end) => Tag}.

reduce_init(IDs) ->
    reduce_timer(#{tick => 2000, t0 => millis_now(), ids => IDs, results => []}).

-define(DOWN(Pid, Ref, X), {'DOWN', Ref, process, Pid, X}).
-define(FINISHED(Z), map_size(map_get(ids, Z)) =:= 0).
reduce(Z) when ?FINISHED(Z) -> maps:get(results, Z);
reduce(Z) ->
    receive
        tick -> reduce(reduce_tick(Z));
        ?DOWN(Pid, Ref, X) -> reduce(reduce_update({Pid, Ref}, X, Z))
    end.

-define(MAPS_UPDATE_WITH(K, V, F),
    fun(X) -> maps:update_with(K, fun(V) -> F end, X) end).
reduce_update(PidRef, Result, Z) ->
    pipe(Z,
         [?MAPS_UPDATE_WITH(ids, IDs, maps:remove(PidRef, IDs)),
          ?MAPS_UPDATE_WITH(results, Rs, [Result|Rs])]).

reduce_tick(#{t0 := T0, ids := IDs, results := Rs} = Z) ->
    As = [length(Rs), maps:size(IDs), duration(T0), tags(IDs)],
    io:fwrite("working: ~w/~w (~w)~s~n", As),
    reduce_timer(Z).

tags(IDs) ->
    case maps:size(IDs) < 4 of
        true -> "["++string:join(maps:values(IDs), ", ")++"]";
        false -> ""
    end.

reduce_timer(Z) ->
    timer:send_after(maps:get(tick, Z), tick),
    Z.

duration(T0) ->
    (millis_now()-T0)/1000.

millis_now() ->
    erlang:system_time(millisecond).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pipe(A0, Fs) ->
    lists:foldl(fun(F, A) -> F(A) end, A0, Fs).
