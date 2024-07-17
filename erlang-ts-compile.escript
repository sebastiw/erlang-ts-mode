#!/usr/bin/env escript

-mode(compile).

main(Args) ->
    case Args of
        [] -> io:fwrite("$0 compile SRC - compile SRC, or all files under SRC.~n", []);
        ["compile", Root] -> compile_all(Root);
        _ -> main([])
    end.

compile_all(R) ->
    {Root, Erls} = erls(R),
    results(map_reduce(mk_compile(Root), Erls)).

map_reduce(Map, Subjects) ->
    reduce(lists:map(mk_spawn(Map), Subjects)).

reduce(Refs) ->
    timer:send_after(2000, tick),
    redc(#{t0 => millis_now(), workers => Refs, results => []}).

-define(DOWN(Pid, Ref, X), {'DOWN', Ref, process, Pid, X}).
redc(#{workers := [], results := O}) -> O;
redc(Z) ->
    receive
        tick -> redc(tick(Z));
        ?DOWN(Pid, Ref, X) -> redc(redc_update({Pid, Ref}, X, Z))
    end.

tick(#{t0 := T0, workers := Ws, results := Rs} = X) ->
    timer:send_after(2000, tick),
    io:fwrite("reducing: ~w/~w (~w)~n", [length(Rs), length(Ws), (millis_now()-T0)/1000]),
    X.

millis_now() ->
    erlang:system_time(millisecond).

-define(MAPS_UPDATE_WITH(K, V, F),
    fun(X) -> maps:update_with(K, fun(V) -> F end, X) end).
redc_update(PidRef, {S, Data}, Z) ->
    pipe(Z,
         [?MAPS_UPDATE_WITH(workers, P, P--[{S, PidRef}]),
          ?MAPS_UPDATE_WITH(results, P, [Data|P])]).

pipe(A0, Fs) ->
    lists:foldl(fun(F, A) -> F(A) end, A0, Fs).

mk_spawn(Mapper) ->
    fun(S) -> {S, erlang:spawn_monitor(fun() -> exit({S, Mapper(S)}) end)} end.

%% We demand that the directory structure looks like this;
%% `Root/*/src/**/*.erl' Our parameter R must be absolute. It also
%% must be one of; 'Root', 'Root/A', 'Root/A/src'
%% 'Root/A/src/**/M.erl'
erls(R) ->
    case take_first(fun no_srcs/1, src_patterns(R)) of
        {Root, Erls} -> {Root, Erls};
        [] -> []
    end.

%% return `false' if there are no erls, otherwise a list of erls.
no_srcs(Pattern) ->
    case filelib:wildcard(Pattern) of
        [] -> false;
        Erls -> {root(hd(Erls)), Erls}
    end.

take_first(_, []) -> [];
take_first(F, [P|Patterns]) ->
    case F(P) of
        false -> take_first(F, Patterns);
        V -> V
    end.

root(Erl) ->
    P = "^(/([a-zA-Z0-9_-]+/)+)[a-zA-Z0-9_]+/src/([a-zA-Z0-9_-]+/)*[a-zA-Z0-9_-]+.erl$",
    Opts = [global, {capture, all_but_first, list}],
    case re:run(Erl, P, Opts) of
        {match, [[Root|_]]} -> Root;
        nomatch -> []
    end.

%% R must be one of; 'Root', 'Root/A', 'Root/A/src'
%% 'Root/A/src/**/M.erl'
src_patterns(R) ->
    [filename:join([R, '*', src, '**', '*.erl']),
     filename:join([R, src, '**', '*.erl']),
     filename:join([R, '**', '*.erl']),
     R].

mk_compile(Root) ->
    fun(Erl) -> compile(Root, Erl) end.

-record(result, {module, erl, beam, errors, warnings, root, incs}).
-define(RESULT(Mod, Erl, Beam, Es, Ws, Root, Incs),
    #result{module = Mod, erl = Erl, beam = Beam, errors = Es, warnings = Ws, root = Root, incs = Incs}).
compile(Root, Erl) ->
    code:add_pathz(filename:join([Root, gpb, ebin])),
    Beam = beamfile(Erl),
    case source_hash(Beam) =:= file_hash(Erl) of
        true -> ?RESULT({mod(Erl)}, Erl, Beam, [], [], Root, []);
        false -> pre_compile(Root, Erl, Beam)
    end.

pre_compile(Root, Erl, Beam) ->
    Incs = incs(Root, Erl),
    case pre_compile(Erl, Incs) of
        [] -> compile(Root, Erl, Beam, Incs);
        Es -> ?RESULT(mod(Erl), Erl, Beam, Es, [], Root, Incs)
    end.

compile(Root, Erl, Beam, Incs) ->
    Opts = opts(Erl, Incs),
    case compile:file(Erl, Opts) of
        {ok, Mod, Bin} -> write(?RESULT(Mod, Erl, Beam, [], [], Root, Incs), Bin);
        {ok, Mod, Bin, Ws} -> write(?RESULT(Mod, Erl, Beam, [], unroll_reports(Ws), Root, Incs), Bin);
        {error, Es, Ws} -> ?RESULT(mod(Erl), Erl, "", unroll_reports(Es), unroll_reports(Ws), Root, Incs);
        error -> ?RESULT(mod(Erl), Erl, "", [], [], Root, Incs)
    end.

pre_compile(Erl, Incs) ->
    case compile:file(Erl, [basic_validation, return|Incs]) of
        {error, Es, Ws} -> pre_compile_filter(Es++Ws);
        _ -> []
    end.

pre_compile_filter(Is) ->
    pipe(Is,
         [fun unroll_reports/1,
          fun(Xs) -> lists:filtermap(fun pre_compile_pred/1, Xs) end]).

pre_compile_pred({_, erl_lint, {undefined_behaviour, B}})  -> {true, {behaviour, B}};
pre_compile_pred({_, epp, {include, I}})                   -> {true, {include, I}};
pre_compile_pred({_, epp, {include, lib, I}})              -> {true, {include, I}};
pre_compile_pred({_, compile, {undef_parse_transform, M}}) -> {true, {parse_transform, M}};
pre_compile_pred(_) -> false.

unroll_reports(Wrapped) ->
    lists:sort(lists:flatmap(fun({_, R}) -> R end, Wrapped)).

opts(Erl, Incs) ->
    [binary,
     {compile_info, cinf(Erl)},
     debug_info,
     deterministic,
     return|Incs].

incs(Root, Erl) ->
    [{i, Root},
     {i, dirname_dirname(Erl)},
     {i, dirname_dirname(Erl, [include])},
     {i, filename:dirname(Erl)}].

cinf(Erl) ->
    [{compiler, 'erlang-ts'},
     {source_hash, file_hash(Erl)}].

file_hash(File) ->
    erlang:phash2(file:read_file(File)).

source_hash(Beam) ->
    try
        {ok, {_, [{_, Bin}]}} = beam_lib:chunks(Beam, ["CInf"]),
        {source_hash, Hash} = lists:keyfind(source_hash, 1, binary_to_term(Bin)),
        Hash
    catch
        _:_ -> undefined
    end.

write(Result, Bin) ->
    case file:write_file(Result#result.beam, Bin) of
        ok -> Result;
        {error, Err} -> Result#result{errors = [{write, Err}]}
    end.

beamfile(Erl) ->
    dirname_dirname(Erl, [ebin, mod(Erl)++".beam"]).

dirname_dirname(F, Suffix) ->
    filename:join([dirname_dirname(F)|Suffix]).

dirname_dirname(F) ->
    filename:dirname(filename:dirname(F)).

mod(Erl) ->
    filename:basename(Erl, ".erl").

results(Rs) ->
    (mk_results(success))(Rs),
    (mk_results(warnings))(Rs),
    (mk_results(errors))(Rs).

mk_results(W) ->
    fun(Rs) -> lists:foreach(fun(R) -> result(W, R) end, Rs) end.

result(Level, ?RESULT(Mod, Erl, _, Es, Ws, _, Incs)) ->
    case {Level, length(Es), length(Ws), Mod} of
        {success, 0, 0, {M}} -> io:fwrite("Success (cached): ~s~n", [M]);
        {success, 0, 0, _} -> io:fwrite("Success: ~s~n", [Mod]);
        {warnings, 0, W, _} when 0 < W -> io:fwrite("Success: ~s~n~p~n", [Erl, Ws]);
        {errors, E, _, _} when 0 < E -> io:fwrite("Fail: ~s~n~p~n~p~n", [Erl, Incs, Es]);
        _ -> ok
    end.
