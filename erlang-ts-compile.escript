#!/usr/bin/env escript

-mode(compile).

%% debugging
-compile({nowarn_unused_function, [dbg/1]}).
dbg({error, L, M, F, R}) -> error({L, M, F, R});
dbg({T, L, M, F, R}) -> io:fwrite(standard_error, "~n~p ~s:~s::~w ~p~n", [T, M, F, L, R]), R.
-define(DBG(Tag, X), dbg({Tag, ?LINE, ?MODULE, ?FUNCTION_NAME, X})).

main(Args) ->
    case Args of
        [] -> io:fwrite("$0 compile SRC - compile SRC, or all files under SRC.~n", []);
        ["compile", R] -> compile_all(R);
        _ -> main([])
    end.

%% We compile in parallel; each file is compiled in a separate process
%% (a.k.a. map-reduce). Trying to figure out the dependency grph is
%% silly; if the compilation fails due to a missing dependency we
%% defer and recompile when the dependency appears.

compile_all(R) ->
    pipe(R,
         [fun absname/1,
          fun srcs/1,
          fun compiler_map/1,
          fun compiler_reduce/1,
          fun results/1]).

%% Return reified (without '..' and '.') absolute filename.

absname(R) ->
    pipe(R,
         [fun filename:absname/1,
          fun(X) -> string:tokens(X, "/") end,
          fun(X) -> lists:foldl(fun reify_name/2, [], X) end,
          fun lists:reverse/1,
          fun lists:flatten/1]).

reify_name(".", Es)  -> Es;
reify_name("..", Es) -> tl(Es);
reify_name(E, Es)    -> ["/"++E|Es].

%% We demand that the directory structure looks like this;
%% ROOT/APP/SRC/**/*.EXT. Our parameter R is an absolute filename,
%% either ROOT, ROOT/APP, ROOT/APP/SRC, or ROOT/APP/SRC/*.EXT.
%% SRC is src | c_src.
%% EXT is erl | xrl | yrl | c | cc.

srcs(R) ->
    pipe(#{},
         [fun(O) -> srcs(O, [R, "*", wild_srcdirs(), "**", wild_basename()]) end,
          fun(O) -> srcs(O, [R, wild_srcdirs(), "**", wild_basename()]) end,
          fun(O) -> srcs(O, [assert_srcdir(R), "**", wild_basename()]) end,
          fun(O) -> srcs(O, [assert_srcdir(R)]) end]).

assert_srcdir(R) ->
    RE = flat("^((/~s)+)/(~s)/~s", [re_alnum(), re_alnum(), re_src()]),
    case regexp(R, RE) of
        {match, _} -> R;
        nomatch -> "NULL"
    end.

srcs(O, Wild) ->
    lists:foldl(fun filter_src/2, O, wildcard(Wild)).

filter_src(F, O) ->
    RE = flat("^((/~s)+)/(~s)/(~s)(/~s)*/(~s)\\.(~s)$",
              [re_alnum(), re_alnum(), re_src(), re_alnum(), re_alnum(), re_ext()]),
    case regexp(F, RE) of
        {match, [[Root, _, App, _, [], _, Ext]]} -> store_src(Root, App, Ext, F, O);
        nomatch -> O
    end.

store_src(Root, App, Ext, F, O0) ->
    pipe(O0,
         [fun(O) -> maps:update_with({App, Ext}, fun(Fs) -> [F|Fs] end, [F], O) end,
          fun(O) -> maps:update_with(root, fun(R) -> R = Root end, Root, O) end]).

wild_srcdirs() ->
    flat("{~s}", [string:join(srcdirs(), ",")]).
wild_basename() ->
    flat("*.{~s}", [string:join(extensions(), ",")]).

re_ext() ->
    string:join(extensions(), "|").
re_src() ->
    string:join(srcdirs(), "|").
re_alnum() ->
    "[a-zA-Z0-9_.-]+".

srcdirs() ->
    ["src", "c_src"].
extensions() ->
    ["erl", "xrl", "yrl", "c", "cc"].

regexp(R, RE) ->
    Opts = [global, {capture, all_but_first, list}],
    re:run(R, RE, Opts).

%% map each file to a process. Also add all our ebins to the path so
%% we can find behaviours.
compiler_map(Srcs0) ->
    {Root, Srcs} = maps:take(root, Srcs0),
    lists:foreach(fun code:add_pathz/1, wildcard([Root, '*', ebin])),
    #{t0 => millis_now(), workers => progress(workers(Root, Srcs)), results => []}.

progress(Ws) ->
    io:fwrite("compiling: ~w files~n", [maps:size(Ws)]),
    Ws.

workers(Root, Srcs) ->
    maps:fold(mk_compile(Root), #{}, Srcs).

mk_compile(Root) ->
    fun({App, Ext}, Srcs, O) -> compiler_spawn(Root, App, Ext, Srcs, O) end.

compiler_spawn(Root, App, Ext, Srcs, O) ->
    SPAWN = mk_compiler_spawn(Root, App, Ext),
    case compiler_batchp(Ext) of
        true -> SPAWN(Srcs, O);
        false -> lists:foldl(SPAWN, O, Srcs)
    end.

mk_compiler_spawn(Root, App, Ext) ->
    fun(S, O) -> O#{compiler_spawn(Root, App, Ext, S) => worker_tag(S, App, Ext)} end.

%% We batch compile (e.g. "gcc a.c b.c d.c") .c and .cc files.

compiler_batchp(Ext) ->
    lists:member(Ext, ["c", "cc"]).

%% If we do batch compilation (e.g. "gcc a.c b.c d.c"), SRCS is a list
%% of strings. Otherwise, it's a string (a filename).

worker_tag(Srcs, App, Ext) when is_integer(hd(hd(Srcs))) ->
    {App, Ext};
worker_tag(S, App, _) when hd(S) =:= $/ ->
    {App, basename(S)}.

compiler_spawn(Root, App, Ext, S) ->
    erlang:spawn_monitor(fun() -> exit(compile(Root, App, Ext, S)) end).

%% reduce the compilation results. Print a progress report every Tick
%% seconds.
compiler_reduce(Z) ->
    timer:send_after(2000, tick),
    reduce(Z).

-define(DOWN(Pid, Ref, X), {'DOWN', Ref, process, Pid, X}).
reduce(#{workers := Ws, results := O}) when map_size(Ws) =:= 0 -> O;
reduce(Z) when is_map(Z) ->
    receive
        tick -> reduce(tick(Z));
        ?DOWN(Pid, Ref, X) -> reduce(reduce_update({Pid, Ref}, X, Z))
    end.

-define(MAPS_UPDATE_WITH(K, V, F),
    fun(_X) -> maps:update_with(K, fun(V) -> F end, _X) end).
reduce_update(PidRef, Result, X) ->
    pipe(X,
         [?MAPS_UPDATE_WITH(workers, P, maps:remove(PidRef, P)),
          ?MAPS_UPDATE_WITH(results, Results, [Result|Results])]).

tick(#{t0 := T0, workers := Ws, results := Rs} = X) ->
    timer:send_after(2000, tick),
    io:fwrite("working: ~w/~w (~w)~s~n", [length(Rs), maps:size(Ws), duration(T0), workers(Ws)]),
    X.

duration(T0) ->
    (millis_now()-T0)/1000.

workers(Ws) ->
    case maps:size(Ws) < 4 of
        true -> flat(" [~p]", [maps:values(Ws)]);
        false -> ""
    end.

%% the compiler

-record(result, {module, erl, beam, error, warning, incs}).
-define(RESULT(M, E, B, Es, Ws, Is),
    #result{module = M, erl = E, beam = B, error = Es, warning = Ws, incs = Is}).

compile(Root, App, "c", Srcs) ->
    cc(Root, App, Srcs);
compile(Root, App, "cc", Srcs) ->
    cc(Root, App, Srcs);
compile(Root, App, "erl", Erl) ->
    erlc(Root, App, Erl);
compile(Root, App, "xrl", Erl) ->
    xrlc(Root, App, Erl);
compile(Root, App, "yrl", Erl) ->
    yrlc(Root, App, Erl).

%% the C compiler. Just runs gcc.

cc(Root, "snappyer", Csrcs) ->
    cc(Root, snappyer, "snappyer.so", "-std=c++11", Csrcs);
cc(Root, "crc32cer", Csrcs) ->
    cc(Root, crc32cer, "crc32cer_nif.so", "-std=gnu99 -finline-functions", Csrcs).

cc(Root, App, SO, Flags, Csrcs) ->
    SOfile = join([Root, App, priv, SO]),
    filelib:ensure_dir(SOfile),
    case check_hashes(SOfile, Csrcs) orelse run_cc(SOfile, App, Flags, Csrcs) of
        true -> ?RESULT(App, Csrcs, SOfile, cached, [], []);
        R = #result{error = []} -> write_hashes(Csrcs, SOfile), R;
        R = #result{error = [_|_]} -> rm_hashes(SOfile), R
    end.

run_cc(SOfile, App, Flags, Csrcs) ->
    GccStanza = gcc_stanza(SOfile, Flags, Csrcs),
    Cmd = flat("~s ; echo $?", [GccStanza]),
    case os:cmd(Cmd) of
        "0\n" -> ?RESULT(App, Csrcs, SOfile, [], [], Flags);
        Err -> ?RESULT(App, Csrcs, SOfile, [lists:filter(fun(C)->C<128 end, Err)], [], Flags)
    end.

gcc_stanza(SOfile, Flags, Csrcs) ->
    ErlUsr = join([code:root_dir(), usr]),
    CC = "gcc -o ~s -shared -fpic -O3 ",
    Incs = " -I ~s/include -L ~s/lib -lei ",
    Srcs = lists:flatmap(fun(S) -> " "++S end, Csrcs),
    flat(CC++Incs++Flags++" ~s", [SOfile, ErlUsr, ErlUsr, Srcs]).

%% the xrl (leex) compiler
xrlc(Root, App, Xrl) ->
    Mod = mod(Xrl, ".xrl"),
    Beam = join([Root, App, ebin, Mod])++".beam",
    Tmp = join([Root, App, ebin, Mod])++".erl",
    filelib:ensure_dir(Tmp),
    case leex:file(Xrl, [{scannerfile, Tmp}, {report, false}, deterministic, return]) of
        {error, Es, Ws} -> ?RESULT(Mod, Xrl, Beam, Es, Ws, []);
        {ok, Tmp, []} -> rm_erl(erlc(Root, Mod, Beam, Tmp, file_hash([Xrl])));
        {ok, Tmp, Ws} -> ?RESULT(Mod, Xrl, Beam, [], Ws, [])
    end.

%% the yrl (yecc) compiler
yrlc(Root, App, Yrl) ->
    Mod = mod(Yrl, ".yrl"),
    Beam = join([Root, App, ebin, Mod])++".beam",
    Tmp = join([Root, App, ebin, Mod])++".erl",
    filelib:ensure_dir(Tmp),
    case yecc:file(Yrl, [{parserfile, Tmp}, {report, false}, deterministic, return]) of
        {error, Es, Ws} -> ?RESULT(Mod, Yrl, Beam, Es, Ws, []);
        {ok, Tmp, []} -> rm_erl(erlc(Root, Mod, Beam, Tmp, file_hash([Yrl])));
        {ok, Tmp, Ws} -> ?RESULT(Mod, Yrl, Beam, [], Ws, [])
    end.

%% util to remove temporary (generated) erl files.
rm_erl(Result) ->
    file:delete(Result#result.erl),
    Result.

%% the erl compiler
erlc(Root, App, Erl) ->
    Mod = mod(Erl, ".erl"),
    Beam = join([Root, App, ebin, Mod])++".beam",
    erlc(Root, Mod, Beam, Erl, file_hash([Erl])).

erlc(Root, Mod, Beam, Erl, SrcHash) ->
    filelib:ensure_dir(Beam),
    case source_hash(Beam) =:= SrcHash of
        true ->
            ?RESULT(Mod, Erl, Beam, cached, [], []);
        false ->
            Incs = incs(Root, Erl),
            case pre_erlc(Erl, Incs) of
                [] -> do_erlc(Mod, Erl, SrcHash, Beam, Incs);
                Es -> ?RESULT(Mod, Erl, Beam, Es, [], Incs)
            end
    end.

do_erlc(Mod, Erl, SrcHash, Beam, Incs) ->
    Opts = opts(SrcHash, Incs),
    case compile:file(Erl, Opts) of
        {ok, Mod, Bin}     -> write(?RESULT(Mod, Erl, Beam, [], [], Incs), Bin);
        {ok, Mod, Bin, Ws} -> write(?RESULT(Mod, Erl, Beam, [], unroll_reports(Ws), Incs), Bin);
        {error, Es, Ws}    -> ?RESULT(Mod, Erl, "", unroll_reports(Es), unroll_reports(Ws), Incs);
        error              -> ?RESULT(Mod, Erl, "", [], [], Incs)
    end.

pre_erlc(Erl, Incs) ->
    case compile:file(Erl, [basic_validation, return|Incs]) of
        {error, Es, Ws} -> pre_erlc_filter(Es++Ws);
        _ -> []
    end.

pre_erlc_filter(Is) ->
    pipe(Is,
         [fun unroll_reports/1,
          fun(Xs) -> lists:filtermap(fun pre_erlc_pred/1, Xs) end]).

pre_erlc_pred({_, erl_lint, {undefined_behaviour, B}})  -> {true, {behaviour, B}};
pre_erlc_pred({_, epp, {include, I}})                   -> {true, {include, I}};
pre_erlc_pred({_, epp, {include, lib, I}})              -> {true, {include, I}};
pre_erlc_pred({_, compile, {undef_parse_transform, M}}) -> {true, {parse_transform, M}};
pre_erlc_pred(_) -> false.

unroll_reports(Wrapped) ->
    lists:sort(lists:flatmap(fun({_, R}) -> R end, Wrapped)).

opts(SrcHash, Incs) ->
    [binary,
     {compile_info, cinf(SrcHash)},
     debug_info,
     deterministic,
     return|Incs].

incs(Root, Erl) ->
    [{i, Root},
     {i, dir_dirname(Erl)},
     {i, dir_dirname(Erl, [include])},
     {i, dirname(Erl)}].

cinf(SrcHash) ->
    [{compiler, 'erlang-ts'},
     {source_hash, SrcHash}].

file_hash(Files) ->
    erlang:phash2([file:read_file(F) || F <- Files]).

source_hash(Beam) ->
    try
        {ok, {_, [{_, Bin}]}} = beam_lib:chunks(Beam, ["CInf"]),
        {source_hash, Hash} = lists:keyfind(source_hash, 1, binary_to_term(Bin)),
        Hash
    catch
        _:_ -> undefined
    end.

check_hashes(Artefact, Srcs) ->
    case file:read_file(filename_hashes(Artefact)) of
        {ok, B} ->
            case regexp(B, "^([0-9]+) => ([0-9]+).$") of
                {match, [[H1, H2]]} -> compare_hash(H1, Srcs) andalso compare_hash(H2, [Artefact]);
                _ -> false
            end;
        _ -> false
    end.

compare_hash(Hash, Srcs) ->
    file_hash(Srcs) =:= list_to_integer(Hash).

write_hashes(Srcs, Artefact) ->
    Str = flat("~w => ~w.~n", [file_hash(Srcs), file_hash([Artefact])]),
    file:write_file(filename_hashes(Artefact), Str).

rm_hashes(Artefact) ->
    file:delete(filename_hashes(Artefact)).

filename_hashes(File) ->
    join([dirname(File), ".starc"]).

write(Result, Bin) ->
    case file:write_file(Result#result.beam, Bin) of
        ok -> Result;
        {error, Err} -> Result#result{error = [{write, Err}]}
    end.

results(Rs) ->
    pipe(Rs,
         [mk_results(success_cached),
          mk_results(success),
          mk_results(warning),
          mk_results(error),
          fun([]) -> done end]).

mk_results(W) ->
    fun(Rs) -> results(W, Rs) end.

results(W, Rs) ->
    {Prints, Rest} = lists:foldr(mk_split_result(W), {[], []}, Rs),
    result_write(W, Prints),
    Rest.

result_write(W, Prints) ->
    case {lists:member(W, [warning, error]), length(Prints)} of
        {_, 0}     -> ok;
        {true, _}  -> lists:foreach(fun(S) -> io:fwrite("~s: ~s~n", [W, S]) end, Prints);
        {false, L} -> io:fwrite("~s: ~w~n", [W, L])
    end.

mk_split_result(W) ->
    fun(R, {L1, L2}) -> split_result(W, R, L1, L2) end.

split_result(W, R, L1, L2) ->
    case result(W, R) of
        miss -> {L1, [R|L2]};
        V -> {[V|L1], L2}
    end.

result(Level, ?RESULT(Mod, _, _, Es, Ws, Incs)) ->
    case {Level, (Es == cached) orelse length(Es), length(Ws), length(Incs)} of
        {success_cached, true, _, _}   -> io_lib:format("~s~n", [Mod]);
        {success, 0, 0, _}             -> io_lib:format("~s~n", [Mod]);
        {warning, 0, W, _} when 0 < W -> io_lib:format("~s~n~p~n", [Mod, Ws]);
        {error, E, _, 0} when 0 < E   -> io_lib:format("~s~n~p~n", [Mod, Es]);
        {error, E, _, _} when 0 < E   -> io_lib:format("~s~n~p~n~p~n", [Mod, Incs, Es]);
        _ -> miss
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utils

dir_dirname(F, Suffix) ->
    join([dir_dirname(F)|Suffix]).

dir_dirname(F) ->
    dirname(dirname(F)).

dirname(X) ->
    filename:dirname(X).

basename(X) ->
    filename:basename(X).

mod(Erl, Ext) ->
    list_to_atom(filename:basename(Erl, Ext)).

wildcard(Es) ->
    filelib:wildcard(join(Es)).

join(X) ->
    filename:join(X).

flat(F, As) ->
    lists:flatten(io_lib:format(F, As)).

millis_now() ->
    erlang:system_time(millisecond).

pipe(A0, Fs) ->
    pipe(A0, null, Fs).

pipe(A0, Ctx, Fs) ->
    lists:foldl(mk_pipe(Ctx), A0, Fs).

mk_pipe(C) ->
    fun(F, A) -> pipe(F, A, C, erlang:fun_info(F, arity)) end.

pipe(F, A, _, {_, 1}) -> F(A);
pipe(F, A, C, {_, 2}) -> F(A, C).
