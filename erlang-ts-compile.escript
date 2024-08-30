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
    pipe([],
         [fun(O) -> srcs(O, join([R, "*", wild_srcdirs(), "**", wild_basename()])) end,
          fun(O) -> srcs(O, join([R, wild_srcdirs(), "**", wild_basename()])) end,
          fun(O) -> srcs(O, join([assert_srcdir(R), "**", wild_basename()])) end,
          fun(O) -> srcs(O, join([assert_srcdir(R)])) end]).

assert_srcdir(R) ->
    RE = flat("^(~s)/(~s)/~s", [re_root(), re_alnum(), re_src()]),
    case regexp(R, RE) of
        {match, _} -> R;
        nomatch -> "NULL"
    end.

-record(srcs, {root, app, ext, srcs}).
-define(SRC(R, A, E, S), #srcs{root = R, app = A, ext = E, srcs = S}).

srcs(O, Wild) ->
    lists:foldl(fun filter_src/2, O, wildcard(Wild)).

filter_src(F, O) ->
    RE = flat("^(~s)/(~s)/~s(/~s)*/~s$", [re_root(), re_alnum(), re_src(), re_alnum(), re_basename()]),
    case regexp(F, RE) of
        {match, [[Root, _, App|_]|_]} -> [?SRC(Root, App, extension(F), F)|O];
        nomatch -> O
    end.

wild_srcdirs() ->
    flat("{~s}", [string:join(srcdirs(), ",")]).
wild_basename() ->
    flat("*.{~s}", [string:join(extensions(), ",")]).

re_basename() ->
    flat("~s\\.~s", [re_alnum(), re_ext()]).
re_ext() ->
    string:join(extensions(), "|").
re_src() ->
    string:join(srcdirs(), "|").
re_alnum() ->
    "[a-zA-Z0-9_-]+".
re_root() ->
    "(/[a-zA-Z0-9_-]+)+".


srcdirs() ->
    ["src", "c_src"].
extensions() ->
    ["erl", "xrl", "yrl", "c", "cc"].

regexp(R, RE) ->
    Opts = [global, {capture, all_but_first, list}],
    re:run(R, RE, Opts).

%% map each file to a process. Also add all our ebins to the path so
%% we can find behaviours.
compiler_map({Root, Subjects}) ->
    io:fwrite("compiling: ~w files~n", [length(Subjects)]),
    lists:foreach(fun code:add_pathz/1, wildcard([Root, '*', ebin])),
    #{t0 => millis_now(), workers => lists:map(mk_compile(Root), Subjects), results => []}.

mk_compile(Root) ->
    fun(S) -> {S, compiler_spawn(S, Root)} end.

compiler_spawn(S, Root) ->
    erlang:spawn_monitor(fun() -> exit({S, compile(Root, S)}) end).

%% reduce the compilation results. Print a progress report every Tick
%% seconds.
compiler_reduce(Z) ->
    timer:send_after(2000, tick),
    reduce(Z).

-define(DOWN(Pid, Ref, X), {'DOWN', Ref, process, Pid, X}).
reduce(#{workers := [], results := O}) -> O;
reduce(Z) when is_map(Z) ->
    receive
        tick -> reduce(tick(Z));
        ?DOWN(Pid, Ref, X) -> reduce(reduce_update({Pid, Ref}, X, Z))
    end.

-define(MAPS_UPDATE_WITH(K, V, F),
    fun(X) -> maps:update_with(K, fun(V) -> F end, X) end).
reduce_update(PidRef, {S, Data}, Z) ->
    pipe(Z,
         [?MAPS_UPDATE_WITH(workers, P, P--[{S, PidRef}]),
          ?MAPS_UPDATE_WITH(results, P, [Data|P])]).

tick(#{t0 := T0, workers := Ws, results := Rs} = X) ->
    timer:send_after(2000, tick),
    io:fwrite("working: ~w/~w (~w)~s~n", [length(Rs), length(Ws), duration(T0), workers(Ws)]),
    X.

duration(T0) ->
    (millis_now()-T0)/1000.

workers(Ws) ->
    case length(Ws) < 4 of
        true -> flat(" [~s]", [string:join(extract_srcs(Ws), ", ")]);
        false -> ""
    end.

extract_srcs(Ws) ->
    lists:map(fun extract_src/1, Ws).

extract_src({{Csrc, _}, _}) -> "gcc "++Csrc;
extract_src({Esrc, _}) -> mod(Esrc).

%% the compiler

-record(result, {module, erl, beam, errors, warnings, root, incs}).
-define(RESULT(Mod, Erl, Beam, Es, Ws, Root, Incs),
    #result{module = Mod, erl = Erl, beam = Beam, errors = Es, warnings = Ws, root = Root, incs = Incs}).
compile(Root, Csrcs) when is_tuple(Csrcs)->
    cc(Root, Csrcs);
compile(Root, Erl) ->
    Beam = beamfile(Erl),
    filelib:ensure_dir(Beam),
    case source_hash(Beam) =:= file_hash(Erl) of
        true -> ?RESULT({mod(Erl)}, Erl, Beam, [], [], Root, []);
        false -> compile(Root, Erl, Beam)
    end.

%% teh C compiler. Just runs gcc.

cc(Root, {"snappyer", Csrcs}) ->
    cc(Root, snappyer, "snappyer.so", "-std=c++11", Csrcs);
cc(Root, {"crc32cer", Csrcs}) ->
    cc(Root, crc32cer, "crc32cer_nif.so", "-std=gnu99 -finline-functions", Csrcs).

cc(Root, App, SO, Flags, Csrcs) ->
    SOfile = join([Root, App, priv, SO]),
    filelib:ensure_dir(SOfile),
    GccStanza = gcc_stanza(SOfile, Flags, Csrcs),
    Cmd = flat("~s ; echo $?", [GccStanza]),
    case os:cmd(Cmd) of
        "0\n" -> ?RESULT(SOfile, "", "", [], [], Root, GccStanza);
        Err -> ?RESULT(SOfile, "", "", [lists:filter(fun(C)->C<128 end, Err)], [], Root, GccStanza)
    end.

gcc_stanza(SOfile, Flags, Csrcs) ->
    ErlUsr = join([code:root_dir(), usr]),
    CC = "gcc -o ~s -shared -fpic -O3 ",
    Incs = " -I ~s/include -L ~s/lib -lei ",
    Srcs = lists:flatmap(fun(S) -> " "++S end, Csrcs),
    flat(CC++Incs++Flags++" ~s", [SOfile, ErlUsr, ErlUsr, Srcs]).

%% the erl compiler
compile(Root, Erl, Beam) ->
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
     {i, dir_dirname(Erl)},
     {i, dir_dirname(Erl, [include])},
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

results(Rs) ->
    pipe(Rs,
         [mk_results(success_cached),
          mk_results(success),
          mk_results(warnings),
          mk_results(errors),
          fun([]) -> done end]).

mk_results(W) ->
    fun(Rs) -> results(W, Rs) end.

results(W, Rs) ->
    {Prints, Rest} = lists:foldr(mk_split_result(W), {[], []}, Rs),
    result_write(W, Prints),
    Rest.

result_write(W, Prints) ->
    case {lists:member(W, [warnings, errors]), length(Prints)} of
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

result(Level, ?RESULT(Mod, Erl, _, Es, Ws, _, Incs)) ->
    case {Level, length(Es), length(Ws), Mod} of
        {success_cached, 0, 0, {M}}    -> io_lib:format("~s~n", [M]);
        {success, 0, 0, _}             -> io_lib:format("~s~n", [Mod]);
        {warnings, 0, W, _} when 0 < W -> io_lib:format("~s~n~p~n", [Erl, Ws]);
        {errors, E, _, _} when 0 < E   -> io_lib:format("~s~n~p~n~p~n", [Erl, Incs, Es]);
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

beamfile(Erl) ->
    dir_dirname(Erl, [ebin, mod(Erl)++".beam"]).

mod(Erl) ->
    filename:basename(Erl, ".erl").

extension(X) ->
    filename:extension(X).

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
