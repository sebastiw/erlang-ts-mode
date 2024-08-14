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
    pipe(erls(R),
         [fun compiler_map/1,
          fun compiler_reduce/1,
          fun results/1]).

%% map each file to a process. Also add all our ebins to the path so
%% we can find behaviours.
compiler_map({Root, Subjects}) ->
    io:fwrite("compiling: ~w files~n", [length(Subjects)]),
    lists:foreach(fun code:add_pathz/1, wildname([Root, '*', ebin])),
    #{t0 => millis_now(), workers => lists:map(mk_compile(Root), Subjects), results => []}.

mk_compile(Root) ->
    fun(S) -> {S, erlang:spawn_monitor(fun() -> exit({S, compile(Root, S)}) end)} end.

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
        true -> flat(" [~s]", [string:join([mod(F) || {F, _} <- Ws], ", ")]);
        false -> ""
    end.

%% We demand that the directory structure looks like this;
%% `Root/*/src/**/*.erl' Our parameter R must be absolute. It also
%% must be one of; 'Root', 'Root/A', 'Root/A/src'
%% 'Root/A/src/**/M.erl'.
%% We also look for 'X/src/../c_src'.

erls(R) ->
    try take_first(fun root_srcs/1, src_patterns(R))
    catch throw:nothing -> []
    end.

%% return `false' if there are no erls, otherwise a list of erls.
root_srcs(Pattern) ->
    case filelib:wildcard(Pattern) of
        [] -> throw(nothing);
        Erls -> {root(hd(Erls)), Erls++c_src(Erls)}
    end.

root(Erl) ->
    P = "^(/([a-zA-Z0-9_-]+/)+)[a-zA-Z0-9_]+/src/([a-zA-Z0-9_-]+/)*[a-zA-Z0-9_-]+.erl$",
    Opts = [global, {capture, all_but_first, list}],
    case re:run(Erl, P, Opts) of
        {match, [[Root|_]]} -> Root;
        nomatch -> []
    end.

%% We require a src to match 'Root/*/src/**/*.erl'.
%% R must be one of;
%%    'Root'
%%    'Root/A'
%%    'Root/A/src'
%%    'Root/A/src/M.erl'
%% Return a list of the 4 possible patterns.
src_patterns(R) ->
    [fnjoin([R, '*', src, '**', '*.erl']),
     fnjoin([R, src, '**', '*.erl']),
     fnjoin([R, '**', '*.erl']),
     R].

%% find c_src next to the erls, if there is any.
c_src(Erls) ->
    CsrcDirs = lists:foldl(fun c_src/2, [], Erls),
    [{dirname_basename(Csrc), wildname([Csrc, '**', "*.{c,cc}"])} || Csrc <- CsrcDirs].

c_src(Erl, O) ->
    Candidate = fnjoin([dirname_dirname(Erl), 'c_src']),
    case filelib:is_dir(Candidate) of
        true -> [Candidate|O];
        false -> O
    end.

%% teh compiler

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
    SOfile = fnjoin([Root, App, priv, SO]),
    filelib:ensure_dir(SOfile),
    GccStanza = gcc_stanza(SOfile, Flags, Csrcs),
    Cmd = flat("~s ; echo $?", [GccStanza]),
    case os:cmd(Cmd) of
        "0\n" -> ?RESULT(SOfile, "", "", [], [], Root, GccStanza);
        Err -> ?RESULT(SOfile, "", "", [lists:filter(fun(C)->C<128 end, Err)], [], Root, GccStanza)
    end.

gcc_stanza(SOfile, Flags, Csrcs) ->
    ErlUsr = fnjoin([code:root_dir(), usr]),
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

dirname_basename(F) ->
    filename:basename(filename:dirname(F)).

dirname_dirname(F, Suffix) ->
    fnjoin([dirname_dirname(F)|Suffix]).

dirname_dirname(F) ->
    filename:dirname(filename:dirname(F)).

wildname(Es) ->
    filelib:wildcard(fnjoin(Es)).

fnjoin(X) ->
    filename:join(X).

beamfile(Erl) ->
    dirname_dirname(Erl, [ebin, mod(Erl)++".beam"]).

mod(Erl) ->
    filename:basename(Erl, ".erl").

%% Call F(I) on each element in Is until we find an V = F(I) that does
%% not throw an exception. Return V or throw(nothing).
take_first(_, []) -> throw(nothing);
take_first(F, [P|Patterns]) ->
    try F(P)
    catch _:_ -> take_first(F, Patterns)
    end.

flat(F, As) ->
    lists:flatten(io_lib:format(F, As)).

millis_now() ->
    erlang:system_time(millisecond).

pipe(A0, Fs) ->
    lists:foldl(fun(F, A) -> F(A) end, A0, Fs).
