#!/usr/bin/env escript

-mode(compile).

-compile({nowarn_unused_function, [dbg/1]}).
dbg({error, L, M, F, R}) -> error({L, M, F, R});
dbg({T, L, M, F, R}) -> io:fwrite(standard_error, "~n~p ~s:~s::~w ~p~n", [T, M, F, L, R]), R.
-define(DBG(Tag, X), dbg({Tag, ?LINE, ?MODULE, ?FUNCTION_NAME, X})).

main(Args) ->
    redbug([]),
    try handle(Args)
    catch C:R:S -> die(34, Args, C, R, S)
    end.

handle([]) ->
    io:fwrite("$0 cp SRC DEST - copy all apps under SRC to DEST.~n", []);
handle(["cp", Src, Dest]) ->
    make_shadow(Src, Dest, #{op => cp});
handle(["ln", Src, Dest]) ->
    make_shadow(Src, Dest, #{op => ln}).

make_shadow(Src, Dest, Cfg) ->
    Count = lists:foldl(mk_do_app(Dest, Cfg), 0, appdirs(Src)),
    io:fwrite("copied ~w apps from ~s to ~s.~n", [Count, Src, Dest]).

appdirs(Src) ->
    Ws = [join([Src|A++B]) || A <- [[], ['*']], B <- [[src, "*.app.src"], [ebin,  "*.app"]]],
    AppFiles = lists:flatmap(fun wildcard/1, Ws),
    lists:usort(lists:map(fun dir_dirname/1, AppFiles)).

mk_do_app(Dest, Cfg) ->
    fun(AppFile, N) -> do_app(AppFile, Dest, Cfg), N+1 end.

do_app(AppDir, Dest, Cfg) ->
    DestAppDir = join([Dest, basename(AppDir)]),
    Srcs = srcs(AppDir, DestAppDir, Cfg),
    extra(c_src, AppDir, DestAppDir, Cfg),
    extra(include, AppDir, DestAppDir, Cfg),
    extra(priv, AppDir, DestAppDir, Cfg),
    appfile(appfilename(AppDir), Srcs, DestAppDir).

srcs(AppDir, DestAppDir, Cfg) ->
    Srcs = wildcard(join([AppDir, src, "**", "*"])),
    DestSrcDir = join([DestAppDir, src]),
    lists:map(mk_op(DestSrcDir, none, Cfg), Srcs).

extra(Dir, AppDir, DestAppDir, Cfg) ->
    SrcPrefix = join([AppDir, Dir]),
    Xs = wildcard(join([SrcPrefix, "**", "*"])),
    DestDir = join([DestAppDir, Dir]),
    lists:map(mk_op(DestDir, SrcPrefix, Cfg), Xs).

mk_op(Dest, Prefix, #{op := Op}) ->
    fun(Src) -> op(Op, Src, add_suffix(Src, Dest, Prefix)) end.

add_suffix(Src, Dest, Prefix) ->
    join([Dest, path_suffix(Src, Prefix)]).

path_suffix(_, none) -> "";
path_suffix(Src, Prefix) ->
    case dirname(string:prefix(Src, Prefix)) of
        "/"++S -> S;
        S -> error({prefix, Src, Prefix, S})
    end.

appfilename(AppDir) ->
    AppSrcFileName = wildcard(join([AppDir, src, "*.app.src"])),
    AppFileName = wildcard(join([AppDir, ebin, "*.app"])),
    case {is_regular(AppSrcFileName), is_regular(AppFileName)} of
        {false, false} ->
            error({neither, AppSrcFileName, AppFileName});
        {true, true} ->
            error({twins, AppSrcFileName, AppFileName});
        {false, true} ->
            AppFileName;
        {true, false} ->
            AppSrcFileName
    end.

appfile(AppFileName, Srcs, DestDir) ->
    {AppName, AppDesc} = app_src(AppFileName),
    DestAppFileName = join([DestDir, ebin, AppName])++".app",
    A = {application, AppName, app_items(AppDesc, Srcs)},
    D = iolist_to_binary(io_lib:format("~p.~n", [A])),
    op(write, D, DestAppFileName).

app_src(F) ->
    case file:consult(F) of
        {ok, [{application, Aname, Adescr}]} -> {Aname, Adescr};
        Err -> error({app_src_fail, {F, Err}})
    end.

app_items(Adescr, Srcs) ->
    Mods = ?DBG(mods, lists:foldl(fun filename_to_mod/2, [], ?DBG(srcs, Srcs))),
    pipe(Adescr,
         [mk_add_item(modules, Mods),
          mk_add_item(registered, []),
          fun app_version/1]).

app_version(AppDescr) ->
    case lists:keytake(vsn, 1, AppDescr) of
        false ->  [{vsn, "0.0.0"}|AppDescr];
        {value, {vsn, Vsn}, AD} ->
            case catch re:run(Vsn, "^([a-z0-9]+|[0-9]+(\.[0-9])+)$") of
                nomatch -> AppDescr;
                _ -> [{vsn, "0.0.0"}|AD]
            end
    end.

mk_add_item(K, V) ->
    fun(X) -> lists:keystore(K, 1, X, {K, V}) end.

filename_to_mod(Src, O) ->
    [list_to_atom(basename(Src, extension(Src)))|O].

%% X is an iolist, DEST is a FQ filename.
op(write, X, Dest) ->
    iop(mkdir, Dest),
    iop(rm, Dest),
    iop(write, {X, Dest});
%% SRC is a FQ filename, DESTDIR is a dirname.
op(Op, Src, DestDir) ->
    Dest = join([DestDir, basename(Src)]),
    iop(regular, Src),
    iop(mkdir, Dest),
    iop(rm, Dest),
    iop(Op, {Src, Dest}).

%% IO primitives. Return a filename or exit.
iop(regular, File) ->
    case is_regular(File) of
        true -> File;
        false -> error({regular, File})
    end;
iop(mkdir, File) ->
    case ensure_dir(File) of
        true -> File;
        false -> error({mkdir, File})
    end;
iop(rm, File) ->
    try (false == filelib:is_file(File)) orelse (ok = file:delete(File)), File
    catch C:R -> error({rm, {File, C, R}})
    end;
iop(write, {X, Dest}) ->
    try ok = file:write_file(Dest, X), Dest
    catch C:R -> error(write, {Dest, C, R})
    end;
iop(cp, {Src, Dest}) ->
    try true = filelib:is_regular(Src), {ok, _} = file:copy(Src, Dest), Dest
    catch C:R -> error({cp, {Src, Dest, C, R}})
    end;
iop(ln, {Src, Dest}) ->
    try true = filelib:is_regular(Src), ok = file:make_symlink(Src, Dest), Dest
    catch C:R -> error({ln, {Src, Dest, C, R}})
    end.

pipe(S, Fs) ->
    lists:foldl(fun(F, Z) -> F(Z) end, S, Fs).

dir_dirname(File) ->
    dirname(dirname(File)).
join(X) ->
    filename:join(X).
basename(X, Y) ->
    filename:basename(X, Y).
basename(X) ->
    filename:basename(X).
dirname(X) ->
    filename:dirname(X).
extension(X) ->
    filename:extension(X).
wildcard(X) ->
    filelib:wildcard(X).
is_dir(X) ->
    filelib:is_dir(X).
is_regular(X) ->
    filelib:is_regular(X).
ensure_dir(X) ->
    case filelib:ensure_dir(X) of
        ok -> true;
        _ -> false
    end.

die(Code, Args, C, R, S) ->
    io:fwrite("error: ~s:~p (~p)~n~p~n", [C, R, Args, S]),
    halt(Code).

redbug([]) -> ok;
redbug(Rtps) ->
    D = join([os:getenv("HOME"), "git/redbug/_build/default/lib/redbug/ebin"]),
    case (not (Rtps=:=[])) andalso is_dir(D) of
        false -> ok;
        true ->
            code:add_patha(D),
            redbug:start(Rtps, #{msgs=>10000})
    end.
