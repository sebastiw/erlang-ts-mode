#!/usr/bin/env escript

-mode(compile).

main(Args) ->
    redbug([]),
    try handle(Args)
    catch C:R:S -> die(34, Args, C, R, S)
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
    AppSrcPattern = join([Src, "*/src/*.app.src"]),
    AppPattern = join([Src, "*/ebin/*.app"]),
    AppFiles = wildcard(AppSrcPattern)++wildcard(AppPattern),
    lists:usort(lists:map(fun dir_dirname/1, AppFiles)).

mk_do_app(Dest, Cfg) ->
    fun(AppFile, N) -> do_app(AppFile, Dest, Cfg), N+1 end.

do_app(AppDir, Dest, Cfg) ->
    DestAppDir = join([Dest, basename(AppDir)]),
    Srcs = srcs(AppDir, DestAppDir, Cfg),
    extra(c_src, AppDir, DestAppDir, Cfg),
    extra(include, AppDir, DestAppDir, Cfg),
    extra(priv, AppDir, DestAppDir, Cfg),
    appfile(AppDir, Srcs, DestAppDir).

srcs(AppDir, DestAppDir, Cfg) ->
    Srcs = wildcard(join([AppDir, src, "**", "*"])),
    DestSrcDir = join([DestAppDir, src]),
    lists:map(mk_op(DestSrcDir, none, Cfg), Srcs).

extra(Dir, AppDir, DestAppDir, Cfg) ->
    SrcPrefix = join([AppDir, Dir]),
    Xs = wildcard(join([SrcPrefix, "**", "*"])),
    DestDir = join([DestAppDir, Dir]),
    lists:map(mk_op(DestDir, SrcPrefix, Cfg), Xs).

mk_op(Dest, Prefix, Cfg) ->
    fun(Src) -> op(Cfg, Src, add_suffix(Src, Dest, Prefix)) end.

add_suffix(Src, Dest, Prefix) ->
    join([Dest, path_suffix(Src, Prefix)]).

path_suffix(_, none) -> "";
path_suffix(Src, Prefix) ->
    case dirname(string:prefix(Src, Prefix)) of
        "/"++S -> S;
        S -> error({prefix, Src, Prefix, S})
    end.

appfile(AppDir, Srcs, DestDir) ->
    AppSrcFileName = wildcard(join([AppDir, src, "*.app.src"])),
    AppFileName = wildcard(join([AppDir, ebin, "*.app"])),
    DestEbin = join([DestDir, ebin]),
    case {is_regular(AppSrcFileName), is_regular(AppFileName)} of
        {false, false} ->
            error({neither, AppSrcFileName, AppFileName});
        {true, true} ->
            error({twins, AppSrcFileName, AppFileName});
        {false, true} ->
            DestAppFileName = join([DestEbin, basename(AppFileName)]),
            op(cp, AppFileName, DestAppFileName);
        {true, false} ->
            DestAppFileName = join([DestEbin, basename(AppSrcFileName, ".src")]),
            {Aname, Adesc} = app_src(AppSrcFileName),
            A = {application, Aname, app_items(Adesc, Srcs)},
            D = iolist_to_binary(io_lib:format("~p.~n", [A])),
            case ensure_dir(DestAppFileName) andalso op(write, DestAppFileName, D) of
                ok -> ok;
                Err -> error({DestAppFileName, Err})
            end
    end.

app_src(F) ->
    case file:consult(F) of
        {ok, [{application, Aname, Adescr}]} -> {Aname, Adescr};
        Err -> error({app_src_fail, {F, Err}})
    end.

app_items(Adescr, Srcs) ->
    Mods = lists:foldl(fun filename_to_mod/2, [], Srcs),
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
    case extension(Src) of
        ".erl" -> [list_to_atom(basename(Src, ".erl"))|O];
        _ -> O
    end.

%% SRC is a FQ filename, DESTDIR is a dirname.
op(#{op := Op}, Src, DestDir) ->
    Dest = join([DestDir, basename(Src)]),
    case is_regular(Src) andalso ensure_dir(Dest) andalso op(Op, Src, Dest) of
        ok -> basename(Src);
        _ -> skip
    end;
op(cp, Src, Dest) -> element(1, file:copy(Src, Dest));
op(ln, Src, Dest) -> file:make_symlink(Src, Dest);
op(write, Dest, X) -> file:write_file(Dest, X).

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
