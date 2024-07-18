#!/usr/bin/env escript

-mode(compile).

main(Args) ->
    case Args of
        [] -> io:fwrite("$0 cp SRC DEST - copy all apps under SRC to DEST.~n", []);
        ["cp", Src, Dest] -> make_shadow(Src, Dest)
    end.

make_shadow(Src, Dest) ->
    Count = lists:foldl(mk_cp_app(Dest), 0, appdirs(Src)),
    io:fwrite("copied ~w apps from ~s to ~s.~n", [Count, Src, Dest]).

appdirs(Src) ->
    AppSrcPattern = filename:join(Src, "**/src/*.app.src"),
    AppPattern = filename:join(Src, "**/ebin/*.app"),
    AppFiles = filelib:wildcard(AppSrcPattern)++filelib:wildcard(AppPattern),
    lists:usort(lists:map(fun appdir/1, AppFiles)).

appdir(AppFile) ->
    filename:dirname(filename:dirname(AppFile)).

mk_cp_app(Dest) ->
    fun(AppFile, N) -> cp_app(AppFile, Dest), N+1 end.

cp_app(AppDir, Dest) ->
    DestAppDir = filename:join([Dest, filename:basename(AppDir)]),
    Srcs = srcs(AppDir, DestAppDir),
    c_srcs(AppDir, DestAppDir),
    incs(AppDir, DestAppDir),
    makefile(AppDir, DestAppDir),
    appfile(AppDir, Srcs, DestAppDir).

srcs(AppDir, DestAppDir) ->
    Srcs = filelib:wildcard(filename:join([AppDir, src, "*"])),
    DestSrcDir = filename:join([DestAppDir, src]),
    lists:map(mk_cp(DestSrcDir, none), Srcs).

incs(AppDir, DestAppDir) ->
    SrcPrefix = filename:join([AppDir, include]),
    Incs = filelib:wildcard(filename:join([SrcPrefix, "**", "*"])),
    DestIncDir = filename:join([DestAppDir, include]),
    lists:map(mk_cp(DestIncDir, SrcPrefix), Incs).

c_srcs(AppDir, DestAppDir) ->
    SrcPrefix = filename:join([AppDir, c_src]),
    Csrcs = filelib:wildcard(filename:join([SrcPrefix, "**", "*"])),
    DestCsrcDir = filename:join([DestAppDir, c_src]),
    lists:map(mk_cp(DestCsrcDir, SrcPrefix), Csrcs).

makefile(AppDir, DestAppDir) ->
    Makefile = filename:join([AppDir, 'Makefile']),
    case filelib:is_regular(Makefile) of
        true -> cp(Makefile, DestAppDir);
        false -> ok
    end.

mk_cp(Dest, Prefix) ->
    fun(Src) -> cp(Src, add_suffix(Src, Dest, Prefix)) end.

add_suffix(Src, Dest, Prefix) ->
    filename:join(Dest, path_suffix(Src, Prefix)).

path_suffix(_, none) -> "";
path_suffix(Src, Prefix) ->
    case filename:dirname(string:prefix(Src, Prefix)) of
        "/"++S -> S;
        S -> error({prefix, Src, Prefix, S})
    end.

%% SRC is a FQ filename, DESTDIR is a dirname.
cp(Src, DestDir) ->
    case filelib:is_regular(Src) of
        true ->
            SrcBaseName = filename:basename(Src),
            Dest = filename:join(DestDir, SrcBaseName),
            do_cp(Src, Dest);
        false ->
            skip
    end.

appfile(AppDir, Srcs, DestDir) ->
    AppSrcFileName = filelib:wildcard(filename:join([AppDir, src, "*.app.src"])),
    AppFileName = filelib:wildcard(filename:join([AppDir, ebin, "*.app"])),
    DestEbin = filename:join([DestDir, ebin]),
    case {filelib:is_regular(AppSrcFileName), filelib:is_regular(AppFileName)} of
        {false, false} ->
            error({neither, AppSrcFileName, AppFileName});
        {true, true} ->
            error({twins, AppSrcFileName, AppFileName});
        {false, true} ->
            DestAppFileName = filename:join([DestEbin, filename:basename(AppFileName)]),
            do_cp(AppFileName, DestAppFileName);
        {true, false} ->
            DestAppFileName = filename:join([DestEbin, filename:basename(AppSrcFileName, ".src")]),
            {ok, [{application, Aname, Adesc}]} = file:consult(AppSrcFileName),
            A = {application, Aname, app_items(Adesc, Srcs)},
            Descr = iolist_to_binary(io_lib:format("~p.~n", [A])),
            case ok == filelib:ensure_dir(DestAppFileName) andalso file:write_file(DestAppFileName, Descr) of
                ok -> ok;
                Err -> error({DestAppFileName, Err})
            end
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
            case re:run(Vsn, "^([a-z0-9]+|[0-9]+(\.[0-9])+)$") of
                nomatch -> AppDescr;
                _ -> [{vsn, "0.0.0"}|AD]
            end
    end.

mk_add_item(K, V) ->
    fun(X) -> lists:keystore(K, 1, X, {K, V}) end.

filename_to_mod(Src, O) ->
    case filename:extension(Src) of
        ".erl" -> [list_to_atom(filename:basename(Src, ".erl"))|O];
        _ -> O
    end.

do_cp(Src, Dest) ->
    case ok == filelib:ensure_dir(Dest) andalso file:copy(Src, Dest) of
        {ok, _} -> filename:basename(Src);
        Err -> error({copy, Src, Dest, Err})
    end.

pipe(S, Fs) ->
    lists:foldl(fun(F, Z) -> F(Z) end, S, Fs).
