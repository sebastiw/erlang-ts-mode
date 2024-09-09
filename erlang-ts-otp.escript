#!/usr/bin/env escript

-mode(compile).

-compile({nowarn_unused_function, [dbg/1]}).
dbg({error, L, M, F, R}) -> error({L, M, F, R});
dbg({T, L, M, F, R}) -> io:fwrite(standard_error, "~n~p ~s:~s::~w ~p~n", [T, M, F, L, R]), R.
-define(DBG(Tag, X), dbg({Tag, ?LINE, ?MODULE, ?FUNCTION_NAME, X})).

main(Args) ->
    try handle(Args)
    catch C:R:S -> die(34, Args, C, R, S)
    end.

handle([]) ->
    io:fwrite("$0 otp VSN DEST- build OTP.", []);
handle(["otp", Vsn, Dest]) ->
    Site = "https://github.com",
    Proj = "erlang",
    Name = "otp",
    pipe(url_releases(Site, Proj, Name),
         [fun http_get/1,
          mk_newest_tgz(Site, Proj, Name, Vsn),
          fun http_get/1,
          mk_tar(Dest, Name),
          fun untar/1,
          fun configure/1,
          fun compile/1,
          fun install/1]).

url_releases(Site, Proj, Name) ->
    string:join([Site, Proj, Name, "releases"], "/").

mk_newest_tgz(Site, Proj, Name, Vsn) ->
    fun(Subj) -> newest_tgz(Subj, Site, Proj, Name, Vsn) end.

newest_tgz(Subj, Site, Proj, Name, Vsn) ->
    RE = flat("<a href=\"/~s/~s/releases/tag/(OTP-~s[^\"]+)\"", [Proj, Name, Vsn]),
    case re:run(Subj, RE, [{capture, all_but_first, list}, global]) of
        {match, [[T]|_]} ->
            io:fwrite("Found ~s.~n", [T]),
            flat("~s/~s/~s/archive/refs/tags/~s.tar.gz", [Site, Proj, Name, T]);
        Err ->
            error({release, {Site, Proj, Name, Err}})
    end.

http_get(Url) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    case httpc:request(get, {Url, []}, [{connect_timeout, 3000}, {autoredirect, true}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, B}} -> B;
        _Err -> error({get, {Url}})
    end.

mk_tar(Name, Dest) ->
    fun(B) -> tar(Name, Dest, B) end.

tar(Dest, Name, B) ->
    FN = filename:join([Dest, Name])++".tgz",
    filelib:ensure_dir(FN),
    ok = file:write_file(FN, B),
    io:fwrite("Wrote ~s~n", [FN]),
    FN.

untar(TarFile) ->
    Dir = filename:dirname(TarFile),
    Base = filename:basename(TarFile),
    case os_cmd(Dir, "tar -xzf ~s", [Base]) of
        {ok, []} ->
            file:delete(TarFile),
            {ok, [OTP]} = file:list_dir(Dir),
            io:fwrite("Untared ~p in ~s/~s~n", [TarFile, Dir, OTP]),
            filename:join([Dir, OTP]);
        Err ->
            error({untar, {Dir, Base, Err}})
    end.

configure(Dir) ->
    [Vsn|_] = lists:reverse(string:tokens(Dir, "/-")),
    Dest = filename:join([filename:dirname(Dir), "erl"++Vsn]),
    io:fwrite("Configuring in ~s with prefix ~s~n", [Dir, Dest]),
    case os_cmd(Dir, cfg(), [Dest]) of
        {ok, _} -> Dir;
        Err -> error({configure, Err})
    end.

cfg() ->
    string:join(
        ["./configure",
         "--prefix=\"~s\"",
         "--without-debugger",
         "--without-eldap",
         "--without-erl_docgen",
         "--without-et",
         "--without-ftp",
         "--without-hipe",
         "--without-javac",
         "--without-jinterface",
         "--without-megaco",
         "--without-observer",
         "--without-odbc",
         "--without-tftp",
         "--without-wx",
         "--without-dynamic-trace",
         "--disable-sctp",
         "--disable-lock-counter"],
        " ").

compile(Dir) ->
    io:fwrite("Compiling in ~s~n", [Dir]),
    case os_cmd(Dir, "make -j8", []) of
        {ok, _} -> Dir;
        Err -> error({compile, Err})
    end.

install(Dir) ->
    io:fwrite("Installing in ~s~n", [Dir]),
    case os_cmd(Dir, "make install", []) of
        {ok, _} -> file:del_dir_r(Dir);
        Err -> error({install, Err})
    end.

os_cmd(Dir, F, As) ->
    case lists:reverse(string:tokens(os:cmd(flat("cd ~s && "++F++" ; echo \"x$?x\"", [Dir|As])), "\n")) of
        ["x0x"|R] -> {ok, lists:reverse(R)};
        Err -> {err, Err}
    end.

flat(F, As) ->
    lists:flatten(io_lib:format(F, As)).

die(Code, Args, C, R, S) ->
    io:fwrite("error: ~s:~p (~p)~n~p~n", [C, R, Args, S]),
    halt(Code).

pipe(A0, Fs) ->
    lists:foldl(fun(F, A) -> F(A) end, A0, Fs).
