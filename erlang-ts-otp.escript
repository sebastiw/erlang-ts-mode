#!/usr/bin/env escript

-mode(compile).

-compile({nowarn_unused_function, [dbg/1]}).
dbg({error, L, M, F, R}) -> error({L, M, F, R});
dbg({T, L, M, F, R}) -> io:fwrite(standard_error, "~n~p ~s:~s::~w ~p~n", [T, M, F, L, R]), R.
-define(DBG(Tag, X), dbg({Tag, ?LINE, ?MODULE, ?FUNCTION_NAME, X})).

main(Args) ->
    try handle(Args)
    catch C:R:S -> die(34, flat("error: ~p~n~s:~p~n~p~n", [Args, C, R, S]))
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
          mk_tar(Dest, Vsn),
          fun untar/1,
          fun configure/1,
          fun compile/1,
          fun install/1]).

url_releases(Site, Proj, Name) ->
    string:join([Site, Proj, Name, "releases"], "/").

mk_newest_tgz(Site, Proj, Name, Vsn) ->
    fun(Subj) -> newest_tgz(Subj, Site, Proj, Name, Vsn) end.

newest_tgz(Subj, Site, Proj, Name, Vsn) ->
    RE = flat("<a href=\"/~s/~s/releases/tag/(OTP-~s[^\"]*)\"", [Proj, Name, Vsn]),
    case re:run(Subj, RE, [{capture, all_but_first, list}, global]) of
        {match, [[T]|_]} ->
            io:fwrite("Found ~s at ~s/~s/~s.~n", [T, Site, Proj, Name]),
            flat("~s/~s/~s/archive/refs/tags/~s.tar.gz", [Site, Proj, Name, T]);
        nomatch ->
            die(22, flat("no such release: ~s at ~s/~s/~s.", [Vsn, Site, Proj, Name]))
    end.

http_get(Url) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    case httpc:request(get, {Url, []}, [{connect_timeout, 3000}, {autoredirect, true}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, B}} -> B;
        _Err -> error({get, {Url}})
    end.

mk_tar(Dest, Vsn) ->
    fun(B) -> tar(Dest, Vsn, B) end.

tar(Dest, Vsn, B) ->
    FN = filename:join([Dest, Vsn])++".tgz",
    filelib:ensure_dir(FN),
    ok = file:write_file(FN, B),
    io:fwrite("Wrote ~s~n", [FN]),
    FN.

untar(TarFile) ->
    Dir = filename:dirname(TarFile),
    Base = filename:basename(TarFile),
    Vsn = filename:basename(Base, ".tgz"),
    case os_cmd(Dir, flat("tar -xzf ~s", [Base])) of
        {ok, []} ->
            file:delete(TarFile),
            [OTP] = filelib:wildcard(filename:join([Dir, flat("*-~s*", [Vsn])])),
            io:fwrite("Untared ~s in ~s~n", [TarFile, OTP]),
            OTP;
        Err ->
            error({untar, {Dir, Base, Err}})
    end.

configure(Dir) ->
    [Vsn|_] = lists:reverse(string:tokens(Dir, "/-")),
    Dest = filename:join([filename:dirname(Dir), "erl"++Vsn]),
    io:fwrite("Configuring in ~s with prefix ~s~n", [Dir, Dest]),
    case os_cmd(Dir, flat(cfg(), [Dest])) of
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
    case os_cmd(Dir, "make -j8") of
        {ok, _} -> Dir;
        Err -> error({compile, Err})
    end.

install(Dir) ->
    io:fwrite("Running install in ~s~n", [Dir]),
    Dest = filename:dirname(Dir),
    [Vsn|_] = lists:reverse(string:tokens(Dir, "-")),
    case os_cmd(Dir, "make install") of
        {ok, _} ->
            file:del_dir_r(Dir),
            [OTP] = filelib:wildcard(filename:join([Dest, flat("erl~s", [Vsn])])),
            io:fwrite("OTP installed in ~s.~n", [OTP]);
        Err ->
            error({install, Err})
    end.

os_cmd(Dir, Cmd) ->
    pipe(flat("cd ~s && ~s ; printf '\nx%dx\n' $?", [Dir, Cmd]),
         [fun os:cmd/1,
          fun(R) -> string:tokens(R, "\n") end,
          fun lists:reverse/1,
          fun os_cmd_res/1]).

os_cmd_res(["x0x"|R]) -> {ok, lists:reverse(R)};
os_cmd_res(Err) -> {err, Err}.

flat(F, As) ->
    lists:flatten(io_lib:format(F, As)).

die(Code, Reason) ->
    io:fwrite("~s~n", [Reason]),
    halt(Code).

pipe(A0, Fs) ->
    lists:foldl(fun(F, A) -> F(A) end, A0, Fs).
