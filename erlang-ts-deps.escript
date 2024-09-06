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
    io:fwrite("$0 dep NAME URL - Fetch application NAME from URL.~n", []);
handle(["dep", Site, Proj, Name, Dest]) ->
    pipe(url_releases(Site, Proj, Name),
         [fun http_get/1,
          mk_newest_tgz(Site, Proj, Name),
          fun http_get/1,
          fun zlib:gunzip/1,
          mk_tar(Name),
          mk_writer(Name, Dest),
          fun(X) -> io:fwrite("Wrote ~p files to ~s/~s.~n", [X, Dest, Name]) end]).

url_releases(Site, Proj, Name) ->
    string:join([Site, Proj, Name, "releases"], "/").

mk_newest_tgz(Site, Proj, Name) ->
    fun(Subj) -> newest_tgz(Subj, Site, Proj, Name) end.

newest_tgz(Subj, Site, Proj, Name) ->
    RE = flat("<a href=\"/~s/~s/releases/tag/([v0-9.]+)", [Proj, Name]),
    case re:run(Subj, RE, [{capture, all_but_first, list}, global]) of
        {match, [[Tag]|_]} -> flat("~s/~s/~s/archive/refs/tags/~s.tar.gz", [Site, Proj, Name, Tag]);
        Err -> error({release, {Site, Proj, Name, Err}})
    end.

http_get(Url) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    case httpc:request(get, {Url, []}, [{connect_timeout, 3000}, {autoredirect, true}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, B}} -> B;
        _Err -> error({get, {Url}})
    end.

mk_tar(Name) ->
    RE = mk_regexp(flat("^~s-[0-9.]*/(src|priv|LICENSE|README)", [Name])),
    fun(B) -> tar(RE, B) end.

tar(RE, B) ->
    {ok, Files} = erl_tar:table({binary, B}),
    erl_tar:extract({binary, B}, [memory, {files, lists:filter(RE, Files)}]).

mk_regexp(RE) ->
    fun(Subject) -> nomatch =/= re:run(Subject, RE) end.

mk_writer(Name, Dest) ->
    fun({ok, Files}) -> lists:foldl(mk_write(Name, Dest), 0, Files) end.

mk_write(Name, Dest) ->
    fun({Filename, Text}, N) -> write(filename(Dest, Name, Filename), Text), N+1 end.

filename(Dest, Name, Filename) ->
    filename:join([Dest, Name|tl(string:tokens(Filename, "/"))]).

write(Filename, Text) ->
    try filelib:ensure_dir(Filename), ok = file:write_file(Filename, Text)
    catch C:R -> error(write, {Filename, C, R})
    end.

flat(F, As) ->
    lists:flatten(io_lib:format(F, As)).

die(Code, Args, C, R, S) ->
    io:fwrite("error: ~s:~p (~p)~n~p~n", [C, R, Args, S]),
    halt(Code).

pipe(A0, Fs) ->
    lists:foldl(fun(F, A) -> F(A) end, A0, Fs).
