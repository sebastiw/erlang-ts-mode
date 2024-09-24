#!/usr/bin/env escript

-mode(compile).

-compile({nowarn_unused_function, [dbg/1]}).
dbg({error, L, M, F, R}) -> error({L, M, F, R});
dbg({T, L, M, F, R}) -> io:fwrite(standard_error, "~n~p ~s:~s::~w ~p~n", [T, M, F, L, R]), R.
-define(DBG(Tag, X), dbg({Tag, ?LINE, ?MODULE, ?FUNCTION_NAME, X})).

main(Args) ->
    try handle(Args)
    catch C:R:S -> die(34, Args, C, {R, S})
    end.

handle([]) ->
    io:fwrite("$0 dep github ORG PROJ VSN DEST~n"
              "$0 dep hex PROJ VSN DEST~n"
              "  Fetch dependency PROJ from SITE and install it in DEST.~n", []);
handle(["dep", "github", Org, Proj, Vsn, Dest]) ->
    handle(Proj, url_github(Org, Proj, Vsn), Dest);
handle(["dep", "hex", Proj, Vsn, "", Dest]) ->
    handle(["dep", "hex", Proj, Vsn, Dest]);
handle(["dep", "hex", Proj, Vsn, Dest]) ->
    handle(Proj, url_hex(Proj, Vsn), Dest).

url_hex(Package, Vsn) ->
    flat("https://repo.hex.pm/tarballs/~s-~s.tar", [Package, Vsn]).

url_github(Org, Proj, V) ->
    url_github(flat("https://github.com/~s/~s", [Org, Proj]), V).

url_github(Prefix, V) ->
    case github_type(V) of
        {ref, X} ->  flat("~s/archive/~s.tar.gz", [Prefix, X]);
        {tag, X} -> flat("~s/archive/refs/tags/~s.tar.gz", [Prefix, X]);
        {branch, X} -> flat("~s/archive/refs/heads/~s.tar.gz", [Prefix, X])
    end.

github_type({K, V}) -> {K, V};
github_type(R) when length(R) == 40 -> {ref, R};
github_type(X) -> error({confusing_github_string, X}).

handle(Proj, URL, Dest) ->
    D = filename:join(Dest, Proj),
    pipe(URL,
         [fun http_get/1,
          mk_uncompress(filename:extension(URL)),
          mk_tar(),
          mk_writer(Proj, Dest),
          fun(X) -> io:fwrite("Wrote ~p files to ~s.~n", [X, D]) end]).

http_get(Url) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    HttpOpts = [{connect_timeout, 3000}, {autoredirect, true}],
    Opts = [{body_format, binary}],
    case httpc:request(get, {Url, []}, HttpOpts, Opts) of
        {ok, {{_, 200, _}, _, B}} -> B;
        _Err -> error({get, {Url}})
    end.

mk_uncompress(".gz") -> fun zlib:gunzip/1;
mk_uncompress(".tar") -> fun unhex/1.

unhex(X) ->
    Filter = fun(F) -> F=="contents.tar.gz" end,
    {ok, [{"contents.tar.gz", B}]} = tar(Filter, X),
    zlib:gunzip(B).

mk_tar() ->
    Filter = fun(F) -> maybe_strip(F) =/= false end,
    fun(B) -> tar(Filter, B) end.

tar(Filter, B) ->
    {ok, Files} = erl_tar:table({binary, B}),
    erl_tar:extract({binary, B}, [memory, {files, lists:filter(Filter, Files)}]).

mk_writer(Proj, Dest) ->
    fun({ok, Files}) -> lists:foldl(mk_write(Proj, Dest), 0, Files) end.

mk_write(Proj, Dest) ->
    fun({Filename, Text}, N) -> write(filename(Dest, Proj, Filename), Text), N+1 end.

filename(Dest, Proj, Filename) ->
    filename:join([Dest, Proj | maybe_strip(Filename)]).

maybe_strip(Filename) ->
    [X|R] = string:tokens(Filename, "/"),
    case {tar_target(X), R} of
        {true, []} -> [X];
        {false, []} -> false;
        {true, R} -> [X|R];
        {false, R} -> maybe_strip(filename:join(R))
    end.

%% match the files we want
tar_target(X) ->
    RE = "\\A((c_src|src|priv|include)|(rebar.config(.src)?|LICENSE|README(\.md)?))\\z",
    nomatch =/= re:run(X, RE).

write(Filename, Text) ->
    try filelib:ensure_dir(Filename), ok = file:write_file(Filename, Text)
    catch C:R:S -> error({write, {Filename, C, R, S}})
    end.

flat(F, As) ->
    lists:flatten(io_lib:format(F, As)).

die(Code, Args, C, R) ->
    io:fwrite("~s: ~p~n~p~n", [C, Args, R]),
    halt(Code).

pipe(A0, Fs) ->
    lists:foldl(fun(F, A) -> F(A) end, A0, Fs).
