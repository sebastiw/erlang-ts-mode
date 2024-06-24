#!/usr/bin/env escript

-mode(compile).

%% debugging
-compile({nowarn_unused_function, [dbg/1]}).
dbg({error, L, M, F, R}) -> error({L, M, F, R});
dbg({T, L, M, F, R}) -> io:fwrite(standard_error, "~n~p ~s:~s::~w ~p~n", [T, M, F, L, R]), R.
-define(DBG(Tag, X), dbg({Tag, ?LINE, ?MODULE, ?FUNCTION_NAME, X})).

%% lazy ternary, as it should be
-define('??'(X), (fun() -> X end)()).
-define('?'(C, A, B), case ?'??'(C) of true -> ?'??'(A); _ -> ?'??'(B) end).

main(Args) ->
    case Args of
        ["bifs"]        -> out(bifs());
        ["guards"]      -> out(guards());
        ["words"]       -> out(words());
        ["erls", Root]  -> out(erls(ex(Root)));
        ["funs", File]  -> out(parse(ex(File)));
        ["man", Mandir] -> out(pages(ex(Mandir)));
        ["name", File]  -> out(info(name, ex(File)));
        ["incs", File]  -> out(info(incs, ex(File)));
        ["libs", File]  -> out(info(libs, ex(File)));
        ["srcs", "otp"] -> out(info(srcs, otp()));
        ["srcs", File]  -> out(info(srcs, ex(File)));
        ["info", File]  -> io:fwrite("~p~n", [info(ex(File))])
    end.

%% print string or list of strings.
-define(IS_PRINTABLE(X), is_integer(hd(X)); is_atom(X)).
out([]) ->
    ok;
out({K, V}) ->
    io:fwrite("(:~s ", [K]), out(V), io:fwrite(")", []);
out(X) when ?IS_PRINTABLE(X) ->
    io:fwrite("~s~n", [X]);
out(Strs) when ?IS_PRINTABLE(hd(Strs)) ->
    lists:foreach(fun out/1, Strs).

format(Format, As) ->
    lists:flatten(io_lib:format(Format, As)).

%% path to otp
otp() ->
    code:root_dir().

words() ->
    keywords()++operators().

keywords() ->
    ["after", "begin", "catch", "case", "cond", "end", "fun", "if",
     "let", "of", "receive", "try", "maybe", "else", "when"].

operators() ->
    ["and", "andalso", "band", "bnot", "bor", "bsl", "bsr", "bxor",
     "div", "not", "or", "orelse", "rem", "xor"].

bifs() ->
    ["abs", "alias", "apply", "atom_to_binary", "atom_to_list",
     "binary_to_atom", "binary_to_existing_atom", "binary_to_float", "binary_to_integer",
     "binary_to_list", "binary_to_term", "binary_part", "bit_size", "bitstring_to_list",
     "byte_size", "ceil", "check_old_code", "check_process_code", "date", "delete_module",
     "demonitor", "disconnect_node", "element", "erase", "error", "exit", "floor",
     "float", "float_to_binary", "float_to_list", "garbage_collect", "get",
     "get_keys", "group_leader", "halt", "hd", "integer_to_list", "integer_to_binary",
     "iolist_size", "iolist_to_binary", "is_alive", "is_map_key", "is_process_alive",
     "length", "link", "list_to_atom", "list_to_binary", "list_to_bitstring",
     "list_to_existing_atom", "list_to_float", "list_to_integer", "list_to_pid",
     "list_to_port", "list_to_ref", "list_to_tuple", "load_module", "make_ref",
     "map_get", "map_size", "max", "min", "module_loaded", "monitor", "monitor_node",
     "node", "nodes", "now", "open_port", "pid_to_list", "port_close",
     "port_command", "port_connect", "port_control", "port_to_list", "pre_loaded",
     "process_flag", "process_info", "processes", "purge_module", "put",
     "ref_to_list", "register", "registered", "round", "self", "setelement",
     "size", "spawn", "spawn_link", "spawn_monitor", "spawn_opt", "spawn_request",
     "spawn_request_abandon", "split_binary", "statistics", "term_to_binary",
     "term_to_iovec", "time", "throw", "tl", "trunc", "tuple_size", "tuple_to_list",
     "unalias", "unlink", "unregister", "whereis"].



guards() ->
    ["is_atom", "is_binary", "is_bitstring", "is_boolean", "is_float", "is_function",
     "is_function", "is_integer", "is_list", "is_map", "is_number", "is_pid",
     "is_port", "is_record", "is_reference", "is_tuple", "abs", "bit_size", "byte_size",
     "element", "float", "hd", "is_map_key", "length", "map_get", "map_size", "max",
     "min", "node", "node", "round", "self", "size", "tl", "trunc", "tuple_size"].

%% select Key from info of File's project, or []
info(name, File) ->
    maps:get(name, info(File));
info(libs, File) ->
    ["libs:"|format_paths(libs, File)];
info(incs, File) ->
    ["incs:"|format_paths(incs, File)];
info(srcs, File) ->
    ["srcs:"|format_paths(srcs, File)].

format_paths(Key, File) ->
    lists:map(fun(P) -> format_path(P) end, maps:get(Key, info(File), [])).

format_path(P) ->
    format("  ~s", [P]).

%% get info of File
info(File) ->
    try root_finder(File)
    catch throw:Cfg -> lists:foldr(fun expand_paths/2, Cfg, [srcs, libs, incs])
    end.

%% we try various possible root dirs. The first match will throw an info.
root_finder(File) ->
    root_acer(File),
    root_otp(File),
    root_rebar(File),
    root_lib_src(File),
    root_src(File),
    root_single(File).

%% look for .acer above us.
root_acer(File) ->
    case acer_file(look_above(".acer", File)) of
        false -> false;
        Cfg -> throw(Cfg)
    end.

%% are we in otp?
root_otp(File) ->
    OTP = otp(),
    case lists:prefix(OTP, File) of
        false -> false;
        true -> throw(small_info(OTP, otp, ["lib/*/src", "lib/*/src/*"]))
    end.

%% is there a rebar.config above us?
root_rebar(File) ->
    case look_above("rebar.config*", File) of
        false -> false;
        Dir -> throw(small_info(Dir, filename:basename(Dir), ["src", "_build/*/lib/*/src"]))
    end.

%% are we in a R/lib/*/src/ tree?
root_lib_src(File) ->
    case lists:reverse(string:tokens(File, "/")) of
        [_, "src", _, "lib"|RRoot] ->
            Dir = filename:join(["/"|lists:reverse(RRoot)]),
            throw(small_info(Dir, filename:basename(Dir), ["lib/*/src", "lib/*/src/*"]));
        _ ->
            false
    end.

%% are we in a R/src/ tree?
root_src(File) ->
    Dir = filename:dirname(File),
    case lists:reverse(string:tokens(Dir, "/")) of
        ["src"|_] ->
            P = filename:dirname(Dir),
            throw(small_info(P, filename:basename(P), [Dir]));
        _ ->
            false
    end.

%% it's just a single file.
root_single(File) ->
    Dir = filename:dirname(File),
    throw(small_info(Dir, filename:basename(Dir), [Dir])).

small_info(Root, Name, Srcs) ->
    #{root => Root, name => Name, srcs => Srcs}.

%% handle the .acer file
acer_file(false) -> false;
acer_file(Dir) ->
    F = ex(filename:join(Dir, ".acer")),
    case filelib:is_regular(F) andalso file:consult(F) of
        {ok, [Cfg]} -> acer_file(Dir, Cfg);
        _ -> false
    end.

acer_file(Dir, Cfg) ->
    pipe(Cfg#{root => Dir},
         [fun acer_file_env/1,
          fun acer_file_name/1,
          fun(X) -> acer_file_dirs([srcs, incs, libs], X) end]).

%% `env' holds a proplist {Key, Val}, whare Val (a string) can contain
%% references to other (previously defined) {K, V}. We expand Val by
%% replacing references ("$K") with its value V. The output is a map
%% #{K => ExpandedV}. Note that the input (the KV proplist) is
%% ordered, and the expander will use previously expanded versions of
%% V. It's all rather similar to lisp's `let*'.
acer_file_env(Cfg) ->
    ExpandOneV = fun(V, KVs) -> acer_file_expand_env(V, Cfg#{env => KVs}) end,
    ExpandOneKV = fun({K, V}, KVs) -> KVs#{K => ExpandOneV(V, KVs)} end,
    ExpandAllKVs = fun(KVs) -> lists:foldl(ExpandOneKV, #{}, KVs) end,
    maps:update_with(env, ExpandAllKVs, #{}, Cfg).

%% If we get name from the user, use it. Otherwise, use basename of Root.
acer_file_name(#{root := Root} = Cfg) ->
    maps:merge(#{name => filename:basename(Root)}, Cfg).

%% we have a set of keys, and for each key we have a set of paths, and
%% each path is expanded.
acer_file_dirs(Keys, Cfg0) ->
    ExpandOne = fun(Path) -> acer_file_expand_env(Path, Cfg0) end,
    ExpandAll = fun(Paths) -> lists:map(ExpandOne, Paths) end,
    Update = fun(Key, Cfg) -> maps:update_with(Key, ExpandAll, [], Cfg) end,
    lists:foldl(Update, Cfg0, Keys).

%% Envs is a [{KEY, VAL}], where VAL can be a bash command ("$(CMD)")
%% or a string. The string can contain vars ("$KEY", where KEY matches
%% "[a-z0-9-_]*"); the vars will be replaced by their VAL.
acer_file_expand_env(Subj, #{root := Root} = Cfg) ->
    case {regexp(Subj, "^[$][(]([^)]*)[)]$"), regexp(Subj, "[$]([a-z0-9_-]*)")} of
        {[], [[]]} -> Subj;
        {[Cmd], [[]]} -> acer_file_sh(Root, Cmd);
        {[], Vars} -> lists:foldl(mk_acer_file_replace(Cfg), Subj, Vars)
    end.

acer_file_sh(WorkDir, Cmd) ->
    case sh(WorkDir, Cmd) of
        R when 0 < length(R) -> lists:last(R);
        R -> exit({sh, WorkDir, Cmd, R})
    end.

mk_acer_file_replace(#{env := Env}) ->
    fun(Var, Subj) -> replace(Subj, "[$]"++Var, acer_file_replacement(Var, Env)) end.

acer_file_replacement(Var, Env) ->
    maps:get(list_to_atom(Var), Env).

%% wrap os:cmd
sh(WorkingDir, Cmd) ->
    C = io_lib:format("cd ~s && ~s ; printf 'x%s\n' $?", [WorkingDir, Cmd]),
    case lists:reverse(os:cmd(C)) of
        "\n0x"++R -> string:tokens(lists:reverse(R), "\n");
        _ -> ""
    end.

%% wrap re:replace
replace(Subject, RE, Replacement) ->
    binary_to_list(iolist_to_binary(re:replace(Subject, RE, Replacement))).

%% wrap re:run
regexp(Subject, RE) ->
    case re:run(Subject, RE, [global, {capture, all_but_first, list}]) of
        {match, [Ms]} -> Ms;
        nomatch -> []
    end.

expand_paths(Key, #{root := Root} = Cfg) ->
    maps:update_with(Key, mk_expand_path(Root), [], Cfg).

mk_expand_path(Root) ->
    fun(Ps) -> dedupe_path(expand_path(Root, Ps)) end.

%% for each P in Paths, join Root and P and expand wildcards.
expand_path(Root, Paths) ->
    lists:filter(fun has_erls/1, lists:flatmap(mk_path_cand(Root), Paths)).

mk_path_cand(Dir) ->
    fun(P) -> filelib:wildcard(ex(filename:join(Dir, P))) end.

has_erls(D) ->
    case file:list_dir(D) of
        {ok, Fs} -> lists:any(fun is_erl/1, Fs);
        _ -> false
    end.

is_erl(F) ->
    lists:member(filename:extension(F), [".beam", ".app", ".erl", ".hrl"]).

%% dedupe a list of ["_/X/ebin", "_/Y/ebin"], looking at second-to-last element
dedupe_path(Paths) ->
    pipe(Paths,
         [fun dedupe_tag/1,
          fun dedupe_sort/1,
          fun dedupe_uniq/1,
          fun dedupe_untag/1]).

dedupe_tag(Ps) ->
    lists:map(fun(P) -> {filename:basename(filename:dirname(P)), P} end, Ps).

dedupe_sort(Ps) ->
    lists:keysort(1, Ps).

dedupe_uniq(Ps) ->
    lists:foldr(fun dedupe_uniq/2, [], Ps).

dedupe_uniq({A, _}, [{A, _}|_] = As) -> As;
dedupe_uniq(A, As) -> [A|As].

dedupe_untag(Ps) ->
    lists:map(fun({_, P}) -> P end, Ps).

%% Look for Target in path of File. Return Path or 'false'.
look_above(Target, File) ->
    Dir = ?'?'(filelib:is_dir(File), File, filename:dirname(File)),
    try lists:foldl(mk_look_above(Target), "/", string:tokens(Dir, "/")),
        false
    catch throw:D -> D
    end.

mk_look_above(Target) ->
    fun(E, Es) -> look_above(E, Es, Target) end.

look_above(E, Es, Target) ->
    Path = filename:join(Es, E),
    case filelib:wildcard(filename:join(Path, Target)) of
        [] -> Path;
        Xs when length(Xs) =< 2 -> throw(Path)
    end.

%% man pages
pages(Root) ->
    pipe(Root ++ "/*.3",
         [fun filelib:wildcard/1,
          fun(X) -> lists:foldl(fun page/2, #{}, X) end,
          fun(X) -> maps:fold(fun files_format/3, [], X) end,
          fun lists:sort/1]).

page(F, A) ->
    A#{filename:basename(F, ".3") => F}.

%% all relevant files in Root, deduped
erls(Root) ->
    case {filelib:is_file(Root), filelib:is_dir(Root)} of
        {false, false} -> [];
        {true, false} -> erls_in_dirs(maps:get(srcs, info(Root)));
        {true, true} -> erls_in_dir(Root)
    end.

erls_in_dirs(Dirs) ->
    lists:sort(lists:flatmap(fun erls_in_dir/1, Dirs)).

erls_in_dir(Dir) ->
    pipe(Dir ++ "/*.erl",
         [fun filelib:wildcard/1,
          fun(X) -> lists:foldl(fun files_filter/2, #{}, X) end,
          fun(X) -> maps:fold(fun files_format/3, [], X) end,
          fun lists:sort/1]).

files_format(K, V, A) ->
    [format("~s=>~s", [K, V])|A].

files_filter(F, A) ->
    M = filename:basename(F, ".erl"),
    case is_file_drop(M, F, A) of
        true -> A;
        false -> A#{M => F}
    end.

is_file_drop(M, F, A) ->
    is_file_plugin(F)
        orelse is_file_duplicated(M, F, A).

is_file_plugin(F) ->
    re:run(F, "/_build/[a-z0-9_-]+/plugins") =/= nomatch.

is_file_duplicated(M, F, A) ->
    case maps:is_key(M, A) of
        false -> false;
        _ -> is_file_in_build(F)
    end.

is_file_in_build(F) ->
    re:run(F, "/_build/") =/= nomatch.

%% reify filename
ex(File) ->
    pipe(filename:split(filename:absname(File)),
         [fun(Ps) -> lists:foldl(fun ex_path/2, [], Ps) end,
          fun lists:reverse/1,
          fun filename:join/1]).

ex_path(".", O) -> O;
ex_path("..", [_|T]) -> T;
ex_path(E, O) -> [E|O].

pipe(Subject, Fs) ->
    lists:foldl(fun(F, S) -> F(S) end, Subject, Fs).

%% parse file, and return all sigs.
parse(File) ->
    lists:sort(do_parse(File)).

do_parse(File) ->
    case lists:reverse(File) of
        "maeb."++_ -> walk(parse_get_ast_beam(File));
        "lre."++_ -> walk(parse_get_ast_erl(File));
        "3."++_ -> man(File);
        _ -> []
    end.

%% all functions in man page
man(Manpage) ->
    pipe(Manpage,
         [fun file:read_file/1,
          fun({ok, B}) -> re:split(B, ".B|.br") end,
          fun(X) -> lists:filtermap(mk_man_frag(Manpage), X) end]).

mk_man_frag(Manpage) ->
    Mod = filename:basename(Manpage, ".3"),
    fun(X) -> man_frag(Mod, X) end.

man_frag(Mod, X) ->
    RE = "\n(erlang:)?([a-zA-Z0-9_]+\\([^\\)]*\\)) ->",
    case re:run(X, RE, [{capture, all_but_first, list}]) of
        {match, [_, M]} ->
            [F, As] = re:split(re:replace(M, "\\\\&", "", [global]), "\\("),
            Arity = man_arity(M),
            {true, format("~s:~s/~w:0(~s", [Mod, F, Arity, As])};
        _ ->
            false
    end.

man_arity(Frag) ->
    case re:run(Frag, "\\([^\\)]*\\)", [{capture, first, list}]) of
        {match, ["()"]} -> 0;
        {match, As} -> length(re:split(As, ","))
    end.

parse_get_ast_beam(File) ->
    try {ok, {_, [{_, Blob}]}} = beam_lib:chunks(File, ["Dbgi"]),
        {_, _, {Forms, _}} = binary_to_term(Blob),
        Forms
    catch
        _:_ -> []
    end.

parse_get_ast_erl(File) ->
    try {ok, Tree} = epp_dodger:parse_file(File),
        Tree
    catch
        _:_ -> []
    end.

%% parse tree walker. Finds signatures of all exported functions.
walk(Forms) ->
    {Mod, _Exports, Sigs} = lists:foldl(fun walker/2, {undefined, [], []}, Forms),
    lists:map(fun(S) -> format_sig(Mod, S) end, Sigs).

format_sig(M, {{F, A, L}, As}) ->
    format("~s:~s/~w:~w(~s)", [M, F, A, L, string:join(As, ", ")]).

walker(Form, {M, Exports, Sigs}) ->
    case type(Form) of
        {module, Module} ->
            {Module, Exports, Sigs};
        {export, Es} ->
            {M, Exports++Es, Sigs};
        {function, Name, Args, Line} ->
            case is_exported(Name, Args, Exports) of
                true -> {M, Exports, add_sig(Name, Args, Line, Sigs)};
                false -> {M, Exports, Sigs}
            end;
        undefined ->
            {M, Exports, Sigs}
    end.

type(Form) ->
    case erl_syntax:type(Form) of
        attribute -> do_attribute(Form);
        function -> do_function(Form);
        error_marker -> undefined;
        eof_marker -> undefined
    end.

do_attribute(Form) ->
    case erl_syntax:atom_value(erl_syntax:attribute_name(Form)) of
        export -> {export, do_export_list(hd(erl_syntax:attribute_arguments(Form)))};
        module -> {module, erl_syntax:atom_value(hd(erl_syntax:attribute_arguments(Form)))};
        _ -> undefined
    end.

do_export_list(Tree) ->
    lists:map(fun do_aq/1, erl_syntax:list_elements(Tree)).

do_aq(AQ) ->
    Name = erl_syntax:atom_value(erl_syntax:arity_qualifier_body(AQ)),
    Arity = erl_syntax:integer_value(erl_syntax:arity_qualifier_argument(AQ)),
    {Name, Arity}.

do_function(Form) ->
    N = erl_syntax:function_name(Form),
    Name = erl_syntax:atom_value(N),
    Line = erl_syntax:get_pos(N),
    Args = do_clauses(erl_syntax:function_clauses(Form)),
    {function, Name, Args, Line}.
do_clauses(Form) ->
    lists:foldl(fun do_clause/2, [], Form).

do_clause(Form, Args) ->
    As = erl_syntax:clause_patterns(Form),
    case Args of
        [] -> [[A] || A <- As];
        _ -> lists:map(fun do_arg/1, lists:zip(Args, As))
    end.

do_arg({A1, A2}) ->
    [A2|A1].

is_exported(Name, Args, Exports) ->
    lists:member({Name, length(Args)}, Exports).

add_sig(Name, Args, Line, Sigs) ->
    Arity = length(Args),
    [{{Name, Arity, Line}, pick_args(Args)}|Sigs].

pick_args(Args) ->
    lists:map(fun pick_arg/1, Args).

pick_arg(Alternatives) ->
    lists:foldr(fun pick_alternative/2, "X", Alternatives).

pick_alternative(Alternative, Current) ->
    case is_generic(Current) andalso erl_syntax:type(Alternative) of
        variable -> trim_var(erl_syntax:variable_literal(Alternative));
        list -> "List";
        tuple -> "Tuple";
        map -> "Map";
        _ -> Current
    end.

is_generic(X) ->
    lists:member(X, ["X", "List", "Tuple", "Map"]).

trim_var(V) ->
    case V of
        "_" -> "X";
        "_"++Var -> trim_var(Var);
        _ -> V
    end.
