#!/usr/bin/env escript

-mode(compile).

main(Args) ->
    case Args of
        [] -> io:fwrite("Build a release using relx.");
        ["starlet", Root] -> release(starlet, Root)
    end.
    
release(System, Root) ->
    code:add_patha(filename:join([Root, bbmustache, ebin])),
    code:add_patha(filename:join([Root, relx, ebin])),
    relx:build_tar(System, opts(System, Root)).

opts(System, Root) ->
    [{root_dir, Root},
     {lib_dirs, [Root]},
     {output_dir, filename:join(Root, "release")},
     {release, {System, vsn()}, [System]},
     {sys_config_src, filename:join([Root, config, "sys.config"])},
     {vm_args_src, filename:join([Root, config, "vm.args"])}].

vsn() ->
    pipe(second,
         [fun erlang:system_time/1,
          fun calendar:system_time_to_rfc3339/1,
          fun(X) -> string:tokens(X, "+") end,
          fun erlang:hd/1]).

pipe(Subject, Fs) ->
    lists:foldl(fun(F, S) -> F(S) end, Subject, Fs).
