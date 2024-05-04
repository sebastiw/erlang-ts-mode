;;;
;;; This file provides syntax highlighting rules.
;;;
;;; You can use `treesit-explore-mode' to find the nodes to match.

(defun trerl-treesit-font-lock-settings-setup()
  (setq-local treesit-font-lock-settings trerl-mode-treesit-font-lock-settings)
  ;; Attempt to imitate erlang.el font-lock keywords levels
  (setq-local treesit-font-lock-level 4)
  (setq-local treesit-font-lock-feature-list
              '((comment errors delimiters
                 ;; quotes
                 )
                (keywords strings function-header)
                (predefined-types vars records macros operators
                 guards attr ext-bifs int-bifs)
                (arrow dollar
                 int-function-calls ext-function-calls
                 exported-function-header
                 fun-n lc)
                )
              ))

(eval-and-compile
  (defconst erlang-atom-quoted-regexp
    "'\\(?:[^\\']\\|\\(?:\\\\.\\)\\)*'"
    "Regexp describing a single-quoted atom"))

(eval-and-compile
  (defconst erlang-atom-regular-regexp
    "\\_<[[:lower:]]\\(?:\\sw\\|\\s_\\)*\\_>"
    "Regexp describing a regular (non-quoted) atom"))

(eval-and-compile
  (defconst erlang-atom-regexp
    (concat "\\(" erlang-atom-quoted-regexp "\\|"
            erlang-atom-regular-regexp "\\)")
    "Regexp describing an Erlang atom."))

(eval-and-compile
  (defconst erlang-atom-regexp-matches 1
    "Number of regexp parenthesis pairs in `erlang-atom-regexp'.

This is used to determine parenthesis matches in complex regexps which
contains `erlang-atom-regexp'."))


(eval-and-compile
  (defconst erlang-variable-regexp
    "\\_<\\([[:upper:]_]\\(?:\\sw\\|\\s_\\)*\\)\\_>"
    "Regexp which should match an Erlang variable.

The regexp must be surrounded with a pair of regexp parentheses."))

(eval-and-compile
  (defconst erlang-variable-regexp-matches 1
    "Number of regexp parenthesis pairs in `erlang-variable-regexp'.

This is used to determine matches in complex regexps which contains
`erlang-variable-regexp'."))

(defconst erlang-module-function-regexp
  (eval-when-compile
    (concat erlang-atom-regexp ":" erlang-atom-regexp))
  "Regexp matching an erlang module:function.")

(defconst erlang-name-regexp
    (concat "\\("
            "\\(?:\\sw\\|\\s_\\)+"
            "\\|"
            erlang-atom-quoted-regexp
            "\\)")
    "Matches a name of a function, macro or record")

(defconst erlang-id-regexp
  (concat "\\(?:\\(qualified-function\\|record\\|macro\\|module\\) \\)?"
          "\\(?:" erlang-atom-regexp ":\\)?"
          erlang-name-regexp "?"
          "\\(?:/\\([0-9]+\\)\\)?"))

(eval-and-compile
  (defun erlang-regexp-opt (strings &optional paren)
    "Like `regexp-opt', except if PAREN is `symbols', then the
resulting regexp is surrounded by \\_< and \\_>."
    (if (eq paren 'symbols)
        (concat "\\_<" (regexp-opt strings t) "\\_>")
      (regexp-opt strings paren))))


(eval-and-compile
  (defvar erlang-keywords
    '("after"
      "begin"
      "catch"
      "case"
      "cond"
      "end"
      "fun"
      "if"
      "let"
      "of"
      "receive"
      "try"
      "maybe"
      "else"
      "when")
    "Erlang reserved keywords"))

(eval-and-compile
  (defconst erlang-keywords-regexp (erlang-regexp-opt erlang-keywords 'symbols)))

(eval-and-compile
  (defvar erlang-operators
    '("and"
      "andalso"
      "band"
      "bnot"
      "bor"
      "bsl"
      "bsr"
      "bxor"
      "div"
      "not"
      "or"
      "orelse"
      "rem"
      "xor")
    "Erlang operators"))
;; What about these?
;; '+' '-' '*' '/' '>', '>=', '<', '=<', '=:=', '==', '=/=', '/='

(eval-and-compile
  (defconst erlang-operators-regexp (erlang-regexp-opt erlang-operators 'symbols)))


(eval-and-compile
  (defvar erlang-guards
    '("is_atom"
      "is_binary"
      "is_bitstring"
      "is_boolean"
      "is_float"
      "is_function"
      "is_integer"
      "is_list"
      "is_map"
      "is_number"
      "is_pid"
      "is_port"
      "is_record"
      "is_reference"
      "is_tuple"
      "atom"
      "binary"
      "bitstring"
      "boolean"
      ;;"float" ; Not included to avoid clashes with the bif float/1
      "function"
      "integer"
      "list"
      "number"
      "pid"
      "port"
      "record"
      "reference"
      "tuple")
    "Erlang guards"))

(eval-and-compile
  (defconst erlang-guards-regexp (erlang-regexp-opt erlang-guards 'symbols)))

(eval-and-compile
  (defvar erlang-predefined-types
    '("any"
      "arity"
      "atom"
      "binary"
      "bitstring"
      "boolean"
      "byte"
      "char"
      "cons"
      "deep_string"
      "float"
      "function"
      "iodata"
      "iolist"
      "integer"
      "list"
      "number"
      "maybe_improper_list"
      "module"
      "mfa"
      "nil"
      "neg_integer"
      "none"
      "non_neg_integer"
      "nonempty_list"
      "nonempty_improper_list"
      "nonempty_maybe_improper_list"
      "nonempty_string"
      "no_return"
      "pid"
      "port"
      "pos_integer"
      "record"
      "reference"
      "string"
      "term"
      "timeout"
      "tuple"
      "map")
    "Erlang type specs types"))

(eval-and-compile
  (defconst erlang-predefined-types-regexp
    (erlang-regexp-opt erlang-predefined-types 'symbols)))

(eval-and-compile
  (defvar erlang-int-bifs
    '("abs"
      "alias"
      "apply"
      "atom_to_binary"
      "atom_to_list"
      "binary_to_atom"
      "binary_to_existing_atom"
      "binary_to_float"
      "binary_to_integer"
      "binary_to_list"
      "binary_to_term"
      "binary_part"
      "bit_size"
      "bitstring_to_list"
      "byte_size"
      "ceil"
      "check_old_code"
      "check_process_code"
      "date"
      "delete_module"
      "demonitor"
      "disconnect_node"
      "element"
      "erase"
      "error"
      "exit"
      "floor"
      "float"
      "float_to_binary"
      "float_to_list"
      "garbage_collect"
      "get"
      "get_keys"
      "group_leader"
      "halt"
      "hd"
      "integer_to_list"
      "integer_to_binary"
      "iolist_size"
      "iolist_to_binary"
      "is_alive"
      "is_atom"
      "is_binary"
      "is_bitstring"
      "is_boolean"
      "is_float"
      "is_function"
      "is_integer"
      "is_list"
      "is_map"
      "is_map_key"
      "is_number"
      "is_pid"
      "is_port"
      "is_process_alive"
      "is_record"
      "is_reference"
      "is_tuple"
      "length"
      "link"
      "list_to_atom"
      "list_to_binary"
      "list_to_bitstring"
      "list_to_existing_atom"
      "list_to_float"
      "list_to_integer"
      "list_to_pid"
      "list_to_port"
      "list_to_ref"
      "list_to_tuple"
      "load_module"
      "make_ref"
      "map_get"
      "map_size"
      "max"
      "min"
      "module_loaded"
      "monitor"
      "monitor_node"
      "node"
      "nodes"
      "now"
      "open_port"
      "pid_to_list"
      "port_close"
      "port_command"
      "port_connect"
      "port_control"
      "port_to_list"
      "pre_loaded"
      "process_flag"
      "process_info"
      "processes"
      "purge_module"
      "put"
      "ref_to_list"
      "register"
      "registered"
      "round"
      "self"
      "setelement"
      "size"
      "spawn"
      "spawn_link"
      "spawn_monitor"
      "spawn_opt"
      "spawn_request"
      "spawn_request_abandon"
      "split_binary"
      "statistics"
      "term_to_binary"
      "term_to_iovec"
      "time"
      "throw"
      "tl"
      "trunc"
      "tuple_size"
      "tuple_to_list"
      "unalias"
      "unlink"
      "unregister"
      "whereis")
    "Erlang built-in functions (BIFs)"))

(eval-and-compile
  (defconst erlang-int-bif-regexp (erlang-regexp-opt erlang-int-bifs 'symbols)))


(eval-and-compile
  (defvar erlang-ext-bifs
    '("adler32"
      "adler32_combine"
      "alloc_info"
      "alloc_sizes"
      "append"
      "append_element"
      "bump_reductions"
      "call_on_load_function"
      "cancel_timer"
      "crc32"
      "crc32_combine"
      "decode_packet"
      "delay_trap"
      "delete_element"
      "display"
      "display_string"
      "dist_get_stat"
      "dist_ctrl_get_data"
      "dist_ctrl_get_data_notification"
      "dist_ctrl_get_opt"
      "dist_ctrl_input_handler"
      "dist_ctrl_put_data"
      "dist_ctrl_set_opt"
      "dmonitor_node"
      "dt_append_vm_tag_data"
      "dt_get_tag"
      "dt_get_tag_data"
      "dt_prepend_vm_tag_data"
      "dt_put_tag"
      "dt_restore_tag"
      "dt_spread_tag"
      "convert_time_unit"
      "exit_signal"
      "external_size"
      "finish_after_on_load"
      "finish_loading"
      "format_cpu_topology"
      "fun_info"
      "fun_info_mfa"
      "fun_to_list"
      "function_exported"
      "garbage_collect_message_area"
      "gather_gc_info_result"
      "get_cookie"
      "get_module_info"
      "has_prepared_code_on_load"
      "hibernate"
      "insert_element"
      "iolist_to_iovec"
      "is_builtin"
      "load_nif"
      "loaded"
      "localtime"
      "localtime_to_universaltime"
      "make_fun"
      "make_tuple"
      "match_spec_test"
      "md5"
      "md5_final"
      "md5_init"
      "md5_update"
      "memory"
      "module_info"
      "monitor_node"
      "monotonic_time"
      "nif_error"
      "phash"
      "phash2"
      "port_call"
      "port_get_data"
      "port_info"
      "port_set_data"
      "ports"
      "posixtime_to_universaltime"
      "prepare_loading"
      "process_display"
      "raise"
      "read_timer"
      "resume_process"
      "send"
      "send_after"
      "send_nosuspend"
      "seq_trace"
      "seq_trace_info"
      "seq_trace_print"
      "set_cookie"
      "set_cpu_topology"
      "setnode"
      "start_timer"
      "subtract"
      "suspend_process"
      "system_flag"
      "system_info"
      "system_monitor"
      "system_profile"
      "system_time"
      "trace"
      "trace_delivered"
      "trace_info"
      "trace_pattern"
      "time_offset"
      "timestamp"
      "universaltime"
      "universaltime_to_localtime"
      "universaltime_to_posixtime"
      "unique_integer"
      "yield")
    "Erlang built-in functions (BIFs) that needs erlang: prefix"))

(eval-and-compile
  (defconst erlang-ext-bif-regexp
    (erlang-regexp-opt (append erlang-int-bifs erlang-ext-bifs) 'symbols)))

(defvar erlang-font-lock-keywords-quotes
  (list
   (list "`\\([-+a-zA-Z0-9_:*][-+a-zA-Z0-9_:*]+\\)'"
         1
         'font-lock-keyword-face
         t))
  "Font lock keyword highlighting words in single quotes in comments.

This is not the highlighting of Erlang strings and atoms, which
are highlighted by syntactic analysis.")


(defvar trerl-mode-treesit-font-lock-settings
  (treesit-font-lock-rules
   :language 'erlang
   :override t
   :feature 'function-header
   `((fun_decl
      clause: (function_clause
               name: (atom) @font-lock-function-name-face))
     (spec
      fun: (atom) @font-lock-type-face))

   :language 'erlang
   :override t
   :feature 'int-bifs
   `((call
      expr: (atom) @font-lock-builtin-face
      (:match ,erlang-int-bif-regexp @font-lock-builtin-face)))

   :language 'erlang
   :override t
   :feature 'ext-bifs
   `((call
      expr:
      (remote
       module: (remote_module module: (atom) ":")
       fun: ((atom) @font-lock-builtin-face
             (:match ,erlang-ext-bif-regexp @font-lock-builtin-face))))
     (call
      expr: (remote
             module: ((remote_module
                       module: (atom) @font-lock-builtin-face ":")
                      (:equal "erlang" @font-lock-builtin-face)))))

   :language 'erlang
   :override nil
   :feature 'int-function-calls
   `((call
      expr: (atom) @font-lock-type-face))

   :language 'erlang
   :override nil
   :feature 'ext-function-calls
   '((call
      expr: (remote
             module: (remote_module module: (atom) ":")
             fun: (atom) @font-lock-type-face))
     (call
      expr: (remote
             module: (remote_module
                      module: (atom) @font-lock-type-face ":"))))

   :language 'erlang
   :override nil
   :feature 'fun-n
   '(((binary_op_expr
       lhs: (atom)
       "/"
       rhs: (integer)) @font-lock-type-face)
     ((fa
       fun: (atom)
       arity: (arity "/" value: (integer))) @font-lock-type-face)
     )

   :language 'erlang
   :override nil
   :feature 'operators
   `((binary_op_expr lhs: (_) _ @font-lock-builtin-face rhs: (_)
      (:match ,erlang-operators-regexp @font-lock-builtin-face)))

   :language 'erlang
   :override nil
   :feature 'dollar
   '((char) @font-lock-constant-face)

   :language 'erlang
   :override nil
   :feature 'arrow
   '((clause_body "->" @font-lock-function-name-face))

   :language 'erlang
   :override nil
   :feature 'lc
   '((lc_exprs "||" @font-lock-keyword-face)
     (generator "<-" @font-lock-keyword-face)
     (b_generator "<=" @font-lock-keyword-face)
     )

   :language 'erlang
   :override nil
   :feature 'keywords
   '((receive_expr "receive" @font-lock-keyword-face)
     (receive_after "after" @font-lock-keyword-face)
     (receive_expr "end" @font-lock-keyword-face)
     (block_expr "begin" @font-lock-keyword-face)
     (block_expr "begin" "end" @font-lock-keyword-face)
     (try_expr "try" @font-lock-keyword-face)
     (try_expr "try" "of" @font-lock-keyword-face)
     (try_expr "try" "catch" @font-lock-keyword-face)
     (try_expr "try" "end" @font-lock-keyword-face)
     (try_after "after" @font-lock-keyword-face)
     (catch_expr "catch" @font-lock-keyword-face)
     (catch_clause "when" @font-lock-keyword-face)
     (case_expr "case" @font-lock-keyword-face)
     (case_expr "case" "of" @font-lock-keyword-face)
     (case_expr "case" "end" @font-lock-keyword-face)
     (cr_clause "when" @font-lock-keyword-face)
     (anonymous_fun "fun" @font-lock-keyword-face)
     (anonymous_fun "fun" "end" @font-lock-keyword-face)
     (fun_clause "when" @font-lock-keyword-face)
     (if_expr "if" @font-lock-keyword-face)
     (if_expr "if" "end" @font-lock-keyword-face)
     (maybe_expr "maybe" @font-lock-keyword-face)
     (maybe_expr "maybe" "else" @font-lock-keyword-face)
     (maybe_expr "maybe" "end" @font-lock-keyword-face)
     )

   :language 'erlang
   :override nil
   :feature 'attr
   '((module_attribute "-" @font-lock-preprocessor-face)
     (module_attribute "-" "module" @font-lock-preprocessor-face)
     (pp_include_lib "-" @font-lock-preprocessor-face)
     (pp_include_lib "-" "include_lib" @font-lock-preprocessor-face)
     (pp_include "-" @font-lock-preprocessor-face)
     (pp_include "-" "include" @font-lock-preprocessor-face)
     (export_attribute "-" @font-lock-preprocessor-face)
     (export_attribute "-" "export" @font-lock-preprocessor-face)
     (import_attribute "-" @font-lock-preprocessor-face)
     (import_attribute "-" "import" @font-lock-preprocessor-face)
     (export_type_attribute "-" @font-lock-preprocessor-face)
     (export_type_attribute "-" "export_type" @font-lock-preprocessor-face)
     (type_alias "-" @font-lock-preprocessor-face)
     (type_alias "-" "type" @font-lock-preprocessor-face)
     (pp_define "-" @font-lock-preprocessor-face)
     (pp_define "-" "define" @font-lock-preprocessor-face)
     (spec "-" @font-lock-preprocessor-face)
     (spec "-" "spec" @font-lock-preprocessor-face)
     (pp_undef "-" @font-lock-preprocessor-face)
     (pp_undef "-" "undef" @font-lock-preprocessor-face)
     (pp_ifdef "-" @font-lock-preprocessor-face)
     (pp_ifdef "-" "ifdef" @font-lock-preprocessor-face)
     (pp_ifndef "-" @font-lock-preprocessor-face)
     (pp_ifndef "-" "ifndef" @font-lock-preprocessor-face)
     (pp_else "-" @font-lock-preprocessor-face)
     (pp_else "-" "else" @font-lock-preprocessor-face)
     (pp_endif "-" @font-lock-preprocessor-face)
     (pp_endif "-" "endif" @font-lock-preprocessor-face)
     (pp_if "-" @font-lock-preprocessor-face)
     (pp_if "-" "if" @font-lock-preprocessor-face)
     (pp_elif "-" @font-lock-preprocessor-face)
     (pp_elif "-" "elif" @font-lock-preprocessor-face)
     (feature_attribute "-" @font-lock-preprocessor-face)
     (feature_attribute "-" "feature" @font-lock-preprocessor-face)
     (compile_options_attribute "-" @font-lock-preprocessor-face)
     (compile_options_attribute "-" "compile" @font-lock-preprocessor-face)
     (opaque "-" @font-lock-preprocessor-face)
     (opaque "-" "opaque" @font-lock-preprocessor-face)
     (record_decl "-" @font-lock-preprocessor-face)
     (record_decl "-" "record" @font-lock-preprocessor-face)
     (attr_name "-" @font-lock-preprocessor-face)
     (attr_name "-" (atom) @font-lock-preprocessor-face)
     )

   :language 'erlang
   :override nil
   :feature 'comment ;; need to look into quote highlighting
   '((comment) @font-lock-comment-face)

   :language 'erlang
   :override t
   :feature 'guards
   `((guard_clause
      exprs: (call
              expr: (atom) @font-lock-builtin-face
              (:match ,erlang-guards-regexp @font-lock-builtin-face))))

   :language 'erlang
   :override t
   :feature 'predefined-types
   `((call
      expr: (atom) @font-lock-builtin-face
      (:match ,erlang-predefined-types-regexp @font-lock-builtin-face)))

   :language 'erlang
   :override nil
   :feature 'macros
   '((macro_call_expr "?" name: (var) @font-lock-constant-face)
     (macro_call_expr "?" name: (atom) @font-lock-constant-face))

   :language 'erlang
   :override t
   :feature 'records
   '((record_name "#" (atom) @font-lock-type-face)
     (record_decl "-" "record" "(" name: (atom) @font-lock-type-face))

   :language 'erlang
   :override t
   :feature 'vars
   '((var) @font-lock-variable-name-face)

   :language 'erlang
   :override t
   :feature 'strings
   '((string) @font-lock-string-face)

   :language 'erlang
   :override t
   :feature 'delimiters
   '((binary "<<" @font-lock-bracket-face)
     (binary ">>" @font-lock-bracket-face)
     (list "[" @font-lock-bracket-face)
     (list "]" @font-lock-bracket-face))

   :language 'erlang
   :override t
   :feature 'errors
   '((ERROR) @font-lock-warning-face)
   )
  "Tree-sitter font-lock settings for `trerl-mode'.")

(provide 'trerl-fontlock)
