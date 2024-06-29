;;; erlang-ts-indent --- Provides indentation rules.
;;;
;;; Commentary:
;;; To debug you can use `treesit-inspect-mode' to see which node
;;; point is on.  Optionally you can set `treesit--indent-verbose' and/or
;;; call `treesit-check-indent'.
;;;
;;; Code:

(setq-local
 indent-tabs-mode nil
 treesit-simple-indent-rules
 `((erlang
    ((parent-is "source_file") parent-bol 0)
    ((node-is "\\.") parent-bol 0)

    ;; { 1, 2, 3, a, b, c }
    ((and (parent-is "tuple") (node-is ",")) (nth-sibling 0) 0)
    ((and (parent-is "tuple") (node-is "}")) (nth-sibling 0) 0)
    ((and (parent-is "tuple") (field-is "expr")) (nth-sibling 0) 1)
    ((parent-is "tuple") (nth-sibling 1) 0)

    ;; [ 1, 2, 3, a, b, c ]
    ((and (parent-is "list") (node-is ",")) (nth-sibling 0) 0)
    ((and (parent-is "list") (node-is "]")) (nth-sibling 0) 0)
    ((and (parent-is "list") (field-is "exprs")) (nth-sibling 0) 1)
    ((parent-is "list") (nth-sibling 1) 0)

    ;; <<<<A,B,C>> || {A,B,C} <= D>>
    ((and (parent-is "binary") (node-is ",")) (nth-sibling 0) 1) ; , in A,
    ((and (field-is "elements") (node-is "bin_element")) parent 2) ;; A
    ((node-is "bin_element") (nth-sibling 1) 0) ;; B C
    ((and (parent-is "binary") (field-is "expr")) parent 2) ; <<A,B,C>> and ||
    ((and (parent-is "binary") (node-is ">>")) parent 0) ; >>
    ((parent-is "b_generator") parent 0) ; <= and D

    ;; -record(foo, {bar, baz = 42 :: integer()}).
    ((and (parent-is "record_decl") (node-is "(")) (nth-sibling 1) 1)
    ((and (parent-is "record_decl") (field-is "name")) (nth-sibling 2) 1)
    ((and (parent-is "record_decl") (node-is "{")) (nth-sibling 2) 1)
    ((match "," "record_decl" nil 4 4) (nth-sibling 2) 0)
    ((and (parent-is "record_decl") (node-is "}")) (nth-sibling 5) 0)
    ((and (parent-is "record_decl") (node-is ")")) (nth-sibling 2) 0)
    ((match "record_field" "record_decl" nil 0 6) (nth-sibling 5) 1)
    ((and (parent-is "record_decl") (node-is "record_field")) (nth-sibling 6) 0)
    ((and (parent-is "record_decl") (node-is "record")) parent-bol 1)
    ((and (parent-is "record_decl") (node-is ",")) (nth-sibling 5) 0)
    ((parent-is "record_decl") (nth-sibling 3) 0)

    ;; these recursive types are tricky so allow these three indentation styles
    ;; 1 :: foo() | bar() | baz
    ;; 2 :: foo() |
    ;;      bar() |
    ;;      baz
    ;; 3 :: foo()
    ;;      | bar()
    ;;      | baz
    ;;
    ;; examples of not accounted for indentation styles
    ;; a :: foo() | bar() |
    ;;      baz
    ;; b :: foo() | bar()
    ;;      | baz
    ((and (field-is "expr") (node-is "pipe")) parent 0) ; first `type()', right after `::'
    ((n-p-gp "|" "pipe" "field_type") parent 0) ; first `|'
    ((node-is "pipe") parent 0) ; middle `type()'
    ((parent-is "pipe") parent-bol 0) ; last `type()' and rest of `|'

    ;; -include("aaa/include/aaa.hrl").
    ;; -include_lib("aaa.hrl").
    ((or (node-is "pp_include_lib") (node-is "pp_include")) parent-bol 0)
    ((and (or (parent-is "pp_include_lib") (parent-is "pp_include")) (node-is "string")) (nth-sibling 2) 1)
    ((and (or (parent-is "pp_include_lib") (parent-is "pp_include")) (node-is ")")) (nth-sibling 2) 0)

    ;; -export([foo/1, bar/2]).
    ;; -export_type([foo/1, bar/2]).
    ((and (or (parent-is "export_attribute") (parent-is "export_type_attribute")) (node-is "export_type")) parent-bol 1)
    ((and (or (parent-is "export_attribute") (parent-is "export_type_attribute")) (node-is "(")) (nth-sibling 1) 1)
    ((and (or (parent-is "export_attribute") (parent-is "export_type_attribute")) (node-is "\\[")) (nth-sibling 2) 1)
    ((and (or (parent-is "export_attribute") (parent-is "export_type_attribute")) (node-is ",")) (nth-sibling 3) 0)
    ((and (parent-is "export_attribute") (field-is "funs")) (nth-sibling 3) 1)
    ((and (parent-is "export_type_attribute") (field-is "types")) (nth-sibling 3) 1)
    ((and (or (parent-is "export_attribute") (parent-is "export_type_attribute")) (node-is "fa")) (nth-sibling 4) 0)
    ((and (or (parent-is "export_attribute") (parent-is "export_type_attribute")) (node-is "]")) (nth-sibling 3) 0)
    ((and (or (parent-is "export_attribute") (parent-is "export_type_attribute")) (node-is ")")) (nth-sibling 2) 0)
    ((or (parent-is "export_attribute") (parent-is "export_type_attribute")) (nth-sibling 3) 0)
    ((or (node-is "export_attribute") (node-is "export_type_attribute")) parent-bol 0)

    ;; -import(bepa, [foo/1, bar/2]).
    ((and (parent-is "import_attribute") (field-is "module")) (nth-sibling 2) 1)
    ((and (parent-is "import_attribute") (field-is "funs") (node-is ",")) (nth-sibling 5) 0)
    ((and (parent-is "import_attribute") (node-is "\\[")) (nth-sibling 2) 1)
    ((and (parent-is "import_attribute") (node-is ",")) (nth-sibling 5) 0)
    ((and (parent-is "import_attribute") (field-is "funs")) (nth-sibling 5) 1)
    ((and (parent-is "import_attribute") (node-is "fa")) (nth-sibling 6) 0)
    ((and (parent-is "import_attribute") (node-is "]")) (nth-sibling 5) 0)
    ((and (parent-is "import_attribute") (node-is ")")) (nth-sibling 2) 0)
    ((parent-is "import_attribute") (nth-sibling 3) 0)
    ((node-is "import_attribute") parent-bol 0)

    ((and (node-is "comment") (parent-is "expr")) grand-parent 0)
    ((node-is "comment") prev-sibling 0)

    ;; begin X end
    ((and (node-is "end") (parent-is "block_expr")) parent 0)
    ((parent-is "block_expr") parent 2)

    ;; try X catch C:E:T -> Y after Z end
    ;; try X of A -> a; B -> b catch C:E:T -> Y after Z end
    ((and (parent-is "try_expr") (node-is "catch_clause")) prev-sibling 0)
    ((and (parent-is "try_expr") (node-is "catch")) parent 0)
    ((and (parent-is "try_expr") (node-is "after")) parent 0)
    ((and (parent-is "try_expr") (node-is "end")) parent 0)
    ((parent-is "try_expr") parent 2)
    ((and (parent-is "clause_body") (node-is "try_expr")) parent-bol 2)

    ;; fun () -> bar end
    ;; if A -> B end
    ;; case X of A -> B end
    ((and (node-is "end") (parent-is "anonymous_fun")) parent 0)
    ((and (node-is "end") (parent-is "if_expr")) parent 0)
    ((and (node-is "end") (parent-is "case_expr")) parent 0)

    ;; maybe X else A -> B end
    ((and (node-is "else") (parent-is "maybe_expr")) parent 0)
    ((and (node-is "end") (parent-is "maybe_expr")) parent 0)

    ;; receive A -> B after X -> Y end
    ((and (node-is "after") (parent-is "receive_expr")) parent 0)
    ((and (node-is "end") (parent-is "receive_expr")) parent 0)


    ;; fun(A,B,C) when X -> Y end
    ((parent-is "fun_clause") prev-sibling 0)
    ((n-p-gp nil "clause_body" "fun_clause") grand-parent 2)
    ((node-is "fun_clause") prev-sibling 0)
    ((node-is "fun_clause") prev-sibling 0)

    ;; foo:bar(a, b, c)
    ;; Support three different styles for the arguments
    ;; 1 foo:bar(A,
    ;;           B,
    ;;           C)
    ;; 2 foo:bar( A
    ;;          , B
    ;;          , C )
    ;; 3 foo:bar(
    ;;     A,
    ;;     B,
    ;;     C
    ;;   )
    ((and (parent-is "expr_args") (node-is ",")) parent 0)
    ((and (parent-is "expr_args") (field-is "args")) grand-parent 2)
    ((and (parent-is "expr_args") (node-is ")")) prev-sibling -2)
    ((parent-is "expr_args") (nth-sibling 1) 0)

    ;; ?macro(a, b, c)
    ;; similar style as fun-calls
    ((and (parent-is "macro_call_args") (node-is ",")) parent 0)
    ((and (parent-is "macro_call_args") (field-is "args")) grand-parent 2)
    ((and (parent-is "macro_call_args") (node-is ")")) prev-sibling -2)
    ((parent-is "macro_call_args") (nth-sibling 1) 0)

    ((parent-is "concatables") prev-sibling 0)

    ((parent-is "args") first-sibling 0)

    ((node-is "module_attribute") parent-bol 0)
    ((parent-is "fun_decl") parent 0)
    ((parent-is "-") parent-bol 0)

    ((node-is "\"") parent-bol 0)
    (catch-all standalone-parent 2))))

(provide 'erlang-ts-indent)
;;; erlang-ts-indent.el ends here
