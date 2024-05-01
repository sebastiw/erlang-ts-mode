(defun trerl-treesit-simple-indent-rules-setup()
  (setq-local indent-tabs-mode nil)
  (setq-local treesit--indent-verbose t)
  ; (treesit-explore-mode 'erlang)
  (treesit-inspect-mode)
  (setq-local treesit-simple-indent-rules trerl-mode-indent-rules)
;  (treesit-check-indent)
)

(defvar trerl-mode-indent-rules
  `((erlang
     ((parent-is "source_file") parent-bol 0)
     ((node-is "\\.") parent-bol 0)

     ;; [ 1, 2, 3, a, b, c ]
     ((and (parent-is "list") (node-is ",")) (nth-sibling 0) 0)
     ((and (parent-is "list") (node-is "]")) (nth-sibling 0) 0)
     ((and (parent-is "list") (field-is "exprs")) (nth-sibling 0) 1)
     ((parent-is "list") (nth-sibling 1) 0)

     ((parent-is "binary") (nth-sibling 0) 1)

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

     ((parent-is "args") prev-sibling 0)
     ((parent-is "clause") prev-sibling 0)
     ((parent-is "body") prev-sibling 0)

     ((node-is "module_attribute") parent-bol 0)
     ((parent-is "fun_decl") parent 0)
     ((node-is "function_clause") parent-bol 0)
     ((node-is "]") prev-adaptive-prefix 9)
     ((node-is ">") parent-bol 0)
     ((node-is "\"") parent-bol 0)
     ((parent-is "-") parent-bol 0)
     (catch-all standalone-parent 2)
     ))
    "Indentation rules for trErl.")

(provide 'trerl-indent)
