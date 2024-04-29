(defun trerl-treesit-simple-indent-rules-setup()
  (setq-local indent-tabs-mode nil)
  (setq-local treesit--indent-verbose t)
  (treesit-explore-mode 'erlang)
  (setq-local treesit-simple-indent-rules trerl-mode-indent-rules)
;  (treesit-check-indent)
;  (add-to-list 'treesit-simple-indent-rules trerl-mode-indent-rules)
)

(defvar trerl-mode-indent-rules
  `((erlang
     ((node-is "\\.") parent-bol 0)

     ;; -include("aaa/include/aaa.hrl").
     ;; -include_lib("aaa.hrl").
     ((or (node-is "pp_include_lib") (node-is "pp_include")) parent-bol 0)
     ((and (or (parent-is "pp_include_lib") (parent-is "pp_include")) (node-is "string")) (nth-sibling 2) 1)
     ((and (or (parent-is "pp_include_lib") (parent-is "pp_include")) (node-is ")")) (nth-sibling 2) 0)

     ;; -export([foo/1, bar/2]).
     ((and (parent-is "export_attribute") (node-is "\\[")) (nth-sibling 2) 1)
     ((and (parent-is "export_attribute") (node-is ",")) (nth-sibling 3) 0)
     ((and (parent-is "export_attribute") (field-is "funs")) (nth-sibling 3) 1)
     ((and (parent-is "export_attribute") (node-is "fa")) (nth-sibling 4) 0)
     ((and (parent-is "export_attribute") (node-is "]")) (nth-sibling 3) 0)
     ((and (parent-is "export_attribute") (node-is ")")) (nth-sibling 2) 0)
     ((parent-is "export_attribute") (nth-sibling 3) 0)
     ((node-is "export_attribute") parent-bol 0)

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

     ((node-is "comment") parent-bol 0)
     ((node-is "module_attribute") parent-bol 0)
     ((node-is "pp_include_lib") parent-bol 0)
     ((node-is "pp_include") parent-bol 0)
     ((parent-is "fun_decl") parent 0)
     ((node-is "function_clause") parent-bol 0)
     ((node-is "]") prev-adaptive-prefix 9)
     ((node-is ">") parent-bol 0)
     ((node-is "\"") parent-bol 0)
     (catch-all parent-bol 0)
     ))
    "Indentation rules for trErl.")

(provide 'trerl-indent)
