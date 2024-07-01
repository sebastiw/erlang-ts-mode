;;; package --- Erlang syntax highlighting.
;;; Commentary:
;;;
;;; This file provides syntax highlighting rules.
;;;
;;; You can use `treesit-explore-mode' to find the nodes to match.
;;; Code:

(require 'treesit)

(defun erlang-ts-fontlock-features ()
  "A `treesit-font-lock-feature-list'."
  '((comment string atom variable) ; level 1
    (function-name keyword boolean bitwise math) ; level 2
    (call-remote call-local))) ; level 3

(defmacro etsf--rule (name query)
  "NAME QUERY FACE."
  `(quote (:language erlang
     :override t
     :feature ,name
     ,query)))

(defvar erlang-ts-fontlock-rules
  (append
   (etsf--rule comment ((comment) @font-lock-comment-face))
   (etsf--rule string ((string) @font-lock-string-face))
   (etsf--rule atom ((atom) @font-lock-constant-face))
   (etsf--rule variable ((var) @font-lock-variable-name-face))
   (etsf--rule function-name ((function_clause (atom) @font-lock-function-name-face)))
   (etsf--rule call-remote ((call (remote) @font-lock-function-call-face)))
   (etsf--rule call-local ((call (atom) @font-lock-function-call-face)))
   (etsf--rule keyword (["after" "begin" "case" "catch" "else" "end" "fun" "if" "maybe" "of" "receive" "try" "when"] @font-lock-keyword-face))
   (etsf--rule boolean (["and" "andalso" "not" "or" "orelse" "xor"] @font-lock-builtin-face))
   (etsf--rule bitwise (["band" "bnot" "bor" "bsl" "bsr" "bxor"] @font-lock-builtin-face))
   (etsf--rule math (["div" "rem"] @font-lock-builtin-face)))
  "A list of `font-lock-rule'.")

(defun erlang-ts-fontlock ()
  "Return a `treesit-font-lock-settings."
  (apply #'treesit-font-lock-rules erlang-ts-fontlock-rules))

(provide 'erlang-ts-fontlock)
;;; erlang-ts-fontlock.el ends here
