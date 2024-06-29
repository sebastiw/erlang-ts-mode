;;; package --- Erlang defun.
;;;
;;; Commentary:
;;;
;;; Provides `treesit-defun' functionality for erlang.
;;;
;;; Code:

(require 'treesit)

(defun erlang-ts-defun-name (node)
  "If NODE is a function clause, return its name name."
  (treesit-node-text
   (treesit-node-child-by-field-name
    (treesit-node-child-by-field-name node "clause") "name")))

(setq-local treesit-defun-type-regexp "fun_decl"
            treesit-defun-name-function #'erlang-ts-defun-name)

(provide 'erlang-ts-defun)
;;; erlang-ts-defun.el ends here
