;;; erlang-ts-defun --- get treesit defun support.
;;; Commentary:
;;; Code:

(require 'treesit)

(defun erlang-ts-defun-regexp ()
  "The `treesit-defun-type-regexp'."
  "fun_decl")

(defun erlang-ts-defun-function-name (node)
  "If NODE is a function, return its name."
  (treesit-node-text
   (treesit-node-child-by-field-name
    (treesit-node-child-by-field-name node "clause") "name")))

(provide 'erlang-ts-defun)
;;; erlang-ts-defun.el ends here
