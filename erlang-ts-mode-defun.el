(defun erlang-ts-treesit-defun-type-regexp-setup()
  """Sets up `treesit-beginning-of-defun' and `treesit-end-of-defun'"""
  (setq-local treesit-defun-type-regexp
              "fun_decl"))

(defun erlang-ts-defun-function-name (node)
  (treesit-node-text (treesit-node-child-by-field-name (treesit-node-child-by-field-name node "clause") "name")))

(defun erlang-ts-treesit-defun-name-function-setup()
  """Sets up `treesit-add-log-current-defun'"""
  (setq-local treesit-defun-name-function
              'erlang-ts-defun-function-name))

(provide 'erlang-ts-defun)

