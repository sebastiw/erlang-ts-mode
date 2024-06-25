;;; erlang-ts-ts --- Some treesitter wrappers.
;;; Commentary:
;;; 
;;; Code:

(require 'treesit)

(defun erlang-ts-at-point ()
  "Return thing at point as text."
  (pcase (treesit-node-at (point))
    ((app (erlang-ts--is-call) n) (cons 'call (treesit-node-text n t)))))

(defun erlang-ts--is-call (node)
  "NODE."
  (treesit-parent-until node 'erlang-ts--callp))

(defun erlang-ts--callp (node)
  "NODE is a call."
  (string= (treesit-node-type node) "call"))

(provide 'erlang-ts-ts)
;;; erlang-ts-ts.el ends here
