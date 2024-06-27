;;; erlang-ts-ts --- Some treesitter wrappers.
;;; Commentary:
;;;
;;; Code:

(require 'treesit)

(defun erlang-ts-at-point ()
  "Return thing at point as text."
  (pcase (treesit-node-at (point))
    ((app (erlang-ts--is-call) (and n (guard n)))
     (let ((expr (treesit-node-child-by-field-name n "expr")))
       (pcase (treesit-query-capture expr '((remote (remote_module (atom) @m) (atom) @f)))
         ('nil (list nil (treesit-node-text expr t)))
         (`((m . ,m) (f . ,f)) (list 'call (treesit-node-text m t) (treesit-node-text f t))))))))

(defun erlang-ts--is-call (node)
  "Return the closest parent of NODE that is a `call'."
  (treesit-parent-until node 'erlang-ts--callp))

(defun erlang-ts--callp (node)
  "NODE is a call."
  (string= (treesit-node-type node) "call"))

(provide 'erlang-ts-ts)
;;; erlang-ts-ts.el ends here
