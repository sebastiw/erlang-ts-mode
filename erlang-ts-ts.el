;;; erlang-ts-ts --- Some treesitter wrappers.
;;; Commentary:
;;
;; Replacements for utilities in erlang.el. Missing;
;;     erlang-get-function-arguments
;;     erlang-get-function-arity
;;     erlang-get-function-name
;;     erlang-get-import
;;     erlang-get-module
;;     erlang-match-next-function
;;
;;; Code:

(require 'treesit)

(defun erlang-ts-module ()
  "The value of the `module' attribute as a string."
  (let ((mod (etst--single-child "module_attribute" (treesit-buffer-root-node))))
    (when mod
      (etst--txt
       (etst--child "name" mod)))))

(defun erlang-ts-at-point ()
  "Thing at point as tagged list."
  (pcase (treesit-node-at (point))
    ((app (etst--ancestor "call") (and n (guard n))) (etst--extract-call n))))

(defun etst--extract-call (node)
  "If NODE is a `call', return (local-call F A Args) or (remote-call M F A Args)."
  (let* ((expr (etst--child "expr" node))
         (args (etst--map-children #'etst--txt (etst--child "args" node)))
         (arity (length args))
         (query '((remote (remote_module (atom) @m) (atom) @f))))
    (pcase (treesit-query-capture expr query)
      ('nil
       (list 'local-call (etst--txt expr) arity args))
      (`((m . ,m) (f . ,f))
       (list 'remote-call (etst--txt m) (etst--txt f) arity args)))))

(defun etst--map-children (fun node)
  "Map named children of NODE with FUN."
  (mapcar fun (treesit-node-children node t)))

(defun etst--single-child (field node)
  "Return FIELD attribute of NODE."
  (pcase (etst--children field node)
    (`(,mod) mod)))

(defun etst--children (field node)
  "List of FIELD children in NODE."
  (treesit-filter-child node (lambda(n) (string= (treesit-node-type n) field))))

(defun etst--child (field node)
  "The child of NODE in FIELD."
  (treesit-node-child-by-field-name node field))

(defun etst--txt (node)
  "Text representation of NODE."
  (treesit-node-text node t))

(defun etst--ancestor (type node)
  "The closest parent of NODE that is a TYPE."
  (treesit-parent-until node (lambda(n) (string= (treesit-node-type n) type))))

(provide 'erlang-ts-ts)
;;; erlang-ts-ts.el ends here
