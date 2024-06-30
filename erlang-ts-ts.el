;;; erlang-ts-ts --- Some treesitter wrappers.
;;; Commentary:
;;
;;; Code:

(require 'treesit)

(defun erlang-ts-module ()
  "The value of the `module' attribute as a string."
  (pcase (etst--single-child "module_attribute" (treesit-buffer-root-node))
    ('nil nil)
    (mod (etst--txt (etst--child "name" mod)))))

(defun erlang-ts-imports ()
  "Imports as a list of `(import M F A)."
  (let* ((root (treesit-buffer-root-node))
         (query '((import_attribute (atom) @m (fa (atom) @f (arity (integer) @a)))))
         (imports (treesit-query-capture root query))
         (o ()))
    (while imports
      (pcase (list (pop imports)(pop imports)(pop imports))
        (`((m . ,m) (f . ,f)(a . ,a))
         (push (list 'import (etst--txt m) (etst--txt f) (etst--txt a)) o))))
    o))

(defun erlang-ts-fdecls ()
  "List of function heads as `(fdecl NAME A LINE ARGS)."
  (etst--fold-children #'etst--fdecl (treesit-buffer-root-node)))

(defun erlang-ts-at-point ()
  "Thing at point as tagged list."
  (pcase (treesit-node-at (point))
    ((app (etst--ancestor "call") (and n (guard n))) (etst--extract-call n))))

;; function heads
(defun etst--fdecl (fdecls node)
  "If NODE is a new function head, cons it to FDECLS."
  (pcase (treesit-node-type node)
    ("fun_decl" (etst--add-fdecl fdecls (etst--child "clause" node)))
    (_ fdecls)))

(defun etst--add-fdecl (fdecls node)
  "If NODE is an fdecl that's not in FDECLS, cons it to FDECLS."
  (pcase (etst--maybe-cons-fdecl fdecls node)
    ('nil fdecls)
    (a a)))

(defun etst--maybe-cons-fdecl (fdecls node)
  "If NODE is a fdecls, return a new FDECLS, else return nil."
  (pcase (etst--extract-fdecl node)
    ((and fdecl `(fdecl ,n ,a ,_ ,_))
     (pcase (car fdecls)
       ('nil (list fdecl))
       (`(fdecl ,name ,arity ,_ ,_)
        (unless (and (string= n name) (string= a arity))
          (cons fdecl fdecls)))))))

(defun etst--extract-fdecl (node)
  "Map NODE to `(fdecl NAME A LINE ARGS)."
  (let* ((name (etst--txt (etst--child "name" node)))
         (args (etst--extract-args node))
         (arity (number-to-string (length args)))
         (line (line-number-at-pos (treesit-node-start node))))
    `(fdecl ,name ,arity ,line ,args)))

;; function calls
(defun etst--extract-call (node)
  "If NODE is a `call', return (local-call F A Args) or (remote-call M F A Args)."
  (let* ((expr (etst--child "expr" node))
         (args (etst--extract-args node))
         (arity (number-to-string (length args)))
         (query '((remote (remote_module (atom) @m) (atom) @f))))
    (pcase (treesit-query-capture expr query)
      ('nil
       (list 'local-call (etst--txt expr) arity args))
      (`((m . ,m) (f . ,f))
       (list 'remote-call (etst--txt m) (etst--txt f) arity args)))))

(defun etst--extract-args (node)
  "If NODE has an `args', return ARGS as a list of strings."
  (pcase (etst--child "args" node)
    ('nil nil)
    (n (etst--map-children #'etst--txt n))))

;; utils
(defun etst--fold-children (fun node)
  "Fold FUN over the children of NODE."
  (seq-reduce fun (treesit-node-children node) nil))

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
