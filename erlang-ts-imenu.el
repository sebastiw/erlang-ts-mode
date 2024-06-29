;;; package --- Erlang imenu.
;;;
;;; Commentary:
;;; This file provides provides jump-tables for imenu.
;;; e.g. by
;;; M-x imenu[RET] CATEGORY[RET] DISPLAY-NAME[RET]
;;;
;;; When testing remember to evaluate `imenu-flush-cache' in between
;;; reevaluations of any of these functions to clear the imenu cache.
;;;
;;; You can use `treesit-explore-mode' to find the nodes to match.
;;;
;;; Code:

(require 'treesit)

(defun erlang-ts-imenu-function-node-p (node)
  "NODE."
  (let ((node-type (treesit-node-type node)))
    (string-match-p "function_clause" node-type)))

(defun erlang-ts-imenu-function-name (node)
  "NODE."
  (let ((fun-name (treesit-node-text (treesit-node-child-by-field-name node "name")))
        (fun-args (treesit-node-text (treesit-node-child-by-field-name node "args"))))
    (concat fun-name fun-args)))

(defun erlang-ts-imenu-spec-node-p (node)
  "NODE."
  (let ((node-type (treesit-node-type node)))
    (string-match-p "spec" node-type)))

(defun erlang-ts-imenu-spec-name (node)
  "NODE."
  (let* ((fun-name (treesit-node-text (treesit-node-child-by-field-name node "fun")))
         (type-sig (treesit-node-child node 1 t))
         (fun-args (treesit-node-text (treesit-node-child-by-field-name type-sig "args")))
         (fun-return (treesit-node-text (treesit-node-child-by-field-name type-sig "ty"))))
    (concat fun-name fun-args " -> " fun-return)))

(defun erlang-ts-imenu-type-node-p (node)
  "NODE."
  (let ((node-type (treesit-node-type node)))
    (string-match-p "type_alias" node-type)))

(defun erlang-ts-imenu-type-name (node)
  "NODE."
  (let ((type-name (treesit-node-text (treesit-node-child-by-field-name node "name")))
        (type-def (treesit-node-text (treesit-node-child node 1 t))))
    (concat type-name " :: " type-def)))

(defun erlang-ts-imenu-record-node-p (node)
  "NODE."
  (let ((node-type (treesit-node-type node)))
    (string-match-p "record_decl" node-type)))

(defun erlang-ts-imenu-record-field-names (rec-field)
  "Help function to return REC-FIELD.name as text."
  (treesit-node-text (treesit-node-child-by-field-name rec-field "name")))

(defun erlang-ts-imenu-record-name (node)
  "NODE."
  (let ((rec-name (treesit-node-text (treesit-node-child-by-field-name node "name")))
        (rec-fields (cdr (treesit-node-children node t))))
    (concat "#" rec-name "{" (mapconcat 'erlang-ts-imenu-record-field-names rec-fields ", ") "}")))

(setq-local treesit-simple-imenu-settings
            ;; List is in the form (CATEGORY NODE-MATCH-P-FUNCTION ? DISPLAY-FUNCTION)
            `(("funs" erlang-ts-imenu-function-node-p nil erlang-ts-imenu-function-name)
              ("specs" erlang-ts-imenu-spec-node-p nil erlang-ts-imenu-spec-name)
              ("types" erlang-ts-imenu-type-node-p nil erlang-ts-imenu-type-name)
              ("records" erlang-ts-imenu-record-node-p nil erlang-ts-imenu-record-name))))

(provide 'erlang-ts-imenu)
;;; erlang-ts-imenu.el ends here
