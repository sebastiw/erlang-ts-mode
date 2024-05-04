;;;
;;; This file provides provides jump-tables for imenu.
;;; e.g. by
;;; M-x imenu[RET] CATEGORY[RET] DISPLAY-NAME[RET]
;;;
;;; When testing remember to evaluate `imenu-flush-cache' in between
;;; reevaluations of any of these functions to clear the imenu cache.
;;;
;;; You can use `treesit-explore-mode' to find the nodes to match.

(defun trerl-imenu-function-node-p (node)
  (let ((node-type (treesit-node-type node)))
    (string-match-p "function_clause" node-type)))

(defun trerl-imenu-function-name (node)
  (let ((fun-name (treesit-node-text (treesit-node-child-by-field-name node "name")))
        (fun-args (treesit-node-text (treesit-node-child-by-field-name node "args"))))
    (concat fun-name fun-args)))

(defun trerl-imenu-spec-node-p (node)
  (let ((node-type (treesit-node-type node)))
    (string-match-p "spec" node-type)))

(defun trerl-imenu-spec-name (node)
  (let* ((fun-name (treesit-node-text (treesit-node-child-by-field-name node "fun")))
         (type-sig (treesit-node-child node 1 t))
         (fun-args (treesit-node-text (treesit-node-child-by-field-name type-sig "args")))
         (fun-return (treesit-node-text (treesit-node-child-by-field-name type-sig "ty"))))
    (concat fun-name fun-args " -> " fun-return)))

(defun trerl-imenu-type-node-p (node)
  (let ((node-type (treesit-node-type node)))
    (string-match-p "type_alias" node-type)))

(defun trerl-imenu-type-name (node)
  (let ((type-name (treesit-node-text (treesit-node-child-by-field-name node "name")))
        (type-def (treesit-node-text (treesit-node-child node 1 t))))
    (concat type-name " :: " type-def)))

(defun trerl-imenu-record-node-p (node)
  (let ((node-type (treesit-node-type node)))
    (string-match-p "record_decl" node-type)))

(defun trerl-imenu-record-field-names (rec-field)
  """Help function to return the name of the record field"""
  (treesit-node-text (treesit-node-child-by-field-name rec-field "name")))

(defun trerl-imenu-record-name (node)
  (let ((rec-name (treesit-node-text (treesit-node-child-by-field-name node "name")))
        (rec-fields (cdr (treesit-node-children node t))))
    (concat "#" rec-name "{" (mapconcat 'trerl-imenu-record-field-names rec-fields ", ") "}")))

(defun trerl-treesit-simple-imenu-settings-setup()
  (setq-local treesit-simple-imenu-settings
              ;; List is in the form (CATEGORY NODE-MATCH-P-FUNCTION ? DISPLAY-FUNCTION)
              `(("funs" trerl-imenu-function-node-p nil trerl-imenu-function-name)
                ("specs" trerl-imenu-spec-node-p nil trerl-imenu-spec-name)
                ("types" trerl-imenu-type-node-p nil trerl-imenu-type-name)
                ("records" trerl-imenu-record-node-p nil trerl-imenu-record-name))))

(provide 'trerl-imenu)
