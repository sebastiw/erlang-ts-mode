;;; package --- Erlang syntax highlighting.
;;; Commentary:
;;;
;;; This file provides syntax highlighting rules.
;;;
;;; You can use `treesit-explore-mode' to find the nodes to match.
;;; Code:

(require 'treesit)

(defvar trerl-fontlock-rules
  '(:language erlang :override t :feature
              comment
              ((comment) @font-lock-comment-face)
              :language erlang :override t :feature
              string
              ((string) @font-lock-string-face)
              :language erlang :override t :feature
              variable-name
              ((var) @font-lock-variable-name-face)
              :language erlang :override nil :feature
              keyword
              (["case" "of" "end" "when"] @font-lock-keyword-face)
              :language erlang :override nil :feature
              constant
              ((atom) @font-lock-constant-face)))

(defvar trerl-font-lock-feature-list
  '((comment string doc) ; level 1
    (function-name keyword type builtin constant) ; level 2
    (variable-name string-interpolation key))) ; level 3

(setq-local
 treesit-font-lock-settings (apply #'treesit-font-lock-rules trerl-fontlock-rules)
 treesit-font-lock-feature-list trerl-font-lock-feature-list)

(provide 'trerl-fontlock)
;;; trerl-fontlock.el ends here
