;;; package --- Erlang syntax highlighting.
;;; Commentary:
;;;
;;; This file provides syntax highlighting rules.
;;;
;;; You can use `treesit-explore-mode' to find the nodes to match.
;;; Code:

(require 'treesit)
(defun trerl-fontlock-settings()
  "Fontlock settings."
  (treesit-font-lock-rules
   :language 'erlang
   :feature 'comment
   :override t
   '((comment) @font-lock-comment-face)))

(provide 'trerl-fontlock)
;;; trerl-fontlock.el ends here
