;;; erlang-ts-keymap --- key map.
;;; Commentary:
;;; Code:

(defvar-keymap erlang-ts-mode-map
  :doc "Keys for erlang-ts-mode."
  :parent prog-mode-map
  "DEL"     'backward-delete-char-untabify
  "TAB"     'erlang-ts-acer-tab
  "C-c C-c" 'comment-region
  "C-c C-d" 'erlang-ts-man
  "C-c C-o" 'xref-pop-marker-stack
  "C-c C-p" 'xref-find-definitions
  "C-c C-u" 'uncomment-region
  "C-c M-b" 'tempo-backward-mark
  "C-c M-f" 'tempo-forward-mark
  "C-c M-t" 'tempo-complete-tag
  "C-c M-n" 'treesit-end-of-defun
  "C-c M-p" 'treesit-beginning-of-defun)
;; ","       'erlang-ts-electric-comma
;; ";"       'erlang-ts-electric-semicolon
;; "<"       'erlang-ts-electric-lt
;; ">"       'erlang-ts-electric-gt
;; "C-c C-a" 'erlang-ts-align-arrows
;; "C-c C-j" 'erlang-ts-generate-new-clause
;; "C-c C-k" 'erlang-ts-compile
;; "C-c C-l" 'erlang-ts-compile-display
;; "C-c C-q" 'erlang-ts-indent-function
;; "C-c C-s" 'erlang-ts-show-syntactic-information
;; "C-c C-y" 'erlang-ts-clone-arguments
;; "C-c C-z" 'erlang-ts-shell-display
;; "C-c M-a" 'erlang-ts-beginning-of-clause
;; "C-c M-e" 'erlang-ts-end-of-clause
;; "C-c M-h" 'erlang-ts-mark-clause
;; "C-m"     'erlang-ts-electric-newline
;; "M-q"     'erlang-ts-fill-paragraph

(provide 'erlang-ts-keymap)
;;; erlang-ts-keymap.el ends here
