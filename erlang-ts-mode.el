;;; trerl --- major mode for Erlang.
;;; Commentary:
;;; The canonical `erlang-mode' is as of this writing 30 years
;;; old.  This is a from-scratch mode based on tree-sitter, xref, and
;;; autocomplete.
;;; Code:

(require 'treesit)
(unless (featurep 'treesit)
  (error "TrErl requires tree-sitter to be installed"))

(defvar treesit-language-source-alist)
(unless (treesit-language-available-p 'erlang)
  (add-to-list
   'treesit-language-source-alist
<<<<<<< HEAD:erlang-ts-mode.el
   (cons 'erlang '("https://github.com/WhatsApp/tree-sitter-erlang")))

=======
   (cons 'erlang '("https://git.sr.ht/~massemanet/tree-sitter-erlang")))
>>>>>>> ff1a933 (syntax highlight works. Ditched whatsapp.):trerl-mode.el
  (treesit-install-language-grammar 'erlang))

(defgroup erlang-ts nil
  "Tree-sitter for Erlang."
  :group 'languages
  :prefix "erlang-ts-")

(require 'trerl-fontlock nil t)
(require 'trerl-indent nil t)
(require 'trerl-imenu nil t)
(require 'trerl-defun nil t)

(defun trerl-setup()
  "Initialize."
  (setq-local
   treesit-defun-type-regexp ()
   treesit-defun-name-function ()
   treesit-simple-imenu-settings ())
  (treesit-major-mode-setup))

(defvar erlang-ts-mode-map
  ;; Similar bindings as erlang-mode
  (let ((map (make-sparse-keymap)))
    ;; (define-key map ";"       'trerl-electric-semicolon)
    ;; (define-key map ","       'trerl-electric-comma)
    ;; (define-key map "<"         'trerl-electric-lt)
    ;; (define-key map ">"         'trerl-electric-gt)
    ;; (define-key map "\C-m"      'trerl-electric-newline)
    (define-key map (kbd "DEL") 'backward-delete-char-untabify)
    ;; (define-key map "\M-q"      'trerl-fill-paragraph)
    ;; (define-key map "\M-\t"     'trerl-complete-tag)
    ;; (define-key map "\C-c\M-\t" 'tempo-complete-tag)
    ;; (define-key map "\M-+"      'trerl-find-next-tag)
    ;; (define-key map "\C-c\M-a"  'trerl-beginning-of-clause)
    ;; (define-key map "\C-c\M-b"  'tempo-backward-mark)
    ;; (define-key map "\C-c\M-e"  'trerl-end-of-clause)
    ;; (define-key map "\C-c\M-f"  'tempo-forward-mark)
    ;; (define-key map "\C-c\M-h"  'trerl-mark-clause)
    (define-key map "\C-c\C-c"  'comment-region)
    ;; (define-key map "\C-c\C-j"  'trerl-generate-new-clause)
    ;; (define-key map "\C-c\C-k"  'trerl-compile)
    ;; (define-key map "\C-c\C-l"  'trerl-compile-display)
    ;; (define-key map "\C-c\C-s"  'trerl-show-syntactic-information)
    ;; (define-key map "\C-c\C-q"  'trerl-indent-function)
    (define-key map "\C-c\C-u"  'uncomment-region)
    ;; (define-key map "\C-c\C-y"  'trerl-clone-arguments)
    ;; (define-key map "\C-c\C-a"  'trerl-align-arrows)
    ;; (define-key map "\C-c\C-z"  'trerl-shell-display)
    ;; (define-key map "\C-c\C-d"  'trerl-man-function-no-prompt)
    map)
  "Keymap for trErl.")

(defvar trerl-mode-abbrev-table
  nil
  "Abbrev table in use in trerl-mode buffers.")

;;;###autoload
(define-derived-mode erlang-ts-mode prog-mode "Erlang"
  "Major mode for editing Erlang, powered by tree-sitter."
  :group 'trerl
  :abbrev-table trerl-mode-abbrev-table
  (when (treesit-ready-p 'erlang)
    (treesit-parser-create 'erlang)
    (trerl-setup)))

;;;###autoload
(dolist (r '("\\.erl$" "\\.app\\.src$" "\\.escript"
             "\\.hrl$" "\\.xrl$" "\\.yrl" "/ebin/.+\\.app"))
  (add-to-list 'auto-mode-alist (cons r 'erlang-ts-mode)))

(provide 'trerl-mode)
;;; trerl-mode.el ends here
