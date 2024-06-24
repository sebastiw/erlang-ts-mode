;;; erlang-ts --- major mode for Erlang.
;;; Commentary:
;;; The canonical `erlang-mode' is as of this writing 30 years
;;; old.  This is a from-scratch mode based on tree-sitter, xref, and
;;; autocomplete.
;;;
;;; Note that ‘erlang-ts-mode-map’, ‘erlang-ts-mode-syntax-table’,
;;; ‘erlang-ts-mode-abbrev-table’, and ‘erlang-ts-mode-hook’ are
;;; created by `define-derived-mode'.

;;; Code:

;; check for treesit
(require 'treesit)
(unless (featurep 'treesit)
  (error "Erlang-ts requires tree-sitter to be installed"))

;; install the parser
(defvar treesit-language-source-alist)
(unless (treesit-language-available-p 'erlang)
  (add-to-list
   'treesit-language-source-alist
   (cons 'erlang '("https://git.sr.ht/~massemanet/tree-sitter-erlang")))
  (treesit-install-language-grammar 'erlang))

;; load our subsystems
(require 'erlang-ts-flycheck nil t)
(require 'erlang-ts-acer nil t)
(require 'erlang-ts-mode-map nil t)
(require 'erlang-ts-fontlock nil t)
(require 'erlang-ts-indent nil t)
(require 'erlang-ts-imenu nil t)
(require 'erlang-ts-defun nil t)

(defgroup erlang-ts nil
  "Tree-sitter for Erlang."
  :group 'languages
  :prefix "erlang-ts-")

(defun erlang-ts-setup()
  "Initialize."
  (treesit-parser-create 'erlang)
  (erlang-ts-mode-keys)

  ;; If ‘treesit-font-lock-settings’ is non-nil, set up fontification
  ;; and enable ‘font-lock-mode’.
  (setq-local
   treesit-font-lock-settings (erlang-ts-fontlock)
   treesit-font-lock-feature-list (erlang-ts-fontlock-features))

  ;; If ‘treesit-simple-indent-rules’ is non-nil, set up indentation.
  (setq-local
   indent-tabs-mode nil
   treesit-simple-indent-rules (erlang-ts-indent))

  ;; If ‘treesit-defun-type-regexp’ is non-nil, set up
  ;; ‘beginning-of-defun-function’ and ‘end-of-defun-function’.
  ;; If ‘treesit-defun-name-function’ is non-nil, set up
  ;; ‘add-log-current-defun’.
  (setq-local
   treesit-defun-type-regexp (erlang-ts-defun-regexp)
   treesit-defun-name-function 'erlang-ts-defun-function-name)

  ;; If ‘treesit-simple-imenu-settings’ is non-nil, set up Imenu.
  (setq-local
   treesit-simple-imenu-settings (erlang-ts-imenu-simple))

  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode erlang-ts-mode prog-mode "Erlang"
  "Major mode for editing Erlang, powered by tree-sitter."
  :group 'erlang-ts
  (when (treesit-ready-p 'erlang)
    (erlang-ts-setup)))

;;;###autoload
(dolist (r '("\\.erl$" "\\.app\\.src$" "\\.escript"
             "\\.hrl$" "\\.xrl$" "\\.yrl" "/ebin/.+\\.app"))
  (add-to-list 'auto-mode-alist (cons r 'erlang-ts-mode)))

(provide 'erlang-ts-mode)
;;; erlang-ts-mode.el ends here
