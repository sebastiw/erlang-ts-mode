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

;; install the parser, if needed.
(unless (treesit-language-available-p 'erlang)
  (let ((src '("https://github.com/sebastiw/erlang-ts-mode" "masse00" "tree-sitter-erlang/src")))
    (add-to-list 'treesit-language-source-alist (cons 'erlang src)))
  (treesit-install-language-grammar 'erlang))

;; register handled file types (or patterns, rather)
(add-to-list 'interpreter-mode-alist (cons "escript" 'erlang-ts-mode))
(let ((es '("\\.erl$" "\\.app\\.src$" "\\.hrl$" "\\.xrl$" "\\.yrl$" "/ebin/.+\\.app$")))
  (dolist (e es)
    (add-to-list 'auto-mode-alist (cons e 'erlang-ts-mode))))

;; load our subsystems
(require 'erlang-ts-acer nil t)
(require 'erlang-ts-defun nil t)
(require 'erlang-ts-fill nil t)
(require 'erlang-ts-flycheck nil t)
(require 'erlang-ts-fontlock nil t)
(require 'erlang-ts-imenu nil t)
(require 'erlang-ts-indent nil t)
(require 'erlang-ts-man nil t)
(require 'erlang-ts-keymap nil t)

;; some globals
(defvar erlang-ts-new-file-hook nil "Run when `erlang-ts-mode' is called in an empty buffer.")
(defvar erlang-ts-otp-version (erlang-ts-otp-version) "OTP version.")
(defvar erlang-ts-man-buffer "" "Man pages buffer.")
(defvar erlang-ts-cache-dir (concat user-emacs-directory "cache/") "Cache dir.")

;; these should be run before erlang-ts-mode
(make-directory erlang-ts-cache-dir t)
(erlang-ts-man-init)

;; This is run as if it was defined in `erlang-ts-mode-hook'. So
;; e.g. `setq-local' is set in the `.erl' buffer.
(defun erlang-ts-setup()
  "Run when `erlang-ts-mode' is called."
  (treesit-parser-create 'erlang)

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

  (when (zerop (buffer-size))
    (run-hooks 'erlang-ts-new-file-hook))

  (setq-local
   fill-paragraph-function 'erlang-ts-fill)

  (erlang-ts-acer-init)
  (erlang-ts-flycheck-init)
  (treesit-major-mode-setup))

(defgroup erlang-ts nil
  "Tree-sitter for Erlang."
  :group 'languages
  :prefix "erlang-ts-")

;;;###autoload
(define-derived-mode erlang-ts-mode prog-mode "Erlang"
  "Major mode for editing Erlang, powered by tree-sitter."
  :group 'erlang-ts
  (when (treesit-ready-p 'erlang)
    (erlang-ts-setup)))

(provide 'erlang-ts-mode)
;;; erlang-ts-mode.el ends here
