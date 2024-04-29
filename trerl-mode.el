(when (< emacs-major-version 29)
    (error "trErl requires Emacs >= 29.0"))

;; Tree-sitter offers different features than emacs’ native
;; parser. two things are required to activate it:
;;     1. Install a Tree-sitter library for the required language.
;;     2. A major mode that invokes the Tree-sitter backend.
;;
;; Usual naming of ts-major mode would be LANG-ts-mode, e.g. erlang-ts-mode

(require 'treesit)
(unless (featurep 'treesit)
  (error "trErl requires tree-sitter to be installed"))

(unless (treesit-language-available-p 'erlang)
  (error "trErl requires tresit-language for Erlang to be installed"))

(add-to-list 'treesit-language-source-alist (cons 'erlang '("https://github.com/WhatsApp/tree-sitter-erlang")))
(if (not (treesit-language-available-p 'erlang))
    (treesit-install-language-grammar 'erlang))

(defgroup trerl nil
  "Tree-sitter for Erlang."
  :group 'languages
  :prefix "trerl-")

(require 'trerl-fontlock)

(defun trerl-setup()
  (when (treesit-ready-p 'erlang)
    (treesit-parser-create 'erlang)

    ;; treesit-font-lock-settings
    (trerl-treesit-font-lock-settings-setup)

    ;; treesit-simple-indent-rules

    ;; treesit-defun-type-regexp

    ;; treesit-defun-name-function

    ;; treesit-simple-imenu-settings

    (treesit-major-mode-setup)))

(defvar trerl-mode-map
  ;; Similar bindings as erlang-mode
  (let ((map (make-sparse-keymap)))
    ; (define-key map ";"       'trerl-electric-semicolon)
    ; (define-key map ","       'trerl-electric-comma)
    ; (define-key map "<"         'trerl-electric-lt)
    ; (define-key map ">"         'trerl-electric-gt)
    ; (define-key map "\C-m"      'trerl-electric-newline)
    (define-key map (kbd "DEL") 'backward-delete-char-untabify)
    ; (define-key map "\M-q"      'trerl-fill-paragraph)
    ; (define-key map "\M-\t"     'trerl-complete-tag)
    ; (define-key map "\C-c\M-\t" 'tempo-complete-tag)
    ; (define-key map "\M-+"      'trerl-find-next-tag)
    ; (define-key map "\C-c\M-a"  'trerl-beginning-of-clause)
    ; (define-key map "\C-c\M-b"  'tempo-backward-mark)
    ; (define-key map "\C-c\M-e"  'trerl-end-of-clause)
    ; (define-key map "\C-c\M-f"  'tempo-forward-mark)
    ; (define-key map "\C-c\M-h"  'trerl-mark-clause)
    (define-key map "\C-c\C-c"  'comment-region)
    ; (define-key map "\C-c\C-j"  'trerl-generate-new-clause)
    ; (define-key map "\C-c\C-k"  'trerl-compile)
    ; (define-key map "\C-c\C-l"  'trerl-compile-display)
    ; (define-key map "\C-c\C-s"  'trerl-show-syntactic-information)
    ; (define-key map "\C-c\C-q"  'trerl-indent-function)
    (define-key map "\C-c\C-u"  'uncomment-region)
    ; (define-key map "\C-c\C-y"  'trerl-clone-arguments)
    ; (define-key map "\C-c\C-a"  'trerl-align-arrows)
    ; (define-key map "\C-c\C-z"  'trerl-shell-display)
    ; (define-key map "\C-c\C-d"  'trerl-man-function-no-prompt)
    map)
  "Keymap for trErl.")

(defvar trerl-mode-syntax-table
  ;; Import from erlang-mode
  (let ((table (make-syntax-table)))
      (modify-syntax-entry ?\n ">" table)
      (modify-syntax-entry ?\" "\"" table)
      (modify-syntax-entry ?# "." table)
      ;; (modify-syntax-entry ?$ "\\" table)   ;; Creates problems with indentation afterwards
      ;; (modify-syntax-entry ?$ "'" table)    ;; Creates syntax highlighting and indentation problems
      (modify-syntax-entry ?$ "/" table)    ;; Misses the corner case "string that ends with $"
      ;; we have to live with that for now..it is the best alternative
      ;; that can be worked around with "string that ends with \$"
      (modify-syntax-entry ?% "<" table)
      (modify-syntax-entry ?& "." table)
      (modify-syntax-entry ?\' "\"" table)
      (modify-syntax-entry ?* "." table)
      (modify-syntax-entry ?+ "." table)
      (modify-syntax-entry ?- "." table)
      (modify-syntax-entry ?/ "." table)
      (modify-syntax-entry ?: "." table)
      (modify-syntax-entry ?< "." table)
      (modify-syntax-entry ?= "." table)
      (modify-syntax-entry ?> "." table)
      (modify-syntax-entry ?\\ "\\" table)
      (modify-syntax-entry ?_ "_" table)
      (modify-syntax-entry ?| "." table)
      (modify-syntax-entry ?^ "'" table)

      ;; Pseudo bit-syntax: Latin1 double angle quotes as parens.
      ;;(modify-syntax-entry ?\253 "(?\273" table)
      ;;(modify-syntax-entry ?\273 ")?\253" table)

      (setq erlang-mode-syntax-table table))
  "Syntax table in use in trerl-mode buffers.")

(defvar trerl-mode-abbrev-table nil
  "Abbrev table in use in trerl-mode buffers.")

;;;###autoload
(define-derived-mode trerl-mode prog-mode "Erlang"
  "Major mode for editing Erlang, powered by tree-sitter."
  :group 'trerl
  ; :syntax-table trerl-mode-syntax-table
  :abbrev-table trerl-mode-abbrev-table
  (trerl-setup))

;;;###autoload
(dolist (r '("\\.erl$" "\\.app\\.src$" "\\.escript"
             "\\.hrl$" "\\.xrl$" "\\.yrl" "/ebin/.+\\.app"))
  (add-to-list 'auto-mode-alist (cons r 'trerl-mode)))

(provide 'trerl-mode)
