(require 'treesit nil t)
(unless (featurep 'treesit)
  (error "trErl requires tree-sitter to be installed"))

(unless (and (version<= "29.1" emacs-version) (treesit-language-available-p 'erlang))
  (add-to-list 'treesit-language-source-alist (cons 'erlang '("https://github.com/WhatsApp/tree-sitter-erlang")))
  (treesit-install-language-grammar 'erlang))

(defgroup erlang-ts nil
  "Tree-sitter for Erlang."
  :group 'languages
  :prefix "erlang-ts-")

(require 'erlang-ts-fontlock)
(require 'erlang-ts-indent)
(require 'erlang-ts-imenu)
(require 'erlang-ts-defun)

(defun erlang-ts-setup()
  (when (treesit-ready-p 'erlang)
    (treesit-parser-create 'erlang)

    ;; treesit-font-lock-settings
    (erlang-ts-treesit-font-lock-settings-setup)

    ;; treesit-simple-indent-rules
    (erlang-ts-treesit-simple-indent-rules-setup)

    ;; treesit-defun-type-regexp
    (erlang-ts-treesit-defun-type-regexp-setup)

    ;; treesit-defun-name-function
    (erlang-ts-treesit-defun-name-function-setup)

    ;; treesit-simple-imenu-settings
    (erlang-ts-treesit-simple-imenu-settings-setup)

    (treesit-major-mode-setup)))

(defvar erlang-ts-mode-map
  ;; Similar bindings as erlang-mode
  (let ((map (make-sparse-keymap)))
    ; (define-key map ";"       'erlang-ts-electric-semicolon)
    ; (define-key map ","       'erlang-ts-electric-comma)
    ; (define-key map "<"         'erlang-ts-electric-lt)
    ; (define-key map ">"         'erlang-ts-electric-gt)
    ; (define-key map "\C-m"      'erlang-ts-electric-newline)
    (define-key map (kbd "DEL") 'backward-delete-char-untabify)
    ; (define-key map "\M-q"      'erlang-ts-fill-paragraph)
    ; (define-key map "\M-\t"     'erlang-ts-complete-tag)
    ; (define-key map "\C-c\M-\t" 'tempo-complete-tag)
    ; (define-key map "\M-+"      'erlang-ts-find-next-tag)
    ; (define-key map "\C-c\M-a"  'erlang-ts-beginning-of-clause)
    ; (define-key map "\C-c\M-b"  'tempo-backward-mark)
    ; (define-key map "\C-c\M-e"  'erlang-ts-end-of-clause)
    ; (define-key map "\C-c\M-f"  'tempo-forward-mark)
    ; (define-key map "\C-c\M-h"  'erlang-ts-mark-clause)
    (define-key map "\C-c\C-c"  'comment-region)
    ; (define-key map "\C-c\C-j"  'erlang-ts-generate-new-clause)
    ; (define-key map "\C-c\C-k"  'erlang-ts-compile)
    ; (define-key map "\C-c\C-l"  'erlang-ts-compile-display)
    ; (define-key map "\C-c\C-s"  'erlang-ts-show-syntactic-information)
    ; (define-key map "\C-c\C-q"  'erlang-ts-indent-function)
    (define-key map "\C-c\C-u"  'uncomment-region)
    ; (define-key map "\C-c\C-y"  'erlang-ts-clone-arguments)
    ; (define-key map "\C-c\C-a"  'erlang-ts-align-arrows)
    ; (define-key map "\C-c\C-z"  'erlang-ts-shell-display)
    ; (define-key map "\C-c\C-d"  'erlang-ts-man-function-no-prompt)
    map)
  "Keymap for trErl.")

(defvar erlang-ts-mode-syntax-table
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
  "Syntax table in use in erlang-ts-mode buffers.")

(defvar erlang-ts-mode-abbrev-table nil
  "Abbrev table in use in erlang-ts-mode buffers.")

;;;###autoload
(define-derived-mode erlang-ts-mode prog-mode "Erlang"
  "Major mode for editing Erlang, powered by tree-sitter."
  :group 'erlang-ts
  ; :syntax-table erlang-ts-mode-syntax-table
  :abbrev-table erlang-ts-mode-abbrev-table
  (erlang-ts-setup))

;;;###autoload
(dolist (r '("\\.erl$" "\\.app\\.src$" "\\.escript"
             "\\.hrl$" "\\.xrl$" "\\.yrl" "/ebin/.+\\.app"))
  (add-to-list 'auto-mode-alist (cons r 'erlang-ts-mode)))

(provide 'erlang-ts-mode)
