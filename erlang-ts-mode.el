;;; erlang-ts --- major mode for Erlang.
;;; Commentary:
;;; The canonical `erlang-mode' is as of this writing 30 years
;;; old.  This is a from-scratch mode based on tree-sitter, xref, and
;;; autocomplete.
;;; Code:

(require 'treesit)
(unless (featurep 'treesit)
  (error "Erlang-ts requires tree-sitter to be installed"))

(defvar treesit-language-source-alist)
(unless (treesit-language-available-p 'erlang)
  (add-to-list
   'treesit-language-source-alist
   (cons 'erlang '("https://git.sr.ht/~massemanet/tree-sitter-erlang")))
  (treesit-install-language-grammar 'erlang))

(defgroup erlang-ts nil
  "Tree-sitter for Erlang."
  :group 'languages
  :prefix "erlang-ts-")

(defun erlang-ts-setup()
  "Initialize."
  (require 'erlang-ts-mode-map nil t)
  (require 'erlang-ts-fontlock nil t)
  (require 'erlang-ts-indent nil t)
  (require 'erlang-ts-imenu nil t)
  (require 'erlang-ts-defun nil t)
  (treesit-major-mode-setup))

(defvar erlang-ts-mode-abbrev-table
  nil
  "Abbrev table in use in erlang-ts-mode buffers.")

;;;###autoload
(define-derived-mode erlang-ts-mode prog-mode "Erlang"
  "Major mode for editing Erlang, powered by tree-sitter."
  :group 'erlang-ts
  :abbrev-table erlang-ts-mode-abbrev-table
  (when (treesit-ready-p 'erlang)
    (treesit-parser-create 'erlang)
    (erlang-ts-setup)))

;;;###autoload
(dolist (r '("\\.erl$" "\\.app\\.src$" "\\.escript"
             "\\.hrl$" "\\.xrl$" "\\.yrl" "/ebin/.+\\.app"))
  (add-to-list 'auto-mode-alist (cons r 'erlang-ts-mode)))

(provide 'erlang-ts-mode)
;;; erlang-ts-mode.el ends here
