;;; erlang-ts-mode-map --- key map.
;;; Commentary:
;;; Code:

(defvar erlang-ts-mode-map)

;; Similar bindings as erlang-mode
(defun erlang-ts-mode-keys ()
  "Define default keys in erlang-ts-mode-map."
  (let ((map erlang-ts-mode-map))
    (define-key map (kbd ";")       (message "not yet implemented: %s" 'erlang-ts-electric-semicolon))
    (define-key map (kbd ",")       (message "not yet implemented: %s" 'erlang-ts-electric-comma))
    (define-key map (kbd "<")       (message "not yet implemented: %s" 'erlang-ts-electric-lt))
    (define-key map (kbd ">")       (message "not yet implemented: %s" 'erlang-ts-electric-gt))
    (define-key map (kbd "C-m")     (message "not yet implemented: %s" 'erlang-ts-electric-newline))
    (define-key map (kbd "DEL")     'backward-delete-char-untabify)
    (define-key map (kbd "M-q")     (message "not yet implemented: %s" 'erlang-ts-fill-paragraph))
    (define-key map (kbd "M-t")     (message "not yet implemented: %s" 'erlang-ts-complete-tag))
    (define-key map (kbd "C-c M-t") (message "not yet implemented: %s" 'tempo-complete-tag))
    (define-key map (kbd "M-+")     (message "not yet implemented: %s" 'erlang-ts-find-next-tag))
    (define-key map (kbd "C-c M-a") (message "not yet implemented: %s" 'erlang-ts-beginning-of-clause))
    (define-key map (kbd "C-c M-b") (message "not yet implemented: %s" 'tempo-backward-mark))
    (define-key map (kbd "C-c M-e") (message "not yet implemented: %s" 'erlang-ts-end-of-clause))
    (define-key map (kbd "C-c M-f") (message "not yet implemented: %s" 'tempo-forward-mark))
    (define-key map (kbd "C-c M-h") (message "not yet implemented: %s" 'erlang-ts-mark-clause))
    (define-key map (kbd "C-c C-c")  'comment-region)
    (define-key map (kbd "C-c C-d")  'erlang-ts-man)
    (define-key map (kbd "C-c C-j") (message "not yet implemented: %s" 'erlang-ts-generate-new-clause))
    (define-key map (kbd "C-c C-k") (message "not yet implemented: %s" 'erlang-ts-compile))
    (define-key map (kbd "C-c C-l") (message "not yet implemented: %s" 'erlang-ts-compile-display))
    (define-key map (kbd "C-c C-s") (message "not yet implemented: %s" 'erlang-ts-show-syntactic-information))
    (define-key map (kbd "C-c C-q") (message "not yet implemented: %s" 'erlang-ts-indent-function))
    (define-key map (kbd "C-c C-u")  'uncomment-region)
    (define-key map (kbd "C-c C-u")  'uncomment-region)
    (define-key map (kbd "C-c C-o")  'xref-find-definitions)
    (define-key map (kbd "C-c C-p")  'xref-pop-marker-stack)
    (define-key map (kbd "C-c C-y") (message "not yet implemented: %s" 'erlang-ts-clone-arguments))
    (define-key map (kbd "C-c C-a") (message "not yet implemented: %s" 'erlang-ts-align-arrows))
    (define-key map (kbd "C-c C-z") (message "not yet implemented: %s" 'erlang-ts-shell-display))
    map)
  "Keymap for erlang-ts.")

(provide 'erlang-ts-mode-map)
;;; erlang-ts-mode-map.el ends here
