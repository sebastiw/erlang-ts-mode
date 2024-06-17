;;; erlang-ts-mode-map --- key map.
;;; Commentary:
;;; Code:

(defvar erlang-ts-mode-map
  ;; Similar bindings as erlang-mode
  (let ((map (make-sparse-keymap)))
    ;; (define-key map ";"       'erlang-ts-electric-semicolon)
    ;; (define-key map ","       'erlang-ts-electric-comma)
    ;; (define-key map "<"         'erlang-ts-electric-lt)
    ;; (define-key map ">"         'erlang-ts-electric-gt)
    ;; (define-key map "\C-m"      'erlang-ts-electric-newline)
    (define-key map (kbd "DEL") 'backward-delete-char-untabify)
    ;; (define-key map "\M-q"      'erlang-ts-fill-paragraph)
    ;; (define-key map "\M-\t"     'erlang-ts-complete-tag)
    ;; (define-key map "\C-c\M-\t" 'tempo-complete-tag)
    ;; (define-key map "\M-+"      'erlang-ts-find-next-tag)
    ;; (define-key map "\C-c\M-a"  'erlang-ts-beginning-of-clause)
    ;; (define-key map "\C-c\M-b"  'tempo-backward-mark)
    ;; (define-key map "\C-c\M-e"  'erlang-ts-end-of-clause)
    ;; (define-key map "\C-c\M-f"  'tempo-forward-mark)
    ;; (define-key map "\C-c\M-h"  'erlang-ts-mark-clause)
    (define-key map "\C-c\C-c"  'comment-region)
    ;; (define-key map "\C-c\C-j"  'erlang-ts-generate-new-clause)
    ;; (define-key map "\C-c\C-k"  'erlang-ts-compile)
    ;; (define-key map "\C-c\C-l"  'erlang-ts-compile-display)
    ;; (define-key map "\C-c\C-s"  'erlang-ts-show-syntactic-information)
    ;; (define-key map "\C-c\C-q"  'erlang-ts-indent-function)
    (define-key map "\C-c\C-u"  'uncomment-region)
    ;; (define-key map "\C-c\C-y"  'erlang-ts-clone-arguments)
    ;; (define-key map "\C-c\C-a"  'erlang-ts-align-arrows)
    ;; (define-key map "\C-c\C-z"  'erlang-ts-shell-display)
    ;; (define-key map "\C-c\C-d"  'erlang-ts-man-function-no-prompt)
    map)
  "Keymap for erlang-ts.")

(provide 'erlang-ts-mode-map)
;;; erlang-ts-mode-map.el ends here
