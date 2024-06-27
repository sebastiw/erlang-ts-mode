;;; erlang-ts-mode-map --- key map.
;;; Commentary:
;;; Code:

(defvar erlang-ts-mode-map)

;; Similar bindings as erlang-mode
(defun erlang-ts-mode-keys ()
  "Define default keys in erlang-ts-mode-map."
  (let ((map erlang-ts-mode-map))
    (define-key map (kbd "TAB")     'erlang-ts-acer-tab)
    (etsmm--nyi '(define-key map (kbd ";")       'erlang-ts-electric-semicolon))
    (etsmm--nyi '(define-key map (kbd ",")       'erlang-ts-electric-comma))
    (etsmm--nyi '(define-key map (kbd "<")       'erlang-ts-electric-lt))
    (etsmm--nyi '(define-key map (kbd ">")       'erlang-ts-electric-gt))
    (etsmm--nyi '(define-key map (kbd "C-m")     'erlang-ts-electric-newline))
    (define-key map (kbd "DEL")     'backward-delete-char-untabify)
    (etsmm--nyi '(define-key map (kbd "M-q")     'erlang-ts-fill-paragraph))
    (etsmm--nyi '(define-key map (kbd "M-t")     'erlang-ts-complete-tag))
    (define-key map (kbd "C-c M-t") 'tempo-complete-tag)
    (etsmm--nyi '(define-key map (kbd "M-+")     'erlang-ts-find-next-tag))
    (etsmm--nyi '(define-key map (kbd "C-c M-a") 'erlang-ts-beginning-of-clause))
    (define-key map (kbd "C-c M-b") 'tempo-backward-mark)
    (etsmm--nyi '(define-key map (kbd "C-c M-e") 'erlang-ts-end-of-clause))
    (define-key map (kbd "C-c M-f") 'tempo-forward-mark)
    (etsmm--nyi '(define-key map (kbd "C-c M-h") 'erlang-ts-mark-clause))
    (define-key map (kbd "C-c C-c") 'comment-region)
    (define-key map (kbd "C-c C-d") 'erlang-ts-man)
    (etsmm--nyi '(define-key map (kbd "C-c C-j") 'erlang-ts-generate-new-clause))
    (etsmm--nyi '(define-key map (kbd "C-c C-k") 'erlang-ts-compile))
    (etsmm--nyi '(define-key map (kbd "C-c C-l") 'erlang-ts-compile-display))
    (etsmm--nyi '(define-key map (kbd "C-c C-s") 'erlang-ts-show-syntactic-information))
    (etsmm--nyi '(define-key map (kbd "C-c C-q") 'erlang-ts-indent-function))
    (define-key map (kbd "C-c C-u") 'uncomment-region)
    (define-key map (kbd "C-c C-o") 'xref-find-definitions)
    (define-key map (kbd "C-c C-p") 'xref-pop-marker-stack)
    (etsmm--nyi '(define-key map (kbd "C-c C-y") 'erlang-ts-clone-arguments))
    (etsmm--nyi '(define-key map (kbd "C-c C-a") 'erlang-ts-align-arrows))
    (etsmm--nyi '(define-key map (kbd "C-c C-z") 'erlang-ts-shell-display))))

(defun etsmm--nyi (def)
  (pcase def
    (`(,_ ,_ (kbd ,k) ,c) (message "erlang-ts-mode-map: not yet implemented: %s -> %s" k c))))

(provide 'erlang-ts-mode-map)
;;; erlang-ts-mode-map.el ends here
