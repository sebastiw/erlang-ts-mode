;;; erlang-ts-man --- Display man pages.
;;; Commentary:
;;; Use erlang-ts-ts to get MFA, use woman to display.
;;; Code:

(require 'woman)
(require 'erlang-ts-ts)

(defun erlang-ts-man ()
  "Find manual page for mod:fun at point."
  (interactive)
  (pcase (erlang-ts-at-point)
    (`(call ,m ,f)
     (pcase (erlang-ts-acer-man m)
       ('nil (error "Can't find man page for %s:%s" m f))
       (file
        (woman-find-file file)
        (when f
          (when (re-search-forward (concat "\s" f "(") nil t)
            (beginning-of-line))))))))

(provide 'erlang-ts-man)
;;; erlang-ts-man.el ends here
