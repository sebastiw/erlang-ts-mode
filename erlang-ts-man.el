;;; erlang-ts-man --- Display man pages.
;;; Commentary:
;;; Use erlang-ts-ts to get MFA, use woman to display.
;;; Code:

(require 'woman)
(require 'acer)
(require 'erlang-ts-ts)

(defun erlang-ts-man ()
  "Find manual page for mod:fun at point."
  (interactive)
  (pcase (erlang-ts-at-point)
    (`(call . ,mfa)
     (message "mfa: %s" mfa)
     (pcase (split-string mfa "[:()]")
       (`(,mod ,fun ,_ ,_)
        (message "m f: %s %s" mod fun)
        (pcase (acer-man-file mod)
          ('nil (error "Can't find man page for %s:%s" mod fun))
          (file
           (woman-find-file file)
           (when fun
             (when (re-search-forward (concat "\s" fun "(") nil t)
               (beginning-of-line))))))))))

(provide 'erlang-ts-man)
;;; erlang-ts-man.el ends here
