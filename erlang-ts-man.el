;;; erlang-ts-man --- Display man pages.
;;; Commentary:
;;; Use erlang-ts-ts to get MFA, use woman to display.
;;; Code:

(require 'woman)
(require 'erlang-ts-ts)
(require 'erlang-ts-acer)

(defvar erlang-ts-otp-version)
(defvar erlang-ts-cache-dir)
(defvar erlang-ts-man-buffer)
(defvar etsm--baseurl "http://erlang.org/download/" "OTP doc base url.")

(defun erlang-ts-man ()
  "Find manual page for mod:fun at point."
  (interactive)
  (pcase (erlang-ts-at-point)
    (`(remote-call ,m ,f ,_ ,_)
     (with-current-buffer erlang-ts-man-buffer
       (goto-char 1)
       (when (re-search-forward (concat "man/man3/" m ".3"))
         (woman-tar-extract-file)
         (when (and f (re-search-forward (concat "\s" f "(") nil t))
           (beginning-of-line)))))))

(defun erlang-ts-man-init ()
  "Open man file."
  (let ((local-file (etsm--file)))
    (when (file-readable-p local-file)
      (let ((currbuf (current-buffer))
            (manbuf (find-file local-file)))
        (switch-to-buffer currbuf)
        (bury-buffer manbuf)
        (setq erlang-ts-man-buffer manbuf)))))

(defun etsm--file ()
  "Man page file name (a compressed tar file with all the man pages).
We search the cache dir for a file with the correct OTP version.
If we find one, we return its name.  If there is no such file, we
search the OTP web site for a suitable file, download it to the
cache dir, and return its name.  If all of this fails, we return nil."
  (let* ((vsn erlang-ts-otp-version)
         (wild (concat "otp_doc_man_" vsn "\\(\\.[0-9]+\\)+\\.tar\\.gz"))
         (abswild (concat erlang-ts-cache-dir wild))
         (files (file-expand-wildcards abswild t t)))
    (pcase (car (reverse files))
      ('nil (etsm--download wild))
      (file file))))

(defun etsm--download (wild)
  "Download the newest man page file matching WILD."
  (let ((basename (etsm--get-basename wild)))
    (when basename
      (let ((url (concat etsm--baseurl basename))
            (local-file (concat erlang-ts-cache-dir basename)))
        (when (url-copy-file url local-file t)
          local-file)))))

(defun etsm--get-basename (wild)
  "Download and search OTP's man page index for regexp WILD.
If there are multiple hits, we want the last (newest) one."
  (let ((buff (url-retrieve-synchronously etsm--baseurl nil t 3)))
    (when buff
      (with-current-buffer buff
        (let (file)
          (goto-char 1)
          (while (re-search-forward wild nil t)
            (setq file (match-string 0)))
          file)))))

  (provide 'erlang-ts-man)
;;; erlang-ts-man.el ends here
