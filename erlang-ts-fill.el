;;; erlang-ts-fill --- paragraph filler that knows about comments.
;;; Commentary:
;;; Stolen fro m`erlang-mode'.
;;;
;;; Code:

(defun erlang-ts-fill (justified)
  "Like \\[fill-paragraph], but handle Erlang comments.  Might be JUSTIFIED."
  (let ((has-comment nil)
        comment-fill-prefix)
    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (cond
       ;; Find the command prefix.
       ((looking-at (concat "\\s *" comment-start-skip))
        (setq has-comment t)
        (setq comment-fill-prefix (buffer-substring (match-beginning 0) (match-end 0))))
       ;; A line with some code, followed by a comment?  Remember that the
       ;; % which starts the comment shouldn't be part of a string or
       ;; character.
       ((progn
          (while (not (looking-at "%\\|$"))
            (skip-chars-forward "^%\n\"\\\\")
            (cond
             ((eq (char-after (point)) ?\\) (forward-char 2))
             ((eq (char-after (point)) ?\") (forward-sexp 1))))
          (looking-at comment-start-skip))
        (setq has-comment t)
        (setq comment-fill-prefix
              (concat (make-string (current-column) ? )
                      (buffer-substring (match-beginning 0) (match-end 0)))))))
    (when has-comment
      ;; Narrow to include only the comment, and then fill the region.
      (let ((beg (save-excursion
                   (while (and (zerop (forward-line -1))
                               (looking-at "^\\s *%")))
                   ;; We may have gone to far.  Go forward again.
                   (or (looking-at "^\\s *%")
                       (forward-line 1))
                   (point)))
            (end (save-excursion
                   (while (progn (forward-line 1)
                                 (looking-at "^\\s *%")))
                   (point))))
                         
        (save-restriction
          (narrow-to-region beg end)
          ;; Lines with only % on them can be paragraph boundaries.
          (let ((paragraph-start (concat paragraph-start "\\|^[ \t%]*$"))
                (paragraph-separate (concat paragraph-start "\\|^[ \t%]*$"))
                (fill-prefix comment-fill-prefix))
            (fill-paragraph justified)))))))

(provide 'erlang-ts-fill)
;;; erlang-ts-fill.el ends here
