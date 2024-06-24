;;; acer --- Summary
;;; Commentary:
;;; Auto Complete for erlang.
;;;
;;; We want to use xref and auto-complete for erlang projects.  A
;;; project is; 1) the current file, 2) the other files in the same
;;; application, 3) other included applications (in an umbrella
;;; project), 4) dependencies, and 5) OTP.
;;;
;;;
;;; Code:
;;; -*- lexical-binding: t -*-

(require 'auto-complete)
(require 'xref)
(require 'cl-generic)
(require 'cl-lib)

(defvar erlang-ts-man-dir (concat user-emacs-directory "cache/erlang_mode_man_pages/"))

;; declare externals
(declare-function erlang-ts-ts-args "erlang")
(declare-function erlang-ts-ts-arity "erlang")
(declare-function erlang-ts-ts-module "erlang")
(declare-function erlang-ts-ts-fun "erlang")
(declare-function erlang-ts-ts-thing "erlang")
(declare-function erlang-ts-ts-import "erlang")

;; global variables
(defvar acer-path (file-name-directory load-file-name))
(defvar acer-escript (concat acer-path "acer.escript"))
(defvar acer-man-path (concat erlang-ts-man-dir "man/" "man3/"))

;; OTP buffers
(defvar acer-buffer-man (get-buffer-create "*acer-man*"))
(defvar acer-buffer-bifs (get-buffer-create "*acer-bifs*"))
(defvar acer-buffer-guards (get-buffer-create "*acer-guards*"))
(defvar acer-buffer-words (get-buffer-create "*acer-words*"))
(defvar acer-buffer-otp-erls (get-buffer-create "*acer-erls-otp*"))
(defvar acer-buffer-otp-srcs (get-buffer-create "*acer-srcs-otp*"))

;; buffer local variables
(defvar-local acer-project-name ())

;; project buffers
(defvar-local acer-buffer-erls ())
(defvar-local acer-buffer-funs ())
(defvar-local acer-buffer-srcs ())

;;; We do most of our work in lists of this base struct
(cl-defstruct acer-item mod fun arity line args file)

(defun erlang-ts-acer-init ()
  "Init acer in current buffer."
  (acer--project-name (buffer-file-name))
  (setq acer-buffer-srcs (acer--create-buffer "paths")
        acer-buffer-funs (acer--create-buffer "funs")
        acer-buffer-erls (acer--create-buffer "erls"))
  (acer--fill-initial)
  (acer--init-ac)
  (acer--init-xref)
  (local-set-key (kbd "TAB") 'acer--tab))

(defun erlang-ts-acer-libs (&optional filename)
  "All libs in the project that FILENAME belongs to."
  (let ((f (if filename filename (buffer-file-name))))
    (pcase (split-string (acer--run-escript-str "libs" f))
      ((and paths (guard (string= "libs:" (car paths))))
       (cdr paths)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; we're using TAB to do 3 things; indenting, replacing, and
;; completing.

(defun acer--tab ()
  "Interactive function (normally bound to TAB)."
  (interactive)
  (or (acer--indent)
      (acer--replace)
      (acer--complete)))

(defun acer--indent ()
  "Try to indent, and return t if point moved."
  (not (= 0 (indent-according-to-mode))))

(defun acer--replace ()
  "Try to replace, and return t if replacement happened."
  (acer-paren))

(defun acer--complete ()
  "Try to complete."
  (auto-complete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete framework

(defun acer--init-ac ()
  "Initialize auto-complete."
  (auto-complete-mode)
  (setq ac-sources
        '(acer-source-mfa
          acer-source-mf
          acer-source-fa
          acer-source-f
          acer-source-var
          acer-source-macro
          acer-source-rec)))

(defvar acer-source-mfa
  '((prefix . acer-prefix-mfa)
    (candidates . acer-candidates-mfa))
  "Define source `mfa' - completes `m:f\('.")

(defvar acer-source-mf
  '((prefix . acer-prefix-mf)
    (candidates . acer-candidates-mf))
  "Define source `mf' - complete `m:f'.")

(defvar acer-source-fa
  '((prefix . acer-prefix-fa)
    (candidates . acer-candidates-fa))
  "Define source `fa' - complete `f\('.")

(defvar acer-source-f
  '((prefix . acer-prefix-f)
    (candidates . acer-candidates-f))
  "Define source `f' - complete `frag'.")

(defvar acer-source-var
  '((prefix . acer-prefix-var)
    (candidates . acer-candidates-var))
  "Define source `var' - complete `Var'.")

(defvar acer-source-macro
  '((prefix . acer-prefix-macro)
    (candidates . acer-candidates-macro))
  "Define source `macro' - complete `?m'.")

(defvar acer-source-rec
  '((prefix . acer-prefix-rec)
    (candidates . acer-candidates-rec))
  "Define source `rec' - complete `\#rec'.")

;; prefix finders
(defun acer-prefix-mfa ()
  "Return start pos of thing if it is `m:f\('."
  (acer--match-left "[a-z][a-zA-Z0-9_]*:[a-z][a-zA-Z0-9_]*("))

(defun acer-prefix-mf ()
  "Return start pos of thing if it is `m:f'."
  (acer--match-left "[a-z][a-zA-Z0-9_]*:[a-z][a-zA-Z0-9_]*"))

(defun acer-prefix-fa ()
  "Return start pos of thing if it is `f\('."
  (acer--match-left "[a-z][a-zA-Z0-9_]*("))

(defun acer-prefix-f ()
  "Return start pos of thing if it is `f'."
  (acer--match-left "[a-z][a-zA-Z0-9_]*"))

(defun acer-prefix-var ()
  "Return start pos of thing if it is `Var'."
  (acer--match-left "[A-Z][a-zA-Z0-9_]*"))

(defun acer-prefix-macro ()
  "Return start pos of thing if it is `?m'."
  (acer--match-left "\\?'?[a-zA-Z0-9_]*"))

(defun acer-prefix-rec ()
  "Return start pos of thing if it is `\#rec'."
  (acer--match-left "#'?[a-z][a-zA-Z0-9_]*"))

;; candidate finders
(defun acer-candidates-mfa ()
  "Get m:f( candidates."
  (let* ((ai (acer--item-from-string-mfa ac-prefix))
         (ais (acer--expand-mf-to-a ai)))
    (mapcar #'acer--print-mfa ais)))

(defun acer-candidates-mf ()
  "Get m:f candidates."
  (let* ((ai (acer--item-from-string-mf ac-prefix))
         (ais (acer--expand-m-to-f ai)))
    (mapcar #'acer--print-mf ais)))

(defun acer-candidates-fa ()
  "Get f( candidates."
  (let* ((ai (acer--item-from-string-fa ac-prefix))
         (ais (acer--expand-f-to-a ai)))
    (mapcar #'acer--print-fa ais)))

(defun acer-candidates-f ()
  "Get fragment (either mod or local fun) candidates."
  (mapcar #'acer--print-f (acer--expand-nil ac-prefix)))

(defun acer-candidates-var ()
  "Get Var candidates."
  (acer--get-vars ac-prefix))

(defun acer-candidates-macro ()
  "Get ?MACRO candidates."
  (acer--get-macros ac-prefix))

(defun acer-candidates-rec ()
  "Get \#rec candidates."
  (mapcar (lambda(m) (concat "#" m)) (acer--get-recs ac-prefix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xref framework

(defun acer--init-xref ()
  "Initialize xref."
  (add-hook 'xref-backend-functions #'acer--xref-backend nil t))

(defun acer--xref-backend ()
  "Declare xref backend."
  'acer)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql acer)))
  "Return acer-item for item at point."
  (pcase (erlang-get-identifier-at-point)
    (`(qualified-function ,m ,f ,a)
     (acer--make-item m f a))
    ((and `(nil ,m ,f ,a) (guard (string= m (erlang-get-module))))
     (acer--make-item nil f a))
    (`(nil ,m ,f ,a)
     (acer--make-item m f a))))

(cl-defmethod xref-backend-definitions ((_backend (eql acer)) ai)
  "List of `xref-item' for `acer-item' AI."
  (mapcar #'acer--ai-to-xi (acer--expand-definitions ai)))

(cl-defmethod xref-backend-apropos ((_backend (eql acer)) frag)
  "List of `xref-item' fragment FRAG."
  (mapcar #'acer--ai-to-xi (acer--expand-nil frag)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql acer)))
  "A completion table."
  ())

(defun acer--ai-to-xi (ai)
  "Construct `xref-item' from `acer-item' AI."
  (let* ((m (acer-item-mod ai))
         (mod (when m (concat m ":")))
         (fun (acer-item-fun ai))
         (arity (acer-item-arity ai))
         (file (acer-item-file ai))
         (line (string-to-number (acer-item-line ai)))
         (summary (concat mod fun "/" arity))
         (location (xref-make-file-location file line 0)))
    (xref-make summary location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expanders. these read from the *acer-* buffers.
;; They all map `acer-item' -> list of `acer-item'.

(defun acer--expand-definitions (ai)
  "Return list of `acer-item' completing `acer-item' AI."
  (let ((mod (acer-item-mod ai))
        (fun (acer-item-fun ai)))
    (if mod
        (acer--expand-mf-to-a ai)
      (acer--expand-f-to-a ai))))

(defun acer--expand-mf-to-a (ai)
  "Return list of `acer-item' by completing AI.
AI.mod and AI.fun should be completed."
  (let ((ai (car (acer--expand-m-to-m ai))))
    (when ai
      (let* ((mod (acer-item-mod ai))
             (fun (acer-item-fun ai))
             (arity (acer-item-arity ai))
             (file (acer-item-file ai))
             (mfa (concat mod ":" fun "/" arity))
             (buffs (list acer-buffer-erls acer-buffer-otp-erls))
             (ais))
        (acer--fill-funs ai)
        (with-current-buffer acer-buffer-funs
          (goto-char 1)
          (while (re-search-forward mfa nil t)
            (push (acer--item-from-line-funs) ais)))
        (mapcar (lambda (aii) (acer--item-merge aii ai)) (nreverse ais))))))

(defun acer--expand-m-to-f (ai)
  "Return list of `acer-item' that completes M:F from AI.
AI.mod should be completed."
  (let ((ai (car (acer--expand-m-to-m ai))))
    (when ai
      (let* ((mod (acer-item-mod ai))
             (fun (acer-item-fun ai))
             (file (acer-item-file ai))
             (mf (concat mod ":" fun))
             (ais))
        (acer--fill-funs ai)
        (with-current-buffer acer-buffer-funs
          (goto-char 1)
          (while (re-search-forward mf nil t)
            (push (acer--item-from-line-funs) ais)))
        (mapcar (lambda (aii) (acer--item-merge aii ai)) (nreverse ais))))))

(defun acer--expand-f-to-a (ai)
  "Return list of `acer-item' with things that complete AI.
AI.mod is ignored, AI.fun is completed."
  (append (acer--expand-f-to-a-local ai)
          (acer--expand-f-to-a-imports ai)
          (acer--expand-f-to-a-bifs ai)))

(defun acer--expand-f-to-a-local (ai)
  "Return list of `acer-item' by completing AI in local buffer.
AI.mod is ignored, AI.fun should be completed."
  (let ((file (buffer-file-name))
        (mod (erlang-get-module))
        (fun (acer-item-fun ai))
        (arity (acer-item-arity ai))
        (hit))
    (save-excursion
      (goto-char 1)
      (while (and (null hit)
                  (erlang-match-next-function (point-max)))
        (beginning-of-line)
        (let* ((f (erlang-get-function-name))
               (args (erlang-get-function-arguments))
               (a (when args (number-to-string (erlang-get-function-arity))))
               (line (number-to-string (line-number-at-pos))))
          (end-of-line)
          (when (and args
                     (equal f fun)
                     (if arity (equal a arity) t))
            (setq hit (acer--make-item mod f a line args file))))))
    (when hit (list hit))))

(defun acer--expand-f-to-a-imports (ai)
  "Return list of `acer-item' where AI.fun is imported.
AI.fun should be completed.  AI.mod is ignored."
  ()) ;; TODO: placeholder.

(defun acer--expand-f-to-a-bifs (ai)
  "Return list of `acer-item' where AI.fun is autoimported from `erlang'.
AI.fun should be completed.  AI.mod is ignored."
  (acer--expand-mf-to-a (acer--item-set 'mod "erlang" ai)))

(defun acer--expand-nil (frag)
  "Return list of `acer-item' with things that complete string FRAG.
We try X = module and X = local function."
  (append (acer--expand-nil-to-m (acer--make-item frag))
          (acer--expand-nil-to-f (acer--make-item nil frag))))

(defun acer--expand-nil-to-m (ai)
  "Return list of `acer-item' by completing AI.mod."
  (append (acer--expand-nil-to-m-in-buffer ai acer-buffer-otp-erls)
          (acer--expand-nil-to-m-in-buffer ai acer-buffer-erls)))

(defun acer--expand-nil-to-f (ai)
  "Return list of `acer-item' with things that complete AI.
AI.mod is ignored."
  (append (acer--expand-nil-to-f-local ai)
          (acer--expand-nil-to-f-imports ai)
          (acer--expand-nil-to-f-bifs ai)))

(defun acer--expand-nil-to-f-local (ai)
  "Return list of `acer-item' by completing AI in local buffer.
AI.mod is ignored,"
  (let ((file (buffer-file-name))
        (mod (erlang-get-module))
        (fun (acer-item-fun ai))
        (ais))
    (save-excursion
      (goto-char 1)
      (while (erlang-match-next-function (point-max))
        (beginning-of-line)
        (let* ((f (erlang-get-function-name))
               (args (erlang-get-function-arguments))
               (a (when args (number-to-string (erlang-get-function-arity))))
               (line (number-to-string (line-number-at-pos))))
          (end-of-line)
          (when (and args
                     (string-prefix-p fun f))
            (push (acer--make-item mod f a line args file) ais)))))
    (nreverse ais)))

(defun acer--expand-nil-to-f-imports (ai)
  "Return list of `acer-item' where AI.fun is imported."
  ()) ;; TODO: placeholder.

(defun acer--expand-nil-to-f-bifs (ai)
  "Return list of `acer-item' where AI.fun is autoimported from `erlang'."
  (let ((patt (concat "^" (acer-item-fun ai)))
        (ais))
    (with-current-buffer acer-buffer-bifs
      (goto-char 1)
      (while (re-search-forward patt nil t)
        (push (acer--make-item "erlang" (acer--line-to-string t)) ais)))
    (mapcan #'acer--expand-m-to-f ais)))

(defun acer--expand-m-to-m (ai)
  "Return list of `acer-item'.
AI.mod should be completed."
  (append (acer--expand-m-to-m-in-buffer ai acer-buffer-otp-erls)
          (acer--expand-m-to-m-in-buffer ai acer-buffer-erls)))

(defun acer--expand-nil-to-m-in-buffer (ai buffer)
  "Expand AI as module in BUFFER."
  (let ((case-fold-search nil)
        (pattern (concat "^" (acer-item-mod ai)))
        (ais))
    (with-current-buffer buffer
      (goto-char 1)
      (while (re-search-forward pattern nil t)
        (push (acer--item-from-line-erls) ais)))
    (nreverse ais)))

(defun acer--expand-m-to-m-in-buffer (ai buffer)
  "Expand AI as module in BUFFER.
AI.mod should be completed."
  (let ((case-fold-search nil)
        (pattern (concat "^" (acer-item-mod ai) "=>")))
    (with-current-buffer buffer
      (goto-char 1)
      (when (re-search-forward pattern nil t)
        (list (acer--item-merge ai (acer--item-from-line-erls)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fillers. These write to the *acer-* buffers.

(defun acer--project-name (filename)
  "Use FILENAME to set project name."
  (unless acer-project-name
    (setq acer-project-name (acer--run-escript-str "name" filename))))

(defun acer--fill-initial ()
  "Fills buffers."
  (acer--fill-man)
  (acer--fill-bifs)
  (acer--fill-guards)
  (acer--fill-words)
  (acer--fill-otp-srcs)
  (acer--fill-project-srcs)
  (acer--fill-erls acer-buffer-otp-srcs acer-buffer-otp-erls)
  (acer--fill-erls acer-buffer-srcs acer-buffer-erls))

(defun acer--fill-man ()
  "Populate paths to all OTP man files."
  (with-current-buffer acer-buffer-man
    (unless (< 0 (buffer-size))
      (acer--run-escript "man" acer-man-path))))

(defun acer--fill-otp-srcs ()
  "Populate paths to all directories containing project erls."
  (with-current-buffer acer-buffer-otp-srcs
    (unless (< 0 (buffer-size))
      (acer--run-escript "srcs" "otp"))))

(defun acer--fill-project-srcs ()
  "Populate paths to all directories containing project erls."
  (let ((file (buffer-file-name)))
    (with-current-buffer acer-buffer-srcs
      (unless (< 0 (buffer-size))
        (acer--run-escript "srcs" file)))))

(defun acer--fill-erls (pbuff buff)
  "Populate BUFF with paths to all erl files with paths from PBUFF."
  (let ((paths (acer--paths pbuff)))
    (with-current-buffer buff
      (unless (< 0 (buffer-size))
        (mapc (lambda(p) (acer--run-escript "erls" p)) paths)))))

(defun acer--fill-funs (ai)
  "Populate all AI.mod's exported functions as per AI.file."
  (let ((mod (acer-item-mod ai))
        (file (acer-item-file ai)))
    (unless (acer--has-funs-p mod)
      (with-current-buffer acer-buffer-funs
        (acer--run-escript "funs" file)))))

(defun acer--fill-bifs ()
  "Populate bifs buffer."
  (with-current-buffer acer-buffer-bifs
    (acer--run-escript "bifs")))

(defun acer--fill-guards ()
  "Populate guards buffer."
  (with-current-buffer acer-buffer-guards
    (acer--run-escript "guards")))

(defun acer--fill-words ()
  "Populate words buffer."
  (with-current-buffer acer-buffer-words
    (acer--run-escript "words")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pretty-printers

(defun acer--print-mfa (ai)
  "Map `acer-item' AI to m:f(as) string."
  (let ((mod (acer-item-mod ai))
        (fun (acer-item-fun ai))
        (args (acer-item-args ai)))
    (concat mod ":" fun "(" args ")")))

(defun acer--print-mf (ai)
  "Map `acer-item' AI to m:f( string."
  (let ((mod (acer-item-mod ai))
        (fun (acer-item-fun ai)))
    (concat mod ":" fun "(")))

(defun acer--print-fa (ai)
  "Map `acer-item' AI to f(as) string."
  (let ((fun (acer-item-fun ai))
        (args (acer-item-args ai)))
    (concat fun "(" args ")")))

(defun acer--print-f (ai)
  "Map `acer-item' AI to m: or f(as) string."
  (let ((mod (acer-item-mod ai))
        (fun (acer-item-fun ai))
        (ari (acer-item-arity ai))
        (args (acer-item-args ai)))
    (cond
     ((and mod (not fun)) (concat mod ":"))
     ((and fun args) (concat fun "(" args ")"))
     ((and fun (string= "0" ari)) (concat fun "()"))
     ((and fun) (concat fun "(")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer helpers

(defun acer--create-buffer (frag)
  "Create acer buffer named by FRAG."
  (get-buffer-create (concat "*acer-" frag "-" acer-project-name "*")))

(defun acer--has-funs-p (mod)
  "Predicate for functions from MOD."
  (with-current-buffer acer-buffer-funs
    (acer--has-info-p (concat "^" mod ":"))))

(defun acer--has-info-p (pattern)
  "Predicate for PATTERN in current buffer."
  (goto-char 1)
  (re-search-forward pattern nil t))

(defun acer--line-to-string (dir)
  "Return text from current buffer as a string.
Select between bol and point (if DIR is `left'), point and eol (if
DIR is `right'), or bol and eol (otherwise)."
  (let ((beg (if (equal dir 'right) (point) (line-beginning-position)))
        (end (if (equal dir 'left) (point) (line-end-position))))
    (buffer-substring-no-properties beg end)))

(defun acer--match-left (pattern)
  "Return beginning of match, if PATTERN precedes point."
  (let ((case-fold-search nil)
        (beg (line-beginning-position))
        (end (point))
        (pat (concat "\\<" pattern "$")))
    (with-restriction beg end
      (save-excursion
        (goto-char beg)
        (when (re-search-forward pat nil t)
          (let* ((p (match-beginning 0))
                 (zw (or (char-before p) 0)))
            (unless (= ?: zw)
              p)))))))

(defun acer--run-escript (subcommand &optional args)
  "Fill current buffer by running escript with SUBCOMMAND and ARGS."
  (goto-char (point-max))
  (let ((cmd (concat acer-escript " " subcommand " " args)))
    (unless (= 0 (call-process-shell-command cmd nil (list (current-buffer) nil)))
      (error "Shell comand failed: %s" cmd))))

(defun acer--run-escript-str (subcommand &optional args)
  "Return result of running escript with SUBCOMMAND and ARGS as string."
  (let ((cmd (concat acer-escript " " subcommand " " args)))
    (string-trim (shell-command-to-string cmd))))

(defun acer--paths (buff)
  "Return, as a list of string, paths from BUFF."
  (let ((paths))
    (with-current-buffer buff
      (goto-char 1)
      (forward-line)
      (while (looking-at "^  /")
        (forward-char 2)
        (push (acer--line-to-string 'right) paths)
        (forward-line)))
    paths))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constuctors, setter, getter, merger for our struct

(defun acer--item-from-string-mfa (str)
  "Return `acer-item' from `m:f\(' STR."
  (pcase (split-string str "[:()]" t)
    (`(,m ,f)
     (acer--make-item m f))))

(defun acer--item-from-string-mf (str)
  "Return `acer-item' from `m:f' STR."
  (pcase (split-string str "[:]" t)
    (`(,m ,f)
     (acer--make-item m f))))

(defun acer--item-from-string-fa (str)
  "Return `avcer-item' from `f\(' STR."
  (pcase (split-string str "[()]" t)
    (`(,f)
     (acer--make-item nil f))))

(defun acer--item-from-line-erls ()
  "Make `acer-item' from current line."
  (let ((str (acer--line-to-string t)))
    (pcase (split-string str "=>" t)
      (`(,m ,file)
       (acer--make-item m nil nil nil nil file)))))

(defun acer--item-from-line-funs ()
  "Make `acer-item' from current line."
  (let ((str (acer--line-to-string t)))
    (pcase (split-string str "[:/()]" t)
      (`(,m ,f "0" ,l)
       (acer--make-item m f "0" l))
      (`(,m ,f ,a ,l ,as)
       (acer--make-item m f a l as)))))

(defun acer--items-from-imports ()
  "Imports as list of `acer-item'."
  (seq-reduce
   (lambda (a v)
     (let ((cv (car v)))
       (seq-reduce
        (lambda (b w) (cons (acer--make-item cv (car w) (cdr w)) b))
        (cdr v)
        a)))
   (erlang-get-import)
   ()))

(defun acer--make-item (m &optional f a l as file)
  "Construct an `acer-item' from components.
M, F, A, L, AS, FILE.  All are nullable strings."
  (let* ((a (when a (if (numberp a) (number-to-string a) a)))
         (l (when l (if (numberp l) (number-to-string l) l))))
    (make-acer-item :mod m :fun f :arity a :line l :args as :file file)))

(defun acer--item-merge (iai dai)
  "Merge IAI and DAI by copying DAI.k -> IAI.k if IAI.k == nil."
  (let ((keys (mapcar #'car (cdr (cl-struct-slot-info 'acer-item))))
        (merger (lambda (acc key) (acer--item-merge-k key acc dai))))
    (cl-reduce merger keys :initial-value iai)))

(defun acer--item-merge-k (key iai dai)
  "Copy DAI.KEY -> IAI.KEY if IAI.KEY == nil."
  (if (acer--item-get key iai)
      iai
    (acer--item-set key (acer--item-get key dai) iai)))

(defun acer--item-set (key val ai)
  "Set KEY to VAL in `acer-item' AI."
  (setf (cl-struct-slot-value 'acer-item key ai) val)
  ai)

(defun acer--item-get (k ai)
  "Get value of field K in `acer-item' AI."
  (cl-struct-slot-value 'acer-item k ai))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paren matching

(defun acer-paren()
  "Replace current sequence of close-parens with a correct such sequence."
  (interactive)
  (let ((group (acer--paren-group)))
    (when group
      (let* ((aptr (car group))
             (bptr (cdr group))
             (curr (buffer-substring-no-properties aptr bptr))
             (replace (acer--paren-replacement aptr bptr)))
        (when (and replace (not (string= replace curr)))
          (delete-region aptr bptr)
          (not (insert replace)))))))

(defun acer--paren-replacement (aptr bptr)
  "Calculate a string of close-parens.
If put between APTR and BPTR,
the sequence would balance parens in current form."
  (when (not (= aptr bptr))
    (let* ((astack (acer--paren-collect-parens (acer--form-prev) aptr))
           (bstack (acer--paren-collect-parens bptr (acer--form-next)))
           (astack (nreverse astack)))
      (while (acer--paren-pair-p (car bstack) (car astack))
        (pop bstack)
        (pop astack))
      (unless bstack
        (acer--paren-replacement-string astack)))))

(defun acer--paren-collect-parens (beg end)
  "Collect parens (as list of char) between BEG and END.
Balanced pairs are removed."
  (let ((stack))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "[]{}()[]" end t)
        (let ((p (char-before)))
          (if (acer--paren-pair-p p (car stack))
              (pop stack)
            (push  p stack)))))
    stack))

(defun acer--paren-replacement-string (stack)
  "Construct replacement string from STACK."
  (concat (mapcar #'acer--paren-matching (nreverse stack))))

(defun acer--paren-pair-p (char1 char2)
  "Is CHAR1 and CHAR2 a paren pair."
  (and char1 (= char1 (acer--paren-matching char2))))

(defun acer--paren-matching (char)
  "If CHAR is a paren, return the matching paren.
Otherwise, return zero."
  (pcase char
    (?\( ?\))
    (?\{ ?\})
    (?\[ ?\])
    (?\) ?\()
    (?\} ?\{)
    (?\] ?\[)
    (_ 0)))

(defun acer--paren-group ()
  "If point is in a close-paren group, return (BEG . END).
BEG and END are the starting and ending positions of the group."
  (save-excursion
    (with-restriction (line-beginning-position) (line-end-position)
      (goto-char 1)
      (let ((ptr (point))
            (p (acer--get-ptr-fwd "[]})]+")))
        (while (and p (< (match-end 0) ptr))
          (goto-char (match-end 0))
          (setq p (acer--get-ptr-fwd "[]})]+")))
        (when (and p
                   (<= (match-beginning 0) ptr)
                   (<= ptr (match-end 0)))
          (cons (match-beginning 0) (match-end 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get stuff from current buffer

(defun acer--get-vars (prefix)
  "All varibles matching PREFIX as list of string.
Restricted from beginning of form to beginning of PREFIX."
  (let ((beg (acer--form-prev))
        (vars))
    (save-excursion
      (re-search-backward (concat prefix "\\="))
      (with-restriction beg (point)
        (goto-char 1)
        (while (acer--get-ptr-fwd "[A-Z][A-Za-z0-9_]*")
          (goto-char (match-end 0))
          (push (match-string-no-properties 0) vars))))
    (cl-remove-duplicates (sort vars 'string<) :test 'string=)))

(defun acer--get-macros (prefix)
  "All macros matching PREFIX as list of string."
  (let* ((frag (substring prefix 1))
         (patt (concat "^-define *( *\\(" frag "[^ ,(]*\\)")))
    (mapcar (lambda(m) (concat "?" m)) (acer--get-frags patt))))

(defun acer--get-recs (prefix)
  "All  matching PREFIX as list of string."
  (let* ((frag (substring prefix 1))
         (patt (concat "^-record *( *\\(" frag "[^ ,(]*\\)")))
    (mapcar (lambda(m) (concat "#" m)) (acer--get-frags patt))))

(defun acer--get-frags (patt)
  "Return all things matching PATT as list of string."
  (let ((frags))
    (save-excursion
      (goto-char 1)
      (while (acer--get-ptr-fwd patt)
        (push (match-string-no-properties 1) frags)
        (goto-char (match-end 1))))
    frags))

(defun acer--form-prev ()
  "Find end of previous form."
  (if (acer--get-ptr-bwd "[.]\\([[:cntrl:]]\\)")
      (match-beginning 1)
    (point-min)))

(defun acer--form-next ()
  "Find beginning of next form."
  (if (acer--get-ptr-fwd "[.]\\([[:cntrl:]]\\)")
      (match-beginning 1)
    (point-max)))

(defun acer--get-ptr-fwd (patt)
  "Get next location of PATT, while ignoring comments and strings."
  (acer--get-ptr (lambda () (re-search-forward patt nil t))))

(defun acer--get-ptr-bwd (patt)
  "Get previous location of PATT, while ignoring comments and strings."
  (acer--get-ptr (lambda () (re-search-backward patt nil t))))

(defun acer--get-ptr (finder)
  "Get location of thing found by FINDER, while ignoring comments and strings."
  (save-excursion
    (let ((case-fold-search nil)
          (ptr (funcall finder)))
      (while (and ptr (acer--ignore-text-at-point-p))
        (setq ptr (funcall finder)))
      ptr)))

(defun acer--ignore-text-at-point-p ()
  "Check if text at point is to be ignored."
  (cl-member (get-text-property (point) 'face)
             '(font-lock-doc-face
               font-lock-string-face
               font-lock-comment-face)))

(provide 'erlang-ts-acer)
;;; erlang-ts-acer.el ends here
