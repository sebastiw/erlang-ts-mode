;;; erlang-ts-acer --- Summary
;;; Commentary:
;;; Auto Complete + Ecks Ref for erlang.
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

(defvar-local etsa--path (file-name-directory (locate-library "erlang-ts-mode")))
(defvar-local etsa--escript (concat etsa--path "etsa.escript"))
(defvar-local etsa--man-path (concat erlang-ts-man-dir "man/" "man3/"))

;; OTP buffers
(defvar-local etsa--buffer-man (get-buffer-create "*etsa--man*"))
(defvar-local etsa--buffer-bifs (get-buffer-create "*etsa--bifs*"))
(defvar-local etsa--buffer-guards (get-buffer-create "*etsa--guards*"))
(defvar-local etsa--buffer-words (get-buffer-create "*etsa--words*"))
(defvar-local etsa--buffer-otp-erls (get-buffer-create "*etsa--erls-otp*"))
(defvar-local etsa--buffer-otp-srcs (get-buffer-create "*etsa--srcs-otp*"))

;; buffer local variables
;; project name
(defvar-local etsa--project-name ())

;; project buffers
(defvar-local etsa--buffer-erls ())
(defvar-local etsa--buffer-funs ())
(defvar-local etsa--buffer-srcs ())

;;; We do most of our work in lists of this base struct
(cl-defstruct etsa--item mod fun arity line args file)

(defun erlang-ts-acer-init ()
  "Init etsa in current buffer."
  (etsa--project-name (buffer-file-name))
  (setq etsa--buffer-srcs (etsa--create-buffer "paths")
        etsa--buffer-funs (etsa--create-buffer "funs")
        etsa--buffer-erls (etsa--create-buffer "erls"))
  (etsa--fill-initial)
  (etsa--init-ac)
  (etsa--init-xref))

(defun erlang-ts-aacer-libs (&optional filename)
  "All libs in the project that FILENAME belongs to."
  (let ((f (if filename filename (buffer-file-name))))
    (pcase (split-string (etsa--run-escript-str "libs" f))
      ((and paths (guard (string= "libs:" (car paths))))
       (cdr paths)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; we're using TAB to do 3 things; indenting, replacing, and
;; completing.

(defun erlang-ts-acer-tab ()
  "Interactive function (normally bound to TAB)."
  (interactive)
  (or (etsa--indent)
      (etsa--replace)
      (etsa--complete)))

(defun etsa--indent ()
  "Try to indent, and return t if point moved."
  (not (= 0 (indent-according-to-mode))))

(defun etsa--replace ()
  "Try to replace, and return t if replacement happened."
  (etsa--paren))

(defun etsa--complete ()
  "Try to complete."
  (auto-complete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete framework

(defun etsa--init-ac ()
  "Initialize auto-complete."
  (auto-complete-mode)
  (setq ac-sources
        '(etsa--source-mfa
          etsa--source-mf
          etsa--source-fa
          etsa--source-f
          etsa--source-var
          etsa--source-macro
          etsa--source-rec)))

(defvar etsa--source-mfa
  '((prefix . etsa--prefix-mfa)
    (candidates . etsa--candidates-mfa))
  "Define source `mfa' - completes `m:f\('.")

(defvar etsa--source-mf
  '((prefix . etsa--prefix-mf)
    (candidates . etsa--candidates-mf))
  "Define source `mf' - complete `m:f'.")

(defvar etsa--source-fa
  '((prefix . etsa--prefix-fa)
    (candidates . etsa--candidates-fa))
  "Define source `fa' - complete `f\('.")

(defvar etsa--source-f
  '((prefix . etsa--prefix-f)
    (candidates . etsa--candidates-f))
  "Define source `f' - complete `frag'.")

(defvar etsa--source-var
  '((prefix . etsa--prefix-var)
    (candidates . etsa--candidates-var))
  "Define source `var' - complete `Var'.")

(defvar etsa--source-macro
  '((prefix . etsa--prefix-macro)
    (candidates . etsa--candidates-macro))
  "Define source `macro' - complete `?m'.")

(defvar etsa--source-rec
  '((prefix . etsa--prefix-rec)
    (candidates . etsa--candidates-rec))
  "Define source `rec' - complete `\#rec'.")

;; prefix finders
(defun etsa--prefix-mfa ()
  "Return start pos of thing if it is `m:f\('."
  (etsa--match-left "[a-z][a-zA-Z0-9_]*:[a-z][a-zA-Z0-9_]*("))

(defun etsa--prefix-mf ()
  "Return start pos of thing if it is `m:f'."
  (etsa--match-left "[a-z][a-zA-Z0-9_]*:[a-z][a-zA-Z0-9_]*"))

(defun etsa--prefix-fa ()
  "Return start pos of thing if it is `f\('."
  (etsa--match-left "[a-z][a-zA-Z0-9_]*("))

(defun etsa--prefix-f ()
  "Return start pos of thing if it is `f'."
  (etsa--match-left "[a-z][a-zA-Z0-9_]*"))

(defun etsa--prefix-var ()
  "Return start pos of thing if it is `Var'."
  (etsa--match-left "[A-Z][a-zA-Z0-9_]*"))

(defun etsa--prefix-macro ()
  "Return start pos of thing if it is `?m'."
  (etsa--match-left "\\?'?[a-zA-Z0-9_]*"))

(defun etsa--prefix-rec ()
  "Return start pos of thing if it is `\#rec'."
  (etsa--match-left "#'?[a-z][a-zA-Z0-9_]*"))

;; candidate finders
(defun etsa--candidates-mfa ()
  "Get m:f( candidates."
  (let* ((ai (etsa--item-from-string-mfa ac-prefix))
         (ais (etsa--expand-mf-to-a ai)))
    (mapcar #'etsa--print-mfa ais)))

(defun etsa--candidates-mf ()
  "Get m:f candidates."
  (let* ((ai (etsa--item-from-string-mf ac-prefix))
         (ais (etsa--expand-m-to-f ai)))
    (mapcar #'etsa--print-mf ais)))

(defun etsa--candidates-fa ()
  "Get f( candidates."
  (let* ((ai (etsa--item-from-string-fa ac-prefix))
         (ais (etsa--expand-f-to-a ai)))
    (mapcar #'etsa--print-fa ais)))

(defun etsa--candidates-f ()
  "Get fragment (either mod or local fun) candidates."
  (mapcar #'etsa--print-f (etsa--expand-nil ac-prefix)))

(defun etsa--candidates-var ()
  "Get Var candidates."
  (etsa--get-vars ac-prefix))

(defun etsa--candidates-macro ()
  "Get ?MACRO candidates."
  (etsa--get-macros ac-prefix))

(defun etsa--candidates-rec ()
  "Get \#rec candidates."
  (mapcar (lambda(m) (concat "#" m)) (etsa--get-recs ac-prefix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xref framework

(defun etsa--init-xref ()
  "Initialize xref."
  (add-hook 'xref-backend-functions #'etsa--xref-backend nil t))

(defun etsa--xref-backend ()
  "Declare xref backend."
  'etsa)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql etsa)))
  "Return etsa--item for item at point."
  (pcase (erlang-get-identifier-at-point)
    (`(qualified-function ,m ,f ,a)
     (etsa--make-item m f a))
    ((and `(nil ,m ,f ,a) (guard (string= m (erlang-get-module))))
     (etsa--make-item nil f a))
    (`(nil ,m ,f ,a)
     (etsa--make-item m f a))))

(cl-defmethod xref-backend-definitions ((_backend (eql etsa)) ai)
  "List of `xref-item' for `etsa--item' AI."
  (mapcar #'etsa--ai-to-xi (etsa--expand-definitions ai)))

(cl-defmethod xref-backend-apropos ((_backend (eql etsa)) frag)
  "List of `xref-item' fragment FRAG."
  (mapcar #'etsa--ai-to-xi (etsa--expand-nil frag)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql etsa)))
  "A completion table."
  ())

(defun etsa--ai-to-xi (ai)
  "Construct `xref-item' from `etsa--item' AI."
  (let* ((m (etsa--item-mod ai))
         (mod (when m (concat m ":")))
         (fun (etsa--item-fun ai))
         (arity (etsa--item-arity ai))
         (file (etsa--item-file ai))
         (line (string-to-number (etsa--item-line ai)))
         (summary (concat mod fun "/" arity))
         (location (xref-make-file-location file line 0)))
    (xref-make summary location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expanders. these read from the *etsa--* buffers.
;; They all map `etsa--item' -> list of `etsa--item'.

(defun etsa--expand-definitions (ai)
  "Return list of `etsa--item' completing `etsa--item' AI."
  (let ((mod (etsa--item-mod ai))
        (fun (etsa--item-fun ai)))
    (if mod
        (etsa--expand-mf-to-a ai)
      (etsa--expand-f-to-a ai))))

(defun etsa--expand-mf-to-a (ai)
  "Return list of `etsa--item' by completing AI.
AI.mod and AI.fun should be completed."
  (let ((ai (car (etsa--expand-m-to-m ai))))
    (when ai
      (let* ((mod (etsa--item-mod ai))
             (fun (etsa--item-fun ai))
             (arity (etsa--item-arity ai))
             (file (etsa--item-file ai))
             (mfa (concat mod ":" fun "/" arity))
             (buffs (list etsa--buffer-erls etsa--buffer-otp-erls))
             (ais))
        (etsa--fill-funs ai)
        (with-current-buffer etsa--buffer-funs
          (goto-char 1)
          (while (re-search-forward mfa nil t)
            (push (etsa--item-from-line-funs) ais)))
        (mapcar (lambda (aii) (etsa--item-merge aii ai)) (nreverse ais))))))

(defun etsa--expand-m-to-f (ai)
  "Return list of `etsa--item' that completes M:F from AI.
AI.mod should be completed."
  (let ((ai (car (etsa--expand-m-to-m ai))))
    (when ai
      (let* ((mod (etsa--item-mod ai))
             (fun (etsa--item-fun ai))
             (file (etsa--item-file ai))
             (mf (concat mod ":" fun))
             (ais))
        (etsa--fill-funs ai)
        (with-current-buffer etsa--buffer-funs
          (goto-char 1)
          (while (re-search-forward mf nil t)
            (push (etsa--item-from-line-funs) ais)))
        (mapcar (lambda (aii) (etsa--item-merge aii ai)) (nreverse ais))))))

(defun etsa--expand-f-to-a (ai)
  "Return list of `etsa--item' with things that complete AI.
AI.mod is ignored, AI.fun is completed."
  (append (etsa--expand-f-to-a-local ai)
          (etsa--expand-f-to-a-imports ai)
          (etsa--expand-f-to-a-bifs ai)))

(defun etsa--expand-f-to-a-local (ai)
  "Return list of `etsa--item' by completing AI in local buffer.
AI.mod is ignored, AI.fun should be completed."
  (let ((file (buffer-file-name))
        (mod (erlang-get-module))
        (fun (etsa--item-fun ai))
        (arity (etsa--item-arity ai))
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
            (setq hit (etsa--make-item mod f a line args file))))))
    (when hit (list hit))))

(defun etsa--expand-f-to-a-imports (ai)
  "Return list of `etsa--item' where AI.fun is imported.
AI.fun should be completed.  AI.mod is ignored."
  ()) ;; TODO: placeholder.

(defun etsa--expand-f-to-a-bifs (ai)
  "Return list of `etsa--item' where AI.fun is autoimported from `erlang'.
AI.fun should be completed.  AI.mod is ignored."
  (etsa--expand-mf-to-a (etsa--item-set 'mod "erlang" ai)))

(defun etsa--expand-nil (frag)
  "Return list of `etsa--item' with things that complete string FRAG.
We try X = module and X = local function."
  (append (etsa--expand-nil-to-m (etsa--make-item frag))
          (etsa--expand-nil-to-f (etsa--make-item nil frag))))

(defun etsa--expand-nil-to-m (ai)
  "Return list of `etsa--item' by completing AI.mod."
  (append (etsa--expand-nil-to-m-in-buffer ai etsa--buffer-otp-erls)
          (etsa--expand-nil-to-m-in-buffer ai etsa--buffer-erls)))

(defun etsa--expand-nil-to-f (ai)
  "Return list of `etsa--item' with things that complete AI.
AI.mod is ignored."
  (append (etsa--expand-nil-to-f-local ai)
          (etsa--expand-nil-to-f-imports ai)
          (etsa--expand-nil-to-f-bifs ai)))

(defun etsa--expand-nil-to-f-local (ai)
  "Return list of `etsa--item' by completing AI in local buffer.
AI.mod is ignored,"
  (let ((file (buffer-file-name))
        (mod (erlang-get-module))
        (fun (etsa--item-fun ai))
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
            (push (etsa--make-item mod f a line args file) ais)))))
    (nreverse ais)))

(defun etsa--expand-nil-to-f-imports (ai)
  "Return list of `etsa--item' where AI.fun is imported."
  ()) ;; TODO: placeholder.

(defun etsa--expand-nil-to-f-bifs (ai)
  "Return list of `etsa--item' where AI.fun is autoimported from `erlang'."
  (let ((patt (concat "^" (etsa--item-fun ai)))
        (ais))
    (with-current-buffer etsa--buffer-bifs
      (goto-char 1)
      (while (re-search-forward patt nil t)
        (push (etsa--make-item "erlang" (etsa--line-to-string t)) ais)))
    (mapcan #'etsa--expand-m-to-f ais)))

(defun etsa--expand-m-to-m (ai)
  "Return list of `etsa--item'.
AI.mod should be completed."
  (append (etsa--expand-m-to-m-in-buffer ai etsa--buffer-otp-erls)
          (etsa--expand-m-to-m-in-buffer ai etsa--buffer-erls)))

(defun etsa--expand-nil-to-m-in-buffer (ai buffer)
  "Expand AI as module in BUFFER."
  (let ((case-fold-search nil)
        (pattern (concat "^" (etsa--item-mod ai)))
        (ais))
    (with-current-buffer buffer
      (goto-char 1)
      (while (re-search-forward pattern nil t)
        (push (etsa--item-from-line-erls) ais)))
    (nreverse ais)))

(defun etsa--expand-m-to-m-in-buffer (ai buffer)
  "Expand AI as module in BUFFER.
AI.mod should be completed."
  (let ((case-fold-search nil)
        (pattern (concat "^" (etsa--item-mod ai) "=>")))
    (with-current-buffer buffer
      (goto-char 1)
      (when (re-search-forward pattern nil t)
        (list (etsa--item-merge ai (etsa--item-from-line-erls)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fillers. These write to the *etsa--* buffers.

(defun etsa--project-name (filename)
  "Use FILENAME to set project name."
  (unless etsa--project-name
    (setq etsa--project-name (etsa--run-escript-str "name" filename))))

(defun etsa--fill-initial ()
  "Fills buffers."
  (etsa--fill-man)
  (etsa--fill-bifs)
  (etsa--fill-guards)
  (etsa--fill-words)
  (etsa--fill-otp-srcs)
  (etsa--fill-project-srcs)
  (etsa--fill-erls etsa--buffer-otp-srcs etsa--buffer-otp-erls)
  (etsa--fill-erls etsa--buffer-srcs etsa--buffer-erls))

(defun etsa--fill-man ()
  "Populate paths to all OTP man files."
  (with-current-buffer etsa--buffer-man
    (unless (< 0 (buffer-size))
      (etsa--run-escript "man" etsa--man-path))))

(defun etsa--fill-otp-srcs ()
  "Populate paths to all directories containing project erls."
  (with-current-buffer etsa--buffer-otp-srcs
    (unless (< 0 (buffer-size))
      (etsa--run-escript "srcs" "otp"))))

(defun etsa--fill-project-srcs ()
  "Populate paths to all directories containing project erls."
  (let ((file (buffer-file-name)))
    (with-current-buffer etsa--buffer-srcs
      (unless (< 0 (buffer-size))
        (etsa--run-escript "srcs" file)))))

(defun etsa--fill-erls (pbuff buff)
  "Populate BUFF with paths to all erl files with paths from PBUFF."
  (let ((paths (etsa--paths pbuff)))
    (with-current-buffer buff
      (unless (< 0 (buffer-size))
        (mapc (lambda(p) (etsa--run-escript "erls" p)) paths)))))

(defun etsa--fill-funs (ai)
  "Populate all AI.mod's exported functions as per AI.file."
  (let ((mod (etsa--item-mod ai))
        (file (etsa--item-file ai)))
    (unless (etsa--has-funs-p mod)
      (with-current-buffer etsa--buffer-funs
        (etsa--run-escript "funs" file)))))

(defun etsa--fill-bifs ()
  "Populate bifs buffer."
  (with-current-buffer etsa--buffer-bifs
    (etsa--run-escript "bifs")))

(defun etsa--fill-guards ()
  "Populate guards buffer."
  (with-current-buffer etsa--buffer-guards
    (etsa--run-escript "guards")))

(defun etsa--fill-words ()
  "Populate words buffer."
  (with-current-buffer etsa--buffer-words
    (etsa--run-escript "words")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pretty-printers

(defun etsa--print-mfa (ai)
  "Map `etsa--item' AI to m:f(as) string."
  (let ((mod (etsa--item-mod ai))
        (fun (etsa--item-fun ai))
        (args (etsa--item-args ai)))
    (concat mod ":" fun "(" args ")")))

(defun etsa--print-mf (ai)
  "Map `etsa--item' AI to m:f( string."
  (let ((mod (etsa--item-mod ai))
        (fun (etsa--item-fun ai)))
    (concat mod ":" fun "(")))

(defun etsa--print-fa (ai)
  "Map `etsa--item' AI to f(as) string."
  (let ((fun (etsa--item-fun ai))
        (args (etsa--item-args ai)))
    (concat fun "(" args ")")))

(defun etsa--print-f (ai)
  "Map `etsa--item' AI to m: or f(as) string."
  (let ((mod (etsa--item-mod ai))
        (fun (etsa--item-fun ai))
        (ari (etsa--item-arity ai))
        (args (etsa--item-args ai)))
    (cond
     ((and mod (not fun)) (concat mod ":"))
     ((and fun args) (concat fun "(" args ")"))
     ((and fun (string= "0" ari)) (concat fun "()"))
     ((and fun) (concat fun "(")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer helpers

(defun etsa--create-buffer (frag)
  "Create etsa buffer named by FRAG."
  (get-buffer-create (concat "*etsa--" frag "-" etsa--project-name "*")))

(defun etsa--has-funs-p (mod)
  "Predicate for functions from MOD."
  (with-current-buffer etsa--buffer-funs
    (etsa--has-info-p (concat "^" mod ":"))))

(defun etsa--has-info-p (pattern)
  "Predicate for PATTERN in current buffer."
  (goto-char 1)
  (re-search-forward pattern nil t))

(defun etsa--line-to-string (dir)
  "Return text from current buffer as a string.
Select between bol and point (if DIR is `left'), point and eol (if
DIR is `right'), or bol and eol (otherwise)."
  (let ((beg (if (equal dir 'right) (point) (line-beginning-position)))
        (end (if (equal dir 'left) (point) (line-end-position))))
    (buffer-substring-no-properties beg end)))

(defun etsa--match-left (pattern)
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

(defun etsa--run-escript (subcommand &optional args)
  "Fill current buffer by running escript with SUBCOMMAND and ARGS."
  (goto-char (point-max))
  (let ((cmd (concat etsa--escript " " subcommand " " args)))
    (unless (= 0 (call-process-shell-command cmd nil (list (current-buffer) nil)))
      (error "Shell comand failed: %s" cmd))))

(defun etsa--run-escript-str (subcommand &optional args)
  "Return result of running escript with SUBCOMMAND and ARGS as string."
  (let ((cmd (concat etsa--escript " " subcommand " " args)))
    (string-trim (shell-command-to-string cmd))))

(defun etsa--paths (buff)
  "Return, as a list of string, paths from BUFF."
  (let ((paths))
    (with-current-buffer buff
      (goto-char 1)
      (forward-line)
      (while (looking-at "^  /")
        (forward-char 2)
        (push (etsa--line-to-string 'right) paths)
        (forward-line)))
    paths))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constuctors, setter, getter, merger for our struct

(defun etsa--item-from-string-mfa (str)
  "Return `etsa--item' from `m:f\(' STR."
  (pcase (split-string str "[:()]" t)
    (`(,m ,f)
     (etsa--make-item m f))))

(defun etsa--item-from-string-mf (str)
  "Return `etsa--item' from `m:f' STR."
  (pcase (split-string str "[:]" t)
    (`(,m ,f)
     (etsa--make-item m f))))

(defun etsa--item-from-string-fa (str)
  "Return `avcer-item' from `f\(' STR."
  (pcase (split-string str "[()]" t)
    (`(,f)
     (etsa--make-item nil f))))

(defun etsa--item-from-line-erls ()
  "Make `etsa--item' from current line."
  (let ((str (etsa--line-to-string t)))
    (pcase (split-string str "=>" t)
      (`(,m ,file)
       (etsa--make-item m nil nil nil nil file)))))

(defun etsa--item-from-line-funs ()
  "Make `etsa--item' from current line."
  (let ((str (etsa--line-to-string t)))
    (pcase (split-string str "[:/()]" t)
      (`(,m ,f "0" ,l)
       (etsa--make-item m f "0" l))
      (`(,m ,f ,a ,l ,as)
       (etsa--make-item m f a l as)))))

(defun etsa--items-from-imports ()
  "Imports as list of `etsa--item'."
  (seq-reduce
   (lambda (a v)
     (let ((cv (car v)))
       (seq-reduce
        (lambda (b w) (cons (etsa--make-item cv (car w) (cdr w)) b))
        (cdr v)
        a)))
   (erlang-get-import)
   ()))

(defun etsa--make-item (m &optional f a l as file)
  "Construct an `etsa--item' from components.
M, F, A, L, AS, FILE.  All are nullable strings."
  (let* ((a (when a (if (numberp a) (number-to-string a) a)))
         (l (when l (if (numberp l) (number-to-string l) l))))
    (make-etsa--item :mod m :fun f :arity a :line l :args as :file file)))

(defun etsa--item-merge (iai dai)
  "Merge IAI and DAI by copying DAI.k -> IAI.k if IAI.k == nil."
  (let ((keys (mapcar #'car (cdr (cl-struct-slot-info 'etsa--item))))
        (merger (lambda (acc key) (etsa--item-merge-k key acc dai))))
    (cl-reduce merger keys :initial-value iai)))

(defun etsa--item-merge-k (key iai dai)
  "Copy DAI.KEY -> IAI.KEY if IAI.KEY == nil."
  (if (etsa--item-get key iai)
      iai
    (etsa--item-set key (etsa--item-get key dai) iai)))

(defun etsa--item-set (key val ai)
  "Set KEY to VAL in `etsa--item' AI."
  (setf (cl-struct-slot-value 'etsa--item key ai) val)
  ai)

(defun etsa--item-get (k ai)
  "Get value of field K in `etsa--item' AI."
  (cl-struct-slot-value 'etsa--item k ai))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paren matching

(defun etsa--paren()
  "Replace current sequence of close-parens with a correct such sequence."
  (interactive)
  (let ((group (etsa--paren-group)))
    (when group
      (let* ((aptr (car group))
             (bptr (cdr group))
             (curr (buffer-substring-no-properties aptr bptr))
             (replace (etsa--paren-replacement aptr bptr)))
        (when (and replace (not (string= replace curr)))
          (delete-region aptr bptr)
          (not (insert replace)))))))

(defun etsa--paren-replacement (aptr bptr)
  "Calculate a string of close-parens.
If put between APTR and BPTR,
the sequence would balance parens in current form."
  (when (not (= aptr bptr))
    (let* ((astack (etsa--paren-collect-parens (etsa--form-prev) aptr))
           (bstack (etsa--paren-collect-parens bptr (etsa--form-next)))
           (astack (nreverse astack)))
      (while (etsa--paren-pair-p (car bstack) (car astack))
        (pop bstack)
        (pop astack))
      (unless bstack
        (etsa--paren-replacement-string astack)))))

(defun etsa--paren-collect-parens (beg end)
  "Collect parens (as list of char) between BEG and END.
Balanced pairs are removed."
  (let ((stack))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "[]{}()[]" end t)
        (let ((p (char-before)))
          (if (etsa--paren-pair-p p (car stack))
              (pop stack)
            (push  p stack)))))
    stack))

(defun etsa--paren-replacement-string (stack)
  "Construct replacement string from STACK."
  (concat (mapcar #'etsa--paren-matching (nreverse stack))))

(defun etsa--paren-pair-p (char1 char2)
  "Is CHAR1 and CHAR2 a paren pair."
  (and char1 (= char1 (etsa--paren-matching char2))))

(defun etsa--paren-matching (char)
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

(defun etsa--paren-group ()
  "If point is in a close-paren group, return (BEG . END).
BEG and END are the starting and ending positions of the group."
  (save-excursion
    (with-restriction (line-beginning-position) (line-end-position)
      (goto-char 1)
      (let ((ptr (point))
            (p (etsa--get-ptr-fwd "[]})]+")))
        (while (and p (< (match-end 0) ptr))
          (goto-char (match-end 0))
          (setq p (etsa--get-ptr-fwd "[]})]+")))
        (when (and p
                   (<= (match-beginning 0) ptr)
                   (<= ptr (match-end 0)))
          (cons (match-beginning 0) (match-end 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get stuff from current buffer

(defun etsa--get-vars (prefix)
  "All varibles matching PREFIX as list of string.
Restricted from beginning of form to beginning of PREFIX."
  (let ((beg (etsa--form-prev))
        (vars))
    (save-excursion
      (re-search-backward (concat prefix "\\="))
      (with-restriction beg (point)
        (goto-char 1)
        (while (etsa--get-ptr-fwd "[A-Z][A-Za-z0-9_]*")
          (goto-char (match-end 0))
          (push (match-string-no-properties 0) vars))))
    (cl-remove-duplicates (sort vars 'string<) :test 'string=)))

(defun etsa--get-macros (prefix)
  "All macros matching PREFIX as list of string."
  (let* ((frag (substring prefix 1))
         (patt (concat "^-define *( *\\(" frag "[^ ,(]*\\)")))
    (mapcar (lambda(m) (concat "?" m)) (etsa--get-frags patt))))

(defun etsa--get-recs (prefix)
  "All  matching PREFIX as list of string."
  (let* ((frag (substring prefix 1))
         (patt (concat "^-record *( *\\(" frag "[^ ,(]*\\)")))
    (mapcar (lambda(m) (concat "#" m)) (etsa--get-frags patt))))

(defun etsa--get-frags (patt)
  "Return all things matching PATT as list of string."
  (let ((frags))
    (save-excursion
      (goto-char 1)
      (while (etsa--get-ptr-fwd patt)
        (push (match-string-no-properties 1) frags)
        (goto-char (match-end 1))))
    frags))

(defun etsa--form-prev ()
  "Find end of previous form."
  (if (etsa--get-ptr-bwd "[.]\\([[:cntrl:]]\\)")
      (match-beginning 1)
    (point-min)))

(defun etsa--form-next ()
  "Find beginning of next form."
  (if (etsa--get-ptr-fwd "[.]\\([[:cntrl:]]\\)")
      (match-beginning 1)
    (point-max)))

(defun etsa--get-ptr-fwd (patt)
  "Get next location of PATT, while ignoring comments and strings."
  (etsa--get-ptr (lambda () (re-search-forward patt nil t))))

(defun etsa--get-ptr-bwd (patt)
  "Get previous location of PATT, while ignoring comments and strings."
  (etsa--get-ptr (lambda () (re-search-backward patt nil t))))

(defun etsa--get-ptr (finder)
  "Get location of thing found by FINDER, while ignoring comments and strings."
  (save-excursion
    (let ((case-fold-search nil)
          (ptr (funcall finder)))
      (while (and ptr (etsa--ignore-text-at-point-p))
        (setq ptr (funcall finder)))
      ptr)))

(defun etsa--ignore-text-at-point-p ()
  "Check if text at point is to be ignored."
  (cl-member (get-text-property (point) 'face)
             '(font-lock-doc-face
               font-lock-string-face
               font-lock-comment-face)))

(provide 'erlang-ts-acer)
;;; erlang-ts-acer.el ends here
