;;; erlang-ts-flycheck --- a checker for erlc
;;; Commentary:
;;; 
;;; Code:
;;; -*- lexical-binding: t -*-

(require 'flycheck)
(require 'flycheck-popup-tip)
(require 'erlang-ts-acer)

(flycheck-popup-tip-mode)
;;(global-flycheck-mode)

;; disable rebar flychecker
(unless (memq 'erlang-rebar3 flycheck-disabled-checkers)
  (push 'erlang-rebar3 flycheck-disabled-checkers))

(setq flycheck-emacs-lisp-load-path 'inherit
      flycheck-erlang-library-path (erlang-ts-acer-libs)
      flycheck-protobuf-protoc-executable "protoc -I../../.."
      flycheck-checker 'erlang-ts)

(flycheck-define-checker erlang-ts
  "An Erlang syntax checker using the Erlang interpreter.
See URL `http://www.erlang.org/'."
  :command ("erlc"
            "-o" temporary-directory
            (option-list "-I" flycheck-erlang-include-path)
            (option-list "-pa" flycheck-erlang-library-path)
            "-Wall"
            source)
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" column ": Warning:" (message) line-end)
   (error line-start (file-name) ":" line ":" column ": " (message) line-end))
  :modes (erlang-ts-mode erlang-mode)
  :enabled (lambda () (string-suffix-p ".erl" (buffer-file-name))))

(provide 'erlang-ts-flycheck)
;;; erlang-ts-flycheck.el ends here
