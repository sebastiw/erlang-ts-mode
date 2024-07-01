# erlang-ts-mode

Tree-sitter offers different features than emacs’ native
parser. Two things are required to activate it:
    1. Install a Tree-sitter library for the required language.
    2. A major mode that invokes the Tree-sitter backend.

`erlang-ts-mode` provides both of these.

# Prerequisites

The Emacs treesit support is built-in for Emacs 29+. It needs to be
compiled with `--with-treesit`. You can check if your emacs has
support like this; `(require 'treesit)`.

In order to compile the parser you need a C compiler.

# To run

To use `erlang-ts-mode`, obtain the code, e.g. from
`github.com/sebastiw/erlang-ts-mode`. Add this to your
`init.el` (change "~/git/erlang-ts-mode" to whatever you use);

```lisp
(require 'treesit)
(when (featurep 'treesit)
  (add-to-list 'load-path  "~/git/erlang-ts-mode")
  (require 'erlang-ts-mode)
  (when (featurep 'erlang-ts-mode)
    (add-to-list 'major-mode-remap-alist `(erlang-mode . erlang-ts-mode))))
```

When you open an erlang file, or run `erlang-ts-mode`, an erlang
treesit parser instance will be associated with the buffer. If needed,
the parser will be downloaded (from github, where you got this very
text) and compiled. This means you have to have a somewhat capable C
compiler installed on your system.

