#!/usr/bin/env bash

set -eu

_usage() {
    cat <<HERE
This is a wrapper around 'tree-sitter' utility.
It's mainly intended for documenting the usage of 'tree-sitter'.

$0 init              - set up an environment
$0 gen               - (re-)generate the parser
$0 build             - (re-)build the parser
$0 parse FILE        - parse FILE
$0 query QUERY FILE  - query the parse tree of FILE
$0 hl FILE           - syntax highlight FILE

You run 'init' once, 'gen'/'build' if you change the grammar, and
'parse'/'query' to test the parser and/or your queries. The grammar
lives in 'grammar.js'.

'hl' uses the queries in 'queries/highlights.scm' and the colors from
'config.json' to do syntax highlighting.

A query looks like this;
'(function_clause name: (atom) @fc)'
HERE
}

_err() {
    [ -n "${1:-}" ] && echo "$1"
    exit 55
}

_init() {
    local CFGSRC="$SELF/config/config.json"

    # do we already have a tree-sitter? Else, instal with cargo.
    if [ -x "$TREESITTER" ]
    then echo "Found tree-sitter"
    elif X="$(command -v tree-sitter)"
    then ln -s "$X" "$TREESITTER"
    elif [ -x ~/.cargo/bin/tree-sitter ]
    then ln -s ~/.cargo/bin/tree-sitter "$TREESITTER"
    elif command -v cargo
    then  cargo install tree-sitter-cli
          ln -s ~/.cargo/bin/tree-sitter "$TREESITTER"
    else _err "No tree-sitter, and no rust installed."
    fi

    # The initial config is created like this;
    # tree-sitter init-config
    # but we use the ready-made one

    if ! CFGDST="$(_cfg)"
    then _err "Config error: $CFGDST"
    elif ! [ -f "$CFGDST" ]
    then mkdir -p "$(dirname "$CFGDST")"
         sed "s|{{PARSER_DIRS}}|$(dirname "$SELF")|g" < "$CFGSRC" > "$CFGDST"
    fi

    # Now we can generate a parser
    echo "generating parser."
    _gen
    _build

    # Check if our language is there
    _dump
}

_cfg() {
    local X

    # a hack to find out where tree-sitter wants its config (different per OS)
    if X="$("$TREESITTER" init-config 2>&1)"
    then echo "${X#"Saved initial configuration to "}"
    elif grep -q "Remove your existing config file first:" <<<"$X"
    then echo "${X#"Remove your existing config file first: "}"
    else _err "error: $X"
    fi
}

_gen() {
    "$TREESITTER" generate --no-bindings --libdir "$SELF/src"
}

_build() {
    "$TREESITTER" build "$SELF"
}

_dump() {
    "$TREESITTER" dump-languages
}

_parse() {
    [ -z "${1:-}" ] && _err "No file."
    "$TREESITTER" parse "$1"
}

_hl() {
    [ -z "${1:-}" ] && _err "No file."
    "$TREESITTER" highlight "$1"
}

_query() {
    local QUERY="$SELF/queries/q"

    [ -z "${1:-}" ] && _err "No query."
    [ -z "${2:-}" ] && _err "No file."
    cat > "$QUERY" <<<"$1" && "$TREESITTER" query "$QUERY" "$2"
}

SELF="$(dirname "$(cd "$(dirname "$0")" && pwd)")"
TREESITTER="$SELF/bin/tree-sitter"

case "${1:-}" in
    "")      _usage;;
    "init")  _init;;
    "gen")   _gen;;
    "build") _build;;
    "dump")  _dump;;
    "parse") _parse "${2:-}";;
    "hl")    _hl "${2:-}";;
    "query") _query "${2:-}" "${3:-}";;
    *)       _err "Unrec operation: ${1:-}"
esac
