#!/usr/bin/env sh

gsc -:r7rs -dynamic . \
    'rapid/assume.sld' \
    'rapid/format.sld' \
    'rapid/match.sld' \
    'gypsum/vector.sld' \
    'gypsum/hash-table.sld' \
    'gypsum/cursor.sld' \
    'gypsum/lens.sld' \
    'gypsum/lens/vector.sld' \
    'gypsum/lens/bin-hash-table.sld' \
    'gypsum/pretty.sld' \
    'gypsum/keymap.sld' \
    'gypsum/elisp-eval.sld' \
    'gypsum/elisp-eval/lexer.sld' \
    'gypsum/elisp-eval/parser.sld' \
    'gypsum/elisp-eval/pretty.sld' \
    'gypsum/elisp-eval/environment.sld' \
    'gypsum/elisp-eval/format.sld' \
    'gypsum/elisp-load.sld' \
    'gypsum/editor-impl.sld' \
    'gypsum/editor.sld' \
    'gypsum/editor/command.sld' \
    ;

#    'gypsum/concurrent.sld' \
#    'gypsum/sim-agent.sld' \

exec gsi -:r7rs .
