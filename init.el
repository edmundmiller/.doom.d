;;; private/emiller/init.el -*- lexical-binding: t; -*-

(doom! :input
       ;;chinese
       ;;japanese

       :completion
       ;;(company +childframe)
       (corfu +orderless +icons +minibuffer)
       ;;helm
       ;;ido
       ;;(ivy +prescient)
       (vertico +orderless +icons)

       :ui
       ;;deft
       doom
       doom-dashboard
       ;;doom-quit
       (emoji +unicode)  ; 🙂
       ;;fill-column
       hl-todo
       ;;hydra
       ;;indent-guides
       ;;ligatures
       ;;minimap
       (modeline +light)
       ;;nav-flash
       ;;neotree
       ophints
       (popup +defaults)
       ;;tabs
       ;;treemacs
       ;;unicode
       (vc-gutter +diff-hl +pretty)
       ;;vi-tilde-fringe
       window-select
       workspaces
       zen

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       ;;god
       ;;lispy
       multiple-cursors
       ;;objed
       ;;parinfer
       rotate-text
       snippets
       word-wrap

       :emacs
       (dired +icons)
       electric
       ;;ibuffer
       undo
       vc

       :term
       eshell
       ;;shell
       ;;term
       vterm

       :checkers
       syntax
       spell
       ;;grammar

       :tools
       ansible
       biblio
       ;;debugger
       direnv
       docker
       editorconfig
       ;;ein
       (eval +overlay)
       ;;gist
       (lookup +docsets +dictionary)
       (lsp +peek)
       ;;macos
       magit
       make
       ;;pass
       pdf
       ;;prodigy
       rgb
       taskrunner
       (terraform +lrp)
       ;;tmux
       tree-sitter
       upload

       :lang
       (astro +lsp)
       ;;agda
       ;;assembly
       ;;(beancount +lsp)
       (cc +lsp +tree-sitter)
       ;;clojure
       ;;common-lisp
       ;;coq
       ;;crystal
       ;;csharp
       data
       ;;(dart +flutter)
       ;;elixir
       ;;elm
       emacs-lisp
       ;;erlang
       (ess +lsp)
       ;;faust
       ;;fsharp
       ;;fstar
       ;;go
       ;;(haskell +dante)
       ;;hy
       ;;idris
       (java +lsp +tree-sitter)
       (javascript +lsp +tree-sitter)
       (json +lsp)
       (julia +lsp +snail +tree-sitter)
       ;;kotlin
       (latex +cdlatex +ref)
       ;;lean
       ;;factor
       ;;ledger
       ;;(lua +fennel)
       (markdown +tree-sitter)
       ;;nim
       (nix +lsp +tree-sitter)
       ;;ocaml
       (org
        +dragndrop
        +journal
        +jupyter
        +gnuplot
        +noter
        +pandoc
        +pomodoro
        +ref
        +roam2)
       ;;perl
       ;;php
       ;;plantuml
       ;;purescript
       (python
        +lsp
        +poetry
        +pyright)
       ;;qt
       ;;racket
       rest
       ;;rst
       ;;(ruby +rails)
       (rust +lsp +tree-sitter)
       ;;scala
       ;;(scheme +guile)
       sh
       ;;sml
       ;;solidity
       ;;swift
       ;;terra
       typst
       (web +lsp +tree-sitter)
       (yaml +lsp)

       :email
       (mu4e +org +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ai
       ;;emms
       ;;calendar
       everywhere
       irc
       (rss +org)
       ;;twitter
       ereader

       :config
       ;;literate
       (default +bindings +smartparens))