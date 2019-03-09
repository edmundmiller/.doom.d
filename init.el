;;; private/emiller/init.el -*- lexical-binding: t; -*-

(doom! :feature
       ;;debugger
       eval
       (evil +everywhere)
       file-templates
       (lookup
        +docsets)
       snippets
       workspaces

       :completion
       (company
        +auto)
       ;;helm
       ;;ido
       ivy

       :ui
       ;;deft
       doom
       doom-dashboard
       ;;doom-modeline
       doom-quit
       evil-goggles
       ;;fci
       hl-todo
       modeline
       nav-flash
       ;;neotree
       treemacs
       (popup
        +all
        +defaults)
       ;;pretty-code
       ;;tabbar
       ;;unicode
       vc-gutter
       vi-tilde-fringe
       window-select

       :editor
       fold
       (format +onsave)
       ;;lispy
       multiple-cursors
       ;;parinfer
       rotate-text

       :emacs
       (dired
        ;;+ranger
        +icons
        )
       electric
       eshell
       imenu
       ;;term
       vc

       :tools
       ;;ansible
       docker
       editorconfig
       ein
       flycheck
       flyspell
       gist
       lsp
       ;;macos
       magit
       make
       password-store
       pdf
       ;;prodigy
       rgb
       ;;terraform
       tmux
       upload
       wakatime
       vterm

       :lang
       ;;assembly
       (cc +irony +rtags)
       clojure
       common-lisp
       ;;coq
       ;;crystal
       ;;csharp
       data
       ;;erlang
       ;;elixir
       ;;elm
       emacs-lisp
       ess
       go
       (haskell +lsp)
       ;;hy
       ;;idris
       ;;(java +meghanada)
       (javascript +lsp)
       ;;julia
       (latex +ref)
       ;;ledger
       ;;lua
       markdown
       ;;nim
       nix
       ;;ocaml
       (org
        +attach
        +babel
        +capture
        +export
        +present)
       perl
       ;;php
       ;;plantuml
       ;;purescript
       (python
        +conda
        +lsp)
       ;;qt
       ;;racket
       ;;rest
       ;;ruby
       (rust +lsp)
       ;;scala
       (sh +fish)
       solidity
       snakemake
       ;;swift
       web
       ;;vala

       :app
       ;;(email +gmail)
       ;;irc
       notmuch
       (rss +org)
       ;;twitter
       (write
       +wordnut
       +langtool)

       :collab
       floobits
       impatient-mode

       :config
       (default +bindings +snippets +smartparnes)
       literate)
