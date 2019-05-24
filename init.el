;;; private/emiller/init.el -*- lexical-binding: t; -*-

(doom!

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
 ;;fill-column
 hl-todo
 ;;indent-guides
 modeline
 nav-flash
 ;;neotree
 ophints
 (popup
  +all
  +defaults)
 ;;pretty-code
 ;;tabbar
 ;;unicode
 vc-gutter
 vi-tilde-fringe
 window-select
 workspaces

 :editor
 (evil +everywhere)
 file-templates
 fold
 (format +onsave)
 lispy
 multiple-cursors
 ;;parinfer
 rotate-text
 snippets

 :emacs
 (dired
  ;;+ranger
  +icons
  )
 electric
 vc

 :term
 eshell
 ;;term
 vterm

 :tools
 ;;ansible
 docker
 editorconfig
 ein
 eval
 flycheck
 flyspell
 gist
 (lookup
  +docsets)
 lsp
 ;;macos
 magit
 make
 pass
 pdf
 ;;prodigy
 reference
 rgb
 ;;terraform
 tmux
 upload
 ;; wakatime

 :lang
 ;;assembly
 (cc +lsp)
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
  ;; +lsp
  )
 ;;qt
 racket
 rest
 ;;ruby
 (rust +lsp)
 ;;scala
 (sh +fish)
 solidity
 snakemake
 ;;swift
 web
 ;;vala

 :email
 ;;(email +gmail)
 notmuch
 ;; (wanderlust +gmail)                    ; WIP

 :app
 ;;irc
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
