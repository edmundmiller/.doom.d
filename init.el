;;; private/emiller/init.el -*- lexical-binding: t; -*-

(doom! :completion
       company
       ;;helm
       ;;ido
       ivy

       :ui
       deft
       doom
       doom-dashboard
       ;;doom-modeline
       doom-quit
       ;;fill-column
       hl-todo
       hydra
       ;;indent-guides
       (modeline +light)
       nav-flash
       ;;neotree
       ophints
       (popup
        +all
        +defaults)
       (pretty-code +iosevka)
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
       ;; lispy
       multiple-cursors
       parinfer
       rotate-text
       snippets

       :emacs
       (dired +icons)
       electric
       vc

       :term
       eshell
       ;;term
       vterm

       :checkers
       syntax              ; tasing you for every semicolon you forget
       spell             ; tasing you for misspelling mispelling
       grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       debugger                    ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       editorconfig
       ;; ein
       eval
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
       rgb
       terraform
       tmux
       upload
       ;; wakatime

       :lang
       ;;assembly
       (cc +lsp)
       clojure
       ;; common-lisp
       ;;coq
       ;;crystal
       ;;csharp
       data
       ;;erlang
       ;;elixir
       ;;elm
       emacs-lisp
       (ess +lsp)
       ;; go
       ;; (haskell +lsp)
       ;;hy
       ;;idris
       ;; (java +lsp)
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
        +dragndrop                            ; file drag & drop support
        +hugo
        +jupyter                              ; ipython support for babel
        +pandoc                               ; pandoc integration into org's exporter
        +pomodoro)
       ;; +present)                             ; using Emacs for presentations
       perl
       ;;php
       ;;plantuml
       ;;purescript
       (python)
       ;; +conda
       ;; +pyenv
       ;; +lsp

       ;;qt
       ;;racket
       rest
       ;;ruby
       (rust +lsp)
       ;;scala
       sh
       solidity
       snakemake
       ;;swift
       web
       ;;vala

       :email
       ;; (mu4e +gmail)       ; WIP
       notmuch
       ;; (wanderlust +gmail)                    ; WIP

       :app
       irc
       (rss +org)
       ;;twitter

       :config
       (default +bindings +smartparnes)
       literate)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((ssh-deploy-async-with-threads . 1)
     (ssh-deploy-on-explicity-save . t)
     (ssh-deploy-async . 1))))
