;;; private/emiller/init.el -*- lexical-binding: t; -*-

;;
(setq
    user-mail-address "Edmund.A.Miller@gmail.com"
    user-full-name "Edmund Miller"

    ;; Change font
    doom-font (font-spec :family "SauceCodePro Nerd Font" :size 17)
    ;; doom-variable-pitch-font (font-spec :family "Source Code Pro")
    ;; doom-unicode-font (font-spec :family "Source Code Pro")
    doom-big-font (font-spec :family "Source Code Pro" :size 21)

    ;; Set Bullets to OG
    org-bullets-bullet-list '("■" "◆" "▲" "▶")
    org-ellipsis " ▼ "
    org-export-with-toc nil)

;; Tern
;; (setenv "PATH" (concat (getenv "PATH") ":/home/emiller/node/bin/"))
;; (setq exec-path (append exec-path '("/home/emiller/node/bin/")))

;; Set up Org
(with-eval-after-load 'org
    (setq org-directory "~/Dropbox/orgfiles")
        (defun org-file-path (filename)
        "Return the absolute address of an org file, given its relative name."
        (concat (file-name-as-directory org-directory) filename))
        (setq org-index-file (org-file-path "i.org"))
        (setq org-archive-location
                (concat (org-file-path "archive.org") "::* From %s"))

    (setq org-agenda-files (list "~/Dropbox/orgfiles/gcal.org"
                                "~/Dropbox/orgfiles/i.org"
                                "~/Dropbox/orgfiles/Lab_Notebook.org"
                                "~/Dropbox/orgfiles/Lab_schedule.org"
                                "~/Dropbox/orgfiles/schedule.org")))
;; Org Capture Templates
(setq org-capture-templates
    '(("a" "Appointment" entry
    (file  "~/Dropbox/orgfiles/gcal.org" "Appointments")
    "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n")

    ("n" "Note" entry
    (file+headline "~/Dropbox/orgfiles/i.org" "Notes")
    "** %?\n%T")

    ("l" "Link" entry
    (file+headline "~/Dropbox/orgfiles/links.org" "Links")
    "* %? %^L %^g \n%T" :prepend t)

    ("t" "To Do Item" entry
    (file+headline "~/Dropbox/orgfiles/i.org" "Unsorted")
    "*** TODO %?\n%T" :prepend t)

    ("j" "Lab Entry" entry
    (file+datetree "~/Dropbox/orgfiles/Lab_Notebook.org" "Lab Journal")
    "** %? %^g \n\nEntered on %U\n  %i\n\n")

    ("d" "Lab To Do" entry
    (file+headline "~/Dropbox/orgfiles/Lab_Notebook.org" "To Do")
    "** TODO %?\n%T" :prepend t)))

;; Start in Insert
(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; Bind capture to =C-c c=
(define-key global-map "\C-cc" 'org-capture)

;; Fix Flycheck for shellscripts
(setq flycheck-shellcheck-follow-sources nil)

;; Org-Gcal
;; (run-with-idle-timer 60 t (org-gcal-sync) )
;; (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))

;; Edit i.org
(defun emiller/visit-i-org ()
				(interactive)
				(find-file "~/Dropbox/orgfiles/i.org"))

;; (global-set-key (kbd "C-c i") 'emiller/visit-i-org)

;;
(doom! :feature
      ;debugger          ; FIXME stepping through code, to help you add bugs
       eval              ; run code, run (also, repls)
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       (lookup           ; helps you navigate your code and documentation
        +devdocs         ; ...on devdocs.io online
        +docsets)        ; ...or in Dash docsets locally
       snippets          ; my elves. They type so I don't have to
       spellcheck        ; tasing you for misspelling mispelling
       (syntax-checker   ; tasing you for every semicolon you forget
        +childframe)     ; use childframes for error popups (Emacs 26+ only)
       version-control   ; remember, remember that commit in November
       workspaces        ; tab emulation, persistence & separate workspaces

       :completion
       (company          ; the ultimate code completion backend
        +auto            ; as-you-type code completion
        +childframe)     ; a nicer company UI (Emacs 26+ only)
        (ivy             ; a search engine for love and life
        +childframe)     ; uses childframes for popups (Emacs 26+ only)
        ;helm            ; the *other* search engine for love and life
        ;ido             ; the other *other* search engine...

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-modeline     ; a snazzy Atom-inspired mode-line
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       evil-goggles      ; display visual hints when editing in evil
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       nav-flash         ; blink the current line after jumping
       neotree           ; a project drawer, like NERDTree for vim
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
      ;tabbar            ; FIXME an (incomplete) tab bar for Emacs
       unicode           ; extended unicode support for various languages
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
      ;posframe          ; use child frames where possible (Emacs 26+ only)

       :emacs
       dired             ; making dired pretty [functional]
       ediff             ; comparing files in Emacs
       electric-indent   ; smarter, keyword-based electric-indent
       eshell            ; a consistent, cross-platform shell (WIP)
       imenu             ; an imenu sidebar and searchable code index
       term              ; terminals in Emacs

       :tools
       editorconfig      ; let someone else argue about tabs vs spaces
       ;ein               ; tame Jupyter notebooks with emacs
       gist              ; interacting with github gists
      ;macos             ; MacOS-specific commands
       make              ; run make tasks from Emacs
       magit             ;
       movetext
       password-store    ; password manager for nerds
       pdf               ; pdf enhancements
       ;reference
       rgb               ; creating color strings
       rotate-text       ; cycle region at point between text candidates
       tmux              ; an API for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp

       :lang
       assembly          ; assembly for fun or debugging
      ;cc                ; C/C++/Obj-C madness
      ;crystal           ; ruby at the speed of c
      ;clojure           ; java with a lisp
      ;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       elixir            ; erlang done right
      ;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ess               ; emacs speaks statistics
      ;go                ; the hipster dialect
       (haskell +intero) ; a language that's lazier than I am
      ;hy                ; readability of scheme w/ speed of python
       (java +meghanada) ; the poster child for carpal tunnel syndrome
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       julia             ; a better, faster MATLAB
       latex             ; writing papers in Emacs has never been so fun
       ledger            ; an accounting system in Emacs
       lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; Exporting org to whatever you want
        +present         ; Emacs for presentations
        +publish)        ; Emacs+Org as a static site generator
       perl              ; write code no one else can comprehend
       php               ; perl's insecure younger brother
      ;plantuml          ; diagrams for confusing people more
      ;purescript        ; javascript, but functional
       python            ; beautiful is better than ugly
       rest              ; Emacs as a REST client
       ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
      ;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
      ;scala             ; java, but good
       sh                ; she sells (ba|z)sh shells on the C xor
       snakemake         ; make with a bioinformatics twist
       solidity          ; do you need a Blockchain? No.
      ;swift             ; who asked for emoji variables?
       typescript        ; javascript, but better
       web               ; the tubes

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
        mail
        ;+gmail)          ; emacs as an email client
        ;irc              ; how neckbeards socialize
        ;(rss +org)       ; emacs as an RSS reader
        ;twitter          ; twitter client https://twitter.com/vnought
        (write            ; emacs as a word processor (latex + org + markdown)
        +wordnut)         ; wordnet (wn) search
        ;+langtool)       ; a proofreader (grammar/style check) for Emacs

        :collab
         floobits          ; peer programming for a price
         impatient-mode    ; show off code over HTTP

       :config
       ;; The default module set reasonable defaults for Emacs. It also provides
       ;; a Spacemacs-inspired keybinding scheme, a custom yasnippet library,
       ;; and additional ex commands for evil-mode. Use it as a reference for
       ;; your own modules.
       (default +bindings +snippets +evil-commands))
