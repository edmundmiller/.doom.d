;; -*- no-byte-compile: t; -*-
;;; lang/mermaid/packages.el

(package! mermaid-ts-mode
  :recipe (:host github
           :repo "JonathanHope/mermaid-ts-mode"
           :branch "main"
           :files ("mermaid-ts-mode.el")))

(package! ob-mermaid)
