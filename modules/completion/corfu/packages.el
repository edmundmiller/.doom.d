
;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :recipe (:files ("*.el" "extensions/*.el")) :pin "457042d486e7542b9a6a832e47e6833d217ffd47")
(package! cape :pin "abacb231157e0c90e29bdda6d15b4b448e48ffbd")
(when (modulep! +icons)
  (package! nerd-icons-completion :pin "c2db8557a3c1a9588d111f8c8e91cae96ee85010")
  (package! nerd-icons-corfu :pin "7077bb76fefc15aed967476406a19dc5c2500b3c"))
(when (modulep! +orderless)
  (package! orderless :pin "89eb3775daa53cfb52ad03015410c23f28c72d30"))
(when (modulep! :os tty)
  (package! corfu-terminal :pin "501548c3d51f926c687e8cd838c5865ec45d03cc"))
(when (modulep! :editor snippets)
  (package! yasnippet-capf :pin "a0a6b1c2bb6decdad5cf9b74202f0042f494a6ab"))