;; -*- no-byte-compile: t; -*-
;;; ui/tree-sitter/packages.el

(package! tree-sitter
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "dbab35d5fc2fd5cb6ba08f31478446706e65282f")
(package! tree-sitter-langs
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "ef42920bb573b40e64b35e43968dac355f87e959")