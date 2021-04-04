;; -*- no-byte-compile: t; -*-
;;; ui/tree-sitter/packages.el

;; -*- no-byte-compile: t; -*-
;;; ui/tree-sitter/packages.el

(package! tree-sitter
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "ae26995c4b006f37b8e46a6125e0b60100b42974")
(package! tree-sitter-langs
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "ae26995c4b006f37b8e46a6125e0b60100b42974")