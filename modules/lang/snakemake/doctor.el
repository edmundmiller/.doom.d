;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/haskell/doctor.el

(unless (executable-find "snakemake")
  (warn! "Snakemake not installed!"))
