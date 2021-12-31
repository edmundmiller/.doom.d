;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "a0d32ab16748b7b0c43d6421f1b497b7caf8e590")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "a0d32ab16748b7b0c43d6421f1b497b7caf8e590"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "a0d32ab16748b7b0c43d6421f1b497b7caf8e590"))
(when (featurep! :completion vertico)
  (package! citar :pin "6e3a194c3ab655693f8194be78542366755c58c9"))
(when (featurep! :lang org +ref)
  (package! org-ref :pin "0d988807301bee68b0483f6b461125c31edfbc2c"))
(when (featurep! :lang org +roam2)
  (package! org-roam-bibtex :pin "c13a05b2c855ba1516241d8a1de33bf2c689d6e4"))
