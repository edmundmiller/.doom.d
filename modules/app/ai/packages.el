;; -*- no-byte-compile: t; -*-
;;; app/ai/packages.el


(package! gptel)
(package! gptel-extensions :recipe (:host github :repo "kamushadenes/gptel-extensions.el"))

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))

(package! whisper :recipe (:host github :repo "natrys/whisper.el" :files ("*.el")))

(package! gptscript-mode :recipe (:host github :repo "emacs-openai/gptscript-mode"))