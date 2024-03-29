;; -*- no-byte-compile: t; -*-
;;; app/ai/packages.el


(package! gptel
  :recipe (:host github :repo "karthink/gptel" :branch "copilot" :files ("*.el")))
(package! gptel-extensions :recipe (:host github :repo "kamushadenes/gptel-extensions.el"))

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))

(package! whisper :recipe (:host github :repo "natrys/whisper.el" :files ("*.el")))
