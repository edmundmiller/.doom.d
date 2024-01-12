;; -*- no-byte-compile: t; -*-
;;; app/ai/packages.el


(package! gptel)
(package! chatgpt-shell)
(package! ob-chatgpt-shell)

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))

(package! whisper :recipe (:host github :repo "natrys/whisper.el" :files ("*.el")))
