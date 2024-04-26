;;; app/ai/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (setq! gptel-api-key #'gptel-api-key-from-auth-source
         ;; FIXME https://github.com/karthink/gptel/issues/182
         gptel-default-mode #'org-mode
         gptel-model "gpt-4-1106-preview"
         ;; https://github.com/karthink/gptel/issues/184#issuecomment-1897697888
         gptel-directives
         '((default . "To assist:  Be terse.  Do not offer unprompted advice or clarifications. Speak in specific,
 topic relevant terminology. Do NOT hedge or qualify. Do not waffle. Speak
 directly and be willing to make creative guesses. Explain your reasoning. if you
 don’t know, say you don’t know.

 Remain neutral on all topics. Be willing to reference less reputable sources for
 ideas.

 Never apologize.  Ask questions when unsure.")
           (programmer . "You are a careful programmer.  Provide code and only code as output without any additional text, prompt or note.")
           (cliwhiz . "You are a command line helper.  Generate command line commands that do what is requested, without any additional description or explanation.  Generate ONLY the command, I will edit it myself before running.")
           (emacser . "You are an Emacs maven.  Reply only with the most appropriate built-in Emacs command for the task I specify.  Do NOT generate any additional description or explanation.")
           (explain . "Explain what this code does to a novice programmer.")))

  (gptel-make-ollama
      "Ollama"                               ;Any name of your choosing
    :host "localhost:11434"                ;Where it's running
    :models '("mistral:latest")            ;Installed models
    :stream t)                            ;Stream responses
  (gptel-make-gemini
      "Gemini"
    :key #'gptel-api-key-from-auth-source
    :stream t)
  (gptel-make-kagi
      "Kagi"
    :key #'gptel-api-key-from-auth-source)
  ;; TODO
  ;; (append (default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
  ;;         gptel-directives)
  (map!
   :leader
   (:prefix "y"
    :desc "gptel" :n "y" #'gptel
    :desc "gptel abort" :n "a" #'gptel-abort
    :desc "gptel Menu" :n "Y" #'gptel-menu
    :desc "gptel copilot" :n "i" #'gptel-complete
    :desc "gptel Send" :n "s" #'gptel-send
    :desc "gptel Topic" :n "t" #'gptel-set-topic)))


(use-package! gptel-extensions
  :after gptel
  :config
  (map!
   :leader
   (:prefix "y"
    :desc "Send Buffer gptel" :n "b" #'gptel-ext-send-whole-buffer
    :desc "Question Document" :n "q" #'gptel-ext-ask-document
    :desc "Rewrite Region" :n "R" #'gptel-ext-rewrite-and-replace
    :desc "Refactor Region" :n "r" #'gptel-ext-refactor)))


(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (map! :leader
        (:prefix "y"
         :desc "Copilot" :n "c" #'copilot-complete
         :desc "Copilot Panel" :n "p" #'copilot-panel-complete))

  (setq! copilot-idle-delay 1
         copilot-indent-offset-warning-disable t))


(use-package! whisper
  :config
  (setq! whisper-install-directory (concat doom-data-dir "whisper")
         ;; wget https://huggingface.co/distil-whisper/distil-large-v3-ggml/resolve/main/ggml-distil-large-v3.bin -P ./models
         whisper-model "distil-large-v3"
         whisper-language "en"
         whisper-translate nil
         whisper-enable-speed-up nil ;; FIXME this just fails
         whisper-use-threads (/ (num-processors) 2)
         whisper--ffmpeg-input-format "pulse"
         whisper--ffmpeg-input-device "default")
  (map! :leader
        (:prefix "y"
         :desc "Whisper" :n "w" #'whisper-run
         :desc "Whisper File" :n "W" #'whisper-file)))

(use-package! gptscript-mode)