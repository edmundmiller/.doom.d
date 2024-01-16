;;; app/ai/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :config
  (setq! gptel-default-mode #'org-mode
         gptel-model "gpt-4-1106-preview"
         gptel-api-key #'gptel-api-key-from-auth-source)
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
  (map!
   :leader
   (:prefix "y"
    :desc "gptel" :n "y" #'gptel
    :desc "gptel Send" :n "y" #'gptel-send
    :desc "gptel Menu" :n "Y" #'gptel-menu
    :desc "gptel Topic" :n "t" #'gptel-set-topic)))


(use-package! gptel-extensions
  :after gptel
  :config
  (map!
   :leader
   (:prefix "y"
    :desc "Send Buffer gptel" :n "b" #'gptel-ext-send-whole-buffer
    :desc "Question Document" :n "q" #'gptel-ext-ask-document
    :desc "Rewrite Region" :n "e" #'gptel-ext-rewrite-and-replace
    :desc "Refactor Region" :n "r" #'gptel-ext-refactor)))

(use-package! chatgpt-shell
  :init
  (setq! chatgpt-shell-openai-key
         (lambda ()
           (auth-source-pick-first-password :host "api.openai.com"))
         chatgpt-shell-chatgpt-streaming t)
  (setq! chatgpt-shell-default-prompts
         (append
          '("Rank these links in the order that I should read them:"
            chatgpt-shell-default-prompts))
         chatgpt-shell-model-versions
         '("gpt-4-1106-preview"
           "gpt-3.5-turbo"
           "gpt-3.5-turbo-0613"
           "gpt-3.5-turbo-16k"
           "gpt-3.5-turbo-16k-0613"
           "gpt-4"
           "gpt-4-0613"))
  (map!
   :leader
   (:prefix "y" ;; y not?
    :desc "ChatGPT" :n "g" #'chatgpt-shell)))


(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :init
  ;; accept completion from copilot and fallback to company
  (defun my-tab ()
    (interactive)
    (or (copilot-accept-completion)
        (corfu-complete)))

  :config
  (map! (:map copilot-completion-map
         :i "s-<tab>" #'copilot-accept-completion
         :i "TAB" #'copilot-accept-completion-by-line
         ;; :i "C-l" #'copilot-accept-completion-by-word
         :i "C-," #'copilot-next-completion
         :i "C-." 'copilot-previous-completion)
        (:map corfu-map
              "<tab>" #'my-tab
              "TAB" #'my-tab
              "C-l" #'copilot-accept-completion-by-word
              "C-S-l"  #'copilot-accept-completion-by-line)
        :leader
        (:prefix "y"
         :desc "Copilot" :n "c" #'copilot-complete
         :desc "Copilot Panel" :n "p" #'copilot-panel-complete))

  (setq! copilot-idle-delay 1
         copilot-indent-offset-warning-disable t))

(use-package! whisper
  :config
  (setq whisper-install-directory (concat doom-data-dir "whisper")
        ;; TODO whisper-install-whispercpp nil
        whisper-model "medium"
        whisper-language "en"
        whisper-translate nil
        whisper-enable-speed-up nil ;; FIXME this just fails
        whisper-use-threads 8
        whisper--ffmpeg-input-format "pulse"
        whisper--ffmpeg-input-device "default")
  (map! :leader
        (:prefix "y"
         :desc "Whisper" :n "w" #'whisper-run
         :desc "Whisper File" :n "W" #'whisper-file)))
