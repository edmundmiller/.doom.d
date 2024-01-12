;;; app/ai/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :config
  (setq! gptel-default-mode #'org-mode)
  ;; gpt-4-1106-preview
  (gptel-make-ollama
   "Ollama"                               ;Any name of your choosing
   :host "localhost:11434"                ;Where it's running
   :models '("mistral:latest")            ;Installed models
   :stream t))                            ;Stream responses


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
  :config
  (setq copilot-idle-delay 1)

  (map! (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("M-n" . 'copilot-next-completion)
              ("M-p" . 'copilot-previous-completion))))


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
