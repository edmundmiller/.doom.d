;;; app/ai/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun whisper--ctranslate2-command (input-file)
  `("whisper-ctranslate2"
    ,@(when whisper-use-threads (list "--threads" (number-to-string whisper-use-threads)))
    "--task" ,(if whisper-translate "translate" "transcribe")
    "--model" ,whisper-model
    "--vad_filter" "True"
    "--compute_type" "auto"
    "--language" ,whisper-language
    "--output_dir" "/tmp/"
    "--output_format" "txt"
    ,input-file))

(add-hook 'whisper-after-transcription-hook
          (lambda ()
            (save-excursion
              (goto-char (point-max))
              (delete-line)
              (goto-char (point-min))
              (delete-line)
              (while (not (eobp))
                (goto-char (pos-bol))
                (when (re-search-forward "\\]" (pos-eol) t 1)
                  (skip-chars-forward " " (pos-eol))
                  (delete-region (pos-bol) (point)))
                (forward-line 1)))))

;;;###autoload
(defun pipe-transcribed-audio-to-gptel ()
  "Pipe whisper's transcription output into `gptel'."
  (let ((transcription (buffer-substring (line-beginning-position)
                                         (line-end-position))))
    (gptel-request transcription
      :system  "Reformat the following text. Don't output anything besides the text. Clean up formatting, punctuation, spelling, and grammer, and split ideas into paragraphs. If there are very obvious cases of bullet point lists, format the output as a list")))

;;;###autoload
(defun my/kagi-summarize (url)
  "Function that requests kagi for a url summary and shows it in a side-window"
  (let ((gptel-backend gptel--kagi)
        (gptel-model "summarize:agnes")) ;or summarize:cecil, summarize:daphne, summarize:muriel
    (gptel-request
        url
      :callback
      (lambda (response info)
        (if response
            (with-current-buffer (get-buffer-create "*Kagi Summary*")
              (let ((inhibit-read-only t))
                (erase-buffer)
                (visual-line-mode 1)
                (insert response)
                (display-buffer
                 (current-buffer)
                 '((display-buffer-in-side-window
                    display-buffer-at-bottom)
                   (side . bottom))))
              (special-mode 1))
          (message "gptel-request failed with message: %s"
                   (plist-get info :status)))))))
