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
