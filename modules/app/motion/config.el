;;; app/motion/config.el -*- lexical-binding: t; -*-

(use-package! org-timeblock
  :after org
  :config
  ;; Ensure evil-mode works properly with org-timeblock
  ;; Instead of disabling evil-mode, we'll set up proper evil keybindings
  (defun +org-timeblock-setup-evil-keybindings ()
    "Set up evil keybindings for org-timeblock modes."
    ;; Evil keybindings for org-timeblock-mode
    (evil-define-key 'normal org-timeblock-mode-map
      "j" #'org-timeblock-forward-block
      "k" #'org-timeblock-backward-block
      "h" #'org-timeblock-day-earlier
      "l" #'org-timeblock-day-later
      (kbd "C-h") #'org-timeblock-day-earlier
      (kbd "C-l") #'org-timeblock-day-later
      (kbd "C-j") #'org-timeblock-forward-block
      (kbd "C-k") #'org-timeblock-backward-block
      "gg" #'org-timeblock-goto-first-block
      "G" #'org-timeblock-goto-last-block
      (kbd "RET") #'org-timeblock-goto
      (kbd "TAB") #'org-timeblock-goto
      "o" #'org-timeblock-goto
      "q" #'quit-window
      "r" #'org-timeblock-redraw-buffers
      "gr" #'org-timeblock-redraw-buffers
      "." #'org-timeblock-goto-today
      "t" #'org-timeblock-toggle-timeblock-list
      "T" #'org-timeblock-toggle-timeblock-list
      "J" #'org-timeblock-jump-to-day
      "V" #'org-timeblock-change-span
      "s" #'org-timeblock-schedule
      "d" #'org-timeblock-set-duration
      "+" #'org-timeblock-new-task
      "a" #'org-timeblock-new-task
      "m" #'org-timeblock-mark-block
      "u" #'org-timeblock-unmark-block
      "U" #'org-timeblock-unmark-all-blocks
      "%" #'org-timeblock-mark-by-regexp
      "x" #'org-timeblock-schedule
      "?" #'describe-mode)

    ;; Evil keybindings for org-timeblock-list-mode
    (evil-define-key 'normal org-timeblock-list-mode-map
      "j" #'next-line
      "k" #'previous-line
      "h" #'org-timeblock-day-earlier
      "l" #'org-timeblock-day-later
      (kbd "C-h") #'org-timeblock-day-earlier
      (kbd "C-l") #'org-timeblock-day-later
      "gg" #'beginning-of-buffer
      "G" #'end-of-buffer
      (kbd "RET") #'org-timeblock-goto
      (kbd "TAB") #'org-timeblock-goto
      "o" #'org-timeblock-goto
      "q" #'quit-window
      "r" #'org-timeblock-redraw-buffers
      "gr" #'org-timeblock-redraw-buffers
      "." #'org-timeblock-goto-today
      "t" #'org-timeblock-toggle-timeblock-list
      "T" #'org-timeblock-toggle-timeblock-list
      "J" #'org-timeblock-jump-to-day
      "V" #'org-timeblock-change-span
      "s" #'org-timeblock-schedule
      "d" #'org-timeblock-set-duration
      "+" #'org-timeblock-new-task
      "a" #'org-timeblock-new-task
      "m" #'org-timeblock-mark-block
      "u" #'org-timeblock-unmark-block
      "U" #'org-timeblock-unmark-all-blocks
      "%" #'org-timeblock-mark-by-regexp
      "x" #'org-timeblock-schedule
      "?" #'describe-mode))

  ;; Set up keybindings after the modes are loaded
  (add-hook 'org-timeblock-mode-hook #'+org-timeblock-setup-evil-keybindings)
  (add-hook 'org-timeblock-list-mode-hook #'+org-timeblock-setup-evil-keybindings)

  ;; Global keybinding to open org-timeblock
  (map! :leader
        (:prefix ("o" . "open")
         :desc "Open org-timeblock" "j" #'org-timeblock)))
