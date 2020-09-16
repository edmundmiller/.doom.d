;;; teco-mu4e.el -*- lexical-binding: t; -*-

(setq mu4e-view-use-gnus t)

(after! mu4e
  (defun my-string-width (str)
    "Return the width in pixels of a string in the current
window's default font. If the font is mono-spaced, this
will also be the width of all other printable characters."
    (let ((window (selected-window))
          (remapping face-remapping-alist))
      (with-temp-buffer
        (make-local-variable 'face-remapping-alist)
        (setq face-remapping-alist remapping)
        (set-window-buffer window (current-buffer))
        (insert str)
        (car (window-text-pixel-size)))))


  (cl-defun mu4e~normalised-icon (name &key set colour height v-adjust)
    "Convert :icon declaration to icon"
    (let* ((icon-set (intern (concat "all-the-icons-" (or set "faicon"))))
           (v-adjust (or v-adjust 0.02))
           (height (or height 0.8))
           (icon (if colour
                     (apply icon-set `(,name :face ,(intern (concat "all-the-icons-" colour)) :height ,height :v-adjust ,v-adjust))
                   (apply icon-set `(,name  :height ,height :v-adjust ,v-adjust))))
           (icon-width (my-string-width icon))
           (space-width (my-string-width " "))
           (space-factor (- 2 (/ (float icon-width) space-width))))
      (concat (propertize " " 'display `(space . (:width ,space-factor))) icon)))


  (defun mu4e~initialise-icons ())
  (setq mu4e-use-fancy-chars t
        mu4e-headers-draft-mark      (cons "D" (mu4e~normalised-icon "pencil"))
        mu4e-headers-flagged-mark    (cons "F" (mu4e~normalised-icon "flag"))
        mu4e-headers-new-mark        (cons "N" (mu4e~normalised-icon "sync" :set "material" :height 0.8 :v-adjust -0.10))
        mu4e-headers-passed-mark     (cons "P" (mu4e~normalised-icon "arrow-right"))
        mu4e-headers-replied-mark    (cons "R" (mu4e~normalised-icon "arrow-right"))
        mu4e-headers-seen-mark       (cons "S" "") ;(mu4e~normalised-icon "eye" :height 0.6 :v-adjust 0.07 :colour "dsilver"))
        mu4e-headers-trashed-mark    (cons "T" (mu4e~normalised-icon "trash"))
        mu4e-headers-attach-mark     (cons "a" (mu4e~normalised-icon "file-text-o" :colour "silver"))
        mu4e-headers-encrypted-mark  (cons "x" (mu4e~normalised-icon "lock"))
        mu4e-headers-signed-mark     (cons "s" (mu4e~normalised-icon "certificate" :height 0.7 :colour "dpurple"))
        mu4e-headers-unread-mark     (cons "u" (mu4e~normalised-icon "eye-slash" :v-adjust 0.05)))

  (if (display-graphic-p)
      (mu4e~initialise-icons)
    ;; When it's the server, wait till the first graphical frame
    (add-hook! 'server-after-make-frame-hook
      (defun mu4e~initialise-icons-hook ()
        (when (display-graphic-p)
          (mu4e~initialise-icons)
          (remove-hook #'mu4e~initialise-icons-hook))))))


;;; To account for the increase width of each flag character, and make perform a few more visual tweaks, we’ll tweak the headers a bit
(after! mu4e
  (defun mu4e-header-colourise (str)
    (let* ((str-sum (apply #'+ (mapcar (lambda (c) (% c 3)) str)))
           (colour (nth (% str-sum (length mu4e-header-colourised-faces))
                        mu4e-header-colourised-faces)))
      (put-text-property 0 (length str) 'face colour str)
      str))

  (defvar mu4e-header-colourised-faces
    '(all-the-icons-lblue
      all-the-icons-purple
      all-the-icons-blue-alt
      all-the-icons-green
      all-the-icons-maroon
      all-the-icons-yellow
      all-the-icons-orange))

  (setq mu4e-headers-fields
        '((:account . 12)
          (:human-date . 8)
          (:flags . 6)
          (:from . 25)
          (:folder . 10)
          (:recipnum . 2)
          (:subject))
        mu4e-headers-date-format "%d/%m/%y"
        mu4e-headers-time-format "%T")

  (plist-put (cdr (assoc :flags mu4e-header-info)) :shortname " Flags") ; default=Flgs
  (setq mu4e-header-info-custom
        '((:account .
           (:name "Account" :shortname "Account" :help "Which account this email belongs to" :function
            (lambda (msg)
              (let ((maildir
                     (mu4e-message-field msg :maildir)))
                (mu4e-header-colourise (replace-regexp-in-string "^gmail" (propertize "g" 'face 'bold-italic)
                                                                 (format "%s"
                                                                         (substring maildir 1
                                                                                    (string-match-p "/" maildir 1)))))))))
          (:folder .
           (:name "Folder" :shortname "Folder" :help "Lowest level folder" :function
            (lambda (msg)
              (let ((maildir
                     (mu4e-message-field msg :maildir)))
                (mu4e-header-colourise (replace-regexp-in-string "\\`.*/" "" maildir))))))
          (:recipnum .
           (:name "Number of recipients"
            :shortname " ⭷"
            :help "Number of recipients for this message"
            :function
            (lambda (msg)
              (propertize (format "%2d"
                                  (+ (length (mu4e-message-field msg :to))
                                     (length (mu4e-message-field msg :cc))))
                          'face 'mu4e-footer-face)))))))

;; While considering width and the mu4e header view — it’s hard to see enough with a with less than 120 characters, so let’s add a hook to mu4e’s header mode.

(after! mu4e
  (defvar mu4e-min-header-frame-width 120
    "Minimum reasonable with for the header view.")
  (defun mu4e-widen-frame-maybe ()
    "Expand the frame with if it's less than `mu4e-min-header-frame-width'."
    (when (< (frame-width) mu4e-min-header-frame-width)
      (set-frame-width (selected-frame) mu4e-min-header-frame-width)))
  (add-hook 'mu4e-headers-mode-hook #'mu4e-widen-frame-maybe))
;; Due to evil, none of the marking commands work when making a visual selection in the headers view of mu4e. Without overriding any evil commands we may actually want to use in and evil selection, this can be easily fixed.

(map! :map mu4e-headers-mode-map
    :after mu4e
    :v "*" #'mu4e-headers-mark-for-something
    :v "!" #'mu4e-headers-mark-for-read
    :v "?" #'mu4e-headers-mark-for-unread
    :v "u" #'mu4e-headers-mark-for-unmark)
;; The main mu4e window is … alright. I’m not afraid of Unicode though, so I’ll define a fancier version. Look, it’s the asterisks. We can do better than asterisks. The keybindings can also be made nicer, why have [x] when we can just have a bold, coloured x. Does the same job, while looking much less garish. We don’t put this in an (after! ...) block as evil-collection-mu4e calls mu4e~main-action-str in Doom’s mu4e (usepackage! ...).

(defadvice! mu4e~main-action-prettier-str (str &optional func-or-shortcut)
  "Highlight the first occurrence of [.] in STR.
If FUNC-OR-SHORTCUT is non-nil and if it is a function, call it
when STR is clicked (using RET or mouse-2); if FUNC-OR-SHORTCUT is
a string, execute the corresponding keyboard action when it is
clicked."
  :override #'mu4e~main-action-str
  (let ((newstr
         (replace-regexp-in-string
          "\\[\\(..?\\)\\]"
          (lambda(m)
            (format "%s"
                    (propertize (match-string 1 m) 'face '(mode-line-emphasis bold))))
          (replace-regexp-in-string "\t\\*" "\t⚫" str)))
        (map (make-sparse-keymap))
        (func (if (functionp func-or-shortcut)
                  func-or-shortcut
                (if (stringp func-or-shortcut)
                    (lambda()(interactive)
                      (execute-kbd-macro func-or-shortcut))))))
    (define-key map [mouse-2] func)
    (define-key map (kbd "RET") func)
    (put-text-property 0 (length newstr) 'keymap map newstr)
    (put-text-property (string-match "[A-Za-z].+$" newstr)
                       (- (length newstr) 1) 'mouse-face 'highlight newstr)
    newstr))

(setq evil-collection-mu4e-end-region-misc "quit")
;; I’d also quite like an easy way to be able to move away from the created mu4e workspace without closing the process (which rules “quit” out). The obvious analogous idea is “hide”, and it turns out that the h key is conveniently unbound in the main mu4e view.

(map! :map mu4e-main-mode-map
      :after mu4e
      :nive "h" #'+workspace/other)
;; I often find myself viewing new mails using the Emacs client. When opening a new window and switching to view the mu4e workspace, and empty workspace is left. We can try to avoid this workspace pollution by deleting the current workspace before switching if it is empty.

(defadvice! delete-current-worspace-if-empty ()
  "Close the current workspace if it is empty."
  :before #'=mu4e
  (unless (+workspace-buffer-list)
    (+workspace-delete (+workspace-current-name))))

(after! mu4e
  (setq sendmail-program "msmtp"
        send-mail-function 'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from"); , "--read-recipients")
        message-send-mail-function 'message-send-mail-with-sendmail))

(after! mu4e
  (defun my-mu4e-set-account ()
    "Set the account for composing a message."
    (unless (and mu4e-compose-parent-message
                 (let ((to (cdr (car (mu4e-message-field mu4e-compose-parent-message :to))))
                       (from (cdr (car (mu4e-message-field mu4e-compose-parent-message :from))))))
                 (if (member to (plist-get mu4e~server-props :personal-addresses))
                     (setq user-mail-address to)
                   (if (member from (plist-get mu4e~server-props :personal-addresses))
                       (setq user-mail-address from)
                       nil)))
      (ivy-read "Account: " (plist-get mu4e~server-props :personal-addresses) :action (lambda (candidate) (setq user-mail-address candidate)))))

  (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account))

(defun mu4e-compose-from-mailto (mailto-string)
  (require 'mu4e)
  (unless mu4e~server-props (mu4e t) (sleep-for 0.1))
  (let* ((mailto (rfc2368-parse-mailto-url mailto-string))
         (to (cdr (assoc "To" mailto)))
         (subject (or (cdr (assoc "Subject" mailto)) ""))
         (body (cdr (assoc "Body" mailto)))
         (org-msg-greeting-fmt (if (assoc "Body" mailto)
                                   (replace-regexp-in-string "%" "%%"
                                                             (cdr (assoc "Body" mailto)))
                                 org-msg-greeting-fmt))
         (headers (-filter (lambda (spec) (not (-contains-p '("To" "Subject" "Body") (car spec)))) mailto)))
    (mu4e~compose-mail to subject headers)))

;; (defvar org-msg-currently-exporting nil
;;   "Helper variable to indicate whether org-msg is currently exporting the org buffer to HTML.
;; Usefull for affecting some of my HTML export config.")

;; (use-package! org-msg
;;   :after mu4e
;;   :config
;;   (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t tex:dvipng"
;;         org-msg-startup "hidestars indent inlineimages"
;;         org-msg-greeting-fmt "\nHi %s,\n\n"
;;         org-msg-greeting-name-limit 3
;;         org-msg-text-plain-alternative t)
;;   (map! :map org-msg-edit-mode-map
;;         :n "G" #'org-msg-goto-body)
;;   (defadvice! org-msg--now-exporting (&rest _)
;;     :before #'org-msg-org-to-xml
;;     (setq org-msg-currently-exporting t))
;;   (defadvice! org-msg--not-exporting (&rest _)
;;     :after #'org-msg-org-to-xml
;;     (setq org-msg-currently-exporting nil))
;;   <<org-msg-restyle>>
;;   (org-msg-mode t))
;; (setq org-msg-enforce-css
;;       (let* ((font-family '(font-family . "-apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Oxygen, Ubuntu, Cantarell,\
;;         \"Fira Sans\", \"Droid Sans\", \"Helvetica Neue\", Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\";"))
;;              (monospace-font '(font-family . "SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace;"))
;;              (font-size '(font-size . "11pt"))
;;              (font `(,font-family ,font-size))
;;              (line-height '(line-height . "1.2"))
;;              (theme-color "#2654BF")
;;              (bold '(font-weight . "bold"))
;;              (color `(color . ,theme-color))
;;              (table `((margin-top . "6px") (margin-bottom . "6px")
;;                       (border-left . "none") (border-right . "none")
;;                       (border-top . "2px solid #222222") (border-bottom . "2px solid #222222")))

;;              (ftl-number `(,color ,bold (text-align . "left")))
;;              (inline-modes '(asl c c++ conf cpp csv diff ditaa emacs-lisp
;;                                  fundamental ini json makefile man org plantuml
;;                                  python sh xml))
;;              (inline-src `((background-color . "rgba(27,31,35,.05)")
;;                            (border-radius . "3px")
;;                            (padding . ".2em .4em")
;;                            (font-size . "90%") ,monospace-font
;;                            (margin . 0)))
;;              (code-src
;;               (mapcar (lambda (mode)
;;                         `(code ,(intern (concat "src src-" (symbol-name mode)))
;;                                ,inline-src))
;;                       inline-modes)))
;;         `((del nil ((color . "grey") (border-left . "none")
;;                     (text-decoration . "line-through") (margin-bottom . "0px")
;;                     (margin-top . "10px") (line-height . "11pt")))
;;           (a nil (,color))
;;           (a reply-header ((color . "black") (text-decoration . "none")))
;;           (div reply-header ((padding . "3.0pt 0in 0in 0in")
;;                              (border-top . "solid #e1e1e1 1.0pt")
;;                              (margin-bottom . "20px")))
;;           (span underline ((text-decoration . "underline")))
;;           (li nil (,line-height (margin-bottom . "0px")
;;                                 (margin-top . "2px")))
;;           (nil org-ul ((list-style-type . "square")))
;;           (nil org-ol (,@font ,line-height (margin-bottom . "0px")
;;                               (margin-top . "0px") (margin-left . "30px")
;;                               (padding-top . "0px") (padding-left . "5px")))
;;           (nil signature (,@font (margin-bottom . "20px")))
;;           (blockquote nil ((padding . "0px 10px") (margin-left . "10px")
;;                            (margin-top . "20px") (margin-bottom . "0")
;;                            (border-left . "3px solid #ccc") (font-style . "italic")
;;                            (background . "#f9f9f9")))
;;           (code nil (,font-size ,monospace-font (background . "#f9f9f9")))
;;           ,@code-src
;;           (nil linenr ((padding-right . "1em")
;;                        (color . "black")
;;                        (background-color . "#aaaaaa")))
;;           (pre nil ((line-height . "1.2")
;;                     (color . ,(doom-color 'fg))
;;                     (background-color . ,(doom-color 'bg))
;;                     (margin . "4px 0px 8px 0px")
;;                     (padding . "8px 12px")
;;                     (width . "95%")
;;                     (border-radius . "5px")
;;                     (font-weight . "500")
;;                     ,monospace-font))
;;           (div org-src-container ((margin-top . "10px")))
;;           (nil figure-number ,ftl-number)
;;           (nil table-number)
;;           (caption nil ((text-align . "left")
;;                         (background . ,theme-color)
;;                         (color . "white")
;;                         ,bold))
;;           (nil t-above ((caption-side . "top")))
;;           (nil t-bottom ((caption-side . "bottom")))
;;           (nil listing-number ,ftl-number)
;;           (nil figure ,ftl-number)
;;           (nil org-src-name ,ftl-number)
;;           (img nil ((vertical-align . "middle")
;;                     (max-width . "100%")))
;;           (img latex-fragment-inline ((transform . ,(format "translateY(-1px) scale(%.3f)"
;;                                                      (/ 1.0 (if (boundp 'preview-scale)
;;                                                                 preview-scale 1.4)))))
;;                                (margin . "0 -0.35em"))
;;           (table nil (,@table ,line-height (border-collapse . "collapse")))
;;           (th nil ((border . "none") (border-bottom . "1px solid #222222")
;;                    (background-color . "#EDEDED") (font-weight . "500")
;;                    (padding . "3px 10px")))
;;           (td nil (,@table (padding . "1px 10px")
;;                            (background-color . "#f9f9f9") (border . "none")))
;;           (td org-left ((text-align . "left")))
;;           (td org-right ((text-align . "right")))
;;           (td org-center ((text-align . "center")))
;;           (kbd nil ((border . "1px solid #d1d5da") (border-radius . "3px")
;;                     (box-shadow . "inset 0 -1px 0 #d1d5da") (background-color . "#fafbfc")
;;                     (color . "#444d56") (padding . "3px 5px") (display . "inline-block")))
;;           (div outline-text-4 ((margin-left . "15px")))
;;           (div outline-4 ((margin-left . "10px")))
;;           (h4 nil ((margin-bottom . "0px") (font-size . "11pt")))
;;           (h3 nil ((margin-bottom . "0px")
;;                    ,color (font-size . "14pt")))
;;           (h2 nil ((margin-top . "20px") (margin-bottom . "20px")
;;                    ,color (font-size . "18pt")))
;;           (h1 nil ((margin-top . "20px")
;;                    (margin-bottom . "0px") ,color (font-size . "24pt")))
;;           (p nil ((text-decoration . "none") (margin-bottom . "0px")
;;                   (margin-top . "10px") (line-height . "11pt") ,font-size
;;                   (max-width . "100ch")))
;;           (b nil ((font-weight . "500") (color . ,theme-color)))
;;           (div nil (,@font (line-height . "12pt"))))))
