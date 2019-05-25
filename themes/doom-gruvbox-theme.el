;;; doom-gruvbox-theme.el --- inspired by gruvbox
(require 'doom-themes)

;;
(defgroup doom-gruvbox-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-gruvbox-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-gruvbox-theme
  :type 'boolean)

(defcustom doom-gruvbox-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-gruvbox-theme
  :type 'boolean)

(defcustom doom-gruvbox-comment-bg doom-gruvbox-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-gruvbox-theme
  :type 'boolean)

(defcustom doom-gruvbox-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-gruvbox-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-gruvbox
  "A dark theme inspired by gruvbox Dark"

  ;; name        default   256       16
  ((bg         '("#282828" nil       nil            ))
   (bg-alt     '("#32302f" nil       nil            ))
   (base0      '("#1d2021" "black"   "black"        ))
   (base1      '("#3c3836" "black"   "black"        ))
   (base2      '("#504945" "#1e1e1e" "brightblack"  ))
   (base3      '("#665c54" "#2e2e2e" "brightblack"  ))
   (base4      '("#7c6f64" "#262626" "brightblack"  ))
   (base5      '("#928374" "#525252" "brightblack"  ))
   (base6      '("#a89984" "#6b6b6b" "brightblack"  ))
   (base7      '("#bdae93" "#979797" "brightblack"  ))
   (base8      '("#d5c4a1" "#dfdfdf" "white"        ))
   (fg         '("#ebdbb2" "#bfbfbf" "brightwhite"  ))
   (fg-alt     '("#fbf1c7" "#2d2d2d" "white"        ))

   (grey       '("#928374"))
   (red        '("#cc241d" "#ff6655" "red"          ))
   (orange     '("#d65d0e" "#dd8844" "brightred"    ))
   (green      '("#98971a" "#99bb66" "green"        ))
   (teal       '("#689d6a" "#44b9b1" "brightgreen"  ))
   (yellow     '("#d79921" "#ECBE7B" "yellow"       ))
   (blue       '("#83a598" "#51afef" "brightblue"   ))
   (dark-blue  '("#458588" "#2257A0" "blue"         ))
   (magenta    '("#d3869b" "#c678dd" "brightmagenta"))
   (violet     '("#b16286" "#a9a1e1" "magenta"      ))
   (cyan       '("#46D9FF" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        orange)
   (comments       (if doom-gruvbox-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-gruvbox-brighter-comments dark-cyan base5) 0.25))
   (constants      magenta)
   (functions      yellow)
   (keywords       red)
   (methods        cyan)
   (operators      blue)
   (type           magenta)
   (strings        green)
   (variables      (doom-lighten blue 0.4))
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-gruvbox-brighter-modeline)
   (-modeline-pad
    (when doom-gruvbox-padded-modeline
      (if (integerp doom-gruvbox-padded-modeline) doom-gruvbox-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt)))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-gruvbox-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; Doom modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; ivy-mode
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-gruvbox-theme.el ends here
