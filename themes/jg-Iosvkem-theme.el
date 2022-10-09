;;-*- lexical-binding: t; no-byte-compile: t; -*-
;;; jg-Iosvkem-theme.el, adapted from the original doom one
;;

(require 'doom-themes)


;;-- variables
(defgroup jg-iosvkem-theme nil
  "Options for the `doom-theme.el' theme."
  :group 'doom-themes)

(defcustom jg-iosvkem-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'jg-iosvkem-theme
  :type 'boolean)

(defcustom jg-iosvkem-brighter-comments t
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'jg-iosvkem-theme
  :type 'boolean)

(defcustom jg-iosvkem-comment-bg jg-iosvkem-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'jg-iosvkem-theme
  :type 'boolean)

(defcustom jg-iosvkem-padded-modeline t
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'jg-iosvkem-theme
  :type '(choice integer boolean))


;;-- end variables

;;; Theme definition
(def-doom-theme jg-Iosvkem
  "A dark theme inspired by VIM Iosvkem"
  (
;;-- colours
  ;; name        default   256       16
   (bg         '("#1b1d1e" "#1b1d1e" nil))
   (bg-alt     '("#262829" "#262829" nil))
   (base0      '("#1b1d1e" "#1b1d1e" "black"))
   (base1      '("#202020" "#202020" "brightblack"))
   (base2      '("#303030" "#303030" "brightblack"))
   (base3      '("#303030" "#303030" "brightblack"))
   (base4      '("#505050" "#505050" "brightblack"))
   (base5      '("#505050" "#505050" "brightblack"))
   (base6      '("#808080" "#808080" "brightblack"))
   (base7      '("#808080" "#808080" "brightblack"))
   (base8      '("#DFDFDF" "#dfdfdf" "white"))
   (fg         '("#dddddd" "#dddddd" "white"))
   (fg-alt     '("#5B6268" "#2d2d2d" "white"))

   (grey       base4)
   (red        '("#d02b61" "#d02b61" "red"))
   (orange     '("#da8548" "#dd8844" "brightred"))
   (green      '("#60aa00" "#60aa00" "green"))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"))
   (yellow     '("#d08928" "#d08928" "yellow"))
   (blue       '("#6c9ef8" "#6c9ef8" "brightblue"))
   (dark-blue  '("#6688aa" "#6688aa" "blue"))
   (magenta    '("#b77fdb" "#b77fdb" "magenta"))
   (violet     '("#a9a1e1" "#a9a1e1" "brightmagenta"))
   (cyan       '("#00aa80" "#00aa80" "brightcyan"))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"))
   (urlblue    '("#57aadd" "#57aadd" "blue"))
   (iolime     '("#bbfc20" "#bbfc20" "green"))
   (iopurple   '("#bb20fc" "#bb20fc" "magenta"))
   (iocyan     '("#20bbfc" "#20bbfc" "cyan"))
   (iopink     '("#fc20bb" "#fc20bb" "red"))
   (ioteal     '("#20fcbb" "#20fcbb" "brightgreen"))


   (aerugo0  "#2f1e1a")
   (aerugo1  "#4f3322")
   (aerugo2  "#723627")
   (aerugo3  "#95392c")
   (aerugo4  "#c75533")
   (aerugo5  "#e76d46")
   (aerugo6  "#934e28")
   (aerugo7  "#a2663c")
   (aerugo8  "#c87d40")
   (aerugo9  "#f5a95b")
   (aerugo10 "#6b8b8c")
   (aerugo11 "#81a38e")
   (aerugo12 "#aac39e")
   (aerugo13 "#ffffff")
   (aerugo14 "#d1d0ce")
   (aerugo15 "#bab7b2")
   (aerugo16 "#898a8a")
   (aerugo17 "#686461")
   (aerugo18 "#554d4b")
   (aerugo19 "#3c3d3b")
   (aerugo20 "#343230")
   (aerugo21 "#87d1ef")
   (aerugo22 "#64a1c2")
   (aerugo23 "#466480")
   (aerugo24 "#2f485c")
   (aerugo25 "#242e35")
   (aerugo26 "#1b2026")
   (aerugo27 "#aa9c8a")
   (aerugo28 "#917f6d")
   (aerugo29 "#86624a")
   (aerugo30 "#715b48")
   (aerugo31 "#5e4835")

;;-- end colours

;;-- face categories
   ;; required for all themes
   (highlight      iopink)
   (vertical-bar   base2)
   (selection      aerugo22)
   (region         bg-alt)

   (builtin        magenta)

   (comments       (if jg-iosvkem-brighter-comments base5 base4))
   (doc-comments   (doom-lighten (if jg-iosvkem-brighter-comments dark-cyan base6) 0.25))

   (constants      green)
   (functions      magenta)
   (keywords       blue)
   (methods        teal)
   (operators      blue)
   (type           cyan)
   (strings        yellow)
   (variables      dark-cyan)
   (numbers        green)


   (error          red)
   (warning        yellow)
   (success        green)

   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

;;-- end face categories

;;-- custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright jg-iosvkem-brighter-modeline)
   (-modeline-pad
    (when jg-iosvkem-padded-modeline
      (if (integerp jg-iosvkem-padded-modeline) jg-iosvkem-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base6)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-alt
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt)))

   )
;;-- end custom categories

;;-- faces
  (
   ((font-lock-comment-face &override) :background (if jg-iosvkem-comment-bg (doom-lighten bg 0.05)) :slant 'italic)
   ((font-lock-doc-face &override) :slant 'normal)
   ((font-lock-function-name-face &override) :weight 'bold)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground iocyan :background bg)
   ((line-number-major-tick &override) :background red)
   ((tooltip &override) :background bg)
   (lazy-highlight :background iocyan :foreground bg :weight 'bold)

   ;; hl-line
   ((hl-line &override) :background base2)
   ((jg-normal-line &override) :background base2)
   ((jg-insert-line &override) :background aerugo1 :foreground aerugo21)

   ;; highlight-numbers
   (highlight-numbers-number :foreground numbers)
   ;; whitespace <built-in>
   ((whitespace-tab &override) :background bg)

   ;;;; highlight-escape-sequences
   ((hes-escape-backslash-face &override) :inherit 'normal :foreground red)
   ((hes-escape-sequence-face &override) :inherit 'normal :foreground red)

   ;; mic-paren
   ((paren-face-match &override) :foreground iopink :background bg :weight 'ultra-bold)

   ;; nav-flash
   ((nav-flash-face &override) :background bg-alt :foreground iopink)

   ;; outline
   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground magenta)
   ((outline-3 &override) :foreground dark-cyan)
   ((outline-6 &override) :foreground (doom-lighten dark-cyan 0.2))
   ((outline-7 &override) :foreground (doom-lighten blue 0.4))
   ((outline-8 &override) :foreground (doom-lighten magenta 0.4))

   ;; company
   ((company-tooltip-selection &override) :foreground iopink)
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

   ;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   ;; -- Major Modes
   ;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   ((markdown-bold-face &override) :foreground cyan)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ((markdown-italic-face &override) :foreground cyan)
   ((markdown-link-face &override) :foreground blue)
   ((markdown-list-face &override) :foreground magenta)
   ((markdown-url-face &override) :foreground base5)
   (markdown-header-delimiter-face :inherit 'bold :foreground red)
   (markdown-header-face :inherit 'bold :foreground fg)
   (markdown-markup-face :foreground red)

   ;; org <built-in>
   (org-hide :foreground hidden)
   (org-link :foreground urlblue :underline t)
   ((org-block &override) :background bg-alt)
   ((org-quote &override) :background bg-alt)
   ((org-block-begin-line &override) :foreground comments :background bg)

   ;; solaire-mode and mode line
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))
   (mode-line :background modeline-bg :foreground modeline-fg :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))
   (mode-line-inactive :background modeline-bg-inactive :foreground modeline-fg-alt :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))

   ;; Base theme variable overrides-
   ;;()
   )

;;-- end faces

  )

;;; jg-iosvkem-theme.el ends here
