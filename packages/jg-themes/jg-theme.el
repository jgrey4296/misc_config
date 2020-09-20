;;; ../../../Volumes/documents/github/emacs_files/packages/jg-themes/template-theme2.el -*- lexical-binding: t; -*-

;;; Variables

;;; Theme definition
(def-doom-theme jg
  "A light theme inspired by Acario light"

;;;; Colors
  ;; name        default   256         16
  ((bg         '("#F5F5F9" "color-255" "black"        ))
   (bg-alt     '("#E9E9F2" "color-254" "brightblack"  ))
   (base0      '("#D0D0E3" "color-188" "black"        ))
   (base1      '("#D0D0E3" "color-188" "brightblack"  ))
   (base2      '("#C0CCD0" "color-152" "brightblack"  ))
   (base3      '("#9EA6B0" "color-103" "brightblack"  ))
   (base4      '("#585C6C" "color-60"  "brightblack"  ))
   (base5      '("#4E4E4E" "color-239" "brightblack"  ))
   (base6      '("#3A3A3A" "color-237" "white"        ))
   (base7      '("#303030" "color-236" "white"        ))
   (base8      '("#1E1E33" "color-236" "brightwhite"  ))
   (fg         '("#0F1019" "color-234" "brightwhite"  ))
   (fg-alt     '("#0D0E16" "color-233" "brightwhite"  ))

   (grey       base5)

   (red        '("#D70000" "color-160" "red"          ))
   (green      '("#005F00" "color-22"  "green"        ))
   (yellow     '("#AF8700" "color-136" "yellow"       ))
   (blue       '("#1F55A0" "color-25"  "blue"         ))
   (magenta    '("#AF005F" "color-125" "magenta"      ))
   (cyan       '("#007687" "color-30"  "cyan"         ))

   (orange     '("#D75F00" "color-166" "brightred"    ))
   (teal       '("#009B7C" "color-36"  "brightgreen"  ))
   (violet     '("#8700AF" "color-91"  "brightmagenta"))

   (bg-blue    '("#DEEAF8" "color-189"   "blue"         ))
   (dark-blue  bg-blue)
   (bg-cyan    '("#D5FAFF" "color-195"   "cyan"         ))
   (dark-cyan  bg-cyan)

   ;; Custom Colours
   (gbp-0 "#000000")
   (gbp-1 "#402820")
   (gbp-2 "#883820")
   (gbp-3 "#e08850")
   (gbp-4 "#f8c880")
   (gbp-5 "#a86850")
   (gbp-6 "#fff0b8")
   (gbp-7 "#e0a080")
   (gbp-8 "#684828")

  (tp-1 "#000808")
  (tp-2 "#783830")
  (tp-3 "#381818")
  (tp-4 "#a07058")
  (tp-5 "#383838")
  (tp-6 "#b04038")
  (tp-7 "#d89090")
  (tp-8 "#508989")
  (tp-9 "#486878")
  (tp-10 "#b0a080")

;;;; face categories -- required for all themes
   (highlight    base3)
   (vertical-bar base3)
   (selection    base3)
   (builtin      base3)
   (comments     tp-9)
   (doc-comments base3)
   (constants    base3)
   (functions    base3)
   (keywords     base3)
   (methods      base3)
   (operators    base3)
   (type         base3)
   (strings      base3)
   (variables    base3)
   (numbers      base3)
   (region       gbp-2)
   (error        base3)
   (warning      base3)
   (success      base3)
   (vc-modified  base3)
   (vc-added     base3)
   (vc-deleted   base3)
   (default      fg)

;;;; custom categories
   (hidden bg)

   )
;;;; --- extra faces ------------------------
  ((default :background gbp-1)
   (hl-line :background gbp-2 :foreground gbp-6)
   (line-number :background gbp-8)

   ;; TODO
   ;; bold
   ;; border
   ;; (cursor :foreground tp-1 :background tp-1)
   ;; error
   ;; evil
   ;; fill-column-indicator
   ;; (font-lock-builtin-face  )
   ;; (font-lock-comment-delimiter-face  )
   ;; (font-lock-comment-face  )
   ;; (font-lock-constant-face  )
   ;; (font-lock-doc-face  )
   ;; (font-lock-function-name-face  )
   ;; (font-lock-keyword-face  )
   ;; (font-lock-negation-char-face  )
   ;; (font-lock-preprocessor-face  )
   ;; (font-lock-regexp-grouping-backslash  )
   ;; (font-lock-regexp-grouping-construct  )
   ;; (font-lock-string-face  )
   ;; (font-lock-type-face  )
   ;; (font-lock-variable-name-face  )
   ;; (font-lock-warning-face  )
   ;; fringe
   ;; header-line
   ;; header-line-highlight
   ;; hl-fill-column-face
   ;; hl-paren-face
   ;; hl-todo
   ;; highlight-numbers-number
   ;; highlight-quoted-quote
   ;; highlight-quoted-symbol
   ;; ibuffer-locked-buffer
   ;; internal-border
   ;; isearch
   ;; isearch-fail
   ;; linum
   ;; line-number-current-line
   ;; link
   ;; match
   (mode-line :background tp-2)
   (mode-line-inactive :background tp-2)
   ;; next-error
   ;; outline-1...8
   ;; persp
   ;; query-replace
   ;; rectangle-preview
   ;; (region :background tp-5)
   ;; scroll-bar
   ;; secondary-selection
   ;; shadow
   (show-paren-match :background violet)
   ;; show-paren-match-expression
   ;; show-paren-mismatch
   ;; success
   ;; tab-bar
   ;; tab-bar-tab
   ;; tab-bar-tab-inactive
   ;; tab-line
   ;; tool-bar
   ;; tooltip
   (trailing-whitespace :background cyan)
   (vertical-border :background tp-2 :foreground tp-2)
   ;; warning
   ;; which-func
   (window-divider :background green)
   )
;;;; --- extra variables ---------------------
  ;; ()
   )
