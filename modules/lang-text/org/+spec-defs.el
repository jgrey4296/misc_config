;;; +spec-defs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(defun +jg-org-capture-get-template (name &optional mode)
  "Expands and returns a yas snippet for org-capture "
  (with-temp-buffer
    (yas-minor-mode)
    (yas-expand-snippet (yas-lookup-snippet name (or mode 'org-mode)) (point))
    (buffer-string)
    )
  )

(spec-handling-new! org-startup
                    "add org startup options to handle in hooks."
                    :target org-startup-options
                    :loop 'append
                    :struct '(key . (list (text var val)))
                    val
                    )

(spec-handling-new! org-capture
                    "Register org-capture templates, with file function finding and snippet expansion"
                    :target org-capture-templates
                    :loop 'append
                    :struct '(key (plist :key :name :file :headline :func :text :snippet :props))
                    (cl-loop for data in val
                             with target
                             with text
                             with template
                             do
                             ;; Build template:
                             (setq target (cond ((plist-get data :file)
                                                 (list 'file+headline
                                                       (plist-get data :file)
                                                       (plist-get data :headline))
                                                 )
                                                ((plist-get data :func)
                                                 (plist-get data :func)
                                                 )
                                                (t (warn "Bad template defined" data))
                                                )
                                   text  (cond ((plist-get data :text)
                                                (plist-get data :text))
                                               ((plist-get data :snippet)
                                                (+jg-org-capture-get-template (plist-get data :snippet)))
                                               )
                                   template  (append (list
                                                      (plist-get data :key)
                                                      (plist-get data :name)
                                                      'entry
                                                      target
                                                      text
                                                      )
                                                     (plist-get data :props)
                                                     )
                                   )
                             collect template
                             )
                    )

(spec-handling-new! babel
                    "maps a babel language name to the library name of its handlers"
                    :target +org-babel-mode-alist
                    :struct '(key . (plist :name :lib :func :mode))
                    :loop 'append
                    (cl-loop for data in val
                             collect (cons (cond ((symbolp (plist-get data :name))
                                                  (plist-get data :name))
                                                 ((stringp (plist-get data :name))
                                                  (intern (plist-get data :name)))
                                                 )
                                           data
                                           ))
                    )

(spec-handling-new! org-src
                    "register languages for org babel"
                    :target org-src-lang-modes
                    :struct '(key . (str . mode))
                    :loop 'append
                    val
                    )

;; default org-startup options
(spec-handling-add! org-startup
                    '(fold
                      ("fold"                org-startup-folded fold)
                      ("overview"            org-startup-folded overview)
                      ("nofold"              org-startup-folded nofold)
                      ("showall"             org-startup-folded showall)
                      ("show2levels"         org-startup-folded show2levels)
                      ("show3levels"         org-startup-folded show3levels)
                      ("show4levels"         org-startup-folded show4levels)
                      ("show5levels"         org-startup-folded show5levels)
                      ("showeverything"      org-startup-folded showeverything)
                      ("content"             org-startup-folded content)
                      ("hideblocks" org-hide-block-startup t)
                      ("nohideblocks" org-hide-block-startup nil)
                      ("hidedrawers" org-hide-drawer-startup t)
                      ("nohidedrawers" org-hide-drawer-startup nil)
                      )
                    '(indent
                      ("indent"              org-startup-indented t)
                      ("noindent"            org-startup-indented nil)
                      )
                    '(numerate
                      ("num"                 org-startup-numerated t)
                      ("nonum"               org-startup-numerated nil)
                      )
                    '(stars
                      ("hidestars"           org-hide-leading-stars t)
                      ("showstars"           org-hide-leading-stars nil)
                      )
                    '(outline
                      ("odd"                 org-odd-levels-only t)
                      ("oddeven"             org-odd-levels-only nil)
                      )
                    '(align
                      ("align"               org-startup-align-all-tables t)
                      ("noalign"             org-startup-align-all-tables nil)
                      )
                    '(images
                      ("inlineimages"        org-startup-with-inline-images t)
                      ("noinlineimages"      org-startup-with-inline-images nil)
                      )
                    '(latex
                      ("latexpreview"        org-startup-with-latex-preview t)
                      ("nolatexpreview"      org-startup-with-latex-preview nil)
                      )
                    '(log
                      ("logdone"             org-log-done time)
                      ("lognotedone"         org-log-done note)
                      ("nologdone"           org-log-done nil)
                      ("lognoteclock-out"    org-log-note-clock-out t)
                      ("nolognoteclock-out"  org-log-note-clock-out nil)
                      ("logrepeat"           org-log-repeat state)
                      ("lognoterepeat"       org-log-repeat note)
                      ("logdrawer"           org-log-into-drawer t)
                      ("nologdrawer"         org-log-into-drawer nil)
                      ("logstatesreversed"   org-log-states-order-reversed t)
                      ("nologstatesreversed" org-log-states-order-reversed nil)
                      ("nologrepeat"         org-log-repeat nil)
                      ("logreschedule"       org-log-reschedule time)
                      ("lognotereschedule"   org-log-reschedule note)
                      ("nologreschedule"     org-log-reschedule nil)
                      ("logredeadline"       org-log-redeadline time)
                      ("lognoteredeadline"   org-log-redeadline note)
                      ("nologredeadline"     org-log-redeadline nil)
                      ("logrefile"           org-log-refile time)
                      ("lognoterefile"       org-log-refile note)
                      ("nologrefile"         org-log-refile nil)
                      )
                     '(footnote
                      ("fninline"            org-footnote-define-inline t)
                      ("nofninline"          org-footnote-define-inline nil)
                      ("fnlocal"             org-footnote-section nil)
                      ("fnauto"              org-footnote-auto-label t)
                      ("fnprompt"            org-footnote-auto-label nil)
                      ("fnconfirm"           org-footnote-auto-label confirm)
                      ("fnplain"             org-footnote-auto-label plain)
                      ("fnadjust"            org-footnote-auto-adjust t)
                      ("nofnadjust"          org-footnote-auto-adjust nil)
                      ("fnanon"              org-footnote-auto-label anonymous)
                      )
                    '(misc
                      ("shrink"              org-startup-shrink-all-tables t)
                      ("descriptivelinks"    org-link-descriptive t)
                      ("literallinks"        org-link-descriptive nil)
                      ("customtime"          org-display-custom-times t)
                      ("noptag"              org-tag-persistent-alist nil)
                      ("beamer"              org-startup-with-beamer-mode t)
                      ("entitiespretty"      org-pretty-entities t)
                      ("entitiesplain"       org-pretty-entities nil)
                      ("constcgs"            constants-unit-system cgs)
                      ("constSI"             constants-unit-system SI)
                      )
                    )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    June 04, 2024
;; Modified:   June 04, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; +spec-defs.el ends here
