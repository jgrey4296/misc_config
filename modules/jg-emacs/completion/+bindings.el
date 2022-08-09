;;; completion/ivy/+bindings.el -*- lexical-binding: t; -*-

(message "Setting up Completion bindings: %s" (current-time-string))

(map! :after (counsel jg-leader-bindings-loaded)
      [remap bookmark-jump] #'+jg-completion-counsel-bookmark
      )

(map! :after ivy
      :map ivy-minibuffer-map
      [remap doom/delete-backward-word] #'ivy-backward-kill-word
      :in "TAB"                         #'ivy-alt-done
      :i "<backtab>"                    #'ivy-dispatching-call
      :n  ","                           #'+ivy/occur
      :n  "."                           #'hydra-ivy/body

      "C-c RET"                         #'+ivy/woccur
      "C-o"                             #'ivy-dispatching-done
      "C-h"                             #'ivy-backward-kill-word
      "M-o"                             #'hydra-ivy/body
      "C-M-o"                           #'ivy-dispatching-call
      "<down>"                          #'ivy-scroll-down-command
      "<up>"                            #'ivy-scroll-up-command

      :localleader
      :desc "Results as Buffer"        :n "b" #'ivy-occur
      )

(map! :after (ivy jg-evil-bindings)
      :map ivy-occur-grep-mode-map
      :desc "Do Ops" "g" jg-binding-operator-map
      )

(map! :after (company jg-leader-bindings-loaded)
      :when (featurep! :completion company)
      ;; :i "C-@"    (cmds! (not (minibufferp)) #'company-complete-common)
       ;; :i "C-SPC"  (cmds! (not (minibufferp)) #'company-complete-common)
       :map company-active-map
         "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
         "C-n"     #'company-select-next
         "C-p"     #'company-select-previous
         "C-j"     #'company-select-next
         "C-k"     #'company-select-previous
         "C-h"     #'company-show-doc-buffer
         "C-u"     #'company-previous-page
         "C-d"     #'company-next-page
         "C-s"     #'company-filter-candidates
         "C-S-s"   (cond ((featurep! :completion helm) #'helm-company)
                         ((featurep! :completion ivy)  #'counsel-company))
         "C-SPC"   #'company-complete-common
         "TAB"     #'company-complete-common-or-cycle
         [tab]     #'company-complete-common-or-cycle
         [backtab] #'company-select-previous
         [f1]      nil
         )

(map! :after (company jg-leader-bindings-loaded)
      :map company-search-map  ; applies to `company-filter-map' too
      "C-n"     #'company-select-next-or-abort
      "C-p"     #'company-select-previous-or-abort
      "C-j"     #'company-select-next-or-abort
      "C-k"     #'company-select-previous-or-abort
      "C-s"     #'company-filter-candidates
      [escape]  #'company-search-abort
      )

(map! :when (featurep! :completion ivy)
      :after (ivy jg-leader-bindings-loaded)
      :map ivy-minibuffer-map
      "C-SPC" #'ivy-call-and-recenter  ; preview file
      "C-l"   #'ivy-alt-done
      "C-v"   #'yank
      )

(map! :after (counsel jg-leader-bindings-loaded)
      :map counsel-ag-map
      "C-SPC"    #'ivy-call-and-recenter ; preview
      "C-l"      #'ivy-done
      [C-return] #'+ivy/git-grep-other-window-action
      )

(map! :after helm
      :map helm-map
      [remap next-line]     #'helm-next-line
      [remap previous-line] #'helm-previous-line
      [left]                #'left-char
      [right]               #'right-char
      "C-S-f"               #'helm-previous-page
      "C-S-n"               #'helm-next-source
      "C-S-p"               #'helm-previous-source
      "C-j"                 #'helm-next-line
      "C-k"                 #'helm-previous-line
      "C-S-j"               #'helm-next-source
      "C-S-k"               #'helm-previous-source
      "C-u"                 #'helm-delete-minibuffer-contents
      "C-s"                 #'helm-minibuffer-history
      ;; Swap TAB and C-z
      :ni "TAB"             #'helm-select-action
      "<tab>"               #'helm-select-action
      "TAB"                 #'helm-select-action
      [tab]                 #'helm-select-action
      "C-z"                 #'helm-execute-persistent-action
      "C-SPC"               #'helm-toggle-visible-mark
      "C-@"                 #'helm-toggle-visible-mark

      :n "j"                #'helm-next-line
      :n "k"                #'helm-previous-line
      :n "J"                #'helm-next-page
      :n "K"                #'helm-previous-page
      :n ";"                #'helm-toggle-visible-mark
      :n "a"                #'helm-mark-all
      :n "u"                #'helm-unmark-all

      "C-x 5" nil
      "C-x 6" nil
      "C-x 7" nil
      "C-x 8" nil
      "C-x 9" nil
      "C-x 0" nil
      )

(map! :after helm
      :map helm-map
      :localleader
      "f" #'helm-toggle-full-frame
      )
(after! helm
  (evil-make-intercept-map helm-map)
)


(map! :after yasnippet
      :map snippet-mode-map
      :localleader
      "1" (cmd! (+jg-misc-browse-url "https://joaotavora.github.io/yasnippet/snippet-development.html"))
      )

(map! :map emacs-lisp-mode-map
      :localleader
      "i f" #'+jg-completion-counsel-features
      )

(map! :after jg-evil-bindings
      :map jg-binding-insert-state-map
      "TAB" #'+jg-completion-complete-or-snippet
      )

(map! :after jg-leader-bindings-loaded
      :leader
      :desc "SCRATCH"                      "6" (cmd! (+jg-completion-ivy-open-as-popup "*scratch*"))
      :desc "Messages"                     "0" (cmd! (+jg-completion-ivy-open-as-popup "*Messages*") (if current-prefix-arg (+jg-text-clear-buffer)))
      :desc "Switch buffer"         ","     #'+jg-completion-switch-buffer
      :desc "Popup Buffer"          "<"     #'+jg-completion-popup-buffer
      :desc "Have you Played?"      "o h h" #'+jg-completion-rps-have-you-playeds
      :desc "New snippet"           "y n"   #'+jg-completion-new-snippet
      :desc "Workspace Counsel"     "W RET" #'+jg-completion-counsel-workspace

      )
