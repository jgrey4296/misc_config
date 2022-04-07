;;; completion/ivy/+bindings.el -*- lexical-binding: t; -*-

(message "Setting up Completion bindings: %s" (current-time-string))

(map! :after counsel
      [remap bookmark-jump] #'+jg-counsel-bookmark)

(map! :map ivy-minibuffer-map
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

(map! :map ivy-occur-grep-mode-map
      :desc "Do Ops" "g" jg-binding-operator-map
      )

  ;;; :completion
(map! (:when (featurep! :completion company)
       :i "C-@"    (cmds! (not (minibufferp)) #'company-complete-common)
       :i "C-SPC"  (cmds! (not (minibufferp)) #'company-complete-common)
       (:after company
        (:map company-active-map
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
         [f1]      nil)
        (:map company-search-map  ; applies to `company-filter-map' too
         "C-n"     #'company-select-next-or-abort
         "C-p"     #'company-select-previous-or-abort
         "C-j"     #'company-select-next-or-abort
         "C-k"     #'company-select-previous-or-abort
         "C-s"     #'company-filter-candidates
         [escape]  #'company-search-abort)))

      (:when (featurep! :completion ivy)
       (:after ivy
        :map ivy-minibuffer-map
        "C-SPC" #'ivy-call-and-recenter  ; preview file
        "C-l"   #'ivy-alt-done
        "C-v"   #'yank)
       (:after counsel
        :map counsel-ag-map
        "C-SPC"    #'ivy-call-and-recenter ; preview
        "C-l"      #'ivy-done
        [C-return] #'+ivy/git-grep-other-window-action))
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
      (:when (featurep! :editor evil +everywhere)
       "C-j"                #'helm-next-line
       "C-k"                #'helm-previous-line
       "C-S-j"              #'helm-next-source
       "C-S-k"              #'helm-previous-source)
      "C-u"                 #'helm-delete-minibuffer-contents
      "C-s"                 #'helm-minibuffer-history
      ;; Swap TAB and C-z
      :ni "TAB"             #'helm-select-action
      "<tab>"               #'helm-select-action
      "TAB"                 #'helm-select-action
      [tab]                 #'helm-select-action
      "C-z"                 #'helm-execute-persistent-action
        )
