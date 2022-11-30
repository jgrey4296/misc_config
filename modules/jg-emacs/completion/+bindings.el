;;; completion/ivy/+bindings.el -*- lexical-binding: t; -*-

(message "Setting up Completion bindings: %s" (current-time-string))

;;-- remap bookmarks
(map! :after counsel
      [remap bookmark-jump] #'+jg-completion-counsel-bookmark
      )
;;-- end remap bookmarks

;;-- ivy
;; (after! (hydra ivy)
;;   (defhydra+ hydra-ivy ()
;;     ("i" (evil-insert-state) :exit t)
;;     )
;;   )


(map! :map ivy-minibuffer-map
      :after ivy
      [remap doom/delete-backward-word] #'ivy-backward-kill-word
      :ni "TAB"                         #'ivy-alt-done
      :i "<backtab>"                    #'ivy-dispatching-call
      :n  ","                           #'+ivy/woccur
      :n  "."                           #'hydra-ivy/body

      :n "o" #'hydra-ivy/body
      :n "a" #'ivy-dispatching-done

      "C-c RET"                         #'+ivy/woccur
      "C-o"                             #'ivy-dispatching-done
      "C-h"                             #'ivy-backward-kill-word
      "M-o"                             #'hydra-ivy/body
      "C-M-o"                           #'ivy-dispatching-call
      "<down>"                          #'ivy-scroll-down-command
      "<up>"                            #'ivy-scroll-up-command

      :localleader
      :desc "Results as Buffer"        :n "b" #'+ivy/woccur
      )

(map! :map ivy-minibuffer-map
      :after ivy
      "C-SPC" #'ivy-call-and-recenter  ; preview file
      "C-l"   #'ivy-alt-done
      "C-v"   #'yank
      :n "|"  #'abort-recursive-edit
      )

(map! :map swiper-map
      :after swiper
      :localleader
      :desc "Results as Buffer"        :n "b" #'ivy-occur
      )

(map! :map ivy-occur-grep-mode-map
      :after ivy
      :desc "Do Ops" "g" jg-binding-operator-map
      )

(map! :map counsel-ag-map
      :after counsel
      "C-SPC"    #'ivy-call-and-recenter ; preview
      "C-l"      #'ivy-done
      [C-return] #'+ivy/git-grep-other-window-action
      )
;;-- end ivy

;;-- company
(map! :map company-active-map
      :after company
      ;; :i "C-@"    (cmds! (not (minibufferp)) #'company-complete-common)
       ;; :i "C-SPC"  (cmds! (not (minibufferp)) #'company-complete-common)

         "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
         "C-n"     #'company-select-next
         "C-p"     #'company-select-previous
         "C-j"     #'company-select-next
         "C-k"     #'company-select-previous
         "C-h"     #'company-show-doc-buffer
         "C-u"     #'company-previous-page
         "C-d"     #'company-next-page
         "C-s"     #'company-filter-candidates
         "C-S-s"   #'counsel-company
         "C-SPC"   #'company-complete-common
         "TAB"     (cmd! (company-cancel) (indent-for-tab-command))
         [tab]     (cmd! (company-cancel) (indent-for-tab-command))
         ;; "TAB"     #'company-complete-common-or-cycle
         ;; [tab]     #'company-complete-common-or-cycle
         [backtab] #'company-select-previous
         [f1]      nil
         )

(map! :map company-search-map  ; applies to `company-filter-map' too
      :after company
      "C-n"     #'company-select-next-or-abort
      "C-p"     #'company-select-previous-or-abort
      "C-j"     #'company-select-next-or-abort
      "C-k"     #'company-select-previous-or-abort
      "C-s"     #'company-filter-candidates
      [escape]  #'company-search-abort
      )

;;-- end company

;;-- helm

;; Movement
(map! :map helm-map
      :after helm
      :desc "quit" :n "|"  #'evil-force-normal-state

      :ni "C-u"             #'helm-delete-minibuffer-contents
      :i  "C-s"             #'helm-minibuffer-history
      "C-SPC"               #'helm-toggle-visible-mark

      [remap next-line]     #'helm-next-line
      [remap previous-line] #'helm-previous-line
      [left]                #'left-char
      [right]               #'right-char
      :i  "C-j"             #'helm-next-line
      :i  "C-k"             #'helm-previous-line

      :n "j"                #'helm-next-line
      :n "k"                #'helm-previous-line
      :n "J"                #'helm-next-page
      :n "K"                #'helm-previous-page
      :n ";"                #'helm-toggle-visible-mark
      :n "a"                #'helm-mark-all
      :n "u"                #'helm-unmark-all
      )

;; Actions
(map! :map helm-map
      :after helm

      :ni [tab] #'helm-select-action
      "C-z"     #'helm-execute-persistent-action
      :n "s"    #'helm-select-action
      :n "1" (cmd! (helm-select-nth-action 0))
      :n "2" (cmd! (helm-select-nth-action 1))
      :n "3" (cmd! (helm-select-nth-action 2))
      :n "4" (cmd! (helm-select-nth-action 3))
      :n "5" (cmd! (helm-select-nth-action 4))
      :n "6" (cmd! (helm-select-nth-action 5))
      :n "7" (cmd! (helm-select-nth-action 6))
      :n "8" (cmd! (helm-select-nth-action 7))
      :n "9" (cmd! (helm-select-nth-action 8))
      )

;; localleader
(map! :map helm-map
      :after helm
      :localleader
      :desc "Toggle Full Frame" "f" #'helm-toggle-full-frame
      )

;; Unbinding
(map! :map helm-map
      :after helm
      "C-S-f" nil
      "C-S-n" nil
      "C-S-p" nil
      "C-S-j" nil
      "C-S-k" nil

      "C-x 5" nil
      "C-x 6" nil
      "C-x 7" nil
      "C-x 8" nil
      "C-x 9" nil
      "C-x 0" nil

      "<f1>"  nil
      "<f2>"  nil
      "<f3>"  nil
      "<f4>"  nil
      "<f5>"  nil
      "<f6>"  nil
      "<f7>"  nil
      "<f8>"  nil
      "<f9>"  nil
      "<f10>" nil
      "<f11>" nil
      "<f12>" nil

      )

(after! helm
  (evil-make-intercept-map helm-map)
)

;;-- end helm

;;-- snippets
(map! :map snippet-mode-map
      :after yasnippet
      :localleader
      "1" (cmd! (browse-url "https://joaotavora.github.io/yasnippet/snippet-development.html"))
      )

;;-- end snippets

;;-- lisp
(map! :map emacs-lisp-mode-map
      :localleader
      "i f" #'+jg-completion-counsel-features
      )

;;-- end lisp

;;-- insert state
(map! :map jg-binding-insert-state-map
      "TAB" #'+jg-completion-complete-or-snippet
      )

;;-- end insert state

;;-- leader helms/ivys
(map! :leader
      :desc "SCRATCH"                      "6" (cmd! (+jg-completion-ivy-open-as-popup "*scratch*"))
      :desc "Messages"                     "0" (cmd! (+jg-completion-ivy-open-as-popup "*Messages*") (when current-prefix-arg (with-current-buffer "*Messages*" (+jg-text-clear-buffer))))
      :desc "Switch buffer"         ","     #'+jg-completion-switch-buffer
      :desc "Popup Buffer"          "<"     #'+jg-completion-popup-buffer
      :desc "Have you Played?"      "o 1"   #'+jg-completion-rps-have-you-playeds
      :desc "New snippet"           "y n"   #'+jg-completion-new-snippet
      :desc "File-Templates"        "y f"   (cmd! (find-file jg-completion-file-templates-dir))
      :desc "Workspace Counsel"     "w RET" #'+jg-completion-counsel-workspace
      )
;;-- end leader helms

;;-- shell
(map! :map shell-mode-map
      :i "TAB" #'+jg-completion-complete-or-snippet
      :n "TAB" #'completion-at-point
      )
;;-- end shell
