;;; completion/ivy/+bindings.el -*- lexical-binding: t; -*-

(doom-log "Setting up Completion bindings: %s" (current-time-string))

(map! :leader
      :desc "Dir: General Insert"          "y 5" (cmd! (find-file general-insert-location))
      )

;;-- remaps
(map! :after counsel
      [remap bookmark-jump]                 #'+jg-ivy-bookmark
      [remap apropos]                       #'counsel-apropos
      ;; [remap bookmark-jump]              #'counsel-bookmark
      [remap compile]                       #'+ivy/compile
      [remap projectile-compile-project]    #'+ivy/project-compile
      [remap projectile-find-file]          #'+ivy/projectile-find-file

      [remap describe-bindings]             #'counsel-descbinds
      [remap describe-face]                 #'counsel-faces
      [remap describe-function]             #'counsel-describe-function
      [remap describe-variable]             #'counsel-describe-variable
      [remap describe-symbol]               #'counsel-describe-symbol
      [remap evil-show-registers]           #'counsel-evil-registers
      [remap evil-show-marks]               #'counsel-mark-ring
      [remap execute-extended-command]      #'counsel-M-x
      [remap find-file]                     #'counsel-find-file
      [remap find-library]                  #'counsel-find-library
      [remap imenu]                         #'counsel-imenu
      [remap info-lookup-symbol]            #'counsel-info-lookup-symbol
      [remap load-theme]                    #'counsel-load-theme
      [remap locate]                        #'counsel-locate
      [remap org-goto]                      #'counsel-org-goto
      [remap org-set-tags-command]          #'counsel-org-tag
      [remap recentf-open-files]            #'counsel-recentf
      [remap set-variable]                  #'counsel-set-variable
      [remap swiper]                        #'counsel-grep-or-swiper
      [remap insert-char]                   #'counsel-unicode-char
      [remap yank-pop]                      #'counsel-yank-pop
      [remap projectile-find-dir]           #'counsel-projectile-find-dir
      [remap projectile-switch-to-buffer]   #'counsel-projectile-switch-to-buffer
      [remap projectile-grep]               #'counsel-projectile-grep
      [remap projectile-ag]                 #'counsel-projectile-ag
      [remap projectile-switch-project]     #'counsel-projectile-switch-project
      )
;;-- end remaps

;;-- ivy

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

;; ;; TODO Omni-completion
;; :i "C-l"    #'+company/whole-lines
;; :i "C-k"    #'+company/dict-or-keywords
;; :i "C-f"    #'company-files
;; :i "C-]"    #'company-etags
;; :i "s"      #'company-ispell
;; :i "C-s"    #'company-yasnippet
;; :i "C-o"    #'company-capf
;; :i "C-n"    #'+company/dabbrev
;; :i "C-p"    #'+company/dabbrev-code-previous
;;-- end company

;;-- lisp
(map! :map emacs-lisp-mode-map
      :localleader
      "i f" #'+jg-ivy-features
      )

;;-- end lisp

;;-- leader helms/ivys
(map! :leader
      :desc "SCRATCH"                      "6" (cmd! (+jg-popup-ivy-open "*scratch*"))
      :desc "Messages"                     "0" (cmd! (+jg-popup-ivy-open "*Messages*") (when current-prefix-arg (with-current-buffer "*Messages*" (+jg-text-clear-buffer))))
      )
;;-- end leader helms/ivys

(map! :map jg-binding-change-map
      :desc "Change Mode" "m"       #'+jg-ivy-change-major-mode
      :desc "Change Minor Mode" "M" #'+jg-ivy-change-minor-mode
      )
