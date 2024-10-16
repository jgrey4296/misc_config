;;; completion/ivy/+bindings.el -*- lexical-binding: t; -*-

(dlog! "Setting up Completion bindings: %s" (current-time-string))

(map! :leader
      :desc "Dir: General Insert"          "y 5" (cmd! (find-file general-insert-location))
      :desc "Messages"                     "0" #'+jg-ivy-popup-messages
      )

;;-- remaps
(map! :after (counsel jg-global-bindings)
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

;;-- lisp
(map! :map emacs-lisp-mode-map
      :localleader
      "i f" #'+jg-ivy-features
      )

;;-- end lisp

(map! :map jg-binding-change-map
      :desc "Change Mode" "m"       #'+jg-ivy-change-major-mode
      :desc "Change Minor Mode" "M" #'+jg-ivy-change-minor-mode
      )
