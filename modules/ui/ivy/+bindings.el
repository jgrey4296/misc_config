;;; completion/ivy/+bindings.el -*- lexical-binding: t; -*-

(dlog! "Setting up Completion bindings: %s" (current-time-string))
(with-state! 'normal #'ivy-resume)
(with-state! 'normal #'counsel-minibuffer-history)
(with-state! 'insert #'counsel-imenu)
(with-state! 'normal #'counsel-command-history)
(with-state! 'normal #'counsel-yank-pop)
(with-state! 'normal #'counsel-evil-registers)
(with-state! 'normal #'counsel-evil-marks)
(with-state! 'normal #'counsel-list-processes)


(map! :leader
      :desc "Dir: General Insert"          "y 5" (cmd! (find-file general-insert-location))
      :desc "Messages"                     "0"    #'+jg-ivy-popup-messages
      )

(map! :map swiper-map
      :after swiper
      :localleader
      :desc "Results as Buffer"        :n "b" #'ivy-occur
      )

;;-- ivy
(map! :map jg-binding-jump-map
      :desc "Ivy resume"            "`"   #'ivy-resume--with-state-normal
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

;;-- counsel
(map! :leader
      :desc "Org Capture"                  "X"     #'counsel-org-capture
      :desc "From Minibuffer history"      "i m"   #'counsel-minibuffer-history--with-state-normal
      :desc "Killed Text"                  "r y"   #'counsel-yank-pop--with-state-normal
      )

(map! :map jg-binding-normal-state-map
      :prefix "I"
       :desc "From evil register"       "0"          #'counsel-evil-registers--with-state-normal
       "m" #'counsel-minibuffer-history--with-state-normal
       "y" #'counsel-yank-pop--with-state-normal
       :desc "General Insert"           "|"          #'general-insert-call

      )

(map! :map jg-binding-insert-state-map
      "C-x c"   #'counsel-unicode-char
      "C-c c"   #'counsel-unicode-char
      "C-c SPC" #'counsel-unicode-char
      )

(map! :map jg-binding-jump-map
      :desc "Search current directory"    "/ d"  #'+ivy/project-search-from-cwd
      )

(map! :map jg-help-map
      :desc "Command History"              "L"  #'counsel-command-history--with-state-normal
      )

(map!
      [remap yank-pop]                      #'counsel-yank-pop--with-state-normal
      [remap evil-show-registers]           #'counsel-evil-registers--with-state-normal
      [remap evil-show-marks]               #'counsel-evil-marks--with-state-normal
      [remap list-processes]                #'counsel-list-processes--with-state-normal
      [remap isearch-forward]               #'swiper
      [remap isearch-backward]              #'swiper
      [remap swiper]                        #'counsel-grep-or-swip
      [remap bookmark-jump]                 #'+jg-ivy-bookmark

      [remap compile]                       #'+ivy/compile
      [remap projectile-compile-project]    #'+ivy/project-compile
      [remap projectile-find-file]          #'+ivy/projectile-find-file

      [remap apropos]                       #'counsel-apropos
      [remap describe-bindings]             #'counsel-descbinds
      [remap describe-face]                 #'counsel-faces
      [remap describe-function]             #'counsel-describe-function
      [remap describe-variable]             #'counsel-describe-variable
      [remap describe-symbol]               #'counsel-describe-symbol
      [remap execute-extended-command]      #'counsel-M-x
      [remap find-file]                     #'counsel-find-file
      [remap find-library]                  #'counsel-find-library
      [remap imenu]                         #'counsel-imenu--with-state-insert
      [remap info-lookup-symbol]            #'counsel-info-lookup-symbol
      [remap load-theme]                    #'counsel-load-theme
      [remap locate]                        #'counsel-locate
      [remap org-goto]                      #'counsel-org-goto
      [remap org-set-tags-command]          #'counsel-org-tag
      [remap recentf-open-files]            #'counsel-recentf
      [remap set-variable]                  #'counsel-set-variable
      [remap insert-char]                   #'counsel-unicode-char
      [remap projectile-find-dir]           #'counsel-projectile-find-dir
      [remap projectile-switch-to-buffer]   #'counsel-projectile-switch-to-buffer
      [remap projectile-grep]               #'counsel-projectile-grep
      [remap projectile-ag]                 #'counsel-projectile-ag
      [remap projectile-switch-project]     #'counsel-projectile-switch-project
      )
;;-- end counsel

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
