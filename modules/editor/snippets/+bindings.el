;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map yas-keymap
       ;; "C-e"         #'+snippets/goto-end-of-field
       ;; "C-a"         #'+snippets/goto-start-of-field
       [M-right]     #'+snippets/goto-end-of-field
       [M-left]      #'+snippets/goto-start-of-field
       [M-backspace] #'+snippets/delete-to-start-of-field
       [backspace]   #'+snippets/delete-backward-char
       [delete]      #'+snippets/delete-forward-char-or-field
       )
(map! :map yas-minor-mode-map
       [remap yas-new-snippet]        #'+snippets/new
       [remap yas-visit-snippet-file] #'+snippets/edit
       )

;;-- browsing
(map! :leader
      :prefix "y"
      :desc "Dir: Code"           "1" (cmd! (find-file jg-snippets-code-templates-dir))
      :desc "Dir: File"           "2" (cmd! (find-file jg-snippets-file-templates-dir))
      :desc "Dir: Projects"       "3" (cmd! (find-file jg-snippets-project-templates-dir))
      :desc "Dir: Git Template"   "4" (cmd! (find-file (expand-file-name "templates/git" doom-user-dir)))
      :desc "Dir: Doom Snippet"   "5" (cmd! (find-file doom-snippets-dir))
      :desc "Dir: Yas  Snippet"   "6" (cmd! (find-file yasnippet-snippets-dir))
      :desc "Debug File Template" "?" #'+file-templates/debug
)
;;-- end browsing

;;-- snippets
(map! :map snippet-mode-map
      :after yasnippet
      "C-c C-k" #'+snippet--abort
      :localleader
      "1" (cmd! (browse-url "https://joaotavora.github.io/yasnippet/snippet-development.html"))
      )

(map! :leader
      :prefix "y"
      :desc "Temp Template"  :v "t" (cmd! (if (eq evil-state 'visual)
                                              (setq aya-current (buffer-substring evil-visual-beginning evil-visual-end))
                                            (aya-expand)))
      :desc "New snippet"           "n" #'+jg-snippets-new-snippet
      :desc "Expand Snippet"        "y" #'yas-expand-from-trigger-key
      :desc "Edit Snippet"          "e" #'yas-visit-snippet-file
      :desc "Insert snippet"        "i" #'yas-insert-snippet
      :desc "Find global snippet"   "/" #'yas-visit-snippet-file
      :desc "Reload snippets"       "r" #'yas-reload-all
      )
;;-- end snippets

;;-- insert state
(map! :map jg-binding-insert-state-map
      "TAB" #'+jg-snippets-complete-or-snippet
      )

;;-- end insert state

;;-- shell
(map! :map shell-mode-map
      :i "TAB" #'+jg-snippets-complete-or-snippet
      :n "TAB" #'completion-at-point
      )
;;-- end shell
