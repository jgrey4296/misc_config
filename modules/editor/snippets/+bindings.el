;;; +bindings.el -*- lexical-binding: t; -*-

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
      )

(map! :leader
      :prefix "y"
      :desc "Temp Template"  :v "t" (cmd! (if (eq evil-state 'visual)
                                              (setq aya-current (buffer-substring evil-visual-beginning evil-visual-end))
                                            (aya-expand)))
      :desc "New snippet"           "n" #'+jg-snippets-new-snippet
      :desc "Expand Snippet"        "y" #'yas-expand-from-trigger-key
      :desc "Edit Snippet"          "e" #'+snippets/edit
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