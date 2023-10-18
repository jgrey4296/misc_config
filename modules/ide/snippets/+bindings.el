;;; +bindings.el -*- lexical-binding: t; -*-

;;-- browsing
(map! :leader
      (:prefix "y"
      :desc "Dir: Code"           "1" (cmd! (find-file jg-snippets-code-templates-dir))
      :desc "Dir: File"           "2" (cmd! (find-file jg-snippets-file-templates-dir))
      :desc "Dir: Projects"       "3" (cmd! (find-file jg-snippets-project-templates-dir))
      :desc "Dir: Lookup-regular" "4" (cmd! (find-file librarian-regular--location))
      :desc "Dir: Git Template"   "7" (cmd! (find-file (expand-file-name "templates/git" doom-user-dir)))
      :desc "Dir: Doom Snippet"   "8" (cmd! (find-file doom-snippets-dir))
      :desc "Dir: Yas  Snippet"   "9" (cmd! (find-file yasnippet-snippets-dir))
      :desc "Debug Snippet Dirs"  "?" #'+jg-snippets-debug-dirs
      )
      (:prefix "f"
       :desc "Debug File Template" "?" #'+file-templates/debug
       )
)
;;-- end browsing

;;-- snippets

(define-key yas-minor-mode-map [menu-bar] nil)
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
      :desc "Random Var"            "v" #'+snippets-random-var
       (:prefix ("l" . "Lorem Ipsum")
        :desc "Sentence"         "s" #'lorem-ipsum-insert-sentences
        :desc "Paragraph"        "p" #'lorem-ipsum-insert-paragraphs
        :desc "List"             "l" #'lorem-ipsum-insert-list
        :desc "Academic"         "a" #'academic-phrases
        :desc "Academic Section" "A" #'academic-phrases-by-section
        :desc "License"               "L" #'license-templates-insert
        )

      )
;;-- end snippets

;;-- insert state
(map! :map jg-binding-insert-state-map
      ;; "TAB" #'+jg-snippets-complete-or-snippet
      "TAB" #'company-complete
      ;; "TAB" #'indent-for-tab-command
      )

;;-- end insert state

;;-- shell
(map! :map shell-mode-map
      :i "TAB" #'+jg-snippets-complete-or-snippet
      :n "TAB" #'completion-at-point
      )
;;-- end shell

(map! :leader
      :prefix "i"
      :desc "Breakpoint" "b" #'+jg-snippets-insert-debug
      )

(map! :map jg-binding-normal-state-map
      :desc "breakpoint"          "I b"   #'+jg-snippets-insert-debug
      )

;; TODO abbrev and dabbrev
