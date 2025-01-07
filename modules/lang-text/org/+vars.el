;;; lang/jg-org/+vars.el -*- lexical-binding: t; -*-

(defvar jg-org-mode-map     (make-sparse-keymap))

(defvar jg-org-capture-map  (make-sparse-keymap))

(defvar jg-org-src-mode-map (make-sparse-keymap))

(after! org
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))
  (add-to-list 'org-file-apps '("\\.epub\\'" . "ebook-viewer %s"))
  )

;;-- org core
;; locations

;; ORG SETUP

;; Save target buffer after archiving a node.
(setq org-archive-subtree-save-file-p t)

(after! ol
  (push '("Scholar" . "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=%s") org-link-abbrev-alist)
  )

(setq org-structure-template-alist '(("a" . "export ascii")
                                     ("c" . "center")
                                     ("C" . "comment")
                                     ("e" . "example")
                                     ("E" . "export")
                                     ("h" . "export html")
                                     ("l" . "export latex")
                                     ("q" . "quote")
                                     ("s" . "src")
                                     ("v" . "verse")
                                     )
      )

;; (textobjects insert navigation additional shift todo heading calendar)
(setq evil-org-key-theme '(textobjects insert shift todo)
      org-cycle-separator-lines 3
      )
;;-- end org core

;;-- locations

;;-- end locations

;;-- agenda
(setq org-agenda-include-diary t
      org-agenda-inhibit-startup t
      org-agenda-deadline-faces '((1.001 . error)
                                  (1.0 . org-warning)
                                  (0.5 . org-upcoming-deadline)
                                  (0.0 . org-upcoming-distant-deadline))
      org-agenda-window-setup 'other-window
      org-agenda-skip-unavailable-files t
      org-agenda-span 10
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-3d"
      )

;;-- end agenda

;;-- journal
(setq org-journal-find-file #'find-file
      )

;;-- end journal

;;-- babel
(setq org-src-preserve-indentation t  ; use native major-mode indentation
      org-src-tab-acts-natively t     ; we do this ourselves
      ;; Show src buffer in popup, and don't monopolize the frame
      org-src-window-setup 'other-window
      )

;; Don't process babel results asynchronously when exporting org, as they
;; won't likely complete in time, and will instead output an ob-async hash
;; instead of the wanted evaluation results.
(after! ob
  (add-to-list 'org-babel-default-lob-header-args '(:sync))
  )

;;-- end babel

;;-- todo config
(let ((project-steps '(sequence
                       "TODO(j!)"      ; A job that needs doing
                       "IDEA(i)"       ; An unconfirmed job
                       "LOOP(l)"       ; A recurring job
                       "ACTIVE(a)"     ; A job that is in progress
                       "BLOCKED(b)"    ; Something external is holding up this task
                       "QUEUED(q)"     ; This task is paused/on hold because of me
                       "NEXT(n)"       ;
                       "|"
                       "DONE(d!)"  ; Task successfully completed
                       "DEAD(k@!)" ; Task was cancelled, aborted or is no longer applicable
                       ))
      (task-status '(sequence
                     "[∅](t)"   ; A task that needs doing
                     "[⇒](s)"   ; Task is in progress
                     "[∃](w)"   ; Task is being held up or paused
                     "|"
                     "[⟙](c!)"    ; Task was completed
                     "[⟘](f@!)"   ; Task was failed
                     ))
      (eval-status '(sequence
                     "TRIAGE(?)"
                     "|"
                     "OKAY(o)"
                     "YES(y)"
                     "NO(N)"
                     "MAYBE(m)"
                     ))
      )
  (setq jg-org-todo-keywords
        (list project-steps task-status eval-status)
        )
  )

(defvar jg-org-todo-faces
  '(("IDEA"    . +org-todo-project)

    ("[⇒]"     . org-list-dt)
    ("ACTIVE"  . org-list-dt)

    ("[∃]"     . org-todo)
    ("BLOCKED" . org-warning)
    ("QUEUED"  . org-date)

    ("NO"      . +org-todo-cancel)
    ("DEAD"    . org-priority)
    ("[⟘]"     . org-priority)
    )
  "Faces for my keywords"
  )

(setq org-use-fast-todo-selection 'auto)

;;-- end todo config

;;-- refiling

(defvar jg-org-refile-targets '((nil :maxlevel . 3)
                                (org-agenda-files :maxlevel . 3))
  )

(setq-default org-refile-use-outline-path 'file
              org-outline-path-complete-in-steps nil ;; needss to be nil for ivy
              )

;;-- end refiling

;;-- pomodoro
;; set pomodoro log variable

(defcustom jg-org-pomodoro-log-file (expand-file-name "setup_files/pomodoro_log.org" doom-user-dir) "The Location of the Pomodoro Log File")

(defcustom jg-org-pomodoro-buffer-name "*Pomodoro Log*"
  "The name of the Pomodoro Log Buffer to record what I did in")

(defcustom jg-org-pomodoro-log-message ";; What did the last Pomodoro session accomplish? C-c to finish\n"
  "The message to add to the log buffer to spur comments")
(after! org-pomodoro
  ;; add a startup hook for pomodoro to tweet the end time
  (add-hook 'org-pomodoro-started-hook '+jg-org-pomodoro-start-hook)
  ;; add a finished hook to ask for a recap of what was done,
  ;; and store it in a pomodoro log file
  (add-hook 'org-pomodoro-finished-hook '+jg-org-pomodoro-end-hook)
  )
;;-- end pomodoro

;;-- visual
(after! org-superstar
  (setq org-hide-leading-stars t)
  )
;;-- end visual

;;-- completion
(after! helm-org
  ;; TODO add a keybind for helm-org-rifle
  (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))
  )
;;-- end completion

;;-- spelling
;; Don't spellcheck org blocks
(pushnew! ispell-skip-region-alist
          '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
          '("#\\+BEGIN_SRC" . "#\\+END_SRC")
          '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

;;-- end spelling

;;-- specs
(speckler-setq! org ()
  org-link-from-user-regexp "\\<John Grey\\>"
  org-fast-tag-selection-single-key nil
  org-group-tags nil
  org-use-fast-tag-selection t
  org-tags-column 50
  org-startup-indented nil
  org-indent--deepest-level 10
  org-element-use-cache t
  org-insert-heading-respect-content t
  org-list-allow-alphabetical t
  )
(speckler-setq! org-locs ()
  :priority 20
  org-archive-location   (format           "%s::%s" (expand-file-name "archive/archive.org" org-directory) "* Main Archive")
  org-id-locations-file  (expand-file-name "org/.orgids" user-cache-dir)
  org-default-notes-file (expand-file-name "notes/misc.org"   org-directory)
  org-attach-id-dir                        "attachments"
  org-journal-dir                   (expand-file-name "journal/"    org-directory)
  org-journal-cache-file            (expand-file-name "org/journal" user-cache-dir)
  org-persist-directory             (expand-file-name "org/persist/" user-cache-dir)
  org-publish-timestamp-directory   (expand-file-name  "org/timestamps/" user-cache-dir)
  org-preview-latex-image-directory (expand-file-name  "org/latex/" user-cache-dir)
  org-clock-persist-file (expand-file-name "org-clock-save.el" user-cache-dir)

  +org-capture-todo-file                   "agenda/triage_todos.org"
  +org-capture-changelog-file              "changelog.org"
  +org-capture-notes-file                  "notes/misc.org"
  +org-capture-journal-file                "journal/journal.org"
  +org-capture-projects-file               "projects/projects.org"
  )
(speckler-add! org-capture ()
  `(todo
    (:key       "t" :name      "Personal todo"
     :file      +org-capture-todo-file :headline  "Triage"
     :snippet "personal-todo"
     :props (:prepend t :empty-lines 1 :kill-buffer t)
     )
    (:key  "g" :name "Global Todo"
     :file +org-capture-todo-file :headline  "Triage"
     :snippet "global_todo"
     :props (:prepend t :empty-lines 2 :kill-buffer t)
     )
    (:key "q" :name "Quick Todo"
     :file +org-capture-todo-file :headline "Triage"
     :text "** TRIAGE Quick note\n%a\n%T\n\n"
     :props (:immediate-finish t :kill-buffer t)
     )
    )
  `(notes
    (:key "n" :name "Note"
     :file +org-capture-notes-file :headline "Triage"
     :text "** %u %?\n%i\n%a"
     :props (:kill-buffer t)
     )
    )
  `(project
    (:key "p" :name "Project Todo"
     :func #'+jg-org-capture-project-todo  :headline  "Triage"
     :snippet "project_todo"
     :props (:kill-buffer t)
     )
    )
  )
(speckler-add! file-templates ()
  '(org
    ("two_pager\\.org$"     :trigger "__pacheco_vega_two_pager" :mode org-mode)
    ("lit_review\\.org$"    :trigger "__lit_review"             :mode org-mode)
    ("inst_pipeline\\.org$" :trigger "__institution_pipeline"   :mode org-mode)
    ("design_doc\\.org$"    :trigger "__designDocNotes"         :mode org-mode)
    ("project\\.org$"       :trigger "__project"                :mode org-mode)
    ("invoice\\.org$"       :trigger "__invoice"                :mode org-mode)
    ("contact\\.org$"       :trigger "__contact"                :mode org-mode)
    ("README\\.org$"        :trigger "__doom-readme"            :mode org-mode :when +file-templates-in-emacs-dirs-p )
    (org-journal-mode :ignore t)
    (org-mode :trigger "__")
    )
  )
(speckler-add! fold ()
  '(org
    :modes (org-mode doom-docs-org-mode)
    :triggers (:open-all   +org/open-all-folds
               :close-all  +org/close-all-folds
               :toggle     org-cycle
               :open       nil
               :open-rec   org-fold-show-subtree
               :close      nil
               )
    )
  )
(speckler-add! lookup-handler ()
  `(org-mode
    :definition ,#'+org-lookup-definition-handler
    :references ,#'+org-lookup-references-handler
    :documentation ,#'+org-lookup-documentation-handler
    )
  )
(speckler-add! whitespace-cleanup ()
  `(org-mode
    ,#'delete-trailing-whitespace
    ;; ,#'+jg-org-clean-heading-spaces
    ,#'+jg-text-cleanup-whitespace
    )
  )
(speckler-add! popup ()
  '(org-mode
    ("^\\*Org Links" :slot -1 :vslot -1 :size 2 :ttl 0)
    ("^ ?\\*\\(?:Agenda Com\\|Calendar\\|Org Export Dispatcher\\)" :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :ttl 0)
    ("^\\*Agenda Files\\*\\'" :select t :quit t :side right)
    ("^\\*Org Agenda\\*\\'"   :select t :quit t :side right :priority 100)
    ("^\\*Org \\(?:Select\\|Attach\\)" :slot -1 :vslot -2 :ttl 0 :size 0.25)
    ("^\\*Org Src"        :size 0.42  :quit nil :select t :autosave t :modeline t :ttl nil)
    ("^\\*Org-Babel")
    ("^\\*Capture\\*$\\|CAPTURE-.*$" :size 0.42 :quit nil :select t :autosave ignore)
    )
  )
(speckler-add! auto-modes ()
  '(org
    ("\\.org\\'" . org-mode)
    )
  )
(speckler-add! eval ()
  '(org-mode :fn +org-eval-handler)
  )
(speckler-add! org-startup ()
  '(plus
    ("agenda"    jg-org-startup-agenda         t)
    ("reference" jg-org-startup-reference      t)
    ("packages"  jg-org-startup-package        t)
    )
  )
(speckler-add! babel ()
  '(default
    (:name D          :lib ob-C)
    (:name amm        :lib ob-ammonite)
    (:name awk        :lib ob-awk)
    (:name calc       :lib ob-calc)
    (:name comint     :lib ob-comint)
    (:name cpp        :lib ob-C)
    (:name ditaa      :lib ob-ditaa)
    (:name dot        :lib ob-dot)
    (:name eval       :lib ob-eval)
    (:name exp        :lib ob-exp)
    (:name forth      :lib ob-forth)
    (:name fortran    :lib ob-fortran)
    (:name gnuplot    :lib ob-gnuplot)
    (:name julia      :lib ob-julia)
    (:name lob        :lib ob-lob)
    (:name makefile   :lib ob-makefile)
    (:name matlab     :lib ob-matlab)
    (:name matlab     :lib ob-octave)
    (:name maxima     :lib ob-maxima)
    (:name ocaml      :lib ob-ocaml)
    (:name octave     :lib ob-octave)
    (:name org        :lib ob-org)
    (:name perl       :lib ob-perl)
    (:name processing :lib ob-processing)
    (:name ruby       :lib ob-ruby)
    (:name sed        :lib ob-sed)
    (:name table      :lib ob-table)
    (:name tangle     :lib ob-tangle)
    )
  )
(speckler-add! org-src ()
  '(default

    )
  )
(speckler-add! treesit-source ()
  '(org           "git@github.com:milisims/tree-sitter-org.git")
  )
(speckler-add! treesit-lang ()
  '(org-mode . org)
  )
;;-- end specs
