;;; lang/jg-org/+vars.el -*- lexical-binding: t; -*-


;;-- org core
;; locations
(setq org-id-locations-file (expand-file-name "~/_cache_/org/.orgids")
      org-archive-location  (string-join `(
                                           ,(expand-file-name "archive.org" org-directory)
                                           "* Main Archive"
                                           )
                                         "::"
                                         )
      org-agenda-files      (list initial-buffer-choice
                                  (expand-file-name "todo.org" org-directory)
                                  )
      )

;; ORG SETUP
(setq-default org-fast-tag-selection-single-key nil
              org-from-is-user-regexp "\\<John Grey\\>"
              org-group-tags nil
              org-use-fast-tag-selection t
              org-tags-column 50
              org-startup-indented nil
              org-indent--deepest-level 20
              org-element-use-cache nil
            )

;; Save target buffer after archiving a node.
(setq org-archive-subtree-save-file-p t)

(push 'org-indent-mode minor-mode-list)
(after! 'ol
  (push '("Scholar" . "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=%s") org-link-abbrev-alist)
  )

;; (textobjects insert navigation additional shift todo heading calendar)
(setq evil-org-key-theme '(textobjects insert shift todo)
    org-cycle-separator-lines 3
    )
;;-- end org core

;;-- todo config
(let ((project-steps '(sequence
                       "TODO(j!)"      ; A job that needs doing
                       "IDEA(i)"       ; An unconfirmed job
                       "LOOP(l)"       ; A recurring job
                       "ACTIVE(a)"     ; A job that is in progress
                       "BLOCKED(b)"    ; Something external is holding up this task
                       "QUEUED(q)"     ; This task is paused/on hold because of me
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
                    "|"
                    "OKAY(o)"
                    "YES(y)"
                    "NO(n)"
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

;;-- end todo config

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

;;-- projectile
(after! org-projectile
  ;; from https://emacs.stackexchange.com/questions/18194/
  (setq org-projectile-capture-template "** TODO [[%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))][%?]]\n\t%t\n\t
%(with-current-buffer (org-capture-get :original-buffer) (buffer-substring (line-beginning-position) (line-end-position)))\n")
  )
;;-- end projectile

;;-- spelling
;; Don't spellcheck org blocks
(pushnew! ispell-skip-region-alist
          '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
          '("#\\+BEGIN_SRC" . "#\\+END_SRC")
          '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

;;-- end spelling

;;-- specs
(spec-handling-add! file-templates
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
(spec-handling-add! fold
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
(spec-handling-add! tagging
                    `(org-mode
                     :set ,#'+jg-org-set-tags
                     :new ,#'+jg-org-set-new-tag
                     :get ,#'org-get-tags
                     )
                    )
(spec-handling-add! lookup-handler
                    `(org-mode
                     :definition ,#'+org-lookup-definition-handler
                     :references ,#'+org-lookup-references-handler
                     :documentation ,#'+org-lookup-documentation-handler
                     )
                    )
(spec-handling-add! whitespace-cleanup
                    `(org-mode
                      ,#'delete-trailing-whitespace
                      ;; ,#'+jg-org-clean-heading-spaces
                      ,#'+jg-text-cleanup-whitespace
                     )
                 )
(spec-handling-add! popup
                    '(org-mode
                      ("^\\*Org Links" :slot -1 :vslot -1 :size 2 :ttl 0)
                      ("^ ?\\*\\(?:Agenda Com\\|Calendar\\|Org Export Dispatcher\\)" :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :ttl 0)
                      ("^\\*Org \\(?:Select\\|Attach\\)" :slot -1 :vslot -2 :ttl 0 :size 0.25)
                      ("^\\*Org Agenda"     :ignore t)
                      ("^\\*Org Src"        :size 0.42  :quit nil :select t :autosave t :modeline t :ttl nil)
                      ("^\\*Org-Babel")
                      ("^\\*Capture\\*$\\|CAPTURE-.*$" :size 0.42 :quit nil :select t :autosave ignore)
                      )
                    )
(spec-handling-add! auto-modes
                   '(org
                     ("\\.org\\'" . org-mode)
                     )
                   )
(spec-handling-add! eval
                    `(org-mode :eval ,#'+org-eval-handler)
                    )
;;-- end specs
