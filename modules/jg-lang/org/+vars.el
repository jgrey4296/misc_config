;;; lang/jg-org/+vars.el -*- lexical-binding: t; -*-

;;-- personal
(setq-default jg-org-external-file-link-types '("jpg" "jpeg" "png" "mp4" "html")
              jg-org-clean-marker nil
              jg-org-preferred-linecount 1500
              jg-org-link-move-base "/Volumes/Overflow/missing_images/"

              jg-org-twitter-loc (expand-file-name  "~/twitterthreads/")
              )
;;-- end personal

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

;;-- org core
(after! org
  ;;ORG SETUP
  (setq-default org-fast-tag-selection-single-key nil
                org-from-is-user-regexp "\\<John Grey\\>"
                org-group-tags nil
                org-use-fast-tag-selection t
                org-tags-column 50
                )

  (push 'org-indent-mode minor-mode-list)
  (push '("Scholar" . "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=%s") org-link-abbrev-alist)
  )
;;-- end org core

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

;;-- file templates
(after! jg-file-templates
  (+jg-completion-add-file-templates
   'org
   '(("two_pager\\.org$"     :trigger "__pacheco_vega_two_pager" :mode org-mode)
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
  )
;;-- end file templates
