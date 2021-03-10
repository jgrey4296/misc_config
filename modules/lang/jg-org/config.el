;; set pomodoro log variable
(defcustom +jg-org-pomodoro-log-file "~/.doom.d/setup_files/pomodoro_log.org" "The Location of the Pomodoro Log File")
(defcustom +jg-org-pomodoro-buffer-name "*Pomodoro Log*"
  "The name of the Pomodoro Log Buffer to record what I did in")
(defcustom +jg-org-pomodoro-log-message ";; What did the last Pomodoro session accomplish? C-c to finish\n"
  "The message to add to the log buffer to spur comments")

(load! "+org-funcs")
(load! "+org-pomodoro-funcs")
(load! "+bindings")
(load! "+vars")

(use-package! academic-phrases
  :defer t
)

(use-package! link-hint
  :config
  ;; override default org link to open externally sometimes
  (link-hint-define-type 'org-link
    :next #'link-hint--next-org-link
    :at-point-p #'link-hint--org-link-at-point-p
    :vars '(org-mode org-agenda-mode org-link-minor-mode)
    :open #'+jg-org-link-hint-external
    :open-multiple t
    :copy #'kill-new)
  (push 'org-link link-hint-types)
  )
(after! helm-org
    ;; TODO add a keybind for helm-org-rifle
    (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
    (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))
  )
(after! org
  ;;ORG SETUP
  (setq-default
   org-agenda-files `(,(expand-file-name "~/.doom.d/setup_files/base_agenda.org"))
   org-archive-location (string-join `(,(expand-file-name "~/.doom.d/setup_files/archive.org")
                                       "* Main Archive") "::")
   org-fast-tag-selection-single-key nil
   org-from-is-user-regexp "\\<John Grey\\>"
   org-group-tags nil
   org-use-fast-tag-selection t
   org-tags-column 80
   )

  (push 'org-indent-mode minor-mode-list)
  (push '("Scholar" . "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=%s") org-link-abbrev-alist)

  (load! "+org-spec-bindings")
  )
(after! org-pomodoro
  ;; add a startup hook for pomodoro to tweet the end time
  (add-hook 'org-pomodoro-started-hook '+jg-org-pomodoro-start-hook)
  ;; add a finished hook to ask for a recap of what was done,
  ;; and store it in a pomodoro log file
  (add-hook 'org-pomodoro-finished-hook '+jg-org-pomodoro-end-hook)
  )
(after! org-projectile
  ;; from https://emacs.stackexchange.com/questions/18194/
  (setq org-projectile-capture-template "** TODO [[%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))][%?]]\n\t%t\n\t
%(with-current-buffer (org-capture-get :original-buffer) (buffer-substring (line-beginning-position) (line-end-position)))\n")
  )
(after! org-superstar
  (setq org-hide-leading-stars t)
  )
