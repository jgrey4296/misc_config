;; set pomodoro log variable
(defcustom +jg-org-pomodoro-log-file "~/.doom.d/setup_files/pomodoro_log.org" "The Location of the Pomodoro Log File")
(defcustom +jg-org-pomodoro-buffer-name "*Pomodoro Log*"
  "The name of the Pomodoro Log Buffer to record what I did in")
(defcustom +jg-org-pomodoro-log-message ";; What did the last Pomodoro session accomplish? C-c to finish\n"
  "The message to add to the log buffer to spur comments")

(load! "+org-funcs")
(load! "+org-pomodoro-funcs")
(load! "+org-ref-funcs")

(use-package! academic-phrases
  :init
  (map! :leader
        :prefix ("xA" . "Academic Phrases")
        "p" #'academic-phrases
        "s" #'academic-phrases-by-section
        )
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

  (map! :map org-mode-map
        "C-c [" nil
        "C-c ]" nil
        )

  (setq jg-org-map (make-sparse-keymap))

  (map! :mode org-mode
        :n "g j" #'org-forward-heading-same-level
        :n "g k" #'org-backward-heading-same-level
        :n "] p" #'org-next-link
        :n "[ p" #'org-previous-link
        :n  "g h" #'helm-org-in-buffer-headings
        )

  ;; add in keybinding to call tag-occurances
  (map! :leader
        ;; AGENDA
        :prefix ("ao" . "Org")
        (:prefix ("a" . "Agenda")
         "/"   'org-occur-in-agenda-files
         "f"   'org-agenda-file-to-front
         "r"   'org-remove-file
         "l"   'org-agenda-list
         "F"   '+jg-org-list-agenda-files
         "t"   'org-tags-view
         )
         ;; Agenda -> Calendar
         )
  (map! :leader
        (:prefix "i"
         "t" #'org-time-stamp)
        (:prefix "j"
         "c" #'org-goto-calendar)
        )
  ; TODO
  (map! :mode org-mode
        "." nil
        ;; SRC CODE
        ". e"   'org-edit-src-code
        ". E"   'org-babel-execute-src-block
        ;; Links
        ". d"   'org-toggle-link-display
        ". o"   '+jg-org-open_link_in_buffer
        ". O"   '+jg-org-open_link_externally
        ". n"   '+jg-org-change_link_name
        ;;Formatting
        "i t"   '+jg-org-insert-heading-trio
        ;; Citation
        "i c" 'org-reftex-citation
        )

  (map! :map org-agenda-mode-map
        "v w"   'org-agenda-week-view
        "v m"   'org-agenda-month-view
        )

  )
(after! org-ref
  (map! :leader "a U r" #'+jg-org-bibtex-load-random)
  (add-hook 'bibtex-mode-hook #'(lambda ()
                                (map! :mode bibtex-mode
                                      :localleader
                                      "p" #'+jg-org-ref-open-bibtex-pdf
                                      )
                                )
            )
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
