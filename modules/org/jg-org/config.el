;; set pomodoro log variable
(defcustom jg-spacemacs-org/pomodoro-log-file "~/.spacemacs.d/setup_files/pomodoro_log.org" "The Location of the Pomodoro Log File")
(defcustom jg-spacemacs-org/pomodoro-buffer-name "*Pomodoro Log*"
  "The name of the Pomodoro Log Buffer to record what I did in")
(defcustom jg-spacemacs-org/pomodoro-log-message ";; What did the last Pomodoro session accomplish? C-c to finish\n"
  "The message to add to the log buffer to spur comments")


(use-package! helm-org
  :defer t
  :config
  (progn
    ;; TODO add a keybind for helm-org-rifle
    (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
    (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))
    )
  )
(after! org
  ;;ORG SETUP
  (setq-default
   org-agenda-files `(,(expand-file-name "~/.spacemacs.d/setup_files/base_agenda.org"))
   org-archive-location (string-join `(,(expand-file-name "~/.spacemacs.d/setup_files/archive.org")
                                       "* Main Archive") "::")
   org-fast-tag-selection-single-key nil
   org-from-is-user-regexp "\\<John Grey\\>"
   org-group-tags nil
   org-use-fast-tag-selection t
   org-tags-column 80
   )

  (push 'org-indent-mode minor-mode-list)


  (defun jg-spacemacs-org/org-mod-keymap ()
    (define-key org-mode-map (kbd "C-c [") nil)
    (define-key org-mode-map (kbd "C-c ]") nil)
    (evil-define-key 'normal org-mode-map (kbd "gl") nil)
    (evil-define-key 'normal org-mode-map (kbd "gL") nil)
    (evil-define-key* 'normal org-mode-map
      (kbd "z i") 'org-indent-mode
      (kbd "t")   'org-todo
      (kbd "g j") 'org-forward-heading-same-level
      (kbd "g k") 'org-backward-heading-same-level
      (kbd "g l") 'jg-spacemacs-org/open_link_in_buffer
      (kbd "g L") 'jg-spacemacs-org/open_link_externally
      (kbd "] p") 'org-next-link
      (kbd "[ p") 'org-previous-link
      )
    )
  (defun jg-spacemacs-org/helm-org-hook ()
    (evil-define-key 'normal org-mode-map
      (kbd "g h")     'helm-org-in-buffer-headings
      )
    (evil-define-key 'normal evil-org-mode-map
      (kbd "g h")     'helm-org-in-buffer-headings
      )
    )

  (add-hook 'org-mode-hook 'jg-spacemacs-org/org-mod-keymap)
  (add-hook 'org-mode-hook 'jg-spacemacs-org/helm-org-hook)

  ;; add in keybinding to call tag-occurances
  (map! :prefix
   ;; AGENDA
   "a o a /"   'org-occur-in-agenda-files
   "a o a f"   'org-agenda-file-to-front
   "a o a r"   'org-remove-file
   "a o a l"   'org-agenda-list
   "a o a F"   'jg-spacemacs-org/list-agenda-files
   "a o a t"   'org-tags-view
   ;; Agenda -> Calendar
   "a o a c"   'org-goto-calendar
   "a o i t"   'org-time-stamp
   ;; LINKS
   "a o l s"   'org-store-link
   "a o l i"   'org-insert-link
   )

  (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
                                            "v w"   'org-agenda-week-view
                                            "v m"   'org-agenda-month-view
                                            )
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "." nil)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
                                            ;; SRC CODE
                                            ". e"   'org-edit-src-code
                                            ". E"   'org-babel-execute-src-block
                                            ;; Links
                                            ". d"   'org-toggle-link-display
                                            ". o"   'jg-spacemacs-org/open_link_in_buffer
                                            ". O"   'jg-spacemacs-org/open_link_externally
                                            ". n"   'jg-spacemacs-org/change_link_name
                                            ;;Formatting
                                            "i t"   'jg-spacemacs-org/insert-heading-trio
                                            ;; Citation
                                            "i c" 'org-reftex-citation
                                            )

  )
(use-package! academic-phrases
  :config
  (map! :mode org
        :localleader
        :prefix ("iA" . "Academic Phrases")
        "p" 'academic-phrases
        "s" 'academic-phrases-by-section)
  )
(after! org-ref
  (map! "a U r" 'jg-spacemacs-org/bibtex-load-random)
  (add-hook 'bibtex-mode-hook (lambda ()
                                (map! :mode bibtex-mode
                                      :localleader
                                      "p" 'jg-spacemacs-org/org-ref-open-bibtex-pdf
                                      )
                                )
            )
  )
(after! org-pomodoro
  ;; add a startup hook for pomodoro to tweet the end time
  (add-hook 'org-pomodoro-started-hook 'jg-spacemacs-org/pomodoro-start-hook)
  ;; add a finished hook to ask for a recap of what was done,
  ;; and store it in a pomodoro log file
  (add-hook 'org-pomodoro-finished-hook 'jg-spacemacs-org/pomodoro-end-hook)
  )
(use-package! ob-sqlite
  :init
  (org-babel-do-load-languages 'org-babel-load-languages '((sqlite .t)))
  )
(after! org-projectile
  ;; from https://emacs.stackexchange.com/questions/18194/
  (setq org-projectile-capture-template "** TODO [[%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))][%?]]\n\t%t\n\t
%(with-current-buffer (org-capture-get :original-buffer) (buffer-substring (line-beginning-position) (line-end-position)))\n")
  )
(after! evil-org
  (evil-define-key* '(normal) evil-org-mode-map
    (kbd ">") #'(lambda () (interactive) (org-demote))
    (kbd "<") #'(lambda () (interactive) (org-promote))
    )
  (evil-define-key* '(visual) evil-org-mode-map
    (kbd ">") 'org-demote-subtree
    (kbd "<") 'org-promote-subtree
    )
  )
(after! org-superstar
  (setq org-hide-leading-stars t)
  )
