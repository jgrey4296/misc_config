(defconst jg_org-packages
  '(
    helm-org
    org
    academic-phrases
    org-ref
    org-pomodoro
    (org-drill :location built-in)
    outline-toc
    )
  )

(defun jg_org/init-helm-org()
  (use-package helm-org
    :defer t
    :config
    (progn
      (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
      (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))
      )
    )
  )
(defun jg_org/post-init-org ()
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


  (defun jg_org/org-mod-keymap ()
    (define-key org-mode-map (kbd "C-c [") nil)
    (define-key org-mode-map (kbd "C-c ]") nil)
    (evil-define-key 'normal org-mode-map (kbd "gl") nil)
    (evil-define-key 'normal org-mode-map (kbd "gL") nil)
    (evil-define-key* 'normal org-mode-map
                      (kbd "z i") 'org-indent-mode
                      (kbd "t")   'org-todo
                      (kbd "g j") 'org-forward-heading-same-level
                      (kbd "g k") 'org-backward-heading-same-level
                      (kbd "g l") 'jg_org/open_link_in_buffer
                      (kbd "g L") 'jg_org/open_link_externally
                      )
    )
  (defun jg_org/helm-org-hook ()
    (evil-define-key 'normal org-mode-map
      (kbd "g h")     'helm-org-in-buffer-headings
      )
    (evil-define-key 'normal evil-org-mode-map
      (kbd "g h")     'helm-org-in-buffer-headings
      )
    )

  (add-hook 'org-mode-hook 'jg_org/org-mod-keymap)
  (add-hook 'org-mode-hook 'jg_org/helm-org-hook)

  (spacemacs/set-leader-keys "a o a" nil)
  (spacemacs/set-leader-keys "a o l" nil)
  (spacemacs/set-leader-keys "a o /" nil)
  (spacemacs/set-leader-keys "a o e" nil)
  (spacemacs/set-leader-keys "a o m" nil)
  (spacemacs/set-leader-keys "a o o" nil)
  (spacemacs/set-leader-keys "a o s" nil)
  (spacemacs/set-leader-keys "a o t" nil)
  (spacemacs/set-leader-keys "a o c" nil)
  (spacemacs/set-leader-keys "a o #" nil)

  ;; add in keybinding to call tag-occurances
  (spacemacs/declare-prefix "a o a" "Agenda")
  (spacemacs/declare-prefix "a o i" "Insert")
  (spacemacs/declare-prefix "a o l" "Links")
  (spacemacs/set-leader-keys
    ;; AGENDA
    "a o a a"   'org-agenda
    "a o a /"   'org-occur-in-agenda-files
    "a o a f"   'org-agenda-file-to-front
    "a o a r"   'org-remove-file
    "a o a l"   'org-agenda-list
    "a o a F"   'jg_layer/list-agenda-files
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
    ". o"   'jg_org/open_link_in_buffer
    ". O"   'jg_org/open_link_externally
    ". n"   'jg_org/change_link_name
    ;;Formatting
    "i t"   'jg_org/insert-heading-trio
    ;; Citation
    "i c" 'org-reftex-citation
    )

  )
(defun jg_org/init-academic-phrases ()
  (use-package academic-phrases
    :config
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "i A p" 'academic-phrases
      "i A s" 'academic-phrases-by-section)
    )
  )
(defun jg_org/pre-init-org-ref ()
  (spacemacs/set-leader-keys "a r" 'jg_org/bibtex-load-random)
  (add-hook 'bibtex-mode-hook (lambda ()
                                (spacemacs/set-leader-keys-for-major-mode 'bibtex-mode
                                  "p" 'jg_org/org-ref-open-bibtex-pdf
                                  )
                                )
            )
    )
(defun jg_org/post-init-org-pomodoro ()
  ;; add a startup hook for pomodoro to tweet the end time
  (add-hook 'org-pomodoro-started-hook 'jg_org/pomodoro-start-hook)
  ;; add a finished hook to ask for a recap of what was done,
  ;; and store it in a pomodoro log file
  (add-hook 'org-pomodoro-finished-hook 'jg_org/pomodoro-end-hook)
  )
(defun jg_org/init-org-drill ()
  (use-package org-drill)
)
(defun jg_org/init-outline-toc ()
  (use-package outline-toc)
  )
