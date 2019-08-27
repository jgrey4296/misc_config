(defconst org-tagging-packages
  '(
    org
    evil
    (tag-clean-minor-mode :location local)
    )
  )


(defun org-tagging/post-init-org ()
  (evil-define-operator org-tagging/org-tagging-helm-start (beg end)
    """ Opens the Tagging Helm """
    (interactive "<R>")
    (setq org-tagging/org-tagging-region `(,beg . ,(line-number-at-pos end)))
    (let* ((candidates (org-tagging/org-tagging-candidates))
           (main-source (cons `(candidates . ,(mapcar 'car candidates)) org-tagging/org-tagging-helm))
           )
           (helm :sources '(main-source org-tagging/org-tagging-fallback-source)
                 :input "")
      ))
  )

(defun org-tagging/post-init-evil ()
  (evil-ex-define-cmd "t[ag]" 'org-tagging/org-tagging-helm-start)
  (evil-ex-define-cmd "to" 'org-tagging/tag-occurrences)
  (evil-ex-define-cmd "toa" 'org-tagging/tag-occurrences-in-open-buffers)
  (evil-ex-define-cmd "tv"  'org-tags-view)
  (evil-ex-define-cmd "ts"  'org-set-tags)
  )

(defun org-tagging/init-tag-clean-minor-mode ()
  (use-package tag-clean-minor-mode
    :defer t
    :commands (tag-clean-minor-mode)
    :init (progn
            (push 'tag-clean-minor-mode minor-mode-list)
            (spacemacs|define-transient-state tag-clean
              :title "Tag Cleaning Transient State"
              :doc (concat "
    | Commands   ^^|
    |------------^^|------------^^|
    | [_q_] Quit   | [_!_] Split  |
    | [_f_] Filter | [_p_] Prev   |
    | [_s_] Sub    | [_l_] Leave  |
")
              :bindings
              ("q" nil :exit t)
              ("f" tag-clean/mark-to-filter)
              ("s" tag-clean/mark-to-sub)
              ("p" tag-clean/previous)
              ("l" tag-clean/leave)
              ("!" org-tagging/org-split-on-headings :exit t)
              )
            (spacemacs/set-leader-keys-for-minor-mode 'tag-clean-minor-mode
              "." 'spacemacs/tag-clean-transient-state/body
              )
            )
    )
  )
