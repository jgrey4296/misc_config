(defconst org-tagging-packages
  '(
    org
    evil
    )
  )


(defun org-tagging/post-init-org()
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

  (spacemacs/set-leader-keys
    ;; TAGS
    "o t o"   'org-tagging/tag-occurances
    "o t a o" 'org-tagging/tag-occurences-in-open-buffers
    "o t v"   'org-tags-view
    "o t s"   'org-set-tags
    )
  (spacemacs/declare-prefix "o t" "Tags" "Tags")
  )

(defun org-tagging/post-init-evil ()
  (evil-ex-define-cmd "t[ag]" 'org-tagging/org-tagging-helm-start)
  (evil-ex-define-cmd "to" 'org-tagging/tag-occurrences)
  (evil-ex-define-cmd "toa" 'org-tagging/tag-occurrences-in-open-buffers)
  )
