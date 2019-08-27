(defconst tag-unify-packages '(
                               helm
                               evil
                               helm-bibtex
                               dash
                               f
                               )
  )

(defun tag-unify/init-f ()
  (use-package f :defer t)
  )


(defun tag-unify/init-dash ()
  (use-package dash :defer t)
  )



(defun tag-unify/post-init-helm ()
  ;; rebuild tag database

  ;; turn to candidates


  )

(defun tag-unify/post-init-helm-bibtex ()
  (setq bibtex-completion-bibliography (tag-unify/build-bibtex-list))
  (spacemacs/set-leader-keys
    "a b" 'tag-unify/helm-bibtex)


  (defvar tag-unify/helm-source-bibtex
    '((name . "BibTeX entries")
      (header-name . "Test")
                   ;; (lambda (name) (format "%s%s: " name (if helm-bibtex-local-bib " (local)" ""))))
      (candidates . helm-bibtex-candidates)
      (match helm-mm-exact-match helm-mm-match helm-fuzzy-match)
      (fuzzy-match)
      (redisplay . identity)
      (multimatch)
      (group . helm)
      (filtered-candidate-transformer helm-bibtex-candidates-formatter helm-flx-fuzzy-matching-sort helm-fuzzy-highlight-matches)
      (action . (("Insert citation"     . helm-bibtex-insert-citation)
                 ("Open PDF"            . helm-bibtex-open-pdf)
                 ("Insert BibTeX key"   . helm-bibtex-insert-key)
                 ("Insert BibTeX entry" . helm-bibtex-insert-bibtex)
                 ("Show entry"          . helm-bibtex-show-entry)
                 )))
    "Simplified source for searching bibtex files")

  (defun tag-unify/helm-bibtex (&optional arg local-bib input)
    " Custom implementation of helm-bibtex"
    (interactive "P")
    (require 'helm-bibtex)
    (when arg
      (bibtex-completion-clear-cache))
    (let* ((candidates (if (or arg (null tag-unify/helm-bibtex-candidates))
                           (progn (message "Generating Candidates")
                                  (bibtex-completion-init)
                                  (setq tag-unify/helm-bibtex-candidates
                                        (mapcar 'tag-unify/process-candidates (bibtex-completion-candidates)))
                                  tag-unify/helm-bibtex-candidates)
                         tag-unify/helm-bibtex-candidates
                         ))
           (key (bibtex-completion-key-at-point))
           (preselect (and key
                           (cl-position-if (lambda (cand)
                                             (member (cons "=key=" key)
                                                     (cdr cand)))
                                           candidates)))
           )
      (helm :sources `(,tag-unify/helm-source-bibtex)
            :full-frame helm-bibtex-full-frame
            :buffer "*helm bibtex*"
            :input input
            :bibtex-local-bib local-bib
            :bibtex-candidates candidates
            )))
  )
