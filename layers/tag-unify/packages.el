(defconst tag-unify-packages '(
                               helm
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
(defun tag-unify/pre-init-helm-bibtex ()
  ;; load the bibliography directory on startup
  (setq bibtex-completion-bibliography (tag-unify/build-bibtex-list))
  (spacemacs|use-package-add-hook helm
    :post-config
    (setq helm-grep-actions (append helm-grep-actions '(("Open Url" . tag-unify/open-url-action))))
    )
  ;; Keybind my bib helm
  (spacemacs/set-leader-keys
    "a b" 'tag-unify/helm-bibtex)

  ;; Define the bib helm
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
           )
      (helm :sources `(,tag-unify/helm-source-bibtex)
            :full-frame helm-bibtex-full-frame
            :buffer "*helm bibtex*"
            :input input
            :bibtex-local-bib local-bib
            :bibtex-candidates candidates
            )))
  )
