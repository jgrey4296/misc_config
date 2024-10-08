;;; completion/ivy/+vars.el -*- lexical-binding: t; -*-

;;-- general-insert
(setq general-insert-location (expand-file-name "general-insert" templates-loc))

;;-- end general-insert

;;-- ivy
(spec-handling-setq! ivy 50
                     ivy-height 17
                     ivy-wrap t
                     ivy-fixed-height-minibuffer t
                     ivy-sort-max-size 7500
                     ivy-magic-slash-non-match-action nil ;; disable magic slash on non-match
                     ivy-use-virtual-buffers nil ;; don't show recent files in switch-buffer
                     ivy-virtual-abbreviate 'full ;; ...but if that ever changes, show their full path
                     ivy-on-del-error-function #'ignore ;; don't quit minibuffer on delete-error
                     ivy-use-selectable-prompt t ;; enable ability to select prompt (alternative to `ivy-immediate-done')
                     ivy-rich-parse-remote-buffer nil
                     ivy-read-action-function #'ivy-hydra-read-action
                     ivy-read-action-format-function #'ivy-read-action-format-columns
                     ivy-switch-buffer-faces-alist nil ;; Remove built-in coloring of buffer list; we do our own
                     ivy-initial-inputs-alist nil   ;; Don't use ^ as initial input. Set this here because `counsel' defines more of its own, on top of the defaults.
                     swiper-action-recenter t
                     avy-all-windows t

                     prescient-filter-method '(literal regexp initialism fuzzy)
      )

(after! ivy
  (ivy-add-actions 'counsel-find-file
                   '(("f" (lambda (x) (find-file-literally x)) "Fundamental")))
  (ivy-add-actions 'ivy-switch-buffer
                   '(
                     ("k" +jg-ivy-kill-buffer "Kill")
                     )
                   )
  (ivy-add-actions '+jg-term-switch
                   '(("k" +jg-ivy-kill-buffer "Kill")
                     )
                   )

  )

(after! ivy-hydra
  (setq ivy-dispatching-done-hydra-exit-keys
        '(("q" nil :exit t)
          ("C-o" nil)
          ("M-o" nil "back")
          ("C-g" nil)
          )
        )
  )

;;-- end ivy

;;-- counsel
(defvar jg-ivy-file-regexp (rx anything))
(defvar jg-ivy-file-reject-regexp (rx "test_" (+ anything)))

(spec-handling-setq! counsel 50
                     counsel-find-file-extern-extensions '("mp4" "mkv" "xlsx" "pdf" "epub")
                     counsel--find-file-predicate #'+jg-ivy-file-predicate
                     )

;;-- end counsel

;;-- specs
(spec-handling-add! popup
                    '(ivy
                      ("^\\*ivy-occur" :size 0.35 :ttl 0 :quit nil)
                      )
                    )
;;-- end specs
