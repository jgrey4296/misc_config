;;; completion/ivy/+vars.el -*- lexical-binding: t; -*-

;;-- general-insert
(setq general-insert-location (expand-file-name "general-insert" templates-loc))

;;-- end general-insert

;;-- ivy
(speckler-setq! ivy 50
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
  (ivy-add-actions 'ivy-switch-buffer
                   '(("k" +jg-ivy-kill-buffer "Kill")))
  (ivy-add-actions '+jg-term-switch
                   '(("k" +jg-ivy-kill-buffer "Kill")))
  (ivy-add-actions 'swiper
                   '(("y" +jg-ivy--action-yank "yank")
                     ("k" +jg-ivy--action-kill "kill")
                     )
                   )
  ;; Override default insert action
  (ivy-set-actions t `(("I" +jg-ivy--action-insert "insert at point")
                       ("i" ivy--action-insert "insert")
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
(defvar jg-counsel-ignore-file (expand-file-name "tools/ignore/search_ignore" templates-loc))

(speckler-setq! counsel 50
                     counsel--find-file-predicate #'+jg-ivy-file-predicate
                     counsel-find-file-extern-extensions   '("mp4" "mkv" "xlsx" "pdf" "epub")
                     counsel-rg-base-command               `("rg"
                                                             "--max-columns" "240"
                                                             "--with-filename"
                                                             "--no-heading"
                                                             "--line-number"
                                                             "--color" "never"
                                                             "--sort=path"
                                                             "%s"
                                                             )
                     counsel-grep-base-command             "grep -E -n -e %s %s"
                     counsel-projectile-grep-base-command  "grep -rnEI %s"
                     )

;;-- end counsel

;;-- specs
(speckler-add! popup
                    '(ivy
                      ("^\\*ivy-occur" :size 0.35 :ttl 0 :quit nil)
                      )
                    )
;;-- end specs
