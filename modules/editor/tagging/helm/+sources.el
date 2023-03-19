;;; +sources.el -*- lexical-binding: t; -*-

(defun +jg-tag-clean-input (x)
  (let ((trimmed (string-trim x)))
    (s-replace-regexp "\s+" "_" trimmed)
    )
  )

(after! helm-files
  (setq helm-grep-actions (append helm-grep-actions '(("Open Url" . jg-tag-open-url-action))))
  ;; Build a Custom grep for bookmarks
  (setq jg-tag-bookmark-helm-source
        (helm-make-source "Bookmark Helm" 'helm-grep-class
          :action (helm-make-actions "Open Url" '+jg-tag-open-url-action
                                     "Insert"   '+jg-tag-insert-bookmarks
                                     "Insert Link" '+jg-tag-insert-links
                                     "Tweet Link"  '+jg-tag-tweet-link-action
                                     )
          :filter-one-by-one   '+jg-tag-grep-filter-one-by-one
          :pattern-transformer '+jg-tag-bookmark-helm-pattern-transformer
          :nomark nil
          :backend "ggrep --color=always -a -d skip %e -n%cH -e %p %f"
          :pcre nil
          )
        )
  (setq jg-tag-twitter-grep-helm-source-alt
        (helm-make-source "twitter grep helm alt" 'helm-grep-class
          :action (helm-make-actions "File Select Helm" #'+jg-tag-file-select-helm
                                     "Display in Temp Buffer" #'+jg-tag-file-display)
          ;; :filtered-candidate-transformer '+jg-tag-grep-filter-candidate-transformer
          :filter-one-by-one '+jg-tag-twitter-grep-filter-one-by-one
          :filtered-candidate-transformer #'+jg-tag-sort-by-files
          :nomark nil
          :backend "ggrep --color=always -a -d skip %e -n%cH -e %p %f"
          :pattern-transformer '+jg-tag-grep-pattern-transformer
          :pcre nil
          ))
)

(after! helm
  (setq jg-tag-twitter-tag-helm-source
        (helm-build-in-file-source "Twitter Helm"
            jg-tag-loc-twitter-tag-index
          :action (helm-make-actions "File Select Helm" #'+jg-tag-file-select-helm
                                     "Insert User Link" #'+jg-tag-insert-twitter-link)
          :candidate-transformer #'+jg-tag-helm-index-file-transformer
          :filtered-candidate-transformer #'+jg-tag-sort-by-files
          )
        ;; ==========
        jg-tag-twitter-grep-helm-source
        (helm-build-in-file-source "Twitter Grep Helm"
            jg-tag-loc-twitter-grep-index
          :action (helm-make-actions "File Select Helm" #'+jg-tag-file-select-helm)
          :candidate-transformer #'+jg-tag-helm-index-file-transformer
          :pattern-transformer #'+jg-tag-grep-pattern-transformer
          :filtered-candidate-transformer #'+jg-tag-sort-by-files
          )
        ;; ==========
        jg-tag-twitter-account-helm-source
        (helm-build-in-file-source "Twitter Account Helm"
            jg-tag-loc-twitter-account-index
          :action (helm-make-actions "File Select Helm" #'+jg-tag-file-select-helm)
          :candidate-transformer #'+jg-tag-helm-index-file-transformer
          :pattern-transformer #'+jg-tag-grep-pattern-transformer
          :filtered-candidate-transformer #'+jg-tag-sort-by-files
          )
        ;; ==========
        jg-tag-file-select-source
        (helm-make-source "Twitter File Select Helm" 'helm-source
          :action (helm-make-actions "Find File" #'+jg-tag-find-file)
          )
        ;; ==========
        jg-tag-helm-source
        (helm-make-source "Helm Tagging" 'helm-source
          :action (helm-make-actions "Re-entrant-set" #'+jg-tag-set-tags-re-entrant
                                     "Set"            #'+jg-tag-set-tags)
          :pattern-transformer #'+jg-tag-clean-input
          )
        ;; ==========
        jg-tag-fallback-source
        (helm-build-dummy-source "Helm Tags Fallback Source"
          :action (helm-make-actions "Re-entrant-Create" #'+jg-tag-set-new-tag-re-entrant
                                     "Create"            #'+jg-tag-set-new-tag)

          :filtered-candidate-transformer (lambda (_c _s) (list helm-pattern)))
        )
)
