;;; +sources.el -*- lexical-binding: t; -*-
(require 'helm-source)
(require 'helm-grep)
(require 'helm-utils)
(require 'helm-files)

(local-load! "transformers")
(local-load! "actions")

(defvar jg-tag-twitter-heading-helm-candidates nil)

(defvar jg-tag-twitter-helm-candidates nil)

(defvar jg-tag-twitter-grep-helm-candidates nil)

(setq helm-grep-actions (append helm-grep-actions '(("Open Url" . jg-tag-open-url-action))))

;; Build a Custom grep for bookmarks

(defvar jg-tag-twitter-grep-helm-source-alt
      (helm-make-source "twitter grep helm alt" 'helm-grep-class
        :action (helm-make-actions "File Select Helm"       #'+jg-tag-file-select-helm
                                   "Display in Temp Buffer" #'+jg-tag-file-display)
        ;; :filtered-candidate-transformer '+jg-tag-grep-filter-candidate-transformer
        :filter-one-by-one '+jg-tag-twitter-grep-filter-one-by-one
        :filtered-candidate-transformer                     #'+jg-tag-sort-by-files
        :nomark nil
        :backend (format "%s --color=always -a -d %s"
                         (pcase system-type
                           ('darwin "ggrep")
                           ('gnu/linux "grep")
                           )
                         "skip %e -n%cH -e %p %f"
                         )
        :pattern-transformer '+jg-tag-grep-pattern-transformer
        :pcre nil
        ))

(defvar jg-tag-twitter-tag-helm-source
      (helm-build-in-file-source "Twitter Helm"
          jg-tag-loc-twitter-tag-index
        :action (helm-make-actions "File Select Helm"       #'+jg-tag-file-select-helm
                                   "Insert User Link"       #'+jg-tag-insert-twitter-link)
        :candidate-transformer                              #'+jg-tag-helm-index-file-transformer
        :filtered-candidate-transformer                     #'+jg-tag-sort-by-files
        )
 )

(defvar jg-tag-twitter-grep-helm-source
      (helm-build-in-file-source "Twitter Grep Helm"
          jg-tag-loc-twitter-grep-index
        :action (helm-make-actions "File Select Helm" #'+jg-tag-file-select-helm)
        :candidate-transformer                        #'+jg-tag-helm-index-file-transformer
        :pattern-transformer                          #'+jg-tag-grep-pattern-transformer
        :filtered-candidate-transformer               #'+jg-tag-sort-by-files
        )
      )

(defvar jg-tag-twitter-account-helm-source
      (helm-build-in-file-source "Twitter Account Helm"
          jg-tag-loc-twitter-account-index
        :action (helm-make-actions "File Select Helm" #'+jg-tag-file-select-helm)
        :candidate-transformer                        #'+jg-tag-helm-index-file-transformer
        :pattern-transformer                          #'+jg-tag-grep-pattern-transformer
        :filtered-candidate-transformer               #'+jg-tag-sort-by-files
        )
)

(defvar jg-tag-file-select-source
      (helm-make-source "Twitter File Select Helm" 'helm-source
        :action (helm-make-actions "Find File" #'+jg-tag-find-file)
        )
      )
