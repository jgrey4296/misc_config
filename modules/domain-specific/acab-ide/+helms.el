;;; domain-specific/acab-ide/+helms.el -*- lexical-binding: t; -*-
;;
;;TODO: add helms for types, crosscuts, patterns, tests, tags,
;;strategies, performatives, channels

(setq jg-trie-layer/rule-helm-source
      (helm-make-source "Rule Helm" 'helm-source-sync
        :action (helm-make-actions "Open Rule" 'trie/find-rule)
        :candidates 'trie/get-rule-helm-candidates
        )
      ;;--------------------
      jg-trie-layer/rule-helm-dummy-source
      (helm-make-source "Rule Dummy Helm" 'helm-source-dummy
        :action (helm-make-actions "Create Rule" 'trie/create-rule)
        )
      )
                                        ;----
(setq jg-trie-layer/type-helm-source
      (helm-make-source "Type Helm" 'helm-source-sync
        :action (helm-make-actions "Open Type" 'trie/find-type)
        :candidates 'trie/get-type-helm-candidates
        )
      ;;--------------------
      jg-trie-layer/type-helm-dummy-source
      (helm-make-source "Type Dummy Helm" 'helm-source-dummy
        :action (helm-make-actions "Create Type" 'trie/create-type)
        )
      )
                                        ;----
(setq jg-trie-layer/crosscut-helm-source
      (helm-make-source "Crosscut Helm" 'helm-source-sync
        :action (helm-make-actions "Open Crosscut" 'trie/find-crosscut)
        :candidates 'trie/get-crosscut-helm-candidates
        )
      )
                                        ;----
(setq jg-trie-layer/pattern-helm-source
      (helm-make-source "Pattern Helm" 'helm-source-sync
        :action (helm-make-actions "Open Pattern" 'trie/find-pattern)
        :candidates 'trie/get-pattern-helm-candidates
        )
      ;;--------------------
      jg-trie-layer/pattern-helm-dummy-source
      (helm-make-source "Pattern Dummy Helm" 'helm-source-dummy
        :action (helm-make-actions "Create Pattern" 'trie/create-pattern)
        )
      )
                                        ;----
(setq jg-trie-layer/test-helm-source
      (helm-make-source "Test Helm" 'helm-source-sync
        :action (helm-make-actions "Open Test" 'trie/find-test)
        :candidates 'trie/get-test-helm-candidates
        )
      ;;--------------------
      jg-trie-layer/test-helm-dummy-source
      (helm-make-source "Test Dummy Helm" 'helm-source-dummy
        :action (helm-make-actions "Create Test" 'trie/create-test)
        )
      )
                                        ;----
(setq jg-trie-layer/tag-helm-source
      (helm-make-source "Tag Helm" 'helm-source-sync
        :action (helm-make-actions "Open Tag" 'trie/toggle-tag)
        :candidates 'trie/get-tag-helm-candidates
        )
      ;;--------------------
      jg-trie-layer/tag-helm-dummy-source
      (helm-make-source "Tag Dummy Helm" 'helm-source-dummy
        :action (helm-make-actions "Create Tag" 'trie/create-tag)
        )
      )
                                        ;----
(setq jg-trie-layer/channel-helm-source
      (helm-make-source "Channel Helm" 'helm-source-sync
        :action (helm-make-actions "Open Channel" 'trie/find-channel)
        :candidates 'trie/get-channel-helm-candidates
        )
      ;;--------------------
      jg-trie-layer/channel-helm-dummy-source
      (helm-make-source "Channel Dummy Helm" 'helm-source-dummy
        :action (helm-make-actions "Create Channel" 'trie/create-channel)
        )
      )

(defun jg-trie-layer/rule-helm ()
  "Helm for inserting and creating rules into jg-trie-layer/rule authoring mode"
  (interactive)
  (helm :sources '(jg-trie-layer/rule-helm-source jg-trie-layer/rule-helm-dummy-source)
        :full-frame t
        :buffer "*Rule Helm*"
        )
  )
;;add type helm
(defun jg-trie-layer/type-helm ()
  "Helm for inserting and creating types into jg-trie-layer/type authoring mode"
  (interactive)
  (helm :sources '(jg-trie-layer/type-helm-source jg-trie-layer/type-helm-dummy-source)
        :full-frame t
        :buffer "*Type Helm*"
        )
  )
;;add crosscut helm
(defun jg-trie-layer/crosscut-helm ()
  "Helm for inserting and creating crosscuts into jg-trie-layer/crosscut authoring mode"
  (interactive)
  (helm :sources '(jg-trie-layer/crosscut-helm-source jg-trie-layer/crosscut-helm-dummy-source)
        :full-frame t
        :buffer "*Crosscut Helm*"
        )
  )
;;add pattern helm
(defun jg-trie-layer/pattern-helm ()
  "Helm for inserting and creating patterns into jg-trie-layer/pattern authoring mode"
  (interactive)
  (helm :sources '(jg-trie-layer/pattern-helm-source jg-trie-layer/pattern-helm-dummy-source)
        :full-frame t
        :buffer "*Pattern Helm*"
        )
  )
;;add test helm
(defun jg-trie-layer/test-helm ()
  "Helm for inserting and creating tests into jg-trie-layer/test authoring mode"
  (interactive)
  (helm :sources '(jg-trie-layer/test-helm-source jg-trie-layer/test-helm-dummy-source)
        :full-frame t
        :buffer "*Test Helm*"
        )
  )
;;add tag helm
(defun jg-trie-layer/tag-helm ()
  "Helm for inserting and creating tags into jg-trie-layer/tag authoring mode"
  (interactive)
  (helm :sources '(jg-trie-layer/tag-helm-source jg-trie-layer/tag-helm-dummy-source)
        :full-frame t
        :buffer "*Tag Helm*"
        )
  )
;;add channel helm
(defun jg-trie-layer/channel-helm ()
  "Helm for inserting and creating channels into jg-trie-layer/channel authoring mode"
  (interactive)
  (helm :sources '(jg-trie-layer/channel-helm-source jg-trie-layer/channel-helm-dummy-source)
        :full-frame t
        :buffer "*Channel Helm*"
        )
  )
