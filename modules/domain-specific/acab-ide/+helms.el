;;; domain-specific/acab-ide/+helms.el -*- lexical-binding: t; -*-
;;
;;TODO: add helms for types, crosscuts, patterns, tests, tags,
;;strategies, performatives, channels

(setq acab-ide/rule-helm-source
      (helm-make-source "Rule Helm" 'helm-source-sync
        :action (helm-make-actions "Open Rule" 'trie/find-rule)
        :candidates 'trie/get-rule-helm-candidates
        )
      ;;--------------------
      acab-ide/rule-helm-dummy-source
      (helm-make-source "Rule Dummy Helm" 'helm-source-dummy
        :action (helm-make-actions "Create Rule" 'trie/create-rule)
        )
      )
                                        ;----
(setq acab-ide/type-helm-source
      (helm-make-source "Type Helm" 'helm-source-sync
        :action (helm-make-actions "Open Type" 'trie/find-type)
        :candidates 'trie/get-type-helm-candidates
        )
      ;;--------------------
      acab-ide/type-helm-dummy-source
      (helm-make-source "Type Dummy Helm" 'helm-source-dummy
        :action (helm-make-actions "Create Type" 'trie/create-type)
        )
      )
                                        ;----
(setq acab-ide/crosscut-helm-source
      (helm-make-source "Crosscut Helm" 'helm-source-sync
        :action (helm-make-actions "Open Crosscut" 'trie/find-crosscut)
        :candidates 'trie/get-crosscut-helm-candidates
        )
      )
                                        ;----
(setq acab-ide/pattern-helm-source
      (helm-make-source "Pattern Helm" 'helm-source-sync
        :action (helm-make-actions "Open Pattern" 'trie/find-pattern)
        :candidates 'trie/get-pattern-helm-candidates
        )
      ;;--------------------
      acab-ide/pattern-helm-dummy-source
      (helm-make-source "Pattern Dummy Helm" 'helm-source-dummy
        :action (helm-make-actions "Create Pattern" 'trie/create-pattern)
        )
      )
                                        ;----
(setq acab-ide/test-helm-source
      (helm-make-source "Test Helm" 'helm-source-sync
        :action (helm-make-actions "Open Test" 'trie/find-test)
        :candidates 'trie/get-test-helm-candidates
        )
      ;;--------------------
      acab-ide/test-helm-dummy-source
      (helm-make-source "Test Dummy Helm" 'helm-source-dummy
        :action (helm-make-actions "Create Test" 'trie/create-test)
        )
      )
                                        ;----
(setq acab-ide/tag-helm-source
      (helm-make-source "Tag Helm" 'helm-source-sync
        :action (helm-make-actions "Open Tag" 'trie/toggle-tag)
        :candidates 'trie/get-tag-helm-candidates
        )
      ;;--------------------
      acab-ide/tag-helm-dummy-source
      (helm-make-source "Tag Dummy Helm" 'helm-source-dummy
        :action (helm-make-actions "Create Tag" 'trie/create-tag)
        )
      )
                                        ;----
(setq acab-ide/channel-helm-source
      (helm-make-source "Channel Helm" 'helm-source-sync
        :action (helm-make-actions "Open Channel" 'trie/find-channel)
        :candidates 'trie/get-channel-helm-candidates
        )
      ;;--------------------
      acab-ide/channel-helm-dummy-source
      (helm-make-source "Channel Dummy Helm" 'helm-source-dummy
        :action (helm-make-actions "Create Channel" 'trie/create-channel)
        )
      )

(defun acab-ide/rule-helm ()
  "Helm for inserting and creating rules into acab-ide/rule authoring mode"
  (interactive)
  (helm :sources '(acab-ide/rule-helm-source acab-ide/rule-helm-dummy-source)
        :full-frame t
        :buffer "*Rule Helm*"
        )
  )
;;add type helm
(defun acab-ide/type-helm ()
  "Helm for inserting and creating types into acab-ide/type authoring mode"
  (interactive)
  (helm :sources '(acab-ide/type-helm-source acab-ide/type-helm-dummy-source)
        :full-frame t
        :buffer "*Type Helm*"
        )
  )
;;add crosscut helm
(defun acab-ide/crosscut-helm ()
  "Helm for inserting and creating crosscuts into acab-ide/crosscut authoring mode"
  (interactive)
  (helm :sources '(acab-ide/crosscut-helm-source acab-ide/crosscut-helm-dummy-source)
        :full-frame t
        :buffer "*Crosscut Helm*"
        )
  )
;;add pattern helm
(defun acab-ide/pattern-helm ()
  "Helm for inserting and creating patterns into acab-ide/pattern authoring mode"
  (interactive)
  (helm :sources '(acab-ide/pattern-helm-source acab-ide/pattern-helm-dummy-source)
        :full-frame t
        :buffer "*Pattern Helm*"
        )
  )
;;add test helm
(defun acab-ide/test-helm ()
  "Helm for inserting and creating tests into acab-ide/test authoring mode"
  (interactive)
  (helm :sources '(acab-ide/test-helm-source acab-ide/test-helm-dummy-source)
        :full-frame t
        :buffer "*Test Helm*"
        )
  )
;;add tag helm
(defun acab-ide/tag-helm ()
  "Helm for inserting and creating tags into acab-ide/tag authoring mode"
  (interactive)
  (helm :sources '(acab-ide/tag-helm-source acab-ide/tag-helm-dummy-source)
        :full-frame t
        :buffer "*Tag Helm*"
        )
  )
;;add channel helm
(defun acab-ide/channel-helm ()
  "Helm for inserting and creating channels into acab-ide/channel authoring mode"
  (interactive)
  (helm :sources '(acab-ide/channel-helm-source acab-ide/channel-helm-dummy-source)
        :full-frame t
        :buffer "*Channel Helm*"
        )
  )
