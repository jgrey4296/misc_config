;;; domain-specific/acab-ide/+bindings.el -*- lexical-binding: t; -*-
;; TODO
(defhydra trie-help-hydra (:color pink)
  "
   | General           ^^|
   |-------------------^^+
   | [_q_] Quit          |
   |                   ^^|
   |                   ^^|
   |                   ^^|
  "

  ("q" nil :exit t)
  )

;; Defines all sub-trie modes: trie, trie-visual, sequence etc
;; TODO (spacemacs/declare-prefix "a s" "Start Editor")
;; TODO (spacemacs/set-leader-keys
;; "a s t" 'jg-trie-layer/toggle-trie-ide)
(map! :map trie-mode-map
  :n "#" 'trie/insert-tag
  :n "C" 'trie/insert-condition
  :n "A" 'trie/insert-action
  :n "T" 'trie/insert-transform
  )
