;;; editor/evil/config.el -*- lexical-binding: t; -*-

(local-load! "+defines")
(local-load! "+core")
(local-load! "+snipe")
(local-load! "+escape")
(local-load! "+extra")

(defer-load! (evil-collection evil-ex) "+evil-ex")

(defer-load! jg-bindings-core "+bindings") ;; -> jg-evil-bindings

(speckler-new! evil-initial (key val)
  "Set initial evil states for modes"
  :struct '(mode evil-state)
  :loop 'do
  (evil-set-initial-state key (car (ensure-list val)))
  )

(speckler-add! popup ()
  '(evil
    ("^\\*evil-registers" :size 0.3)
    ("^\\*Command Line"   :size 8)
    ("^\\*Ex-Commands\\*" :quit t :select nil :ttl 5)
    )
  )

;; --------------------------------------------------
;; For evil-escape?:
;; TODO handle evil-exchange
;; TODO handle +lsp-signature-stop-maybe-h
;; TODO handle +vc-gutter-update-h
;; TODO handle closing popups/poppy windows
;; TODO handle yas-abort-snippet

;; evil-surround and embrace
(setq-default evil-embrace-show-help-p t
              embrace-show-help-p t
              evil-embrace-evil-surround-keys '(?\( ?\[ ?\{ ?\) ?\] ?\} ?\" ?\' ?< ?> ?b ?B ?t ?\C-\[ ?w ?W ?s ?p ?f ?F)
              evil-surround-pairs-alist (append jg-evil-surround-pairs-base
                                                '((?t . evil-surround-read-tag)
                                                  (?< . evil-surround-read-tag)
                                                  (?f . evil-surround-function)
                                                  (?F . evil-surround-prefix-function))
                                                )
              )
