;;; editor/evil/config.el -*- lexical-binding: t; -*-

;;-- defs

(defvar +evil-repeat-keys (cons ";" ",")
  "The keys to use for universal repeating motions.

This is a cons cell whose CAR is the key for repeating a motion forward, and
whose CDR is for repeating backward. They should both be `kbd'-able strings.

Set this to `nil' to disable universal-repeating on these keys.")

(defvar +evil-want-o/O-to-continue-comments t
  "If non-nil, the o/O keys will continue comment lines if the point is on a
line with a linewise comment.")

(defvar +evil-preprocessor-regexp "^\\s-*#[a-zA-Z0-9_]"
  "The regexp used by `+evil/next-preproc-directive' and
`+evil/previous-preproc-directive' on ]# and [#, to jump between preprocessor
directives. By default, this only recognizes C directives.")

(defvar jg-evil-surround-pairs-base '((?\( . ("( " . " )"))
                                      (?\[ . ("[ " . " ]"))
                                      (?\{ . ("{ " . " }"))

                                      (?\) . ("(" . ")"))
                                      (?\] . ("[" . "]"))
                                      (?\} . ("{" . "}"))

                                      (?# . ("#{" . "}"))
                                      (?b . ("(" . ")"))
                                      (?p . ("(" . ")"))
                                      (?B . ("{" . "}"))
                                      (?> . ("<" . ">"))
                                      )
  )

(defvar evil-textobj-anyblock-blocks '(("(" . ")")
                                       ("{" . "}")
                                       ("\\[" . "\\]")
                                       ("<" . ">"))
  )

;;-- end defs

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

(local-load! "+core")
(local-load! "+snipe")
(local-load! "+escape")
(local-load! "+extra")

(defer-load! (evil-collection evil-ex) "+evil-ex")

(defer-load! jg-bindings-core "+bindings") ;; -> jg-evil-bindings

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
