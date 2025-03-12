;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-ibuffer-never-show-regexps
  (rx (or
       (: "*" (or "Minibuf-"
                  "scratch"
                  "Messages"
                  "DOC"
                  "which-key"
                  "server"
                  "Async-native-compile-log"
                  "Echo Area"
                  "eldoc"
                  "org-src-fontification:"
                  "code-converting-work"
                  "code-conversion-work"
                  "helm candidates"
                  "RNC Input"
                  "counsel"
                  "refs-/"
                  "Neotree"
                  "http "
                  "diff-hl"
                  "temp"
                  "string-pixel-width"
                  "helm-bookmarks"
                  )
          (* any)
          )
       (: (or "markdown-code-fontification"
              "org-src-fontification"
              )
          (* any))
       )
      )
  )

(speckler-setq! ibuffer ()
  ibuffer-show-empty-filter-groups nil
  ibuffer-default-sorting-mode 'alphabetic
  ibuffer-filter-group-name-face '(:inherit (success bold))
  ibuffer-old-time 2

  jg-ibuffer-default-filter "-clutter"
  jg-ibuffer-default-group "default"
  jg-ibuffer-ivy-predicate-patterns (rx (or "*helpful"
                                            "*helm-"
                                            "doom"
                                            "*dired-log"
                                            "magit"
                                            "*Free Keys"
                                            )
                                        )
  )
(speckler-add! popup ()
  '(ibuffer
    ("^\*Ibuffer\*$"         :side right  :ttl 5 :width  0.5 :quit nil :select t :priority 50)
    )
  )
(speckler-add! fold ()
  `(ibuffer
    :modes (ibuffer-mode)
    :priority 50
    :triggers (:delete     nil
               :open-all   nil
               :close-all  nil
               :toggle     #'ibuffer-toggle-filter-group
               :open       nil
               :open-rec   nil
               :close      nil
               )
    )
  )
(speckler-add! evil-ex ()
  '(ibuffer
    ("buffers" . #'ibuffer)
    )
  )
