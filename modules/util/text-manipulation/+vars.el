;;; util/text/+vars.el -*- lexical-binding: t; -*-
(setq-default jg-text-last-similarity-arg 1
              jg-text-debug-snippet-name "util.debug"
              )

(setq-default evil-surround-pairs-alist
  '((?\( . ("( " . " )"))
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
    (?t . evil-surround-read-tag)
    (?< . evil-surround-read-tag)
    (?f . evil-surround-function))
)

(setq-default buffer-invisibility-spec
              '((jg-text-invis . t)
                t
                )
              )

(defun disable-vl-mode ()
  (interactive)
  (visual-line-mode -1)
  )

(add-hook! 'text-mode-hook :append #'disable-vl-mode)
