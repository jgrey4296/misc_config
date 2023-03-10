;;; timeline-insert-state.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: July 23, 2021
;; Modified: July 23, 2021
;; Version: 0.0.1
;; Homepage: https://github.com/johngrey/timeline-insert-state
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(evil-define-state timeline-insert
  "Insert State with spaces remapped"
  :tag "<TI>"
  :message "-- Timeline Insert --"
  :suppress-keymap nil
  :entry-hook (evil-start-track-last-insertion)
  :exit-hook (evil-stop-track-last-insertion)
  :input-method t
  )


(map! :map evil-timeline-insert-state-map
      "." ".BCE"
      "," " -> "
      ;; TODO map a control to choose the SPC replacement
      )


(map! :map timeline-mode-map
      ;; TODO activate the state somewhere better
      "i" #'evil-timeline-insert-state
      ;; tag
      ;; verify
      ;; amend
      ;; add person
      )

(provide 'timeline-insert-state)
;;; timeline-insert-state.el ends here
