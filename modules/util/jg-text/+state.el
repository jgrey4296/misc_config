;;; +state.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: July 21, 2021
;; Modified: July 21, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/johngrey/+state
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary: an insert state with controllable SPC replacement
;;
;;
;;
;;; Code:

(evil-define-state jg-insert
  "Insert State with spaces remapped"
  :tag "<JGI>"
  :message "-- JG Insert --"
  :suppress-keymap nil
  :entry-hook (evil-start-track-last-insertion)
  :exit-hook (evil-stop-track-last-insertion)
  :input-method t
  )


(map! :map evil-jg-insert-state-map
      "SPC" "_"
      ;; TODO map a control to choose the SPC replacement
      )


(map! :leader
      ;; TODO activate the state somewhere better
      "a" #'evil-jg-insert-state
      )

(provide '+state)
;;; +state.el ends here
