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

;;-- space remapping state
(defvar jg-insert-state-sep "_")

(evil-define-state jg-insert
  "Insert State with spaces remapped"
  :tag "<JGI>"
  :message "-- JG Insert --"
  :suppress-keymap nil
  :entry-hook (evil-start-track-last-insertion)
  :exit-hook (evil-stop-track-last-insertion)
  :input-method t
  )

(defun +jg-insert-state-set-sep (str)
  (interactive (list (read-string (format "Separator: ('%s') " jg-insert-state-sep))))
  (setq jg-insert-state-sep str)
  )

(map! :map evil-jg-insert-state-map
      "SPC" (cmd! (insert jg-insert-state-sep))
      "§"   #'+jg-insert-state-set-sep
      ;; TODO map a control to choose the SPC replacement
      )


;;-- end space remapping state

(map! :map jg-binding-normal-state-map
      :after jg-evil-bindings
      :desc "SPC? Insert" "I SPC" #'evil-jg-insert-state
      )

(provide '+state)
;;; +state.el ends here