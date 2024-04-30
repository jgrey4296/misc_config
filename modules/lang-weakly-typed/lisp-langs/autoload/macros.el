;;; macros.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defmacro with-temp-frame (setup body cleanup &rest handlers)
  (declare (indent defun))
  (let ((tempf-var (make-symbol "temp-frame"))
        (currf-var (make-symbol "curr-frame"))
        (windowconf-var (make-symbol "windowconf"))
        )
    `(let ((,tempf-var (make-frame))
           (,currf-var (selected-frame))
           (,windowconf-var (current-window-configuration))
           (inhibit-redisplay t)
           )
       ,setup
       (condition-case err
           (with-selected-frame ,tempf-var
             ,body
             )
         (:success ,cleanup
                   (delete-frame ,tempf-var)
                   (select-frame ,currf-var)
                   (set-window-configuration ,windowconf-var)
                   (redraw-frame)
                   )
         ,@handlers
         )
       )
    )
  )

;;;###autoload
(defmacro with-ert-frame (setup body cleanup)
  " Wrap ert test code into a temp frame, catching any test failed signals to clean up before reraising "
  (declare (indent defun))
  (let ((tempf-var (make-symbol "temp-frame"))
        (currf-var (make-symbol "curr-frame"))
        (windowconf-var (make-symbol "windowconf"))
        )
    `(let ((,tempf-var (make-frame))
           (,currf-var (selected-frame))
           (,windowconf-var (current-window-configuration))
           (inhibit-redisplay t)
           )
       ,setup
       (condition-case err
           (with-selected-frame ,tempf-var
             ,body
             )
         (:success ,cleanup
          (delete-frame ,tempf-var)
          (select-frame ,currf-var)
          (set-window-configuration ,windowconf-var)
          (redraw-frame)
          )
         (ert-test-failed
          ,cleanup
          (delete-frame ,tempf-var)
          (select-frame ,currf-var)
          (set-window-configuration ,windowconf-var)
          (redraw-frame)
          (signal 'ert-test-failed (cdr err)))
         )
       )
    )
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    April 25, 2024
;; Modified:   April 25, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; macros.el ends here
