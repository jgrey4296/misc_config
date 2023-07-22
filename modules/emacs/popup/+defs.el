;;; +defs.el -*- lexical-binding: t; -*-


(defvar +popup-default-display-buffer-actions
  '(+popup-display-buffer-stacked-side-window-fn)
  "The functions to use to display the popup buffer.")

(defconst +popup-window-parameters '(ttl quit select modeline popup)
  "A list of custom parameters to be added to `window-persistent-parameters'.
Modifying this has no effect, unless done before ui/popup loads.")

(defvar +popup-default-parameters
  '((transient . t)   ; remove later
    (quit . t)        ; remove later
    (select . nil) ; remove later
    (no-other-window . t))
  "The default window parameters.")

(defvar +popup-default-alist
  '((window-height . 0.16) ; remove later
    (reusable-frames . visible))
  "The default alist for `display-buffer-alist' rules.")

(defvar +popup-margin-width 1
  "Size of the margins to give popup windows. Set this to nil to disable margin
adjustment.")

(defvar +popup--remember-last            t)

(defvar +popup--inhibit-transient        nil)

(defvar +popup--inhibit-select           nil)

(defvar +popup--last                     nil)

(defvar-local +popup--timer              nil)
