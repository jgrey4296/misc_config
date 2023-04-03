;;; +vars.el -*- lexical-binding: t; -*-

(defconst +popup-window-parameters '(ttl quit select modeline popup)
  "A list of custom parameters to be added to `window-persistent-parameters'.
Modifying this has no effect, unless done before ui/popup loads.")
(defvar +popup-default-display-buffer-actions
  '(+popup-display-buffer-stacked-side-window-fn)
  "The functions to use to display the popup buffer.")
(defvar +popup-default-alist
  '((window-height . 0.16) ; remove later
    (reusable-frames . visible))
  "The default alist for `display-buffer-alist' rules.")
(defvar +popup-default-parameters
  '((transient . t)   ; remove later
    (quit . t)        ; remove later
    (select . ignore) ; remove later
    (no-other-window . t))
  "The default window parameters.")
(defvar +popup-margin-width 1
  "Size of the margins to give popup windows. Set this to nil to disable margin
adjustment.")
(defvar +popup--remember-last            t)
(defvar +popup--inhibit-transient        nil)
(defvar +popup--inhibit-select           nil)
(defvar +popup--last                     nil)
(defvar-local +popup--timer              nil)

(setq window-control-popup-persist-default '(:side bottom
                                             :height 0.3
                                             :quit t
                                             :select nil
                                             :modeline t
                                             :ttl nil)
      )
(setq jg-popup-ivy-predicate-patterns (rx (or "*helpful" "*helm-" "doom" "*dired-log" "magit" "*Free Keys")))
(setq neo-toggle-window-keep-p t)


;;-- fold spec
(after! jg-ui-reapply-hook-ready
  (+jg-fold-add-spec 'neotree
                     `((neotree-mode)
     :open-all   nil
     :close-all  neotree-collapse-all
     :toggle     nil
     :open       +neotree/expand-or-open
     :open-rec   nil
     :close      +neotree/collapse
     )))
;;-- end fold spec
