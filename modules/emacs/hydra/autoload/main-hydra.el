;;; +hydra.el -*- lexical-binding: t; -*-
;; Row padding:
;; "*                   ^^ *"
(require 'hydra-utils)
(require 'hydra)
(load! "sub-hydras")

;;;###autoload (autoload '+jy-hydra-ui-toggles/body "main-hydra")
(defhydra +jg-hydra-ui-toggles ()
  (format "%s\n"
          (hydra-utils-format-columns
           '(visuals hl-line modeline "high_l_ight changes")
           '(guides whitespace)
           '(w_r_apping truncate smartparen)
           '(navigation auto-hide cursor eww preview)
           )
          )

  ("v" (progn (hydra-utils-push #'+jg-hydra-ui-toggles/body)
              (+jg-hydra-visuals/body)) nil :exit t)
  ("g" (progn (hydra-utils-push #'+jg-hydra-ui-toggles/body)
              (+jg-hydra-guides/body)) nil :exit t)
  ("r" (progn (hydra-utils-push #'+jg-hydra-ui-toggles/body)
              (+jg-hydra-wrap/body)) nil :exit t)
  ("n" (progn (hydra-utils-push #'+jg-hydra-ui-toggles/body)
              (+jg-hydra-nav/body)) nil :exit t)
  ("s" (message "Smartparen: %s" (setq sp-autoinsert-pair (not sp-autoinsert-pair))) nil :exit nil)
  ("m" #'hide-mode-line-mode nil :exit nil)
  ("h" #'global-hl-line-mode          nil  :exit nil)
  ("t" #'toggle-truncate-lines        nil  :exit nil)
  ("w" #'whitespace-mode              nil  :exit nil)
  ("a" (message "Autohide start hidden %s"
                (setq autohide-minor-mode-start-hidden (not autohide-minor-mode-start-hidden))
                nil  :exit nil))
  ("c" #'global-centered-cursor-mode   nil  :exit nil)
  ("e" #'browse-select-toggle-browsing nil  :exit nil)
  ("p" #'browse-select-toggle-preview  nil  :exit nil)
  ("l" #'global-highlight-changes-mode nil :exit nil)
  ("q" #'hydra--utils-clear :exit t)
)
