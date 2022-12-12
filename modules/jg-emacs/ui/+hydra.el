;;; +hydra.el -*- lexical-binding: t; -*-

(load! "+sub-hydras")

;; Row padder:
;; "*                   ^^ *"
;;

;;-- stacking
(defvar jg-ui-toggle-hydra-stack nil)
(defun +jghdoc (var)
  (if var 1 0)
  )
(defun +jgh-push (func)
  (push func jg-ui-toggle-hydra-stack)
  )
(defun +jgh-pop ()
  (interactive)
  (when jg-ui-toggle-hydra-stack
    (funcall-interactively (pop jg-ui-toggle-hydra-stack))
    )
  )

;;-- end stacking


(defhydra +jg-ui-toggle-hydra ()
  "
_v_isuals    _g_uides       w_r_apping    _n_avigation
^^^^^^^------------------------------------------------
_h_l-line    _w_hitespace   _t_runcate    _a_uto-hide
_m_odeline ^^^^                           _c_ursor
^^^^                        _s_martparen  _e_ww
^^^^^^                                    _p_review
"

  ("v" (progn (+jgh-push #'+jg-ui-toggle-hydra/body)
              (+jg-ui-visuals-hydra/body)) nil :exit t)
  ("g" (progn (+jgh-push #'+jg-ui-toggle-hydra/body)
              (+jg-ui-guides-hydra/body)) nil :exit t)
  ("r" (progn (+jgh-push #'+jg-ui-toggle-hydra/body)
              (+jg-ui-wrap-hydra/body)) nil :exit t)
  ("n" (progn (+jgh-push #'+jg-ui-toggle-hydra/body)
              (+jg-ui-nav-hydra/body)) nil :exit t)
  ("s" (message "Smartparen: %s"
                (setq sp-autoinsert-pair (not sp-autoinsert-pair)))
   nil :exit nil)
  ("m" #'hide-mode-line-mode nil :exit nil)
  ("h" #'global-hl-line-mode          nil  :exit nil)
  ("t" #'toggle-truncate-lines        nil  :exit nil)
  ("w" #'whitespace-mode              nil  :exit nil)
  ("a" (message "Autohide start hidden %s"
                (setq autohide-minor-mode-start-hidden (not autohide-minor-mode-start-hidden))
                nil  :exit nil))
  ("c" #'global-centered-cursor-mode  nil  :exit nil)
  ("e" #'+jg-browse-toggle-browsing   nil  :exit nil)
  ("p" #'+jg-browse-toggle-preview    nil  :exit nil)
  ("q" (setq jg-ui-toggle-hydra-stack nil) :exit t)
)
