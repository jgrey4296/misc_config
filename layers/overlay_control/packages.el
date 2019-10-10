(defconst overlay_control-packages
  '(
    helm
    (overlay-ctrl-minor-mode :location local)
    )
  )

(defun overlay_control/init-overlay-ctrl-minor-mode ()
  (use-package overlay-ctrl-minor-mode
    :commands ()
    :config
    (spacemacs/set-leader-keys
      "s ." 'overlay_control/overlay-apply-hydra
      "r o" 'overlay_control/overlay-register-hydra
      )
    )
  )

(defun overlay_control/pre-init-helm ()
  (spacemacs|use-package-add-hook helm
    :post-config
    ;; build helm for selecting overlay type

    )
  )
