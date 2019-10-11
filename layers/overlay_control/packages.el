(defconst overlay_control-packages
  '(
    helm
    (overlay-ctrl-minor-mode :location local)
    (font-lock+ :location (recipe :fetcher git :url "https://github.com/emacsmirror/font-lock-plus"))
    )
  )

(defun overlay_control/init-overlay-ctrl-minor-mode ()
  (use-package overlay-ctrl-minor-mode
    :commands (overlay-ctrl-minor-mode global-overlay-ctrl-mode)
    :config
    (spacemacs|define-transient-state overlay-apply
      :title "Overlay Application"
      :doc (concat "
    | Overlay Commands                 ^^|
    |----------------------------------^^|--------------------------
    | [_q_] Quit                         | [_r_] Overlay Region
    | [_a_] Add To Register              | [_w_] Overlay Word
    | [_b_] Apply Register To Buffer     | [_h_] Hide-Overlay
    | [_c_] Clear Register               | [_C_] Clear Overlays
    |----------------------------------^^---------------------------
    | [_d_] Set Overlay Type             | [_D_] Set Hide Text
")
      :bindings
      ("q" overlay_control/re-run-overlays :exit t)
      ("a" overlay_control/add-to-register)
      ("b" overlay_control/apply-overlay-register)
      ("C" overlay_control/clear-on-buffer)
      ("c" overlay_control/clear-register)
      ("d" overlay_control/overlay-helm)
      ("r" overlay_control/apply-to-region)
      ("h" overlay_control/apply-hide-overlay)
      ("D" overlay_control/set-hide-text)
      ("w" overlay_control/overlay-word)
      )
    (spacemacs/set-leader-keys
      "a /" 'spacemacs/overlay-apply-transient-state/body
      )
    )
  )

(defun overlay_control/pre-init-helm ()
  (spacemacs|use-package-add-hook helm
    :post-config
    ;; build helm for selecting overlay type
    (setq overlay_control/overlay-helm-source
          (helm-make-source "Overlay Helm" 'helm-source-sync
            :action (helm-make-actions "Set Next Overlay" 'overlay_control/set-overlay)
            :candidates overlay_control/helm-candidates
            :nomark t
            )
          )
    (defun overlay_control/overlay-helm ()
      (interactive)
      (helm :sources overlay_control/overlay-helm-source
            :buffer "*Helm Overlays*"
            )
      )
    )
  )

(defun overlay_control/init-font-lock+ ()
  (use-package font-lock+)
  )
