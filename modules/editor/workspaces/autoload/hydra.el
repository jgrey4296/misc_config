;;; +hydra.el -*- lexical-binding: t; -*-
;;
;; TODO hydra for control of workspaces, windows, window-ring settings

;;;###autoload
(after! hydra
  (defhydra hydra-workspace ()
    (format "%s\n" (+jg-hydra-format-columns
                    '("|Projects"
                      "_?_ project type"
                      "_`_ root"
                      "walk next"
                      "Walk next (quit)"
                      sidebar
                      todos
                      )
                    '("|Projects"
                      "Clear cache"
                      "Project cache clear"
                      configs
                      editorconfig
                      locals
                      recent
                      )
                    '("|Workspaces"

                      )
                    '("|Windows"
                      "Auto-_b_alance"
                      dedicate
                      "horizontal Shrink"
                      "vertical Shrink"
                      "_/_ toggle layout"
                      "_\\_ rotate buffers"
                      )
                    '(blank
                      "%-10(+jg-hydra-doc evil-auto-balance-windows)"
                      "%-10(window-dedicated-p (selected-window))"
                      )
                    ))
    ;; Projects
    ("`" (progn (find-file (doom-project-root))) nil                           :exit t)
    ("w" project-walk-next                                                     :exit nil)
    ("W" project-walk-next                                                     :exit t)
    ("s" +neotree/find-this-file                                               :exit t)
    ("t" magit-todos-list                                                      :exit t)
    ("?" +jg-projects-detect-type                                              :exit t)
    ("C" projectile-invalidate-cache                                           :exit t)
    ("P" projectile-invalidate-cache                                           :exit t)
    ("e" editorconfig-find-current-editorconfig                                :exit t)
    ("l" projectile-edit-dir-locals                                            :exit t)
    ("c" +jg-projects-open-configs                                             :exit t)
    ("r" projectile-recentf                                                    :exit t)

    ;; Workspaces

    ;; Windows
    ("h" shrink-window-horizontally nil                                        :exit nil)
    ("v" shrink-window nil                                                     :exit nil)
    ("b" (setq evil-auto-balance-windows (not evil-auto-balance-windows))  nil :exit nil)
    ("/"  +jg-ui-window-layout-toggle                                          :exit nil)
    ("\\" +jg-ui-window-rotate-forward                                         :exit nil)
    ("d" +jg-ui-toggle-window-dedication                                       :exit nil)
    )

  (defhydra hydra-window-ring ()
    (format "%s\n" (+jg-hydra-format-columns
                    '("|Window-Ring"
                      new
                      convert
                      deconvert
                      "_K_ clear"
                      reset
                      print
                      Edit
                      )
                    '("%-10(persp-parameter 'window-ring)"
                      loop
                      expand
                      add
                      Remove
                      "c_h_oose"
                      )
                    '(blank
                      "%-4(persp-parameter 'window-ring-loop)"
                      )
                    ))
    ;; Window Ring
    ("p" window-ring-print-order        :exit t)
    ("E" window-ring-edit-order         :exit t)
    ("l" window-ring-toggle-loop        :exit nil)
    ("n" window-ring-new                :exit t)
    ("c" window-ring-convert            :exit t)
    ("d" window-ring-deconvert          :exit t)
    ("e" window-ring-shrink-sides       :exit t)
    ("r" window-ring-reset-columns      :exit t)
    ("K" window-ring-clear-ring         :exit t)
    ("a" window-ring-add-current-buffer :exit t)
    ("R" window-ring-remove-buffer      :exit t)
    ("h" nil)
    )
  )
