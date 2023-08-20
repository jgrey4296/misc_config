;;; +hydra.el -*- lexical-binding: t; -*-
;;
;; TODO hydra for control of workspaces, windows, carousel settings
(require 'hydra)

;;;###autoload (autoload 'hydra-workspace/body "ide/workspaces/autoload/hydras" nil t)
(defhydra hydra-workspace ()
  (format "%s\n" (hydra-utils-format-columns
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
                    "%-10(hydra-utils-doc evil-auto-balance-windows)"
                    "%-10(window-dedicated-p (selected-window))"
                    )
                  ))
  ;; Projects
  ("`" (progn (find-file (doom-project-root))) nil                           :exit t)
  ("w" zimmerframe-next                                                     :exit nil)
  ("W" zimmerframe-next                                                     :exit t)
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

;;;###autoload (autoload 'hydra-carousel/body "ide/workspaces/autoload/hydras" nil t)
(defhydra hydra-carousel()
  (format "%s\n" (hydra-utils-format-columns
                  '("|Carousel: %-10(persp-parameter 'carousel)"
                    new
                    convert
                    deconvert
                    "_K_ clear"
                    reset
                    print
                    Edit
                    )
                  '("|Window Claimed: %-10(window-parameter (selected-window) 'carousel-claimed)"
                    "_l_oop %-5(persp-parameter 'carousel-loop)"
                    expand
                    add
                    Remove
                    "c_h_oose"
                    )
                  '("|"
                    "_[_ move left"
                    "_]_ move right"
                    "claim _w_indow"
                    )
                  ))
  ;; Window Ring
  ("p" carousel-print-order        :exit t)
  ("E" carousel-edit-order         :exit t)
  ("l" carousel-toggle-loop        :exit nil)
  ("n" carousel-new                :exit t)
  ("c" carousel-convert            :exit t)
  ("d" carousel-deconvert          :exit t)
  ("e" carousel-shrink-sides       :exit t)
  ("r" carousel-reset-columns      :exit t)
  ("K" carousel-clear-ring         :exit t)
  ("a" carousel-add-current-buffer :exit t)
  ("R" carousel-remove-buffer      :exit t)
  ("[" carousel-move-buffer-left   :exit nil)
  ("]" carousel-move-buffer-right  :exit nil)
  ("w" carousel-claim-window       :exit nil)
  ("h" nil)
  )
