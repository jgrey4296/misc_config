;;; completion/ivy/autoload/hydras.el -*- lexical-binding: t; -*-

(defhydra hydra-ivy (:hint nil :color pink)
  (format "%s\n" (+jg-hydra-format-columns
                  '("|Top-to-bottom" "_g_: Top" "_G_: Bottom" "_u_: scroll up" "_d_: scroll down")
                  '(blank k blank j)
                  '("|Call" forward "_RET_: done" "_TAB_: alt-done" occur)
                  '("|Cancel" insert quit)
                  `(,(format "%-12s" "|options") "_c_alling: %-8s(if ivy-calling \"on\" \"off\")" "_m_atcher: %-8s(ivy--matcher-desc)" "_t_runcate: %-7`truncate-lines" "_C_ase-fold: %-6`ivy-case-fold-search")
                  '("|Actions _w_/_s_/_a_" "%8s(ivy-action-name)" "_<_/_>_: shrink/grow")
                  ))
  ;; arrows
  ("h" ivy-beginning-of-buffer)
  ("j" ivy-next-line)
  ("k" ivy-previous-line)
  ("g" ivy-beginning-of-buffer)
  ("G" ivy-end-of-buffer)
  ("d" ivy-scroll-up-command)
  ("u" ivy-scroll-down-command)
  ("e" ivy-scroll-down-command)
  ;; actions
  ("q" keyboard-escape-quit :exit t)
  ("<escape>" keyboard-escape-quit :exit t)
  ("TAB" ivy-alt-done :exit nil)
  ("RET" ivy-done :exit t)
  ("i" nil)
  ("C-SPC" ivy-call-and-recenter :exit nil)
  ("f" ivy-call)
  ("c" ivy-toggle-calling)
  ("m" ivy-toggle-fuzzy)
  ("t" (setq truncate-lines (not truncate-lines)))
  ("o" ivy-occur :exit t)
  ("w" ivy-prev-action)
  ("s" ivy-next-action)
  ("a" nil) ;;ivy-hydra--read-action)
  (">" ivy-minibuffer-grow)
  ("<" ivy-minibuffer-shrink)
  ("C" ivy-toggle-case-fold)
  )

(unless (fboundp 'ivy-dispatching-done-hydra-exit-keys)
    (setq ivy-dispatching-done-hydra-exit-keys nil)
  )
(add-to-list 'ivy-dispatching-done-hydra-exit-keys '("C-o" nil))

(map! :map ivy-minibuffer-map
      [remap doom/delete-backward-word] #'ivy-backward-kill-word
      "C-c C-e"                         #'+ivy/woccur
      "C-o"                             #'ivy-dispatching-done
      "M-o"                             #'hydra-ivy/body
      )
