;;; completion/ivy/autoload/hydras.el -*- lexical-binding: t; -*-
(require 'ivy)
(require 'hydra)


;;;###autoload (autoload 'jg-ivy-hydra/body "ui/ivy/autoload/hydras" nil t)
(defhydra jg-ivy-hydra (:hint nil :color pink)
  (format "%s\n" (hydra-utils-format-columns
                  '("|Top-to-bottom" "_g_: Top" "_G_: Bottom" "_u_: scroll up" "_d_: scroll down")
                  '(blank k blank j)
                  '("|Call" forward "_RET_: done" "_TAB_: alt-done" occur)
                  '("|Cancel" insert quit)
                  `(,(format "%-12s" "|options") "_c_alling: %-8s(if ivy-calling \"on\" \"off\")" "_m_atcher: %-8s(ivy--matcher-desc)" "_t_runcate: %-7`truncate-lines" "_C_ase-fold: %-6`ivy-case-fold-search")
                  '("|Actions _[_/_]_/_a_" "%8s(ivy-action-name)" "_<_/_>_: shrink/grow")
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
  ("C-SPC" ivy-call-and-recenter :exit nil)
  ("f" ivy-call)
  ("c" ivy-toggle-calling)
  ("m" ivy-toggle-fuzzy)
  ("t" (setq truncate-lines (not truncate-lines)))
  ("o" ivy-occur :exit t)
  ("[" ivy-prev-action)
  ("]" ivy-next-action)
  ("a" ivy-hydra--read-action)
  (">" ivy-minibuffer-grow)
  ("<" ivy-minibuffer-shrink)
  ("C" ivy-toggle-case-fold)

  ("q" nil)
  ("i" nil)
  ("<escape>" keyboard-escape-quit :exit t)
  ("TAB" ivy-alt-done :exit nil)
  ("RET" ivy-done :exit t)
  ("|"   keyboard-escape-quit :exit t)
  )

(unless (fboundp 'ivy-dispatching-done-hydra-exit-keys)
    (setq ivy-dispatching-done-hydra-exit-keys nil)
  )
(add-to-list 'ivy-dispatching-done-hydra-exit-keys '("C-o" nil))