;;; ui/popup/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+modes")
(load! "+macros")

;; Default popup rules & bootstrap
(set-popup-rules!
  (when (modulep! +all)
    '(("^\\*"  :slot 1 :vslot -1 :select t)
      ("^ \\*" :slot 1 :vslot -1 :size +popup-shrink-to-fit)))
  (when (modulep! +defaults)
    '(("^\\*Completions" :ignore t)
      ("^\\*Local variables\\*$"
       :vslot -1 :slot 1 :size +popup-shrink-to-fit)
      ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)"
       :vslot -2 :size 0.3  :autosave t :quit t :ttl nil)
      ("^\\*\\(?:doom \\|Pp E\\)"  ; transient buffers (no interaction required)
       :vslot -3 :size +popup-shrink-to-fit :autosave t :select ignore :quit t :ttl 0)
      ("^\\*doom:"  ; editing buffers (interaction required)
       :vslot -4 :size 0.35 :autosave t :select t :modeline t :quit nil :ttl t)
      ("^\\*doom:\\(?:v?term\\|e?shell\\)-popup"  ; editing buffers (interaction required)
       :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil)
      ("^\\*\\(?:Wo\\)?Man "
       :vslot -6 :size 0.45 :select t :quit t :ttl 0)
      ("^\\*Calc"
       :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0)
      ("^\\*Customize"
       :slot 2 :side right :size 0.5 :select t :quit nil)
      ("^ \\*undo-tree\\*"
       :slot 2 :side left :size 20 :select t :quit t)
      ;; `help-mode', `helpful-mode'
      ("^\\*\\([Hh]elp\\|Apropos\\)"
       :slot 2 :vslot -8 :size 0.42 :select t)
      ("^\\*eww\\*"  ; `eww' (and used by dash docsets)
       :vslot -11 :size 0.35 :select t)
      ("^\\*xwidget"
       :vslot -11 :size 0.35 :select nil)
      ("^\\*info\\*$"  ; `Info-mode'
       :slot 2 :vslot 2 :size 0.45 :select t)))
  '(("^\\*Warnings" :vslot 99 :size 0.25)
    ("^\\*Backtrace" :vslot 99 :size 0.4 :quit nil)
    ("^\\*CPU-Profiler-Report "    :side bottom :vslot 100 :slot 1 :height 0.4 :width 0.5 :quit nil)
    ("^\\*Memory-Profiler-Report " :side bottom :vslot 100 :slot 2 :height 0.4 :width 0.5 :quit nil)
    ("^\\*Process List\\*" :side bottom :vslot 101 :size 0.25 :select t :quit t)
    ("^\\*\\(?:Proced\\|timer-list\\|Abbrevs\\|Output\\|Occur\\|unsent mail.*?\\|message\\)\\*" :ignore t)))

(add-hook 'doom-init-ui-hook #'+popup-mode 'append)

(add-hook! '+popup-buffer-mode-hook
           #'+popup-adjust-fringes-h
           #'+popup-adjust-margins-h
           #'+popup-set-modeline-on-enable-h
           #'+popup-unset-modeline-on-disable-h)


;;
;;; Hacks

(load! "+hacks")

(provide 'module/popup)
