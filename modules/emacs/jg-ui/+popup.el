;;; util/window-control/+popup.el -*- lexical-binding: t; -*-

(message "Setting up popup rules: %s" (current-time-string))
;; Setup popup vars:
(setq +popup--display-buffer-alist nil
      display-buffer-alist         nil
      )

(remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h)

(set-popup-rules!
  ;; My rulesets
  '(("^\\*shell"             :side bottom :ttl nil :height 0.3 :quit t :select t)
    ("\\*.*?scratch.*?\\*"   :side right  :ttl nil :width  50  :quit t :select t)
    ("^\\*Messages"          :side left   :ttl nil :width  0.4 :quit t :select nil)
    ("^\\*compilation"       :side bottom :ttl 5   :height 0.4 :quit t :select nil)
    ("^\\*Pp Eval Output\\*" :side right  :ttl 5   :width 0.4  :quit t :select nil)
    ("^\\*Buffer Locals:"    :side right  :ttl 5   :width 0.4  :quit t :select nil)
    )

  '(("^\\*Warnings" :vslot 99 :size 0.25)
    ("^\\*Backtrace" :vslot 99 :size 0.4 :quit nil)
    ("^\\*CPU-Profiler-Report "    :side bottom :vslot 100 :slot 1 :height 0.4 :width 0.5 :quit nil)
    ("^\\*Memory-Profiler-Report " :side bottom :vslot 100 :slot 2 :height 0.4 :width 0.5 :quit nil)
    ("^\\*Process List\\*" :side bottom :vslot 101 :size 0.25 :select t :quit t)
    ("^\\*\\(?:Proced\\|timer-list\\|Abbrevs\\|Output\\|Occur\\|unsent mail\\)\\*" :ignore t))

  ;; Doom Standards
  (when (featurep! +all)
    '(("^\\*"  :slot 1 :vslot -1 :select t)
      ("^ \\*" :slot 1 :vslot -1 :size +popup-shrink-to-fit)))

  (when (featurep! +defaults)
    '(("^\\*Completions" :ignore t)
      ("^\\*Local variables\\*$" :vslot -1 :slot 1 :size +popup-shrink-to-fit)
      ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)" :vslot -2 :size 0.3  :autosave t :quit t :ttl nil)
      ;; transient buffers (no interaction required)
      ("^\\*\\(?:doom \\|Pp E\\)" :vslot -3 :size +popup-shrink-to-fit :autosave t :select ignore :quit t :ttl 0)
      ;; editing buffers (interaction required)
      ("^\\*doom:" :vslot -4 :size 0.35 :autosave t :select t :modeline t :quit nil :ttl t)
      ;; editing buffers (interaction required)
      ("^\\*doom:\\(?:v?term\\|e?shell\\)-popup" :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil)
      ("^\\*\\(?:Wo\\)?Man " :vslot -6 :size 0.45 :select t :quit t :ttl 0)
      ("^\\*Calc" :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0)
      ("^\\*Customize" :slot 2 :side right :size 0.5 :select t :quit nil)
      ("^ \\*undo-tree\\*" :slot 2 :side left :size 20 :select t :quit t)
      ;; `help-mode', `helpful-mode'
      ("^\\*\\([Hh]elp\\|Apropos\\)" :slot 2 :vslot -8 :size 0.35 :select t)
      ;; `eww' (and used by dash docsets)
      ("^\\*eww\\*" :vslot -11 :size 0.35 :select t)
      ;; `Info-mode'
      ("^\\*info\\*$" :slot 2 :vslot 2 :size 0.45 :select t)))

  )

;;(set-popup-rule! PREDICATE &key
;; IGNORE ACTIONS SIDE SIZE WIDTH HEIGHT SLOT
;; VSLOT TTL QUIT SELECT MODELINE AUTOSAVE PARAMETERS)
;;(set-popup-rules! &rest RULESETS)
