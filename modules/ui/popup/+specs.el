;;; config/ui/popup/config.el -*- lexical-binding: t; -*-

(doom-log "Setting up initial popup rules: %s" (current-time-string))
;; Setup popup vars:

(spec-handling-add! popup
                    '(my-rules
                      ("^\\*shell"            :side bottom :ttl nil :height 0.3 :quit t :select t :priority 100)
                      ("\*.*?scratch.*?\\*"   :side right  :ttl nil :width  50  :quit t :select t)
                      ("\\*Messages"          :side bottom :ttl nil :height 0.4 :quit t :select nil :priority 100)
                      ("\\*compilation"       :side bottom :ttl 5   :height 0.4 :quit t :select nil)
                      ("\\*Pp Eval Output\\*" :side right  :ttl 20  :width 0.4  :quit t :select nil)
                      ("\\*Buffer Locals:"    :side right  :ttl 5   :width 0.4  :quit t :select nil)
                      ;; ("\\s-\\*NeoTree\\*"   :side left   :ttl nil :height 0.4 :quit nil :select nil :priority -90)
                      ("\\*Async Shell Command\\*\\'" :actions (display-buffer-no-window))
                      )
                    )
(spec-handling-add! popup
                    '(general
                      ("^\\*Warnings" :vslot 99 :size 0.25)
                      ("^\\*Backtrace" :vslot 99 :size 0.4 :quit nil :ttl nil)
                      ("^\\*CPU-Profiler-Report "    :side bottom :vslot 100 :slot 1 :height 0.4 :width 0.5 :quit nil)
                      ("^\\*Memory-Profiler-Report " :side bottom :vslot 100 :slot 2 :height 0.4 :width 0.5 :quit nil)
                      ("^\\*Process List\\*" :side bottom :vslot 101 :size 0.25 :select t :quit t)
                      ("^\\*\\(?:Proced\\|timer-list\\|Abbrevs\\|Output\\|Occur\\|unsent mail\\)\\*" :ignore t)
                      )
                    )
(spec-handling-add! popup
                    '(doom
                      ;; Doom
                      ("^\\*info\\*\\'"                                          :slot 2 :vslot 2 :size 0.45 :select t :priority -100)
                      ;; `Info-mode'
                      ("^\\*eww\\*"                                            :vslot -11 :size 0.35 :select t :priority -100)
                      ("^\\*\\([Hh]elp\\|Apropos\\)"                           :slot 2 :vslot -8 :size 0.35 :select t :quit nil :priority -100)
                      ;; `help-mode', `helpful-mode'
                      ("^ \\*undo-tree\\*"                                     :slot 2 :side left :size 20 :select t :quit t :priority -100)
                      ("^\\*Customize"                                         :slot 2 :side right :size 0.5 :select t :quit nil :priority -100)
                      ("^\\*Calc"                                              :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0 :priority -100)
                      ("^\\*\\(?:Wo\\)?Man "                                   :vslot -6 :size 0.45 :select t :quit t :ttl 0 :priority -100)
                      ;; editing buffers (interaction required )
                      ("^\\*doom:\\(?:v?term\\|e?shell\\)-popup"               :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil :priority -100)
                      ;; editing buffers (interaction required
                      ("^\\*doom:"                                             :vslot -4 :size 0.35 :autosave t :select t :modeline t :quit nil :ttl t :priority -100)
                      ;; transient buffers (no interaction required)
                      ("^\\*\\(?:doom \\|Pp E\\)"                              :vslot -3 :size +popup-shrink-to-fit :autosave t :select ignore :quit t :ttl 0 :priority -100)
                      ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)" :vslot -2 :size 0.3  :autosave t :quit t :ttl nil :priority -100)
                      ("^\\*Local variables\\*\\'"                               :vslot -1 :slot 1 :size +popup-shrink-to-fit :priority -100)
                      ("^\\*Completions" :ignore t :priority -100)
                      ("^ \\*"                                                 :slot 1 :vslot -1 :size +popup-shrink-to-fit :priority -200)
                      ("^\\*"                                                  :slot 1 :vslot -1 :select t :priority -200)
                      )
                    )
;; IGNORE ACTIONS SIDE SIZE WIDTH HEIGHT SLOT
;; VSLOT TTL QUIT SELECT MODELINE AUTOSAVE PARAMETERS)
