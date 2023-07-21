;;; +evil-ex.el -*- lexical-binding: t; -*-

(evil-ex-define-cmd "repl"        #'+eval:repl)             ; invoke or send to repl
(evil-ex-define-cmd "h[elp]"      #'+evil:help)
