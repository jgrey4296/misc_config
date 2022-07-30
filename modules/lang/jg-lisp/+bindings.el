;;; lang/emacs-lisp/+bindings.el -*- lexical-binding: t; -*-

(map! :map emacs-lisp-mode-map
      :localleader
      :desc "Sort Defuns" "S" #'+jg-lisp-sort-defuns
      :desc "Docs: Lisp"  "0" (cmd! (+jg-misc-browse-url "https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html"))
      :desc "Docs: Melpa" "1" (cmd! (+jg-misc-browse-url "https://melpa.org/#/"))
      (:prefix ("e" . "eval")
       :desc "Byte Compile" :n "c" #'byte-compile-file
       )
      )

(setq edebug-eval-mode-map (make-sparse-keymap))

(evil-make-intercept-map edebug-eval-mode-map)

(map! :map edebug-eval-mode-map
      ;; TODO add a reminder for this
      "s" #'edebug-step-mode
      "q" #'edebug-stop
      "n" #'edebug-next-mode
      "t" #'edebug-trace-mode
      "d" #'edebug-trace-display
      "i" #'edebug-step-in
      "o" #'edebug-step-out
      "g" #'edebug-go-mode
      )
