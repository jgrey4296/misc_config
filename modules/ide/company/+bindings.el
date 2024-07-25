;;; +bindings.el -*- lexical-binding: t; -*-

(evil-make-overriding-map jg-company-search-map 'insert)

(map! :map jg-company-search-map
      [escape] (cmd! (company-enable-overriding-keymap company-active-map))
      "|"      (cmd! (company-enable-overriding-keymap company-active-map))
      "RET"    #'company-complete-selection
      "DEL"    #'company-search-delete-char
      "Q"      #'company-search-abort
      "J"      #'company-select-next
      "K"      #'company-select-previous

      "a" #'company-search-printing-char
      "b" #'company-search-printing-char
      "c" #'company-search-printing-char
      "d" #'company-search-printing-char
      "e" #'company-search-printing-char
      "f" #'company-search-printing-char
      "g" #'company-search-printing-char
      "h" #'company-search-printing-char
      "i" #'company-search-printing-char
      "j" #'company-search-printing-char
      "k" #'company-search-printing-char
      "l" #'company-search-printing-char
      "m" #'company-search-printing-char
      "n" #'company-search-printing-char
      "o" #'company-search-printing-char
      "p" #'company-search-printing-char
      "q" #'company-search-printing-char
      "r" #'company-search-printing-char
      "s" #'company-search-printing-char
      "t" #'company-search-printing-char
      "u" #'company-search-printing-char
      "v" #'company-search-printing-char
      "w" #'company-search-printing-char
      "x" #'company-search-printing-char
      "y" #'company-search-printing-char
      "z" #'company-search-printing-char

      "-" #'company-search-printing-char
      "_" #'company-search-printing-char

      "0" #'company-search-printing-char
      "1" #'company-search-printing-char
      "2" #'company-search-printing-char
      "3" #'company-search-printing-char
      "4" #'company-search-printing-char
      "5" #'company-search-printing-char
      "6" #'company-search-printing-char
      "7" #'company-search-printing-char
      "8" #'company-search-printing-char
      "9" #'company-search-printing-char

      )

(map! :map jg-company-search-map  ; applies to `company-filter-map' too
      "C-n"     #'company-select-next-or-abort
      "C-p"     #'company-select-previous-or-abort
      "C-j"     #'company-select-next-or-abort
      "C-k"     #'company-select-previous-or-abort
      "C-s"     #'company-filter-candidates
      [escape]  #'company-search-abort
      )

(map! :map jg-company-active-map
      [escape] #'company-abort
      "j" #'company-select-next
      "k" #'company-select-previous
      "f" #'company-filter-candidates
      "J" #'company-next-page
      "K" #'company-previous-page
      "q" #'company-abort
      "<return>" #'company-complete-selection
      :g "RET" #'company-complete-selection
      :g "TAB" #'company-show-doc-buffer
      "|" #'company-other-backend
      )

(map! :map jg-company-active-map
      ;; :i "C-@"    (cmds! (not (minibufferp)) #'company-complete-common)
      ;; :i "C-SPC"  (cmds! (not (minibufferp)) #'company-complete-common)

      "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
      "C-n"     #'company-select-next
      "C-p"     #'company-select-previous
      "C-j"     #'company-select-next
      "C-k"     #'company-select-previous
      "C-h"     #'company-show-doc-buffer
      "C-u"     #'company-previous-page
      "C-d"     #'company-next-page
      "C-s"     #'company-filter-candidates
      "C-S-s"   #'counsel-company
      "C-SPC"   #'company-complete-common
      ;; "TAB"     (cmd! (company-cancel) (indent-for-tab-command))
      ;; [tab]     (cmd! (company-cancel) (indent-for-tab-command))
      ;; "TAB"     #'company-complete-common-or-cycle
      ;; [tab]     #'company-complete-common-or-cycle
      [backtab] #'company-select-previous
      [f1]      nil
         )

(map! :map jg-binding-insert-state-map
      ;; "TAB" #'+jg-snippets-complete-or-snippet
      "TAB" #'company-complete
      ;; "TAB" #'indent-for-tab-command
      )

(after! company
  (setq company-active-map jg-company-active-map
        company-search-map jg-company-search-map
        )
 )
