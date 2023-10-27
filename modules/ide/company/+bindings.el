;;; +bindings.el -*- lexical-binding: t; -*-

(defvar jg-company-active-map (make-sparse-keymap))
(defvar jg-company-search-map (make-sparse-keymap))

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

(map! :map jg-company-active-map
      [escape] #'company-abort
      "j" #'company-select-next
      "k" #'company-select-previous
      "f" #'company-filter-candidates
      "J" #'company-next-page
      "K" #'company-previous-page
      "q" #'company-abort
      "RET" #'company-complete-selection
      )

(after! company
  (setq company-active-map jg-company-active-map
        company-search-map jg-company-search-map
        )
 )
