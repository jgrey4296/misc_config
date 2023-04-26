;;; editor/window-control/+vars.el -*- lexical-binding: t; -*-

;;-- defs
(defvar +ligatures-extra-symbols
  '(;; org
    :name          "»"
    :src_block     "»"
    :src_block_end "«"
    :quote         "“"
    :quote_end     "”"
    ;; Functional
    :lambda        "λ"
    :def           "ƒ"
    :composition   "∘"
    :map           "↦"
    ;; Types
    :null          "∅"
    :true          "𝕋"
    :false         "𝔽"
    :int           "ℤ"
    :float         "ℝ"
    :str           "𝕊"
    :bool          "𝔹"
    :list          "𝕃"
    ;; Flow
    :not           "￢"
    :in            "∈"
    :not-in        "∉"
    :and           "∧"
    :or            "∨"
    :for           "∀"
    :some          "∃"
    :return        "⟼"
    :yield         "⟻"
    ;; Other
    :union         "⋃"
    :intersect     "∩"
    :diff          "∖"
    :tuple         "⨂"
    :pipe          "" ;; FIXME: find a non-private char
    :dot           "•")
  "Maps identifiers to symbols, recognized by `set-ligatures'.

This should not contain any symbols from the Unicode Private Area! There is no
universal way of getting the correct symbol as that area varies from font to
font.")

(defvar +ligatures-extra-alist '((t))
  "A map of major modes to symbol lists (for `prettify-symbols-alist').")

(defvar +ligatures-composition-alist
  '((?!  . "\\(?:!\\(?:==\\|[!=]\\)\\)")                                      ; (regexp-opt '("!!" "!=" "!=="))
    (?#  . "\\(?:#\\(?:###?\\|_(\\|[#(:=?[_{]\\)\\)")                         ; (regexp-opt '("##" "###" "####" "#(" "#:" "#=" "#?" "#[" "#_" "#_(" "#{"))
    (?$  . "\\(?:\\$>>?\\)")                                                  ; (regexp-opt '("$>" "$>>"))
    (?%  . "\\(?:%%%?\\)")                                                    ; (regexp-opt '("%%" "%%%"))
    (?&  . "\\(?:&&&?\\)")                                                    ; (regexp-opt '("&&" "&&&"))
    (?*  . "\\(?:\\*\\(?:\\*[*/]\\|[)*/>]\\)?\\)")                            ; (regexp-opt '("*" "**" "***" "**/" "*/" "*>" "*)"))
    (?+  . "\\(?:\\+\\(?:\\+\\+\\|[+:>]\\)?\\)")                              ; (regexp-opt '("+" "++" "+++" "+>" "+:"))
    (?-  . "\\(?:-\\(?:-\\(?:->\\|[>-]\\)\\|<[<-]\\|>[>-]\\|[:<>|}~-]\\)\\)") ; (regexp-opt '("--" "---" "-->" "--->" "->-" "-<" "-<-" "-<<" "->" "->>" "-}" "-~" "-:" "-|"))
    (?.  . "\\(?:\\.\\(?:\\.[.<]\\|[.=>-]\\)\\)")                             ; (regexp-opt '(".-" ".." "..." "..<" ".=" ".>"))
    (?/  . "\\(?:/\\(?:\\*\\*\\|//\\|==\\|[*/=>]\\)\\)")                      ; (regexp-opt '("/*" "/**" "//" "///" "/=" "/==" "/>"))
    (?:  . "\\(?::\\(?:::\\|[+:<=>]\\)?\\)")                                  ; (regexp-opt '(":" "::" ":::" ":=" ":<" ":=" ":>" ":+"))
    (?\; . ";;")                                                              ; (regexp-opt '(";;"))
    (?0  . "0\\(?:\\(x[a-fA-F0-9]\\).?\\)") ; Tries to match the x in 0xDEADBEEF
    ;; (?x . "x") ; Also tries to match the x in 0xDEADBEEF
    ;; (regexp-opt '("<!--" "<$" "<$>" "<*" "<*>" "<**>" "<+" "<+>" "<-" "<--" "<---" "<->" "<-->" "<--->" "</" "</>" "<<" "<<-" "<<<" "<<=" "<=" "<=<" "<==" "<=>" "<===>" "<>" "<|" "<|>" "<~" "<~~" "<." "<.>" "<..>"))
    (?<  . "\\(?:<\\(?:!--\\|\\$>\\|\\*\\(?:\\*?>\\)\\|\\+>\\|-\\(?:-\\(?:->\\|[>-]\\)\\|[>-]\\)\\|\\.\\(?:\\.?>\\)\\|/>\\|<[<=-]\\|=\\(?:==>\\|[<=>]\\)\\||>\\|~~\\|[$*+./<=>|~-]\\)\\)")
    (?=  . "\\(?:=\\(?:/=\\|:=\\|<[<=]\\|=[=>]\\|>[=>]\\|[=>]\\)\\)")         ; (regexp-opt '("=/=" "=:=" "=<<" "==" "===" "==>" "=>" "=>>" "=>=" "=<="))
    (?>  . "\\(?:>\\(?:->\\|=>\\|>[=>-]\\|[:=>-]\\)\\)")                      ; (regexp-opt '(">-" ">->" ">:" ">=" ">=>" ">>" ">>-" ">>=" ">>>"))
    (??  . "\\(?:\\?[.:=?]\\)")                                               ; (regexp-opt '("??" "?." "?:" "?="))
    (?\[ . "\\(?:\\[\\(?:|]\\|[]|]\\)\\)")                                    ; (regexp-opt '("[]" "[|]" "[|"))
    (?\\ . "\\(?:\\\\\\\\[\\n]?\\)")                                          ; (regexp-opt '("\\\\" "\\\\\\" "\\\\n"))
    (?^  . "\\(?:\\^==?\\)")                                                  ; (regexp-opt '("^=" "^=="))
    (?w  . "\\(?:wwww?\\)")                                                   ; (regexp-opt '("www" "wwww"))
    (?{  . "\\(?:{\\(?:|\\(?:|}\\|[|}]\\)\\|[|-]\\)\\)")                      ; (regexp-opt '("{-" "{|" "{||" "{|}" "{||}"))
    (?|  . "\\(?:|\\(?:->\\|=>\\||=\\|[]=>|}-]\\)\\)")                        ; (regexp-opt '("|=" "|>" "||" "||=" "|->" "|=>" "|]" "|}" "|-"))
    (?_  . "\\(?:_\\(?:|?_\\)\\)")                                            ; (regexp-opt '("_|_" "__"))
    (?\( . "\\(?:(\\*\\)")                                                    ; (regexp-opt '("(*"))
    (?~  . "\\(?:~\\(?:~>\\|[=>@~-]\\)\\)"))                                  ; (regexp-opt '("~-" "~=" "~>" "~@" "~~" "~~>"))
  "An alist of all ligatures used by `+ligatures-extras-in-modes'.

The car is the character ASCII number, cdr is a regex which will call
`font-shape-gstring' when matched.

Because of the underlying code in :ui ligatures module, the regex should match a
string starting with the character contained in car.

This variable is used only if you built Emacs with Harfbuzz on a version >= 28")

(defvar +ligatures-in-modes
  '(not special-mode comint-mode eshell-mode term-mode vterm-mode Info-mode
        elfeed-search-mode elfeed-show-mode)
  "List of major modes where ligatures should be enabled.

  If t, enable it everywhere (except `fundamental-mode').
  If the first element is 'not, enable it in any mode besides what is listed.
  If nil, don't enable ligatures anywhere.")

(defvar +ligatures-extras-in-modes t
  "List of major modes where extra ligatures should be enabled.

Extra ligatures are mode-specific substituions, defined in
`+ligatures-extra-symbols' and assigned with `set-ligatures!'. This variable
controls where these are enabled.

  If t, enable it everywhere (except `fundamental-mode').
  If the first element is 'not, enable it in any mode besides what is listed.
  If nil, don't enable these extra ligatures anywhere (though it's more
efficient to remove the `+extra' flag from the :ui ligatures module instead).")

(defvar +ligatures--init-font-hook nil)

(defun +ligatures--correct-symbol-bounds (ligature-alist)
  "Prepend non-breaking spaces to a ligature.

This way `compose-region' (called by `prettify-symbols-mode') will use the
correct width of the symbols instead of the width measured by `char-width'."
  (let ((len (length (car ligature-alist)))
        (acc (list   (cdr ligature-alist))))
    (while (> len 1)
      (setq acc (cons #X00a0 (cons '(Br . Bl) acc))
            len (1- len)))
    (cons (car ligature-alist) acc)))

(defun +ligatures--enable-p (modes)
  "Return t if ligatures should be enabled in this buffer depending on MODES."
  (unless (eq major-mode 'fundamental-mode)
    (or (eq modes t)
        (if (eq (car modes) 'not)
            (not (apply #'derived-mode-p (cdr modes)))
          (apply #'derived-mode-p modes)))))

(defun +ligatures-init-buffer-h ()
  "Set up ligatures for the current buffer.

Extra ligatures are mode-specific substituions, defined in
`+ligatures-extra-symbols', assigned with `set-ligatures!', and made possible
with `prettify-symbols-mode'. This variable controls where these are enabled.
See `+ligatures-extras-in-modes' to control what major modes this function can
and cannot run in."
  (when after-init-time
    (let ((in-mode-p
           (+ligatures--enable-p +ligatures-in-modes))
          (in-mode-extras-p
           (and (modulep! +extra)
                (+ligatures--enable-p +ligatures-extras-in-modes))))
      (when in-mode-p
        (if (boundp '+ligature--composition-table)
            (setq-local composition-function-table +ligature--composition-table)
          (run-hooks '+ligatures--init-font-hook)
          (setq +ligatures--init-font-hook nil)))
      (when in-mode-extras-p
        (prependq! prettify-symbols-alist
                   (alist-get major-mode +ligatures-extra-alist)))
      (when (and (or in-mode-p in-mode-extras-p)
                 prettify-symbols-alist)
        (when prettify-symbols-mode
          (prettify-symbols-mode -1))
        (prettify-symbols-mode +1)))))


;;-- end defs

(setq-default highlight-parentheses-delay      0.3
              jg-ui-default-face-gen-palette-dir "/Volumes/documents/github/jgrey4296.github.io/resources/palettes/"
              display-line-numbers             t
              display-line-numbers-major-tick  20
)

(after! neotree
  (push "^__pycache__$" neo-hidden-regexp-list)
  (push "^G\\(PATH\\|R?TAGS\\)$" neo-hidden-regexp-list)
  (push "^__init__.py$" neo-hidden-regexp-list)
  )

;;-- smartparens
(setq-default sp-autoinsert-pair t
              sp-autoinsert-quote-if-followed-by-closing-pair nil
              sp-escape-char ""
              )

;;-- end smartparens

;;-- parens
(setq-default highlight-parentheses-colors            '("black")
              highlight-parentheses-background-colors '("#60aa00" "yellow" "#da8548" "#d02b61")
              global-hl-line-modes '(bibtex-mode prog-mode text-mode conf-mode special-mode org-agenda-mode comint-mode)
              )

;;-- end parens

;;-- modeline
;; We display project info in the modeline ourselves
(setq projectile-dynamic-mode-line nil
      ;; Set these early so they don't trigger variable watchers
      doom-modeline-bar-width 3
      doom-modeline-github          nil
      doom-modeline-mu4e            nil
      doom-modeline-persp-name      nil
      doom-modeline-minor-modes     nil
      doom-modeline-major-mode-icon nil
      doom-modeline-buffer-file-name-style 'relative-from-project
      ;; Only show file encoding if it's non-UTF-8 and different line endings
      ;; than the current OSes preference
      doom-modeline-buffer-encoding 'nondefault
      doom-modeline-default-eol-type
      (cond (IS-MAC 2)
            (IS-WINDOWS 1)
            (0))

      doom-modeline-icon nil
      )

;;-- end modeline
