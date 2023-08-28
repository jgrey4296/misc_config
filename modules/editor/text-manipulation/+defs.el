;;; +defs.el -*- lexical-binding: t; -*-

(defvar jg-text-last-similarity-arg 1)

;;-- formatting

(defvar-local jg-text-whitespace-clean-hook '(#'delete-trailing-whitespace
                                              #'+jg-text-cleanup-whitespace)
  )

(defvar +word-wrap-extra-indent 'double
  "The amount of extra indentation for wrapped code lines.

When 'double, indent by twice the major-mode indentation.
When 'single, indent by the major-mode indentation.
When a positive integer, indent by this fixed amount.
When a negative integer, dedent by this fixed amount.

Otherwise no extra indentation will be used.")

(defvar +word-wrap-fill-style nil
  "How to handle `fill-column' in `+word-wrap-mode'.

When 'auto, long lines will soft-wrap at `fill-column'. If `auto-fill-mode' is
enabled, its behaviour will not be affected.

When 'soft, long lines will soft-wrap at `fill-column' and `auto-fill-mode' will
be forcibly disabled.

Otherwise long lines will soft-wrap at the window margin and `auto-fill-mode'
will not be affected.")

(defvar +word-wrap-disabled-modes
  '(fundamental-mode so-long-mode)
  "Major-modes where `+global-word-wrap-mode' should not enable
`+word-wrap-mode'.")

(defvar +word-wrap-visual-modes
  '(org-mode)
  "Major-modes where `+word-wrap-mode' should not use
`adaptive-wrap-prefix-mode'.")

(defvar +word-wrap-text-modes
  '(text-mode markdown-mode markdown-view-mode gfm-mode gfm-view-mode rst-mode
    latex-mode LaTeX-mode)
  "Major-modes where `+word-wrap-mode' should not provide extra indentation.")

(defvar +format-on-save-enabled-modes
  '(not emacs-lisp-mode    ; elisp's mechanisms are good enough
    sql-mode           ; sqlformat is currently broken
    tex-mode           ; latexindent is broken
    latex-mode
    org-msg-edit-mode) ; doesn't need a formatter
  "A list of major modes in which to reformat the buffer upon saving.

If this list begins with `not', then it negates the list.
If it is `t', it is enabled in all modes.
If nil, it is disabled in all modes, the same as if the +onsave flag wasn't
  used at all.

Irrelevant if you do not have the +onsave flag enabled for this module.")

(defvar +format-preserve-indentation t
  "If non-nil, the leading indentation is preserved when formatting the whole
buffer. This is particularly useful for partials.

Indentation is always preserved when formatting regions.")

(defvar-local +format-with nil
  "Set this to explicitly use a certain formatter for the current buffer.")

(defvar +format-with-lsp t
  "If non-nil, format with LSP formatter if it's available.

This can be set buffer-locally with `setq-hook!' to disable LSP formatting in
select buffers.")

;;-- end formatting

;;-- ligatures

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
    :not           "¬"
    :in            "∈"
    :not-in        "∉"
    :and           "∧"
    :or            "∨"
    :for           "∀"
    :some          "∃"
    :return        "⏎"
    :yield         "⤶ "
    ;; Other
    :union         "⋃"
    :intersect     "∩"
    :diff          "∖"
    :tuple         "⨂"
    :pipe          "|"
    :dot           "•")
  "Maps identifiers to symbols, recognized by `set-ligatures'.

This should not contain any symbols from the Unicode Private Area! There is no
universal way of getting the correct symbol as that area varies from font to
font.")

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

(defvar +ligature--composition-table (make-char-table nil))

;;-- end ligatures

;;-- spelling

(defvar +spell-excluded-faces-alist '())

(defvar +spell-correct-interface #'+spell-correct-ivy-fn)

(defvar +spell-excluded-faces-alist
  '((markdown-mode
     . (markdown-code-face markdown-html-attr-name-face markdown-html-attr-value-face markdown-html-tag-name-face markdown-inline-code-face markdown-link-face markdown-markup-face markdown-plain-url-face markdown-reference-face markdown-url-face))
    (org-mode
     . (org-block org-block-begin-line org-block-end-line org-cite org-cite-key org-code org-date org-footnote org-formula org-inline-src-block org-latex-and-related org-link org-meta-line org-property-value org-ref-cite-face org-special-keyword org-tag org-todo org-todo-keyword-done org-todo-keyword-habt org-todo-keyword-kill org-todo-keyword-outd org-todo-keyword-todo org-todo-keyword-wait org-verbatim))
    (latex-mode
     . (font-latex-math-face font-latex-sedate-face font-lock-function-name-face font-lock-keyword-face font-lock-variable-name-face)))
  "Faces in certain major modes that spell-fu will not spellcheck.")

;;-- end spelling
