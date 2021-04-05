;;; domain-specific/bibtex/+vars.el -*- lexical-binding: t; -*-

(setq-default bibtex-completion-additional-search-fields '("tags" "year")
              bibtex-completion-bibliography nil
              bibtex-completion-pdf-field "file"
              bibtex-completion-pdf-open-function #'(lambda (x) (org-open-link-from-string (format "[[file:%s]]" x)))
              bibtex-user-optional-fields '(("annotation" "Personal Annotation") ("tags" "Set of tags") ("isbn" "ISBN of file") ("doi" "DOI of file") ("url" "Url of file") ("file" "The path of the file") ("translator" "The Translators of the work"))

              jg-bibtex-clean-add-hooks '(+jg-bibtex-smart-replace-nonascii-hook +jg-bibtex-dont-break-lines-hook +jg-bibtex-clean-doi-hook  +jg-bibtex-align-hook +jg-bibtex-check-file-hook)
              jg-bibtex-clean-remove-hooks '(org-ref-replace-nonascii orcb-clean-doi org-ref-bibtex-format-url-if-doi orcb-check-journal)

              jg-bibtex-scholar-search-fields '("author" "editor" "ALTauthor" "Alteditor" "year" "doi" "isbn")
              jg-bibtex-scholar-search-fields-exact '("title")
              jg-bibtex-scholar-search-string "https://scholar.google.com/scholar?hl=en&q=%s"
              jg-bibtex-fill-column 50000

              jg-bibtex-loc-bibtex      "~/github/writing/resources/bibliography"
              jg-bibtex-loc-completions "~/github/writing/resources/completions"

              jg-bibtex-pdf-loc-regexp  "file[[:digit:]]*\s*=\s*{\\(.+mega\\)/\\(.+pdflibrary\\)?"
              jg-bibtex-pdf-replace-match-string "~/Mega"
              jg-bibtex-pdf-replace-library-string "pdflibrary"


              jg-bibtex-helm-candidates nil
              jg-bibtex-candidates-names '()

              jg-bibtex-rand-log ".emacs_rand_bib_log"

              jg-bibtex-remove-field-newlines-regexp "file\\|url\\|title"
              )

(setq jg-bibtex-non-ascii-inverted
  '(("{\\'i}"                . "í")
    ("{\\ae}"                . "æ")
    ("{\\'c}"                . "ć")
    ("{\\'e}"                . "é")
    ("{\\\"a}"                . "ä")
    ("{\\`e}"                . "è")
    ("{\\`a}"                . "à")
    ("{\\'a}"                . "á")
    ("{\\o}"                 . "ø")
    ("{\\\"e}"                . "ë")
    ("{\\\"u}"                . "ü")
    ("{\\~n}"                . "ñ")
    ("{\\c{n}}"              . "ņ")
    ("{\\~n}"                . "ñ")
    ("{\\aa}"                . "å")
    ("{\\\"o}"                . "ö")
    ("{\\'A}"                . "Á")
    ("{\\'i}"                . "í")
    ("{\\'o}"                . "ó")
    ("{\\'o}"                . "ó")
    ("{\\'u}"                . "ú")
    ("{\\'u}"                . "ú")
    ("{\\'y}"                . "ý")
    ("{\\v{s}}"              . "š")
    ("{\\v{c}}"              . "č")
    ("{\\v{r}}"              . "ř")
    ("{\\v{s}}"              . "š")
    ("{\\.I}"                . "İ")
    ("{\\\\u{g}}"              . "ğ")
    ("$\\delta$"             . "δ")
    ("{\\c{c}}"              . "ç")
    ("{\\ss}"                . "ß")
    ("$\\le$"                . "≤")
    ("$\\ge$"                . "≥")
    ("$\\theta$"             . "θ")
    ("$\\mu$"                . "μ")
    ("$\\rightarrow$"        . "→")
    ("$\\leftrightharpoons$" . "⇌")
    ("$\\times$"             . "×")
    ("$\\deg$"               . "°")
    ("{\\c{s}}"              . "ş")
    ("$\\gamma$"             . "γ")
    ("$\\gamma$"             . "ɣ")
    ("degC"                 . "º")
    ("$\\eta$"               . "η")
    ("$\\mu$"                . "µ")
    ("$\\alpha$"             . "α")
    ("$\\beta$"              . "β")
    ("$\\epsilon$"           . "ɛ")
    ("\\textrm{VI}"          . "Ⅵ")
    ("\\textrm{III}"         . "Ⅲ")
    ("\\textrm{V}"           . "Ⅴ")
    ("$\\lambda$"            . "λ")
    ("$\\pi$"                . "π")
    ("$\\infty$"             . "∞")
    ("$\\chi$"               . "χ")
    ("\\textasciitilde{}"    . "∼")
    ("\\textemdash{}"        . "‑")
    (" "                    . " ")
    ("..."                  . "…")
    ("\\textbullet "         . "•")
    (" "                    . " ")
    (" "                    . " ")
    (" "                    . " ")
    ("-"                    . "–")
    ("-"                    . "−")
    ("-"                    . "–")
    ("-"                    . "—")
    ("\\textemdash{}"        . "‒")
    ("'"                    . "‘")
    ("'"                    . "’")
    ("'"                    . "’")
    ("\\\""                   . "“")
    ("'"                    . "’")
    ("\\\""                   . "”"))
  )
