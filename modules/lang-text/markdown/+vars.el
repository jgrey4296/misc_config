;;; +vars.el -*- lexical-binding: t; -*-

(defvar +markdown-compile-functions
  '(+markdown-compile-marked
    +markdown-compile-pandoc
    +markdown-compile-markdown
    +markdown-compile-multimarkdown)
  "A list of commands to try when attempting to build a markdown file with
`markdown-open' or `markdown-preview', stopping at the first one to return non-nil.

Each function takes three argument. The beginning position of the region to
capture, the end position, and the output buffer.")

(setq markdown-enable-math t ; syntax highlighting for latex fragments
      markdown-enable-wiki-links t
      markdown-italic-underscore t
      markdown-asymmetric-header t
      markdown-gfm-additional-languages '("sh")
      markdown-make-gfm-checkboxes-buttons t
      markdown-fontify-whole-heading-line t

      ;; HACK Due to jrblevin/markdown-mode#578, invoking `imenu' throws a
      ;;      'wrong-type-argument consp nil' error if you use native-comp.
      markdown-nested-imenu-heading-index (not (ignore-errors (native-comp-available-p)))

      ;; `+markdown-compile' offers support for many transpilers (see
      ;; `+markdown-compile-functions'), which it tries until one succeeds.
      markdown-command #'+markdown-compile
      ;; This is set to `nil' by default, which causes a wrong-type-arg error
      ;; when you use `markdown-open'. These are more sensible defaults.
      markdown-open-command (cond (IS-MAC "open") (IS-LINUX "xdg-open"))

      ;; A sensible and simple default preamble for markdown exports that
      ;; takes after the github asthetic (plus highlightjs syntax coloring).
      markdown-content-type "application/xhtml+xml"
      markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
                           "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")

      markdown-xhtml-header-content (concat "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
                                            "<style> body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; } </style>"
                                            "<script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>"
                                            "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
                                            "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>")
      )

(after! smartparens-markdown
  (sp-with-modes '(markdown-mode gfm-mode)
    (sp-local-pair "```" "```" :post-handlers '(:add ("||\n[i]" "RET")))

    ;; The original rules for smartparens had an odd quirk: inserting two
    ;; asterixex would replace nearby quotes with asterixes. These two rules
    ;; set out to fix this.
    (sp-local-pair "**" nil :actions :rem)
    (sp-local-pair "*" "*"
                   :actions '(insert skip)
                   :unless '(:rem sp-point-at-bol-p)
                   ;; * then SPC will delete the second asterix and assume
                   ;; you wanted a bullet point. * followed by another *
                   ;; will produce an extra, assuming you wanted **|**.
                   :post-handlers '(("[d1]" "SPC") ("|*" "*"))))

  ;; This keybind allows * to skip over **.
  (map! :map markdown-mode-map
        :ig "*" (general-predicate-dispatch nil
                  (looking-at-p "\\*\\* *")
                  (cmd! (forward-char 2)))))

(spec-handling-add! lookup-handler
                    '((markdown-mode gfm-mode)
                     ;; `markdown-follow-thing-at-point' may open an external program or a
                     ;; buffer. No good way to tell, so pretend it's async.
                     :file (markdown-follow-thing-at-point :async t)
                     )
                    )

(spec-handling-add! lookup-regular
                    '(markdown-mode
                     ("mdbook" . "https://rust-lang.github.io/mdBook/")
                     ("markdown cheatsheet" . "https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet")
                     ("markdown Syntax" . "https://www.markdownguide.org/basic-syntax/")
                     ("github markdown" . "https://github.github.com/gfm/")
                     ("markdown refernce" . "https://commonmark.org/help/")
                     ("Pelican" . "https://docs.getpelican.com/en/latest/")
                     )
                    )

(spec-handling-add! auto-modes
                    '(markdown
                      ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
                      ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)
                      )
                    )
