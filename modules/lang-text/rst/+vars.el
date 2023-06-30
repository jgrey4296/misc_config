;;; +vars.el -*- lexical-binding: t; -*-

(spec-handling-add! lookup-regular
                    '(rst-mode
                     ("Restructured Text Reference" .  "https://restructuredtext.documatt.com/index.html")
                     ("Docutils Reference" .  "https://docutils.sourceforge.io/docs/user/rst/quickstart.html")
                     ("Guide" .  "https://www.writethedocs.org/guide/writing/reStructuredText/")
                     ("Sphinx" .  "https://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html")
                     )
                    )

(spec-handling-add! auto-modes
                    '(rst
                      ("\\.txt\\'" . rst-mode)
                      ("\\.rst\\'" . rst-mode)
                      ("\\.rest\\'" . rst-mode)
                      )
                    )
