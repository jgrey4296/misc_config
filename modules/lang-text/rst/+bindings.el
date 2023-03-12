;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map rst-mode-map
      :localleader
      :desc "Reference" "1" (cmd! (browse-url "https://restructuredtext.documatt.com/index.html"))
      :desc "Reference" "2" (cmd! (browse-url "https://docutils.sourceforge.io/docs/user/rst/quickstart.html"))
      :desc "Reference" "3" (cmd! (browse-url "https://www.writethedocs.org/guide/writing/reStructuredText/"))
      :desc "Reference" "4" (cmd! (browse-url "https://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html"))
      )
