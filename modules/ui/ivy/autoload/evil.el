;; completion/ivy/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+ivy:project-search "ui/ivy/autoload/evil" nil t)
(evil-define-command +ivy:project-search (query &optional all-files-p)
  "Ex interface for `+ivy/project-search'."
  (interactive "<a><!>")
  (+ivy/project-search all-files-p query))

;;;###autoload (autoload '+ivy:project-search-from-cwd "ui/ivy/autoload/evil" nil t)
(evil-define-command +ivy:project-search-from-cwd (query &optional recurse-p)
  "Ex interface for `+ivy/project-search-from-cwd'."
  (interactive "<a><!>")
  (+ivy/project-search-from-cwd (not recurse-p) query))
