;; +defs.el -*- lexical-binding: t; -*-

(defvar +python-ipython-command '("ipython" "-i" "--simple-prompt" "--no-color-info")
  "Command to initialize the ipython REPL for `+python/open-ipython-repl'.")

(defvar +python-jupyter-command '("jupyter" "console" "--simple-prompt")
  "Command to initialize the jupyter REPL for `+python/open-jupyter-repl'.")

(defvar jg-python-dev-mode nil)

(defvar jg-python-dev-cmd "-X dev")

(defvar jg-python-docs-url           "https://docs.python.org/3/")

(defvar jg-python-lib-url-suffix     "library/%s.html")

(defvar jg-python-import-block-end-re "^\\(__all__\\|[[:graph:]]+?\\s-+=\\|def\\|class\\|if TYPE_CHECKING:\\)")

(defvar jg-python-summary-buffer      "*Python-Summary*")
