;;; +projects.el -*- lexical-binding: t; -*-

(def-project-mode! +web-jekyll-mode
  :modes '(web-mode js-mode coffee-mode css-mode haml-mode pug-mode)
  :files (and (or "_config.yml" "_config.toml")
              (or "_layouts/" "_posts/"))
  :on-enter
  (when (eq major-mode 'web-mode)
    (web-mode-set-engine "django")))

(def-project-mode! +web-django-mode
  :modes '(web-mode python-mode)
  :files ("manage.py")
  :on-enter
  (when (derived-mode-p 'web-mode)
    (web-mode-set-engine "django")))

(def-project-mode! +web-wordpress-mode
  :modes '(php-mode web-mode css-mode haml-mode pug-mode)
  :files (or "wp-config.php" "wp-config-sample.php"))
