;;; lang/jg-cron/config.el -*- lexical-binding: t; -*-


(use-package! cron-mode
  :defer t
  :commands cron-mode
  :mode ("/crontab\\..+" . cron-mode)
  )
