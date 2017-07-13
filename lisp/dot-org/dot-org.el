;;; org-mode configuration

(require 'use-package)

(require 'org)
(require 'org-agenda)

(use-package org-journal
  :load-path "site-lisp/org-journal"
  :config
    ;; Give all journal files a .org suffix, triggering org-mode
    (setq org-journal-file-format "%Y%m%d.org")
    ;; Match date.org files for the calendar view
    (setq org-journal-file-pattern
      "^\\(?1:[0-9]\\{4\\}\\)\\(?2:[0-9][0-9]\\)\\(?3:[0-9][0-9]\\).org$")
    (setq org-journal-carryover-items nil))

;; register custom input method
(register-input-method
  "TeXlIkE" "UTF-8" 'quail-use-package
  "\\" "TeX input method without subscripts."
  "quail/latin-ltx")
;; Enable Agda-style unicode input for Org
(add-hook 'org-mode-hook (lambda () (set-input-method "TeXlIkE")))

;; Point org-agenda at the org-dir
(setq org-agenda-files (list org-dir org-journal-dir))

;; AutoRefill mode to enforce paragraphs (defun toggle-autorefill
(add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))

;; Global keyword set
(setq org-todo-keywords
      '((type "TODO(!)" "DOING(!)" "|" "DONE(!)" "WONTDO(@)" "IMPOSSIBLE(@)")))

;; Fine-grained TODO logging
(setq org-log-done t)
(setq org-log-into-drawer t)

;; Keep DONE items out of agenda view
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-dim-blocked-tasks t)
(setq org-enforce-todo-dependencies t)

;; Keep SCHEDULED items out of agenda view until date
(setq org-agenda-todo-ignore-scheduled 'future)

;; Set agenda timestamp appearance
(setq org-agenda-deadline-leaders
      '("Deadline:  " "DUE  %3dd: " "LATE  %2dd: "))
(setq org-agenda-scheduled-leaders
      '("Scheduled: " "AVAIL %2dd: "))

;; Add INBOX and other contexts to agenda pop-up
(setq org-agenda-custom-commands
      '(("i" "INBOX" tags "-{^@}/!" nil)))

;; org-agenda window dedication
(add-hook 'org-agenda-mode-hook
      (lambda ()
        (interactive)
        (set-window-dedicated-p (selected-window) 1)))

;; save org-clock history
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; org-clock prompt after idle
(setq org-clock-idle-time 25)

(provide 'dot-org)
