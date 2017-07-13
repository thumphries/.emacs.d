(eval-and-compile
  (mapc #'(lambda (path)
            (add-to-list 'load-path
                         (expand-file-name path user-emacs-directory)))
        '("lisp" "site-lisp" "site-lisp/use-package"))
  (require 'use-package))

(use-package diminish
  :load-path "site-lisp/diminish"
  :commands (diminish))

(use-package haskell-mode-autoloads
  :load-path "site-lisp/haskell-mode"
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))
  :config
    (add-hook 'haskell-mode-hook (lambda () (local-set-key (kbd "C-c d") #'dante-mode))))

(use-package dante
  :load-path "site-lisp/dante"
  :commands 'dante-mode
  :config
    (let
      ((methods `((mafia . ,(lambda (root)
                              (when (and (vc-root-dir)
                                         (directory-files root nil ".*\\.cabal$"))
                                '("mafia" "quick" dante-target))))
                  (bare  . ,(lambda (_) '("cabal" "repl" dante-target))))))
      (setq dante-repl-command-line-methods-alist methods))
    (add-hook 'dante-mode-hook #'flycheck-mode))

(use-package flycheck
  :load-path "site-lisp/flycheck"
  :commands 'flycheck-mode)

(use-package markdown-mode
  :load-path "site-lisp/markdown-mode"
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

(use-package ivy
  :load-path "site-lisp/ivy"
  :diminish (ivy-mode . "")
  :init
    (use-package counsel
      :load-path "site-lisp/ivy"
      :bind
        (("M-x" . counsel-M-x)))
    (use-package swiper
      :load-path "site-lisp/ivy"
      :bind
        (("\C-s" . swiper)))
  :config
    (ivy-mode 1)
    ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
    (setq ivy-use-virtual-buffers t)
    ;; number of result lines to display
    (setq ivy-height 10)
    ;; does not count candidates
    (setq ivy-count-format "")
    ;; fuzzy matching
    (setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy))))

(use-package find-file-in-project
  :load-path "site-lisp/find-file-in-project"
  :bind (("C-c p f" . find-file-in-project)))

(use-package magit
  :load-path "site-lisp/magit/lisp"
  :commands
    (magit-mode)
  :bind
    (("C-c g l" . magit-log)
     ("C-c g c" . magit-commit)
     ("C-c g a" . magit-commit-amend)
     ("C-c g s" . magit-status)
     ("C-c g d" . magit-diff-unstaged)
     ("C-c g e" . magit-diff-staged)
     ("C-c g b" . magit-blame)
     ("C-c g q" . magit-blame-quit)
     ("C-c g u" . magit-stage-file))
  :mode
    ("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|BRANCH_DESCRIPTION\\)\\'" . global-git-commit-mode)
  :init
    (use-package dash
      :load-path "site-lisp/dash")
    (use-package with-editor
      :load-path "site-lisp/with-editor")
    (setq magit-completing-read-function 'ivy-completing-read))

(use-package rust-mode
  :load-path "site-lisp/rust-mode"
  :mode ("\\.rs\\'" . rust-mode))

(use-package dot-org
  :load-path "lisp/dot-org"
  :mode ("\\.org\\'" . org-mode)
  :bind
    (("C-c C-j" . org-journal-new-entry)
     ("C-c a"   . org-agenda))
  :init
    (setq org-dir "~/Documents/org/")
    (setq org-journal-dir "~/Documents/journal/"))

(load-theme 'tango-dark)

;; Disable electric-indent-mode
(electric-indent-mode 0)

;; CUA mode for selection / mutation / etc.
(cua-mode 't)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(global-set-key (kbd "C-x C-v") 'cua-set-rectangle-mark)
;; Unbind C-RET, it doesn't work in terminals and is thus a bad habit
(define-key cua-global-keymap [C-return] nil)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Enable line numbers everywhere (besides some major modes)
(progn
  (setq linum-mode-inhibit-modes-list '(eshell-mode
                                        shell-mode
                                        term-mode
                                        org-mode
                                        erc-mode
                                        calendar
                                        calendar-mode
                                        magit-mode))
  (defadvice linum-on (around linum-on-inhibit-for-modes)
    "Stop the load of linum-mode for some major modes."
      (unless (member major-mode linum-mode-inhibit-modes-list) ad-do-it))
  (ad-activate 'linum-on)
  (global-linum-mode 1)
  (setq linum-format "%4d "))

;; Show trailing whitespace in bright red when programming
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; Disable various visual cruft
(tool-bar-mode -1)
(if window-system
    (scroll-bar-mode -1) ;; scrollbar doesn't exist in cli
    (menu-bar-mode 0))   ;; still want menu bar in OS X

;; mark ring
;; C-SPC C-SPC - add to mark ring
;; C-u C-SPC <repeat> - cycle marks
(setq set-mark-command-repeat-pop t)

;; allow clicking around in xterm
(require 'mouse)
(xterm-mouse-mode)
(setq mouse-autoselect-window t)

;; Show columns in modeline
(setq column-number-mode t)

;; Really annoying to have the bell ringing when overscrolling
;; ... best to just disable it, really.
(setq ring-bell-function 'ignore)

;; add trailing newlines on save
(setq require-final-newline t)

;; This auto-reloads modified files.
(global-auto-revert-mode t)

; jump to scratch instead of gnu welcome
(setq inhibit-startup-screen t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("/Users/tim/Documents/journal/20170713.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
