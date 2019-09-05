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

(use-package purescript-mode-autoloads
  :load-path "site-lisp/purescript-mode"
  :mode (("\\.purs\\'" . purescript-mode)))

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
        (("M-x" . counsel-M-x)
         ("C-c p f" . counsel-git)))
    (use-package swiper
      :load-path "site-lisp/ivy"
      :bind
        (("\C-s" . swiper)))
  :config
    (ivy-mode 1)
    ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
    (setq ivy-use-virtual-buffers t)
    ;; number of result lines to display
    (setq ivy-height 15)
    ;; does not count candidates
    (setq ivy-count-format "")
    ;; fuzzy matching
    (setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy))))

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

(use-package go-mode
  :load-path "site-lisp/go-mode"
  :mode (("\\.go\\'" . go-mode)
         ("\\.mod\\'" . go-mode))
  :init
    (defun my-go-mode-hook ()
      (add-hook 'before-save-hook 'gofmt-before-save)
      (setq tab-width 2 indent-tabs-mode 1))
    (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package prop-menu
  :load-path "site-lisp/prop-menu"
  :commands (prop-menu-by-completing-read prop-menu-show-menu))

(use-package idris-mode
  :load-path "site-lisp/idris-mode"
  :mode ("\\.idr\\'" . idris-mode))

(use-package nix-mode
  :load-path "site-lisp/nix-mode"
  :mode ("\\.nix\\'" . nix-mode))

(use-package yaml-mode
  :load-path "site-lisp/yaml-mode"
  :mode
    (("\\.yaml\\'" . yaml-mode)
     ("\\.yml\\'" . yaml-mode)))

(use-package protobuf-mode
  :load-path "site-lisp/protobuf-mode"
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package bazel-mode
  :load-path "site-lisp/bazel-mode"
  :mode (("\\.bzl\\'" . bazel-mode)
         ("BUILD" . bazel-mode)
         ("BUILD\\.*" . bazel-mode)
         ("*\\.BUILD" . bazel-mode)
         ("WORKSPACE" . bazel-mode)))

(use-package direnv
  :load-path "site-lisp/direnv"
  :bind
    (("C-c d" . direnv-update-environment))
  :mode ((".envrc" . direnv-envrc-mode)))

(use-package dot-org
  :load-path "lisp/dot-org"
  :mode ("\\.org\\'" . org-mode)
  :bind
    (("C-c C-j" . org-journal-new-entry)
     ("C-c a"   . org-agenda))
  :init
    (setq org-dir "~/Documents/org/")
    (setq org-journal-dir "~/Documents/journal/"))

(use-package multiple-cursors
  :load-path "site-lisp/multiple-cursors"
  :init
    (global-unset-key (kbd "S-<down-mouse-1>"))
  :bind
    (("C-c c a" . mc/edit-lines)
     ("C-c c e" . mc/edit-ends-of-lines)
     ("C-c c s" . mc/mark-all-dwim)
     ("C-c c y" . yank-rectangle)
     ("C-c c j" . mc/mark-next-like-this)
     ("C-c c k" . mc/mark-previous-like-this)
     ("C-c c n" . mc/mark-next-word-like-this)
     ("C-c c p" . mc/mark-previous-word-like-this)
     ("S-<down-mouse-1>" . mc/add-cursor-on-click)))


(use-package ansi-theme
  :load-path "lisp/ansi-theme"
  :no-require t)

;; (load-theme 'tango-dark)

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

; deal with "please enter yes or no" hell prompt
(defalias 'yes-or-no-p 'y-or-n-p) ; stfu

; enable direnv-mode by default if it's available
(if (locate-file "direnv" exec-path) (direnv-mode))
