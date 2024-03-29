;; Discourage garbage collection during init
(setq gc-cons-threshold (* 50 1000 1000))

;; Set high tolerance for bad elisp
(setq max-lisp-eval-depth 10000) ;; default 500
(setq max-specpdl-size 50000) ;; default 1300

;; Disable cl deprecation warnings
(setq byte-compile-warnings '(cl-functions))

;; Bootstrap use-package
(eval-and-compile
  (mapc #'(lambda (path)
            (add-to-list 'load-path
                         (expand-file-name path user-emacs-directory)))
        '("lisp" "site-lisp" "site-lisp/use-package"))
  (require 'use-package))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :load-path "site-lisp/exec-path-from-shell"
    :config (exec-path-from-shell-initialize)))

(use-package untabify-mode
  :load-path "lisp/untabify"
  :commands (untabify-mode untabify-all)
  :defer t)

(use-package f
  :load-path "site-lisp/f"
  :defer t)

(use-package s
  :load-path "site-lisp/s"
  :defer t)

(use-package ht
  :load-path "site-lisp/ht"
  :defer t)

(use-package dash
  :load-path "site-lisp/dash"
  :defer t)

(use-package popup
  :load-path "site-lisp/popup-el"
  :defer t)

(use-package doom-themes
  :load-path "site-lisp/emacs-doom-themes"
  :config
    (load-theme 'doom-one t))

(use-package shrink-path
  :load-path "site-lisp/shrink-path"
  :defer t)

(use-package memoize
  :load-path "site-lisp/emacs-memoize"
  :commands 'memoize
  :defer t)

(use-package all-the-icons
  :load-path "site-lisp/all-the-icons"
  :after (memoize)
  :commands 'all-the-icons-for-file
  :defer t)

(use-package doom-modeline
  :load-path "site-lisp/doom-modeline"
  :demand t
  :after (dash shrink-path all-the-icons)
  :config
    (doom-modeline-mode)
    (setq doom-modeline-icon nil))

(use-package hide-mode-line
  :load-path "site-lisp/emacs-hide-mode-line"
  :defer t)

(use-package smartparens
  :load-path "site-lisp/smartparens"
  :commands (smartparens-mode smartparens-strict-mode)
  :defer t
  :config
    (defun my--go-open-block (&rest _ignored)
      (newline)
      (indent-according-to-mode)
      (previous-line)
      (indent-according-to-mode))
    (sp-local-pair 'go-mode "{" nil :post-handlers '(( my--go-open-block "RET"))))

(use-package hydra
  :load-path "site-lisp/hydra"
  :defer t
  :commands 'defhydra
  :config
    (progn
      ;;** Example 5: mini-vi
      (defun hydra-vi/pre ()
        (set-cursor-color "#e52b50"))

      (defun hydra-vi/post ()
        (set-cursor-color "#ffffff"))

      (defhydra hydra-vi (:pre hydra-vi/pre :post hydra-vi/post :color amaranth)
        "vi"
        ("l" forward-char)
        ("h" backward-char)
        ("j" next-line)
        ("k" previous-line)
        ("m" set-mark-command "mark")
        ("a" move-beginning-of-line "beg")
        ("e" move-end-of-line "end")
        ("d" delete-region "del" :color blue)
        ("y" kill-ring-save "yank" :color blue)
        ("q" nil "quit"))
      (hydra-set-property 'hydra-vi :verbosity 1)

      (defhydra hydra-edit (nil nil)
        "edit"
        ("r" replace-regexp "replace-regexp")
        ("a" align-regexp "align-regexp")
        ("g" goto-line "goto-line")
        ("R" revert-buffer "revert-buffer")
        ("D" make-directory "make-directory")
        ("M" lacarte-execute-menu-command "menu"))
      (hydra-set-property 'hydra-edit :verbosity 1))
  :bind
    (("C-c v" . hydra-vi/body)
     ("C-x C-b" . hydra-edit/body)))

(use-package dumb-jump
  :load-path "site-lisp/dumb-jump"
  :after (popup)
  :commands 'dumb-jump-mode
  :bind
    (("C-c j g" . dumb-jump-go))
  :defer t
  :config
    (setq dumb-jump-selector 'ivy))

(use-package spinner
  :load-path "site-lisp/spinner"
  :defer t)

(use-package diminish
  :load-path "site-lisp/diminish"
  :commands (diminish)
  :defer t)

(use-package reformatter
  :load-path "site-lisp/emacs-reformatter"
  :commands (reformatter-define)
  :defer t)

(use-package ormolu
  :load-path "site-lisp/ormolu"
  :bind (("C-c C-r" . ormolu-format-buffer))
  :commands (ormolu-format-on-save-mode)
  :defer t)

(use-package haskell-mode-autoloads
  :load-path "site-lisp/haskell-mode"
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))
  :after (yasnippet ormolu)
  :config
    (defun my-haskell-mode-hook ()
      (yas-minor-mode))
    (add-hook 'haskell-mode-hook 'my-haskell-mode-hook))

(use-package purescript-mode-autoloads
  :load-path "site-lisp/purescript-mode"
  :mode (("\\.purs\\'" . purescript-mode)))

(use-package dante
  :load-path "site-lisp/dante"
  :defer t
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
  :commands 'flycheck-mode
  :defer t)

(use-package markdown-mode
  :load-path "site-lisp/markdown-mode"
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

(use-package ivy
  :load-path "site-lisp/ivy"
  :defer t
  :diminish (ivy-mode . "")
  :config
    (ivy-mode 1)
    ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
    (setq ivy-use-virtual-buffers t)
    ;; number of result lines to display
    (setq ivy-height 15)
    ;; does not count candidates
    (setq ivy-count-format "")
    ;; matching strategies
    (setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (counsel-git-grep . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))
    ;; sorting strategies
    (setq ivy-sort-matches-functions-alist
      '((ivy-switch-buffer . ivy-sort-function-buffer)
        (counsel-find-file . ivy-sort-function-buffer)
        (counsel-git . ivy-sort-function-buffer)
        (t . nil))))

(use-package swiper
  :load-path "site-lisp/ivy"
  :after (ivy)
  :defer t
  :bind
    (("\C-s" . swiper)))

(use-package counsel
  :load-path "site-lisp/ivy"
  :after (ivy swiper)
  :defer t
  :bind
    (("C-x b" . counsel-switch-buffer)
     ("C-x C-f" . counsel-find-file)
     ("C-c p f" . counsel-git)
     ("C-c p s" . counsel-git-grep)))

(use-package amx
  :load-path "site-lisp/amx"
  :after (ivy swiper counsel)
  :defer t
  :bind ("M-x" . amx)
  :config
    ;; amx doesn't "require" ivy, need to make it load
    (require 'ivy)
    (amx-mode))

(use-package with-editor
  :load-path "site-lisp/with-editor"
  :defer t)

(use-package magit
  :load-path "site-lisp/magit/lisp"
  :after (dash with-editor ivy)
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
    (("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|BRANCH_DESCRIPTION\\)\\'" . global-git-commit-mode)
     ("git-rebase-todo" . git-rebase-mode))
  :config
    (setq magit-completing-read-function 'ivy-completing-read))

(use-package rust-mode
  :load-path "site-lisp/rust-mode"
  :mode ("\\.rs\\'" . rust-mode))

(use-package go-mode
  :load-path "site-lisp/go-mode"
  :mode (("\\.go\\'" . go-mode)
         ("\\.mod\\'" . go-mode))
  :after (yasnippet)
  :config
    (defun my-go-mode-hook ()
      (if
        (locate-file "goimports" exec-path)
        (progn
          (setq gofmt-command "goimports")
          (setq gofmt-args '("-local" "github.com/thumphries,formation"))))
      (setq gofmt-show-errors 'echo)
      (add-hook 'before-save-hook 'gofmt-before-save)
      (setq tab-width 2 indent-tabs-mode 1)
      (yas-minor-mode)

      (defhydra hydra-golang (nil nil)
        "golang"
        ("i" godef-describe "info")
        ("d" godoc-at-point "doc")))
    (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package prop-menu
  :load-path "site-lisp/prop-menu"
  :defer t
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

(use-package hcl-mode
  :load-path "site-lisp/emacs-hcl-mode"
  :defer t)

(use-package terraform-mode
  :load-path "site-lisp/emacs-terraform-mode"
  :after (hcl-mode)
  :mode (("\\.tf" . terraform-mode)
         ("\\.tfvars" . terraform-mode)))

(use-package typescript-mode
  :load-path "site-lisp/typescript"
  :mode (("\\.ts" . typescript-mode)))

(use-package graphql-mode
  :load-path "site-lisp/graphql-mode"
  :mode (("\\.gql" . graphql-mode)
         ("\\.graphql" . graphql-mode)))

(use-package web-mode
  :load-path "site-lisp/web-mode"
  :mode (("\\.svelte" . web-mode)
         ("\\.html" . web-mode)
         ("\\.tmpl" . web-mode)
         ("\\.gotpl" . web-mode))
  :config
    (defun my-web-mode-hook ()
      (setq web-mode-engines-alist
        '(("go"    . "\\.gotpl\\'")
          ("svelte" . "\\.svelte\\'")
          ("blade"  . "\\.blade\\.")))
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-enable-auto-pairing t)
      (local-set-key (kbd "C-c C-k") 'web-mode-comment-or-uncomment))
    (add-hook 'web-mode-hook 'my-web-mode-hook))

(use-package direnv
  :load-path "site-lisp/direnv"
  :if (locate-file "direnv" exec-path)
  :bind
    (("C-c d" . direnv-update-environment))
  :mode ((".envrc" . direnv-envrc-mode))
  :config (direnv-mode))

(use-package yasnippet
  :load-path "site-lisp/yasnippet"
  :defer t
  :commands (yas-global-mode yas-minor-mode yas-expand)
  :config
    (yas-reload-all))

(use-package lsp-mode
  :load-path "site-lisp/lsp-mode"
  :after (f ht spinner yasnippet)
  :commands (lsp)
  :bind
    (("C-c l l" . lsp))
  :config
    ;; Unsure why the :after can't do this for me
    (require 'yasnippet))

(use-package lsp-ui
  :load-path "site-lisp/lsp-ui"
  :after (dash flycheck yasnippet lsp-mode)
  :commands (lsp-ui-mode)
  :defer t)

(use-package dot-org
  :load-path "lisp/dot-org"
  :mode ("\\.org\\'" . org-mode)
  :bind
    (("C-c C-j" . org-journal-new-entry)
     ("C-c a"   . org-agenda))
  :init
    (setq org-dir "~/data/docs/org/")
    (setq org-journal-dir "~/data/docs/org/2020/"))

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


(use-package lacarte
  :load-path "site-lisp/lacarte"
  :bind
    ("M-o" . lacarte-execute-menu-command))

(use-package esup
  :load-path "site-lisp/esup"
  :defer t
  :commands (esup)
  :config
    (setq esup-depth 0))

(defun byte-compile-dependencies ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/site-lisp/") 0))

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

(defun allow-tabs ()
  (progn
    (message "Tabs allowed in this buffer.")
    (untabify-mode 'f)
    (setq untabify-on-save nil)))

(defun murder-tabs ()
  (progn
    (message "Murdering tabs...")
    (untabify-mode 't)
    (whitespace-mode)
    (setq whitespace-tab 'trailing-whitespace)
    (setq whitespace-style '(face tabs indentation::space))))

(defun whitespace-rules ()
  (progn
    (if
      (or
        ;; tabby mode whitelist
        (derived-mode-p 'go-mode)
        (derived-mode-p 'web-mode)
        (derived-mode-p 'makefile-mode))
      (allow-tabs)
      (murder-tabs))

    ;; Show trailing whitespace in bright red regardless of mode
    (setq show-trailing-whitespace t)))

(add-hook 'prog-mode-hook (lambda () (progn (whitespace-rules) (smartparens-mode))))
(add-hook 'text-mode-hook (lambda () (progn (whitespace-rules) (smartparens-mode))))

;; when in fundamental mode, try to figure out buffer type on save
(defun my-normal-mode-hook ()
  (when (eq major-mode 'fundamental-mode) (normal-mode)))
(add-hook 'before-save-hook #'my-normal-mode-hook)

;; Automatically u+x when there's a shebang up top
(add-hook 'after-save-hook
        #'(lambda ()
        (and (save-excursion
               (save-restriction
                 (widen)
                 (goto-char (point-min))
                 (save-match-data
                   (looking-at "^#!"))))
             (not (file-executable-p buffer-file-name))
             (shell-command (concat "chmod u+x " buffer-file-name))
             (message
              (concat "Saved " buffer-file-name " as executable")))))

;; Disable various visual cruft
(if window-system
    (progn
      (scroll-bar-mode -1) ;; scrollbar doesn't exist in cli
      (tool-bar-mode -1))
    (progn
      (tool-bar-mode -1)
      (menu-bar-mode -1)))

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

;; Let the GC run at a more normal cadence now.
(setq gc-cons-threshold (* 2 1000 1000))
