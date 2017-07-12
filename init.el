(eval-and-compile
  (mapc #'(lambda (path)
            (add-to-list 'load-path
                         (expand-file-name path user-emacs-directory)))
        '("site-lisp" "site-lisp/use-package"))
  (require 'use-package))

(use-package diminish
  :load-path "site-lisp/diminish"
  :commands (diminish))

(use-package haskell-mode-autoloads
  :load-path "site-lisp/haskell-mode"
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)))

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

(cua-mode 't)
(electric-indent-mode 0)
(load-theme 'tango-dark)

;; CUA mode for selection / mutation / etc.
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(global-set-key (kbd "C-x C-v") 'cua-set-rectangle-mark)

(show-paren-mode 1)

(global-linum-mode 1)
(setq linum-format "%4d ") ; Default formatting has no spacing

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
