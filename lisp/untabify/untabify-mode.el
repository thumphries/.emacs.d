(defun untabify-all ()
  "Untabify the current buffer, unless `untabify-on-save' is nil."
  (and untabify-on-save (untabify (point-min) (point-max))))

(define-minor-mode untabify-mode
  "Removes all tabs, with prejudice." nil " untab" nil

  ;; No tab indentation
  (setq indent-tabs-mode nil)

  ;; Hook untabify-all on save
  (make-local-variable 'untabify-on-save)
  (setq-default untabify-on-save t)
  (setq untabify-on-save t)
  (add-hook 'before-save-hook #'untabify-all))

(provide 'untabify-mode)
