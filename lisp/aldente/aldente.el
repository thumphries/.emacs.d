(provide 'aldente)

(defun aldente--prior-indent-n (maxdepth)
  (interactive)
  (let
    ((start-column (current-column))
     (depth 0)
     indent)
    (save-excursion
      (beginning-of-line)
      (while
        (/= depth maxdepth)
        (setq depth (+ depth 1))
        (if
          (re-search-backward "^[^\n]" nil t)
          (let ((end (save-excursion (forward-line 1) (point))))
            (move-to-column start-column)
            ;; Is start-column inside a tab on this line?
            (if (> (current-column) start-column)
            (backward-char 1))
            (or (looking-at "[ \t]") t)
            (skip-chars-forward " \t" end)
            (or (= (point) end) (setq indent (current-column)))
            (beginning-of-line))
          (error "Found only %d levels of indentation (wanted %d)" depth maxdepth))))
    indent))

(defun aldente--copy-indent (depth)
  (interactive)
  (let
    ((new-column (aldente--prior-indent-n depth)))
    (if new-column (indent-to new-column 0))))

(defun copy-indent (depth)
  (interactive "P")
  (aldente--copy-indent depth))

(defun copy-indent-inner ()
  (interactive)
  (aldente--copy-indent 2))

(defun copy-indent-outer ()
  (interactive)
  (aldente--copy-indent 3))
