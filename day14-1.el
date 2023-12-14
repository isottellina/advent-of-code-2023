
(cl-defun forward-line-preserve-column (&optional (arg 1) &aux (column (current-column)))
  (forward-line arg)
  (move-to-column column))

(defun char-above ()
  (save-excursion (unless (= (line-number-at-pos) 1)
                    (forward-line-preserve-column -1)
                    (char-after))))

(defun replace-char (c)
  (delete-char 1)
  (insert-char c)
  (backward-char))

(with-current-buffer "input.txt"
  (goto-char (point-min))
  (setq maximum-line (save-excursion (goto-char (point-max))
                                     (line-number-at-pos (1- (point)))))
  ; Tilting the platorm up
  (cl-loop while (search-forward "O" nil t)
           do (goto-char (match-beginning 0))
           do (replace-char ?.)
           do (save-excursion (while-let ((above (char-above))
                                          (_ (= above ?.)))
                                (forward-line-preserve-column -1))
                              (replace-char ?O))
           do (forward-char))

  ; Counting each rock
  (goto-char (point-min))
  (cl-loop while (search-forward "O" nil t)
           sum (1+ (- maximum-line (line-number-at-pos)))))
