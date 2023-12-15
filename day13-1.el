(cl-defun line-to-number (line)
  (--reduce-from (+ (* 2 acc) (if (= it ?#) 1 0)) 0 line))

(cl-defun find-reflection (lines)
  (let ((same-indices (--find-indices (eq (car it) (cdr it)) (windows-2 lines))))
    (cl-loop for index-to-test in same-indices
             for split-lists = (-split-at (1+ index-to-test) lines)
             if (--all? (apply #'= it) (-zip-lists (reverse (car split-lists)) (cadr split-lists)))
             return index-to-test)))

(defun windows-2 (list)
  (cl-loop for current-link = list then (cdr current-link)
           while (and current-link (cdr current-link))
           collect (cons (car current-link) (cadr current-link))))

(cl-defun find-reflection-score (pattern)
  (let* ((split-pattern (--map (append it nil) (string-split (string-trim pattern) "\n")))
         (horizontal-lines (-map #'line-to-number split-pattern))
         (vertical-lines (-map #'line-to-number (-unzip split-pattern)))
         (horizontal-reflection (find-reflection horizontal-lines))
         (vertical-reflection (find-reflection vertical-lines)))
    (cond (vertical-reflection (1+ vertical-reflection))
          (horizontal-reflection (* (1+ horizontal-reflection) 100)))))


(with-current-buffer "input.txt"
  (goto-char (point-min))
  (cl-loop while (not (eobp))
           do (set-mark (point))
           do (or (search-forward "\n\n" nil t) (goto-char (point-max)))
           sum (find-reflection-score (buffer-substring-no-properties (point) (mark)))))
