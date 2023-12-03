(require 'dash)

(defun column-at-pos (position)
  (save-excursion
    (goto-char position)
    (current-column)))

(defun position-touch-match (position match-start match-limit)
  (let ((match-line (line-number-at-pos match-start)))
    (and (<= (1- match-line) (line-number-at-pos position) (1+ match-line))
         (<= (1- (column-at-pos match-start)) (column-at-pos position) (column-at-pos match-limit)))))

(defun touching-numbers (position numbers)
  (let ((result (cl-loop for number in numbers
                         if (position-touch-match position (-second-item number) (-third-item number))
                         collect (-first-item number))))
    (when (>= (length result) 2)
        result)))

(with-current-buffer "input.txt"
  (setq stars (cl-loop initially do (goto-char (point-min))
                       while (search-forward "*" nil t)
                       collect (match-beginning 0))
        numbers (cl-loop initially do (goto-char (point-min))
                         while (re-search-forward "[[:digit:]]+" nil t)
                         collect (list (string-to-number (match-string 0)) (match-beginning 0) (match-end 0))))
  (cl-loop for star-position in stars
           sum (let ((matching-numbers (touching-numbers star-position numbers)))
                 (if matching-numbers (-product matching-numbers) 0))))
