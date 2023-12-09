;; -*- lexical-binding: t; -*-

(require 'dash)

(defun day9/get-previous-value (seq)
  (cl-labels ((list-of-differences (seq)
                (cl-loop for idx from 0
                         for current-cdr = (nthcdr idx seq)
                         while (cdr current-cdr)
                         collect (- (cadr current-cdr) (car current-cdr))))
              (previous-value (seq
                               &aux (distinct-seq (-distinct seq)))
                (if (= (length distinct-seq) 1)
                    (car distinct-seq)
                  (- (-first-item seq) (previous-value (list-of-differences seq))))))
    (previous-value seq)))

(with-current-buffer "input.txt"
  (goto-char (point-min))
  (cl-loop until (and (eolp) (bolp))
           sum (day9/get-previous-value (cl-loop while (re-search-forward "-?[[:digit:]]+" (line-end-position) t)
                                                 collect (string-to-number (match-string 0))))
           do (forward-line)))
