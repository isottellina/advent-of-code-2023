;; -*- lexical-binding: t; -*-

(require 's)

(defun winning-points (winning-numbers got-numbers)
  (let ((final-result 0))
    (cl-loop for got-number in got-numbers
             if (seq-contains-p winning-numbers got-number)
             do (setq final-result (if (/= final-result 0) (* final-result 2) 1)))
    final-result))

(with-current-buffer "input.txt"
  (goto-char (point-min))
  (cl-loop do (skip-chars-forward "[:space:]")
           until (eobp)
           sum (let* ((game-id (progn (re-search-forward "Card\\(?:[[:blank:]]+\\)\\([[:digit:]]+\\):[[:blank:]]+")
                                      (string-to-number (match-string 1))))
                      (winning-numbers (progn (re-search-forward "\\([[:digit:]]+[[:blank:]]*\\)+")
                                              (mapcar #'string-to-number (s-split " " (match-string 0) t))))
                      (got-numbers (progn (re-search-forward "\\([[:blank:]]*[[:digit:]]+[[:blank:]]*\\)+")
                                          (mapcar #'string-to-number (s-split " " (match-string 0) t)))))
                 (winning-points winning-numbers got-numbers))))
