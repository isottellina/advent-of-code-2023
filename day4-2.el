;; -*- lexical-binding: t; -*-

(require 's)

(defun matching-numbers (winning-numbers got-numbers)
  (cl-loop for got-number in got-numbers
           count (seq-contains-p winning-numbers got-number)))

(with-current-buffer "input.txt"
  (goto-char (point-min))
  (let* ((winning-numbers-by-card (cl-loop do (skip-chars-forward "[:space:]")
                                           until (eobp)
                                           collect (let* ((game-id (progn (re-search-forward "Card\\(?:[[:blank:]]+\\)\\([[:digit:]]+\\):[[:blank:]]+")
                                                                          (string-to-number (match-string 1))))
                                                          (winning-numbers (progn (re-search-forward "\\([[:digit:]]+[[:blank:]]*\\)+")
                                                                                  (mapcar #'string-to-number (s-split " " (match-string 0) t))))
                                                          (got-numbers (progn (re-search-forward "\\([[:blank:]]*[[:digit:]]+[[:blank:]]*\\)+")
                                                                              (mapcar #'string-to-number (s-split " " (match-string 0) t)))))
                                                     (cons game-id (matching-numbers winning-numbers got-numbers)))))
         (card-numbers (cl-loop with final-result
                                for game-id from 1 to (length winning-numbers-by-card)
                                for initial-card-number = (alist-get game-id final-result 1)
                                do (cl-loop repeat initial-card-number
                                            do (cl-loop for card-won from 1 to (alist-get game-id winning-numbers-by-card)
                                                        do (cl-incf (alist-get (+ game-id card-won) final-result 1))))
                                finally return final-result))
         (total-card-number (cl-loop for game-id from 1 to (length winning-numbers-by-card)
                                     sum (alist-get game-id card-numbers 1))))
    total-card-number))
