;; -*- lexical-binding: t; -*-

(cl-defun possible-alternatives (possibility-string constraints)
  (cl-labels ((possible-node? (possibility-to-test possibility-string-left reversed-constraints)
                (cond ((not possibility-string-left) (possible-leaf? possibility-to-test reversed-constraints))
                      ((= (car possibility-string-left) ??) (+ (possible-node? (cons ?. possibility-to-test) (cdr possibility-string-left) reversed-constraints)
                                                               (possible-node? (cons ?# possibility-to-test) (cdr possibility-string-left) reversed-constraints)))
                      (t (possible-node? (cons (car possibility-string-left) possibility-to-test) (cdr possibility-string-left) reversed-constraints))))
              (possible-leaf? (possibility-to-test reversed-constraints)
                (let ((possibility-groups (mapcar #'length (--filter (= (car it) ?#) ; Keep part of a group
                                                                (--partition-by (= it ?#) possibility-to-test)))))
                  (if (equal possibility-groups reversed-constraints) 1 0))))
    (possible-node? nil possibility-string (reverse constraints))))


(with-current-buffer "input.txt"
  (goto-char (point-min))
  (cl-loop until (eobp)
   do (re-search-forward "\\([?.#]+\\) \\([[:digit:],]+\\)")
   sum (possible-alternatives (append (match-string 1) '()) ; Convert string to list
                                  (mapcar #'string-to-number (split-string (match-string 2) ",")))
   do (forward-line)))
