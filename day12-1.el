;; -*- lexical-binding: t; -*-

(defun nth-from-last (n list)
  (nth (- (length list) n 1) list))

(cl-defun possible-alternatives (possibility-string constraints)
  (cl-labels ((count-possible-node (possibility-to-test possibility-string-left reversed-constraints)
                (cond ((not possibility-string-left) (if (possible-leaf? possibility-to-test reversed-constraints) 1 0))
                      ((not (possible-node? possibility-to-test reversed-constraints)) 0)
                      ((= (car possibility-string-left) ??) (+ (count-possible-node (cons ?. possibility-to-test) (cdr possibility-string-left) reversed-constraints)
                                                               (count-possible-node (cons ?# possibility-to-test) (cdr possibility-string-left) reversed-constraints)))
                      (t (count-possible-node (cons (car possibility-string-left) possibility-to-test) (cdr possibility-string-left) reversed-constraints))))
              (possible-node? (possibility-to-test reversed-constraints)
                (let* ((possibility-groups (mapcar #'length (--filter (= (car it) ?#) ; Keep part of a group
                                                                      (--partition-by (= it ?#) possibility-to-test))))
                       (suffix-with-constrains (-common-suffix possibility-groups reversed-constraints))
                       (suffix-length (length suffix-with-constrains))
                       (group-length (length possibility-groups)))
                                        ; For this node to be possible, the currently formed groups must be either a complete prefix
                                        ; there must be only one different number, at the end, lesser than the corresponding number
                  (or (= suffix-length group-length) ; Complete prefix
                      (and (= suffix-length (1- group-length))
                           (<= (nth-from-last suffix-length  possibility-groups) (nth-from-last suffix-length reversed-constraints))))))
              (possible-leaf? (possibility-to-test reversed-constraints)
                (let ((possibility-groups (mapcar #'length (--filter (= (car it) ?#) ; Keep part of a group
                                                                     (--partition-by (= it ?#) possibility-to-test)))))
                  (equal possibility-groups reversed-constraints))))
    (count-possible-node nil possibility-string (reverse constraints))))


; Part 1
(with-current-buffer "input.txt"
  (goto-char (point-min))
  (cl-loop until (eobp)
   do (re-search-forward "\\([?.#]+\\) \\([[:digit:],]+\\)")
   sum (possible-alternatives (append (match-string 1) '()) ; Convert string to list
                              (mapcar #'string-to-number (split-string (match-string 2) ",")))
   do (forward-line)))

; Part 2
; Doesn't work with current state
(with-current-buffer "input.txt"
  (goto-char (point-min))
  (cl-loop until (eobp)
   do (re-search-forward "\\([?.#]+\\) \\([[:digit:],]+\\)")
   do (print (match-string 0))
   sum (possible-alternatives (append (match-string 1) "?" (match-string 1) "?" (match-string 1) "?"
                                      (match-string 1) "?" (match-string 1) '()) ; Convert string to list and multiply by 5. This is dumb but it works.
                              (-flatten (-repeat 5
                               (mapcar #'string-to-number (split-string (match-string 2) ",")))))
   do (forward-line)))
