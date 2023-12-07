(require 'dash)

(defconst card-faces
  '(?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?T ?J ?Q ?K ?A))

(defun card-get-group-n (seq)
  (let* ((card-numbers (mapcar (lambda (l) (cons (car l) (- (length l) 1)))
                               (seq-sort-by #'length #'> (seq-group-by #'identity seq))))
         (joker-number (alist-get ?J card-numbers 0))
         (card-numbers-without-joker (--remove (= (car it) ?J) card-numbers)))
    (setf (cdar card-numbers-without-joker) (+ (cdar card-numbers-without-joker) joker-number))
    (mapcar #'cdr card-numbers-without-joker)))

(card-get-group-n "KTJJT")

(defun card-primary-rank (seq)
  "Rank based on the poker hand"
  (pcase (card-get-group-n seq)
    ('(5) 6)
    ('(4 1) 5)
    ('(3 2) 4)
    ('(3 1 1) 3)
    ('(2 2 1) 2)
    ('(2 1 1 1) 1)
    ('(1 1 1 1 1) 0)))

(defun card-secondary-rank< (seq seq2)
  "Rank based on the faces"
  (cl-loop for seq-card across seq
           for seq-card2 across seq2
           for face-seq = (seq-position card-faces seq-card)
           for face-seq2 = (seq-position card-faces seq-card2)
           if (/= face-seq face-seq2)
           return (< face-seq face-seq2)))

(defun card-hand< (card-hand1 card-hand2)
  (cond ((< (card-primary-rank card-hand1) (card-primary-rank card-hand2)) t)
        (t (card-secondary-rank< card-hand1 card-hand2))))

(with-current-buffer "input.txt"
  (goto-char (point-min))
  (let ((hands-bids (cl-loop until (and (bolp) (eolp))
                             do (re-search-forward "\\([2-9TJQKA]+\\) \\([[:digit:]]+\\)")
                             collect (cons (match-string-no-properties 1) (string-to-number (match-string 2)))
                             do (forward-line))))
    (seq-sort (pcase-lambda (`(,card-hand1 . _) `(,card-hand2 . _)) (card-hand< card-hand1 card-hand2)) hands-bids)))
    (-sum (seq-map-indexed (pcase-lambda (`(_ . ,bid) idx) (* bid (+ idx 1)))
                           (seq-sort (pcase-lambda (`(,card-hand1 . _) `(,card-hand2 . _)) (card-hand< card-hand1 card-hand2)) hands-bids)))))
