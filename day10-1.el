;; -*- lexical-binding: t; -*-

(require 'ht)

(cl-defun first-linep (&optional (pos (point)))
  (= (line-number-at-pos pos) 1))

(cl-defun last-linep (&optional (pos (point)))
  "Note: this is not actually complete.
It will return t if the next line is empty.

It's good enough though."
  (save-excursion (goto-char pos)
                  ; If we can't move forward, then true
                  (if (/= (forward-line) 0) t
                    ; Otherwise, check if that newline is empty
                    (and (bolp) (eolp)))))

(defconst direction-maps (let ((hashtable (make-hash-table)))
                           (puthash 'up '(0 . -1) hashtable)
                           (puthash 'down '(0 . 1) hashtable)
                           (puthash 'right '(1 . 0) hashtable)
                           (puthash 'left '(-1 . 0) hashtable)
                           hashtable))

(defconst direction-pipes (let ((hashtable (make-hash-table)))
                            (puthash ?| (list 'up 'down) hashtable)
                            (puthash ?- (list 'left 'right) hashtable)
                            (puthash ?L (list 'up 'right) hashtable)
                            (puthash ?J (list 'up 'left) hashtable)
                            (puthash ?7 (list 'left 'down) hashtable)
                            (puthash ?F (list 'right 'down) hashtable)
                            hashtable))

(cl-defun forward-line-preserve-column (&optional (arg 1) &aux (column (current-column)))
  (forward-line arg)
  (move-to-column column))

(cl-defun valid-directionp (direction)
  (and (symbolp direction) (ht-contains? direction-maps direction)))

(defun inverse-move (direction)
  (unless (valid-directionp direction)
    (signal 'wrong-type-argument direction))
  (pcase direction
    ('up 'down)
    ('down 'up)
    ('right 'left)
    ('left 'right)))

(cl-defun do-move (direction)
  "Try to execute the move described by the symbol. Returns nil if the move
   wasn't possible. Otherwise return the new position"
  (unless (valid-directionp direction)
    (signal 'wrong-type-argument direction))
  (pcase (list direction (bolp) (eolp) (first-linep) (last-linep))
    (`(up ,_ ,_ nil ,_) (forward-line-preserve-column -1) (point))
    (`(down ,_ ,_ ,_ nil) (forward-line-preserve-column 1) (point))
    (`(right ,_ nil ,_ ,_) (forward-char) (point))
    (`(left nil ,_ ,_ ,_) (forward-char -1) (point))))

(cl-defun pipes-connected-to-remote (&optional (position (point)) &aux (return-value nil))
  "Return the pipes that are connected to this position, ignoring the
pipe at that position"
  (cl-flet ((append-if-connects-to-current-pipe (direction _)
              (save-excursion (do-move direction)
                              (let ((char-at-direction (char-after)))
                                (if (-contains? (gethash char-at-direction direction-pipes nil) (inverse-move direction))
                                    (add-to-list 'return-value direction))))))
    (save-excursion (goto-char position)
      (maphash #'append-if-connects-to-current-pipe direction-maps)
      return-value)))

(cl-defun max-value-in-ht (ht &aux (return-value 0))
  (maphash (lambda (_ value) (setq return-value (max return-value value))) ht)
  return-value)

(cl-defun farthest-point-from (possible-directions &optional (initial-position (point)) &aux (minimal-distance (make-hash-table)))
  (cl-labels ((next-step (position-we-came-from &optional (current-position (point)))
                (car (-remove-item (inverse-move position-we-came-from) (gethash (char-after current-position) direction-pipes nil))))
              (go-to-next-step (direction-to-go steps-done)
                (if (= (do-move direction-to-go) initial-position) nil
                  (puthash (point) (min (1+ steps-done) (gethash (point) minimal-distance 999999999999)) minimal-distance)
                  (go-to-next-step (next-step direction-to-go) (1+ steps-done)))))
    (cl-loop for direction in possible-directions
             do (go-to-next-step direction 0))
    (max-value-in-ht minimal-distance)))

(with-current-buffer "input.txt"
  (goto-char (point-min))
  (search-forward "S")
  (goto-char (match-beginning 0))
  (farthest-point-from (pipes-connected-to-remote)))
