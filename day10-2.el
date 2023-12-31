;; -*- lexical-binding: t; -*-

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


(cl-defun col-and-line-at-pos (&optional (pos (point)))
  (save-excursion (goto-char pos)
                  (cons (current-column) (line-number-at-pos))))

(cl-defun buffer-dimensions (&optional (buffer (current-buffer)))
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      ; If we're at the beginning of the line, we're in the empty line
      ; generated by saving.
      (forward-char -1)
      (col-and-line-at-pos (point)))))

(defconst direction-maps '(up down left right))

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
  (and (symbolp direction) (seq-contains-p direction-maps direction)))

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
  (cl-flet ((append-if-connects-to-current-pipe (direction)
              (save-excursion (do-move direction)
                              (let ((char-at-direction (char-after)))
                                (if (seq-contains-p (gethash char-at-direction direction-pipes nil) (inverse-move direction))
                                    (add-to-list 'return-value direction))))))
    (save-excursion (goto-char position)
      (mapc #'append-if-connects-to-current-pipe direction-maps)
      return-value)))

(cl-defun get-points-in-loop (possible-directions &optional (initial-position (point)) &aux (points-in-loop (list initial-position)))
   (cl-labels ((next-step (position-we-came-from &optional (current-position (point)))
                (car (-remove-item (inverse-move position-we-came-from) (gethash (char-after current-position) direction-pipes nil))))
              (go-to-next-step (direction-to-go)
                (if (= (do-move direction-to-go) initial-position) nil
                  (cl-pushnew (point) points-in-loop)
                  (go-to-next-step (next-step direction-to-go)))))
    ; Only go in one direction, it's a loop after all
    (go-to-next-step (car possible-directions))
    points-in-loop))

(cl-defun get-loop-svg (points-in-loop &optional (buffer-dimensions (buffer-dimensions))
                                       &aux (svg (svg-create (+ (car buffer-dimensions) 5)
                                                             (+ (cdr buffer-dimensions) 5) :background "white")))
    (svg-polygon svg (cl-loop for loop-point in points-in-loop
                              collect (pcase-let ((`(,col . ,line) (col-and-line-at-pos loop-point)))
                                        (cons col line)))
                 :stroke-color "black" :fill-color "black")
    svg)

(with-current-buffer "input.txt"
  (goto-char (point-min))
  (search-forward "S")
  (goto-char (match-beginning 0))
  (let* ((points-in-loop (get-points-in-loop (pipes-connected-to-remote)))
         (svg (get-loop-svg points-in-loop))
         (svg-buffer (generate-new-buffer "*SVG"))
         (pbm-buffer (generate-new-buffer "*PBM*")))
    ; Convert SVG to PBM with imagemagick
    (with-current-buffer svg-buffer
      (svg-print svg)
      (call-process-region (point-min) (point-max) "convert" nil pbm-buffer nil "MSVG:-" "-compress" "none" "pbm:-"))
    ; Count 1s in the image and subtract the number of points in the loop
    (let* ((number-ones (with-current-buffer pbm-buffer
                          (goto-char (point-min))
                          (forward-line 2) ; Skip the header
                          (cl-loop while (search-forward "1" nil t)
                                   count 1)))
           (number-loop (length points-in-loop)))
      ; Kill the buffers to cleanup everything
      (kill-buffer svg-buffer)
      (kill-buffer pbm-buffer)
      (- number-ones number-loop))))
