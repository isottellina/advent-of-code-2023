;; -*- lexical-binding: t; -*-

(cl-defun forward-line-preserve-column (&optional (arg 1) &aux (column (current-column)))
  (forward-line arg)
  (move-to-column column))

(defun peek-char (direction)
  (cl-assert (member direction '(north west south east)))
  (save-excursion (pcase direction
                    ('north (unless (= (line-number-at-pos) 1)
                              (forward-line-preserve-column -1)
                              (char-after)))
                    ('west (unless (= (current-column) 0)
                             (char-before)))
                    ('east (let ((characters-to-line-end (- (line-end-position) (point))))
                             (unless (<= characters-to-line-end 1)
                               (forward-char)
                               (char-after))))
                    ('south (let ((is-end-of-buffer-after-forward-line (save-excursion (forward-line) (eobp))))
                              (unless is-end-of-buffer-after-forward-line
                                (forward-line-preserve-column 1)
                                (char-after)))))))

(defun go-direction (direction)
  (cl-assert (member direction '(north west south east)))
  (pcase direction
    ('north (forward-line-preserve-column -1))
    ('south (forward-line-preserve-column 1))
    ('east (forward-char))
    ('west (backward-char))))


(defun replace-char (c)
  (delete-char 1)
  (insert-char c)
  (backward-char))

(defun search-function (direction)
  (cl-assert (member direction '(north west south east)))
  (pcase direction
    ('north #'search-forward)
    ('west #'search-forward)
    ('south #'search-backward)
    ('east #'search-backward)))

(defun tilt-buffer (direction)
  (cl-assert (member direction '(north west south east)))
  (goto-char (cond ((member direction '(north west)) (point-min))
                   ((member direction '(south east)) (point-max))))

  (cl-loop while (funcall (search-function direction) "O" nil t)
           do (goto-char (match-beginning 0))
           do (replace-char ?.)
           do (save-excursion (while-let ((peeked (peek-char direction))
                                          (_ (= peeked ?.)))
                                (go-direction direction))
                              (replace-char ?O))
           do (when (member direction '(north west)) (forward-char))))


(with-current-buffer "input.txt"
  (goto-char (point-min))
  (setq maximum-line (save-excursion (goto-char (point-max))
                                     (line-number-at-pos (1- (point)))))

  ; Finding the period and finding out what the 1000000000th step is going to look like
  (pcase-let* ((`(,buffer-contents . ,final-content)
                (cl-loop for current-content = (buffer-substring-no-properties (point-min ) (point-max))
                         for current-hash = (buffer-hash)
                         for idx from 0
                         do (message "Cycle %d" idx)
                         until (member current-hash buffer-hashes)
                         collect current-hash into buffer-hashes
                         collect current-content into buffer-contents
                         do (progn (tilt-buffer 'north)
                              (tilt-buffer 'west)
                              (tilt-buffer 'south)
                              (tilt-buffer 'east))
                         finally return (cons buffer-contents current-content)))
               (period-start (seq-position buffer-contents final-content))
               (period-length (- (length buffer-contents) period-start))
               (index-of-final-state (+ (mod (- 1000000000 period-start) period-length) period-start)))

    (with-temp-buffer
      (insert (seq-elt buffer-contents index-of-final-state))
      (goto-char (point-min))
      (cl-loop while (search-forward "O" nil t)
               sum (1+ (- maximum-line (line-number-at-pos)))))))
