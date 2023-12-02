(cl-defstruct pick
  (red 0 :type number)
  (blue 0 :type number)
  (green 0 :type number))

(cl-defstruct game
  (id 0 :type number)
  (picks nil :type list))

(defun parse-regexp (regexp)
  (when (looking-at regexp)
      (progn (goto-char (match-end 0))
             (skip-chars-forward "[:blank:]")
             (match-string-no-properties 0))))

(defun parse-pick ()
  (let ((red 0)
        (blue 0)
        (green 0))
    (cl-loop while (parse-regexp "\\([[:digit:]]+\\) \\(red\\|blue\\|green\\)")
             do (let ((number (string-to-number (match-string-no-properties 1)))
                      (color (match-string-no-properties 2)))
                  (cond ((string= color "red") (setq red (+ red number)))
                        ((string= color "blue") (setq blue (+ blue number)))
                        ((string= color "green") (setq green (+ green number)))))
             do (skip-chars-forward ",[:blank:]")
             finally do (skip-chars-forward ";[:blank:]")
             finally return (make-pick :red red :blue blue :green green))))

(defun parse-game ()
  (parse-regexp "Game \\([[:digit:]]+\\):")
  (make-game
   :id (string-to-number (match-string-no-properties 1))
   :picks (cl-loop until (eolp)
                   finally do (skip-chars-forward "[:space:]")
                   collect (parse-pick))))

(defun is-pick-possible (pick pick-limit)
  (and (<= (pick-red pick) (pick-red pick-limit))
       (<= (pick-blue pick) (pick-blue pick-limit))
       (<= (pick-green pick) (pick-green pick-limit))))

(defun is-game-possible (game pick-limit)
  (-every (lambda (pick) (is-pick-possible pick pick-limit)) (game-picks game)))

(with-temp-buffer
  (insert-file-contents "input.txt")
  (goto-char (point-min))
  (let ((pick-limit (make-pick :red 12 :blue 14 :green 13))
        (existing-games (cl-loop until (eobp)
                                collect (parse-game))))
    (cl-loop for game in existing-games
             if (is-game-possible game pick-limit)
             sum (game-id game))))
