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

(defun minimum-cubes (game)
  (let ((red 0)
        (blue 0)
        (green 0))
    (cl-loop for pick in (game-picks game)
             do (progn (setq red (max red (pick-red pick)))
                       (setq blue (max blue (pick-blue pick)))
                       (setq green (max green (pick-green pick)))))
    (make-pick :red red :blue blue :green green)))

(defun pick-power (pick)
  (* (pick-red pick) (pick-blue pick) (pick-green pick)))


(with-temp-buffer
  (insert-file-contents "input.txt")
  (goto-char (point-min))
  (let ((existing-games (cl-loop until (eobp)
                                 collect (parse-game))))
    (cl-loop for game in existing-games
             sum (pick-power (minimum-cubes game)))))
