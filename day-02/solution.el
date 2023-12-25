;;; solution.el --- Day 2 Advent of Code Solution

;; Calculate the absolute path to the 'common' directory
(defvar aoc-root-dir (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(defvar aoc-common-dir (expand-file-name "common" aoc-root-dir))
;; Add 'common' to the load-path
(add-to-list 'load-path aoc-common-dir)

(defvar input-file (expand-file-name "input.txt" (file-name-directory (or load-file-name (buffer-file-name)))))


(require 'file-utils)
(require 'result-utils)
(require 'ert)

;; Parse a single set of cubes (e.g., "3 blue, 4 red")
;; to ((3 . "blue"), (4 . "red"))
(defun parse-set (set)
  (let ((parts (split-string set ", ")))
    (mapcar (lambda (part)
              (let ((info (split-string part " ")))
                (cons (intern (cadr info)) (string-to-number (car info)))))
            parts)))

;; Parse game data from a single line
;; Assoc of Game ID to list of sets of cubes - represented by inner assoc of rgb
;; ((1 . (((3. "blue"), (4 . "red")) ((2 . "green"))))
;;  (2 . ......))
(defun parse-game (line)
  (when (string-match "Game \\([0-9]+\\): \\(.*\\)" line)
    (let ((id (string-to-number (match-string 1 line)))
          (sets (split-string (match-string 2 line) "; ")))
      (cons id (mapcar 'parse-set sets)))))

;; Check if a game is possible with given cube counts
(defun is-game-possible (game red-count green-count blue-count)
  (let ((possible t))
    (dolist (set (cdr game) possible)   ; loop each game set, return possible
      (dolist (pair set)        ; loop each colour in set as pair
        (cond ((eq (car pair) 'red) (setq possible (and possible (<= (cdr pair) red-count))))
              ((eq (car pair) 'green) (setq possible (and possible (<= (cdr pair) green-count))))
              ((eq (car pair) 'blue) (setq possible (and possible (<= (cdr pair) blue-count)))))))
    possible))

;; Main function to process the games
(defun process-games (games red-count green-count blue-count)
  (let ((sum 0))
    (dolist (game games sum)
      (when (is-game-possible game red-count green-count blue-count)
        (setq sum (+ sum (car game)))))))

(defun minimum-cubes-for-game (game)
  "We need to find the minimum number of red, green, and blue cubes for each game to be possible.
   This means checking the maximum number of each colour shown at any point during the game."
  (let ((red-max 0) (green-max 0) (blue-max 0))
    (dolist (set (cdr game))    ; loop each game set
      (dolist (pair set)        ; loop each colour in set as pair
        (cond ((eq (car pair) 'red) (setq red-max (max red-max (cdr pair))))    ; update each max if larger candidate
              ((eq (car pair) 'green) (setq green-max (max green-max (cdr pair))))
              ((eq (car pair) 'blue) (setq blue-max (max blue-max (cdr pair)))))))
    (list red-max green-max blue-max))) ; return max for each colour

;; Calculate the power of a set of cubes
;; which is just the multiplying colour together
(defun power-of-set (red green blue)
  (* red green blue))

(defun day-02-part-01 (lines r g b)
  "Part 1"
  (let ((parsed-games (mapcar 'parse-game lines)))
    (process-games parsed-games r g b)))

(defun day-02-part-02 (lines)
  "Part 2"
  (let ((parsed-games (mapcar 'parse-game lines))
        (total-power 0))
    (dolist (game parsed-games total-power)     ; loop each parsed game as game, return total-power
      (let* ((min-cubes (minimum-cubes-for-game game))  ; return (r g b) list for the game
             (power (apply 'power-of-set min-cubes)))   ; which is passed as params to power-of-set
        (setq total-power (+ total-power power))))))    ; and added to the total

(defvar day-02-test-data
  '("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
  "List of provided test strings for day 02.")

;; Define the test
(ert-deftest day-02-tests ()
  (let ((parsed-games (mapcar 'parse-game day-02-test-data))
        (red-cubes 12)
        (green-cubes 13)
        (blue-cubes 14))
    ;; Part 1 Test
    (should (= (process-games parsed-games red-cubes green-cubes blue-cubes) 8))
    ;; Part 2 Test - Replace 2286 with the actual expected result for your test data
    (should (= (day-02-part-02 day-02-test-data) 2286))))

;; Running the functions and displaying the results
(let ((lines (read-lines input-file))
      (red-cubes 12)
      (green-cubes 13)
      (blue-cubes 14))
  (display-results (list (day-02-part-01 lines red-cubes green-cubes blue-cubes)
                         (day-02-part-02 lines))
                   '("Part 01 - The sum of the IDs"
                     "Part 02 - The sum of the power of the minimum sets")))

;; Running the test
(ert-run-tests-interactively "day-02-tests")
