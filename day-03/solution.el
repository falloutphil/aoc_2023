;;; solution.el --- Day 3 Advent of Code Solution

;; Calculate the absolute path to the 'common' directory
(defvar aoc-root-dir (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(defvar aoc-common-dir (expand-file-name "common" aoc-root-dir))
;; Add 'common' to the load-path
(add-to-list 'load-path aoc-common-dir)

(defvar day-03-input-file (expand-file-name "input.txt" (file-name-directory (or load-file-name buffer-file-name))))

(require 'file-utils)
(require 'result-utils)
(require 'ert)

(defun parse-schematic (line)
  "Parse a single line of the schematic into a list of characters."
  (split-string line "" t))

(defun is-adjacent-to-symbol? (x y schematic)
  "Check if the position (x, y) is adjacent to any symbol."
  (catch 'found-symbol ; use catch to allow an immediate exit from nested loops
    (cl-loop for dx from -1 to 1 do
             (cl-loop for dy from -1 to 1 do
                      (when (and (not (and (= dx 0) (= dy 0))) ; Exclude the center cell
                                 (>= (+ x dx) 0) (>= (+ y dy) 0) ; Ensure indices are within bounds
                                 (< (+ x dx) (length (nth 0 schematic))) ; x + dx less than schematic width
                                 (< (+ y dy) (length schematic))) ; y + dy less than schematic height
                        (let ((cell (nth (+ x dx) (nth (+ y dy) schematic))))
                          ;;(message "DEBUG: Digit %s at %d,%d, neighbour %d,%d a symbol?  %s"
                                   ;;(nth x (nth y schematic)) x y dx dy cell)
                          (when (string-match-p "[^0-9.]" cell) ; not a digit or full-stop
                            (throw 'found-symbol t)))))) ; throw to return t immediately when a symbol is found
    nil)) ; return nil if no symbol is found after all iterations


(defun sum-part-numbers (schematic)
  "Calculate the sum of all part numbers in the schematic."
  (let ((sum 0))
    ;; Loop through each cell in the schematic.
    (cl-loop for y from 0 below (length schematic) do
             (cl-loop for x from 0 below (length (nth y schematic)) do
                      (let ((cell (nth x (nth y schematic))))
                        ;; Check if the cell is a digit.
                        (when (string-match-p "[0-9]" cell)
                          ;; Find the whole number this digit is a part of.
                          (let ((end x)
                                (number-string cell))
                            ;; Extend the number to the right.
                            (while (and (< (+ end 1) (length (nth y schematic)))
                                        (string-match-p "[0-9]" (nth (+ end 1) (nth y schematic))))
                              (setq end (1+ end))
                              (setq number-string (concat number-string (nth end (nth y schematic)))))
                            ;; Check each part of the number for adjacency to a symbol.
                            ;; Early exit on finding symbol into body of the when.
                            (when (cl-loop for i from x to end
                                           thereis (is-adjacent-to-symbol? i y schematic))
                              ;; If any part of the number is adjacent to a symbol, add to sum.
                              (setq sum (+ sum (string-to-number number-string))))
                            ;; Skip past the end of the current number.
                            (setq x end))))))
    sum))


(defvar day-03-test-data
  '("467..114.."
    "...*......"
    "..35..633."
    "......#..."
    "617*......"
    ".....+.58."
    "..592....."
    "......755."
    "...$.*...."
    ".664.598..")
  "Example schematic for day 3.")

(ert-deftest day-03-tests ()
  (let ((test-schematic (mapcar 'parse-schematic
                                day-03-test-data)))
    (should (= (sum-part-numbers test-schematic) 4361))))

(defun day-03-part-01 (lines)
  "Calculate the sum of part numbers for Day 3, Part 1."
  (sum-part-numbers (mapcar 'parse-schematic lines)))

(let ((lines (read-lines day-03-input-file)))
  (display-results (list (day-03-part-01 lines))
                   '("Part 01 - The sum of the part numbers")))

(ert-run-tests-interactively "day-03-tests")
