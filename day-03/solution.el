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
(require 'cl-lib)

(defun parse-schematic (line)
  "Parse a single line of the schematic into a vector of characters."
  (vconcat (split-string line "" t)))

(defun is-adjacent-to-symbol? (x y schematic)
  "Check if the position (x, y) is adjacent to any symbol."
  (catch 'found-symbol
    (cl-loop for dx from -1 to 1 do
             (cl-loop for dy from -1 to 1 do
                      (when (and (not (and (= dx 0) (= dy 0)))
                                 (>= (+ x dx) 0) (>= (+ y dy) 0)
                                 (< (+ x dx) (length schematic))
                                 (< (+ y dy) (length (aref schematic 0))))
                        (let ((cell (aref (aref schematic (+ y dy)) (+ x dx))))
                          (when (string-match-p "[^0-9.]" cell)
                            (throw 'found-symbol t))))))
    nil))

(defun sum-part-numbers (schematic)
  "Calculate the sum of all part numbers in the schematic."
  (let ((sum 0))
    (cl-loop for y from 0 below (length schematic) do
             (cl-loop for x from 0 below (length (aref schematic y)) do
                      (let ((cell (aref (aref schematic y) x)))
                        (when (string-match-p "[0-9]" cell)
                          (let ((end x)
                                (number-string cell))
                            (while (and (< (+ end 1) (length (aref schematic y)))
                                        (string-match-p "[0-9]" (aref (aref schematic y) (+ end 1))))
                              (cl-incf end)
                              (setq number-string (concat number-string (aref (aref schematic y) end))))
                            (when (cl-loop for i from x to end thereis (is-adjacent-to-symbol? i y schematic))
                              (cl-incf sum (string-to-number number-string)))
                            (setq x end))))))
    sum))

(defvar day-03-test-data
  (vector
   (parse-schematic "467..114..")
   (parse-schematic "...*......")
   (parse-schematic "..35..633.")
   (parse-schematic "......#...")
   (parse-schematic "617*......")
   (parse-schematic ".....+.58.")
   (parse-schematic "..592.....")
   (parse-schematic "......755.")
   (parse-schematic "...$.*....")
   (parse-schematic ".664.598.."))
  "Example schematic for day 3.")

(ert-deftest day-03-tests ()
  (should (= (sum-part-numbers day-03-test-data) 4361)))

(defun day-03-part-01 (lines)
  "Calculate the sum of part numbers for Day 3, Part 1."
  (sum-part-numbers (vconcat (mapcar 'parse-schematic lines))))

(let ((lines (read-lines day-03-input-file)))
  (display-results (list (day-03-part-01 lines))
                   '("Part 01 - The sum of the part numbers")))

(ert-run-tests-interactively "day-03-tests")
