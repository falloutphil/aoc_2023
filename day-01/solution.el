;;; solution.el --- Day 1 Advent of Code Solution

;; Calculate the absolute path to the 'common' directory
(defvar aoc-root-dir (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(defvar aoc-common-dir (expand-file-name "common" aoc-root-dir))
;; Add 'common' to the load-path
(add-to-list 'load-path aoc-common-dir)

(defvar input-file (expand-file-name "input.txt" (file-name-directory (or load-file-name (buffer-file-name)))))

;; Ensure the file-utils are loaded
(require 'file-utils)
(require 'ert)

(defvar str2num
  '(("one" . "o1e") ("two" . "t2o") ("three" . "t3e") ("four" . "f4r")
    ("five" . "f5e") ("six" . "s6x") ("seven" . "s7n") ("eight" . "e8t") ("nine" . "n9e"))
  "Mapping of word digits to a numerical string.  Keep first and last letters in case adjacent numbers share letters.")

(defun replace-words (text)
  "Replace all instances of words in TEXT according to the str2num mapping."
  (let ((result text))
    (dolist (pair str2num result)
      (setq result (replace-regexp-in-string (car pair) (cdr pair) result)))))

(defun extract-calibration-value (line)
  "Extract the calibration value from a line of text."
  (let ((digits (seq-filter (lambda (char) (and (>= char ?0) (<= char ?9))) line)))
    (if (>= (length digits) 2)
      (string-to-number (concat (list (car digits) (car (last digits)))))
      (string-to-number (concat (list (car digits) (car digits)))))))

(defun sum-calibration-values (extractor data-as-line-list)
  "Calculate the sum of calibration values from the file at FILE-PATH."
  (apply '+ (mapcar extractor data-as-line-list)))

(defun day-01-part-01 (lines)
  "Part 1"
  (message "The sum of all calibration values is: %s"
           (sum-calibration-values 'extract-calibration-value lines)))

(defun day-01-part-02 (lines)
  "Part 2"
  (message "The sum of all calibration values is: %s"
           (sum-calibration-values (lambda (line) (extract-calibration-value (replace-words line))) lines)))

(defvar part-01-test-data
  '("1abc2"
    "pqr3stu8vwx"
    "a1b2c3d4e5f"
    "treb7uchet")
  "List of provided test strings for part 1.")

(defvar part-02-test-data
  '("two1nine"
    "eightwothree"
    "abcone2threexyz"
    "xtwone3four"
    "4nineeightseven2"
    "zoneight234"
    "7pqrstsixteen")
  "List of proviuded test strings with mixed words and numbers for part 2.")

;; Define a test for the `add-two` function
(ert-deftest day-01-tests ()
  (should (= (sum-calibration-values
              'extract-calibration-value part-01-test-data) 142))
  (should (= (sum-calibration-values
              (lambda (line) (extract-calibration-value (replace-words line))) part-02-test-data) 281)))

;; Run tests
(ert-run-tests-interactively "day-01-tests")

;; If tests pass run the real thing!
(let ((lines (read-lines input-file)))
  (day-01-part-01 lines)
  (day-01-part-02 lines))
