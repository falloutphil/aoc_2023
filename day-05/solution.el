;;; solution.el --- Day 5 Advent of Code Solution

;; Calculate the absolute path to the 'common' directory
(defvar aoc-root-dir (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(defvar aoc-common-dir (expand-file-name "common" aoc-root-dir))
;; Add 'common' to the load-path
(add-to-list 'load-path aoc-common-dir)

(defvar day-05-input-file (expand-file-name "input.txt" (file-name-directory (or load-file-name buffer-file-name))))

(require 'file-utils)
(require 'result-utils)
(require 'ert)
(require 'cl-lib)

(defun parse-input (line)
  "Parse a single line of the input into a structure."
  '(structured data to return 1234))


(defvar day-05-test-data
  (mapcar 'parse-input
          '("SOME NUMBERS AND STUFF TO TEST WITH"
            "ANOTHER LINE OF TEST DATA"
            ))
  "Example schematic for day 5.")

(defun some-function (parsed-line)
  1234)

(defun some-other-function (parsed-line)
  5678)


(ert-deftest day-05-tests ()
  (should (= (some-function day-05-test-data) 1234))
  (should (= (some-other-function day-05-test-data) 5678)))

(defun day-05-part-01 (lines)
  "Day 5, Part 1."
  (some-function (mapcar 'parse-input lines)))

(defun day-05-part-02 (lines)
  "Day 5, Part 2."
  (some-other-function (mapcar 'parse-input lines)))


;(let ((lines (read-lines day-05-input-file)))
;  (display-results (list (day-05-part-01 lines)
;                         (day-05-part-02 lines))
;                   '("Part 01 - Some text to describe"
;                     "Part 02 - Some more text to describe")))

(ert-run-tests-interactively "day-05-tests")
