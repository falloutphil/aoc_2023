;;; solution.el --- Day 3 Advent of Code Solution

(defvar day-03-input-file (expand-file-name "input.txt" (file-name-directory (or load-file-name buffer-file-name))))

(defun parse-schematic (line)
  "Parse a single line of the schematic into a list of characters."
  (split-string line "" t))

(defun is-adjacent-to-symbol? (x y schematic)
  "Check if the position (x, y) is adjacent to any symbol."
  (cl-loop for dx from -1 to 1
           for dy from -1 to 1
           when (and (not (and (= dx 0) (= dy 0))) ; Exclude the center cell
                     (>= (+ x dx) 0) (>= (+ y dy) 0) ; Ensure indices are within bounds
                     (< (+ x dx) (length schematic)) (< (+ y dy) (length (nth 0 schematic))))
           thereis (let ((cell (nth (+ y dy) (nth (+ x dx) schematic))))
                     (string-match-p "[*#+$]" cell))))

(defun sum-part-numbers (schematic)
  "Calculate the sum of all part numbers in the schematic."
  (cl-loop for y from 0 below (length schematic)
           sum (cl-loop for x from 0 below (length (nth y schematic))
                        when (and (is-adjacent-to-symbol? x y schematic)
                                  (string-match-p "[0-9]+" (nth x (nth y schematic))))
                        sum (string-to-number (nth x (nth y schematic))))))

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

(defun day-03-part-01 ()
  "Calculate the sum of part numbers for Day 3, Part 1."
  (let ((lines (with-temp-buffer
                 (insert-file-contents day-03-input-file)
                 (split-string (buffer-string) "\n" t))))
    (sum-part-numbers (mapcar 'parse-schematic lines))))

(let ((result (day-03-part-01)))
  (message "Part 01 - The sum of the part numbers: %s" result))

(ert-run-tests-interactively "day-03-tests")
