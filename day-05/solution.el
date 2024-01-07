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

(defun parse-input (lines)
  "Parse the entire input into a structured format."
  (let ((current-map nil)
        (maps ()))
    (dolist (line lines)
      (cond
       ((string-match "^seeds: \\(.+\\)" line)
        (setq maps (cons (cons 'seeds (mapcar 'string-to-number (split-string (match-string 1 line) " "))) maps)))
       ((string-match "^\\(\\w+\\)-to-\\(\\w+\\) map:" line)
        (setq current-map (intern (concat (match-string 1 line) "-to-" (match-string 2 line)))))
       ((string-match "\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)" line)
        (let* ((dest-start (string-to-number (match-string 1 line)))
               (src-start (string-to-number (match-string 2 line)))
               (length (string-to-number (match-string 3 line)))
               (existing-maps (assoc current-map maps))
               (existing-ranges (cdr existing-maps)))
          (if existing-maps
              (setcdr existing-maps (append existing-ranges (list (list dest-start src-start length))))
            (setq maps (cons (cons current-map (list (list dest-start src-start length))) maps)))))
       (t nil)))
    (reverse maps)))


(defvar day-05-test-data
  (parse-input
    '("seeds: 79 14 55 13"
      ""
      "seed-to-soil map:"
      "50 98 2"
      "52 50 48"
      ""
      "soil-to-fertilizer map:"
      "0 15 37"
      "37 52 2"
      "39 0 15"
      ""
      "fertilizer-to-water map:"
      "49 53 8"
      "0 11 42"
      "42 0 7"
      "57 7 4"
      ""
      "water-to-light map:"
      "88 18 7"
      "18 25 70"
      ""
      "light-to-temperature map:"
      "45 77 23"
      "81 45 19"
      "68 64 13"
      ""
      "temperature-to-humidity map:"
      "0 69 1"
      "1 0 69"
      ""
      "humidity-to-location map:"
      "60 56 37"
      "56 93 4"))
  "Example schematic for day 5.")

(defun map-lookup (value mapping)
  "Find the corresponding value in the mapping or return the same value if not found."
  (let ((found (cl-find-if (lambda (range)
                             (and (<= (nth 1 range) value)
                                  (> (+ (nth 1 range) (nth 2 range)) value)))
                           (cdr mapping))))
    (if found
        (+ (nth 0 found) (- value (nth 1 found)))
      value)))

(defun find-location (seed maps)
  "Find the location corresponding to the given seed."
  (let ((soil (map-lookup seed (assoc 'seed-to-soil maps)))
        (fertilizer)
        (water)
        (light)
        (temperature)
        (humidity)
        (location))
    (setq fertilizer (map-lookup soil (assoc 'soil-to-fertilizer maps)))
    (setq water (map-lookup fertilizer (assoc 'fertilizer-to-water maps)))
    (setq light (map-lookup water (assoc 'water-to-light maps)))
    (setq temperature (map-lookup light (assoc 'light-to-temperature maps)))
    (setq humidity (map-lookup temperature (assoc 'temperature-to-humidity maps)))
    (setq location (map-lookup humidity (assoc 'humidity-to-location maps)))
    location))

(defun lowest-location (maps)
  "Find the lowest location for the initial seeds."
  (let ((seeds (cdr (assoc 'seeds maps))))
    (apply 'min (mapcar (lambda (seed) (find-location seed maps)) seeds))))

(ert-deftest day-05-tests ()
  (should (= (lowest-location day-05-test-data)  35)))

(defun day-05-part-01 (lines)
  "Day 5, Part 1."
  (lowest-location (parse-input lines)))

(let ((lines (read-lines day-05-input-file)))
  (display-results (list (day-05-part-01 lines))
                   '("Part 01 - The lowest location number")))

(ert-run-tests-interactively "day-05-tests")
