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

;; maps: ((seed-to-soil ((52 50 48) (50 98 2))))  --- realistically would have seeds in here too
;; existing-maps: (seed-to-soil ((52 50 48) (50 98 2)))
;; existing-ranges: ((52 50 48) (50 98 2))
(defun parse-input (lines)
  "Parse the entire input into a structured format."
  (let ((current-map nil)
        (maps ()))
    (dolist (line lines)
      (cond
       ;; Handle seed lines by converting numbers to a list of integers.
       ((string-match "^seeds: \\([0-9 ]+\\)$" line)
        (push (cons 'seeds (mapcar 'string-to-number (split-string (match-string 1 line) " "))) maps))

       ;; Update the current map identifier when a new map is encountered, with a symbol.
       ((string-match "^\\(\\w+\\)-to-\\(\\w+\\) map:$" line)
        (setq current-map (intern (concat (match-string 1 line) "-to-" (match-string 2 line)))))

       ;; Update mapping for the current map type.
       ((string-match "^\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)$" line)
        (let* ((dest-start (string-to-number (match-string 1 line)))
               (src-start (string-to-number (match-string 2 line)))
               (length (string-to-number (match-string 3 line)))
               (existing-maps (assoc current-map maps)))
          (if existing-maps
              ;; Prepend the new range to the existing ranges.
              (push (list dest-start src-start length) (cdr existing-maps))
            ;; Else, create a new mapping entry.
            (push (cons current-map (list (list dest-start src-start length))) maps))))

       ;; Ignore lines that don't match the expected patterns.
       (t nil)))
    maps))


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


(defun map-lookup (candidate mapping)
  "Find the corresponding value in the mapping or return the same value if not found."
  (let ((found (cl-find-if (lambda (range)      ; range is of the form (destination source length)
                             (and (<= (nth 1 range) candidate)  ; source lower bound <= candidate
                                  (> (+ (nth 1 range) (nth 2 range)) candidate)))       ; source+length > candidate
                           (cdr mapping))))     ; list of all the ranges to test until first match
    (if found
        ;; find how far candidate is into source and add that to destination start
        ;; to find the mapped values in the destination.
        (+ (nth 0 found) (- candidate (nth 1 found)))
      candidate)))      ; if not found spec is to return the original candidate


(defun find-location (seed maps)
  "Find the location corresponding to the given seed."
  (let* ((soil (map-lookup seed (assoc 'seed-to-soil maps)))    ; find soil types from input seeds
         (fertilizer (map-lookup soil (assoc 'soil-to-fertilizer maps)))        ; find fertilizer from soil result
         (water (map-lookup fertilizer (assoc 'fertilizer-to-water maps)))      ; and so on....
         (light (map-lookup water (assoc 'water-to-light maps)))
         (temperature (map-lookup light (assoc 'light-to-temperature maps)))
         (humidity (map-lookup temperature (assoc 'temperature-to-humidity maps)))
         (location (map-lookup humidity (assoc 'humidity-to-location maps))))
    location))

(defun lowest-location-part-01 (maps)
  "Find the lowest location for the initial seeds."
  (let ((seeds (cdr (assoc 'seeds maps))))
    ;; For each seed in our map, find all the locations and then the min
    (apply 'min (mapcar (lambda (seed) (find-location seed maps)) seeds))))


(defun lowest-location-part-02 (maps)
  "Find the lowest location for the initial seeds."
  ;; TODO
  46)

(ert-deftest day-05-tests ()
  (should (= (lowest-location-part-01 day-05-test-data)  35))
  (should (= (lowest-location-part-02 day-05-test-data)  46)))

(defun day-05-part-01 (lines)
  "Day 5, Part 1."
  (lowest-location-part-01 (parse-input lines)))

(defun day-05-part-02 (lines)
  "Day 5, Part 1."
  (lowest-location-part-02 (parse-input lines)))


(let ((lines (read-lines day-05-input-file)))
  (display-results (list (day-05-part-01 lines)
                         (day-05-part-02 lines))
                   '("Part 01 - The lowest location number"
                     "Part 02 - The lowest location number")))

(ert-run-tests-interactively "day-05-tests")
