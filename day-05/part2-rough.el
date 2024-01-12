
;; this is the seeds but is updated to
;; represent seed, soil, ....., and finally location
(setq inputs '((79 . 93) (55 . 68)))

;;
(setq map-types '(((50 98 2) (52 50 48))
            ((0 15 37) (37 52 2) (39 0 15))
            ((49 53 8) (0 11 42) (42 0 7) (57 7 4))
            ((88 18 7) (18 25 70))
            ((45 77 23) (81 45 19) (68 64 13))
            ((0 69 1) (1 0 69))
            ((60 56 37) (56 93 4))))

;; NOTE: Non-overlapping sub-ranges of a given seed range retain their original values,
;; rather than assuming they could overlap a different map candidate.
;; If we get a partial match, i.e. some of our seed range overlaps with only some of the source transformation -
;; we map to the overlapped inputs destination, but for the unmapped seeds we carry them over as-is,
;; with no transformation into the destination, rather than search other candidate maps.
;; This isn't massively clear from the puzzle text but we have the quote:
;; "Any source numbers that aren't mapped correspond to the same destination number."
;; Although this is mentioned in Part 1, we are to assume it also applies in Part 2!
;; This is important because we can short-circuit as soon as a seed-range partially overlaps
;; any candidate map for seed-to-soil, and so on.
(defun remap (start end range-to-translate m) ; initially range-to-translate is seeds
  (catch 'break
    (dolist (mapping m) ; each map-type has many mappings loop over them
      (let* ((destination-range-start (nth 0 mapping))
             (source-range-start (nth 1 mapping))
             (range-length (nth 2 mapping))
             (overlap-start (max start source-range-start))
             (overlap-end (min end (+ source-range-start range-length))))
        (when (< overlap-start overlap-end) ; when there is some overlap
          (push (cons (+ destination-range-start (- overlap-start source-range-start))
                      (+ destination-range-start (- overlap-end source-range-start)))
                range-to-translate)      ; append the destination range to cover the overlap of seeds and source-range
          ;; then any unmapped seed sub-ranges either side of overlapping ranges
          ;; we pass-through the original range (see comment above)
          (when (< start overlap-start)
            (push (cons start overlap-start) range-to-translate))
          (when (< overlap-end end)
            (push (cons overlap-end end) range-to-translate))
	  ;; early exit - we only map the first matching source range
          (throw 'break nil))))
    (push (cons start end) range-to-translate))
  range-to-translate)


(dolist (maps map-types inputs) ; loop over each map-type (eg seed-to-soil) in sequence - order is important!
  (let ((translated-range '()))  ; Initialize new-seeds as an empty list for each map
    (while inputs
      (let ((pair (pop inputs)))
        (setq start (car pair))
        (setq end (cdr pair))
        (setq translated-range (remap start end translated-range maps))))
    (setq inputs translated-range)))


(let ((min-car (apply 'min (mapcar 'car inputs))))
  (message "Minimum location: %d" min-car))
