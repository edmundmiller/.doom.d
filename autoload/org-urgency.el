;;; autoload/org-urgency.el -*- lexical-binding: t; -*-

;; Activate this sorting in the agenda:
(setq org-agenda-cmp-user-defined #'+org-urgency/org-urgency-cmp)
(setq org-agenda-sorting-strategy '(user-defined-down))

;; -----------------------------
;; Taskwarrior-style urgency — tunable
;; -----------------------------
(require 'org)
(require 'cl-lib)

(defgroup my-org-urgency nil
  "Taskwarrior-like urgency scoring for Org-mode."
  :group 'org)

(defcustom my/org-urgency-coeffs
  '((next        . 15.0)   ;; +next tag
    (due         . 12.0)   ;; due-date factor (will be scaled 0-1)
    (prio-high   .  6.0)
    (prio-med    .  3.9)
    (prio-low    .  1.8)
    (scheduled   .  5.0)
    (active      .  4.0)   ;; not used yet, placeholder for CLOCKING etc.
    (age         .  2.0)   ;; multiplied by age / age-max
    (tags        .  1.0)
    (project     .  1.0)
    (waiting     . -3.0)
    (blocked     . -5.0))
  "Alist mapping urgency term symbols to coefficients.
Adapt any number to change its weight.  Reload or `eval-buffer` to apply."
  :type '(alist :key-type symbol :value-type number)
  :group 'my-org-urgency)

(defun my/org--u (key)
  "Return coefficient for KEY from `my/org-urgency-coeffs`.
If KEY is absent or nil, return 0."
  (or (cdr (assq key my/org-urgency-coeffs)) 0))

(defun my/org-calc-urgency ()
  "Compute a Taskwarrior-style urgency score for the current Org heading."
  (let* ((deadline  (org-entry-get nil "DEADLINE"))
         (scheduled (org-entry-get nil "SCHEDULED"))
         (tags      (org-get-tags))
         (prio      (org-entry-get nil "PRIORITY"))
         (todo      (org-get-todo-state))
         (today     (org-time-string-to-absolute (format-time-string "%Y-%m-%d")))
         (age-max   365)                    ; you can also defcustom this
         (age       (when-let ((c (org-entry-get nil "CREATED")))
                      (abs (- today (org-time-string-to-absolute
                                     (substring c 1 -1))))))
         (score 0.0))

    ;; +next tag
    (when (member "next" tags)
      (cl-incf score (my/org--u 'next)))

    ;; due-date factor (same cut-offs Taskwarrior uses)
    (when deadline
      (let* ((due-day (org-time-string-to-absolute (substring deadline 1 -1)))
             (days    (- due-day today))
             (factor  (cond
                       ((>= days 14) 0.2)          ; >14 days away
                       ((>= days -7)               ; within 14 → -7
                        (+ 0.2 (* 0.8 (/ (+ 14.0 (- days)) 21.0))))
                       (t 1.0))))                 ; 7 days overdue or more
        (cl-incf score (* (my/org--u 'due) factor))))

    ;; scheduled flag
    (when scheduled
      (cl-incf score (my/org--u 'scheduled)))

    ;; waiting state
    (when (string= todo "WAITING")
      (cl-incf score (my/org--u 'waiting)))  ; note: waiting is negative

    ;; priority A/B/C
    (pcase prio
      ("A" (cl-incf score (my/org--u 'prio-high)))
      ("B" (cl-incf score (my/org--u 'prio-med)))
      ("C" (cl-incf score (my/org--u 'prio-low))))

    ;; age term (requires CREATED: timestamp)
    (when age
      (let* ((factor (min 1.0 (/ (float age) age-max))))
        (cl-incf score (* (my/org--u 'age) factor))))

    ;; tag count factor (0.8 / 0.9 / 1.0 × coeff)
    (let* ((n (length tags))
           (tag-factor (cond ((= n 0) 0)
                             ((= n 1) 0.8)
                             ((= n 2) 0.9)
                             (t 1.0))))
      (cl-incf score (* (my/org--u 'tags) tag-factor)))

    ;; project flag (you decide what counts as a “project”)
    (when (or (org-entry-get nil "PROJECT") ; :PROJECT: property
              (org-get-outline-path))       ; any parent == “project”
      (cl-incf score (my/org--u 'project)))

    score))

;;;###autoload
(defun org-urgency/org-urgency-cmp (a b)
  "Compare two agenda entries by custom urgency."
  (let* ((am (get-text-property 0 'org-marker a))
         (bm (get-text-property 0 'org-marker b))
         ;; Compute each entry's urgency by moving to its marker
         (ua (with-current-buffer (marker-buffer am)
               (org-with-point-at am (+emiller/org-calc-urgency))))
         (ub (with-current-buffer (marker-buffer bm)
               (org-with-point-at bm (+emiller/org-calc-urgency)))))
    ;; Sort descending (highest urgency first)
    (cond ((> ua ub) -1) ((< ua ub) 1) (t nil))))
