;;; autoload/org-urgency.el -*- lexical-binding: t; -*-

;; Activate this sorting in the agenda:
(setq org-agenda-cmp-user-defined #'+org-urgency/org-urgency-cmp)
(setq org-agenda-sorting-strategy '(user-defined-down))

(defgroup org-urgency nil
  "Taskwarrior-like urgency scoring for Org." :group 'org)

(defcustom org-urgency-coeffs
  '((next . 15.0) (due . 12.0)
    (prio-high . 6.0) (prio-med . 3.9) (prio-low . 1.8)
    (scheduled . 5.0) (active . 4.0)
    (age . 2.0) (tags . 1.0) (project . 1.0)
    (waiting . -3.0) (blocked . -5.0))
  "Alist of urgency term → coefficient."
  :type '(alist :key-type symbol :value-type number)
  :group 'my-org-urgency)

;;;###autoload
(defun org-urgency/org-calc-urgency ()
  "Compute a Taskwarrior-style urgency score for the current Org heading."
  (let* ((deadline (org-entry-get nil "DEADLINE"))
         (scheduled (org-entry-get nil "SCHEDULED"))
         (tags (org-get-tags))
         (prio (org-entry-get nil "PRIORITY"))
         (today (org-time-string-to-absolute (format-time-string "%Y-%m-%d")))
         (score 0.0))
    ;; +15 for :next: tag
    (when (member "next" tags)
      (setq score (+ score 15.0)))
    ;; Deadline factor
    (when deadline
      (let* ((due-day (org-time-string-to-absolute (substring deadline 1 -1)))
             (days (- due-day today))
             (factor (cond ((>= days 14) 0.2)
                           ((>= days -7) (+ 0.2 (* 0.8 (/ (+ days 14.0) 21.0))))
                           (t 1.0))))
        (setq score (+ score (* 12.0 factor)))))
    ;; Scheduled tasks
    (when scheduled
      (setq score (+ score 5.0)))
    ;; Waiting TODO state
    (when (string= (org-get-todo-state) "WAITING")
      (setq score (- score 3.0)))
    ;; Priority A/B/C
    (when prio
      (setq score (+ score
                     (cond ((string= prio "A") 6.0)
                           ((string= prio "B") 3.9)
                           ((string= prio "C") 1.8)
                           (t 0.0)))))
    ;; Tags count factor
    (let ((n (length tags)))
      (when (> n 0)
        (setq score (+ score (* 1.0
                                (cond ((= n 1) 0.8)
                                      ((= n 2) 0.9)
                                      (t 1.0)))))))
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
