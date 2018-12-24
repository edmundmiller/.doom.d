;;; tools/christmas/white-christmas.el -*- lexical-binding: t; -*-

;;; -*- lexical-binding: t; -*-

(defface wc-background-face
  '((t (:background "#282c34")))
  "Background face")

(defface wc-flake-face
  '((t (:inherit
	wc-background-face
	:foreground "white")))
  "Flake face.")

(defvar wc-flake ?❄
  "Flake char.")

(defvar wc-background ?\s
  "Background char.")

(defvar wc-speed 0.1
  "Speed of flakes falling down.")

(defvar wc--flake-alist nil
  "Column/row coordinates of flakes.")

(defvar wc--width 0
  "Available witdh.")

(defvar wc--height 0
  "Available height.")

(defun wc--clear (x y)
  "Clear position X/Y."
  (wc--draw x y wc-background 'wc-background-face))

(defun wc--update-flake (column)
  "Update flake position for COLUMN."
  (let ((item (assq column wc--flake-alist)))
    ;; clear old pos
    (wc--clear (car item) (cdr item))
    ;; reset flake to start or advance
    (setf (cdr item)
          (if (= (cdr item) wc--height)
              1
            (1+ (cdr item))))
    (wc--draw (car item)
              (cdr item)
              wc-flake
              'wc-flake-face)))

(defun wc--draw (x y char face)
  "Draw CHAR at position X/Y."
  (goto-char (+ x (* (1- y) (1+ wc--width))))
  (when (memq (char-after) (list wc-flake wc-background))
    (delete-char 1)
    (insert (propertize (char-to-string char) 'face face))
    (goto-char (point-min))))

(defun wc--setup (buffer)
  "Setup BUFFER for `white-christmas'."
  (with-current-buffer buffer
    (erase-buffer)
    (buffer-disable-undo)

    (setq wc--flake-alist nil)
    (setq cursor-type nil)

    (setq wc--width (1- (window-body-width)))
    (setq wc--height (window-body-height))


    (dotimes (_i wc--height)
      (dotimes (_j wc--width)
        (insert (propertize (char-to-string wc-background)
                            'face 'wc-background-face)))
      (newline))

    (wc--insert-greeting)

    (current-buffer)))

(defun wc--insert-greeting ()
  (let ((str "MERRY CHRISTMAS!"))
    (goto-line (/ wc--height 2))
    (move-to-column (- (/ wc--width 2) (/ (length str) 2)))
    (delete-char (length str))
    (insert (propertize str 'face 'wc-flake-face))))

(defun white-christmas ()
  "Let it snow."
  (interactive)
  (pop-to-buffer-same-window
   (wc--setup (get-buffer-create "*white-christmas*")))
  (message "Press any key to quit.")

  (let (col)
    (while (not (input-pending-p))
      (setq col (1+ (random wc--width)))
      (unless (assq col wc--flake-alist)
        (wc--draw col 1 wc-flake 'wc-flake-face)
        (push (cons col 1) wc--flake-alist))
      (dolist (c->r wc--flake-alist)
        (wc--update-flake (car c->r)))

      (sit-for wc-speed))
    (discard-input)))
