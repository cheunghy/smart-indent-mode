;;; smart-indent-mode.el --- Smart indent for indent based modes

;; Copyright (C) 2015 Zhang Kai Yu

;; Author: Kai Yu <yeannylam@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar-local smart-indent-offset 2
  "The indentation used by `smart-indent-mode'.")

(defvar-local smart-indent-saved-indent-line-function nil
  "The saved `indent-line-function' value.
It will save when enable `smart-indent-mode' and restore
when disable `smart-indent-mode'.")

(defvar-local smart-indent-saved-indent-region-function nil
  "The saved `indent-region-function' value.
It will save when enable `smart-indent-mode' and restore
when disable `smart-indent-mode'.")

(defun smart-indent-backspace (n &optional kill-flag)
  "Smart indent backspace."
  (interactive "p\nP")
  (if (use-region-p)
      (delete-active-region)
    (if (<= 1 (current-column) (current-indentation))
        (delete-char (- smart-indent-offset))
      (delete-char (- n) kill-flag))))

(defun smart-indent-delete-char (n &optional kill-flag)
  "Smart indent delete char."
  (interactive "p\nP")
  (if (use-region-p)
      (delete-active-region)
    (if (and (<= 0 (current-column)) (< (current-column) (current-indentation)))
        (delete-char smart-indent-offset)
      (delete-char n kill-flag))))

(defun smart-indent-return ()
  "Smart indent return."
  (interactive)
  (let ((cur-indent (current-indentation)))
    (newline)
    (indent-to cur-indent)))

(defun smart-indent-indent-to-align ()
  "Smart indent return."
  (let (curind)
    (save-excursion
      (forward-line -1)
      (setq curind (current-indentation)))
    (indent-to curind)))

(defun smart-indent-indent-line ()
  "Smart indent tab."
  (if (-contains? '('newline 'newline-and-indent) last-command)
      (smart-indent-indent-to-align)
    (if (<= (current-column) (current-indentation))
        (insert (make-string smart-indent-offset ? ))
      (save-excursion
        (beginning-of-line 1)
        (insert (make-string smart-indent-offset ? ))))))

(defun smart-indent-indent-region (start end &optional quiet)
  )

(defun smart-indent-shift-right (n)
  "Shift region or line right by N."
  (interactive "p")
  (if (use-region-p)
      (smart-indent-shift-region n)
    (smart-indent-shift-line n)))

(defun smart-indent-shift-left (n)
  "Shift region or line left by N."
  (interactive "p")
  (if (use-region-p)
      (smart-indent-shift-region (- n))
    (smart-indent-shift-line (- n))))

(defun smart-indent-shift-line (n)
  "Shift line by N * `smart-indent-offset', if N is positive, shift right,
if N is negative, shift left."
  (let ((inside-indentation (<= (current-column) (current-indentation)))
        (cur-indentation (current-column)))
    (save-excursion
      (cond
       ((< n 0)
        (indent-line-to
         (max (- (current-indentation) (* (- n) smart-indent-offset)) 0)))
       ((> n 0)
        (indent-line-to (+ (current-indentation) (* n smart-indent-offset))))))
    ;; Fix save-excursion won't work if cursor inside indentation
    (if inside-indentation
        (goto-char (max (+ (line-beginning-position)
                           cur-indentation (* n smart-indent-offset))
                        (line-beginning-position))))))

(defun smart-indent-shift-region (n)
  "Shift region by N * `smart-indent-offset', if N is positive, shift right,
if N is negative, shift left."
  (unless (use-region-p) (error "Region is not active!"))
  (let ((pt (copy-marker (point-marker)))
        (mk (copy-marker (mark-marker))))
    (if (> pt mk)
        (exchange-point-and-mark))
    (while (< (point) (mark))
      (smart-indent-shift-line n)
      (forward-line))
    (push-mark mk nil t)
    (goto-char pt)))

;;;###autoload
(defvar smart-indent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'smart-indent-return)
    (define-key map [backspace] 'smart-indent-backspace)
    (define-key map (kbd "C-d") 'smart-indent-delete-char)
    (define-key map (kbd "s-[") 'smart-indent-shift-left)
    (define-key map (kbd "s-]") 'smart-indent-shift-right)
    map))

;;;###autoload
(define-minor-mode smart-indent-mode
  "Mode for easy expand line when expand line is activated."
  :lighter " SI"
  :keymap smart-indent-mode-map
  (if smart-indent-mode
      (progn
        (setq smart-indent-saved-indent-region-function indent-region-function)
        (setq smart-indent-saved-indent-line-function indent-line-function)
        (set (make-local-variable 'indent-line-function)
             'smart-indent-indent-line)
        (set (make-local-variable 'indent-region-function)
             'smart-indent-indent-region))
    (setq indent-region-function smart-indent-saved-indent-region-function)
    (setq indent-line-function smart-indent-saved-indent-line-function)))

(provide 'smart-indent-mode)
;;; smart-indent-mode.el ends here
