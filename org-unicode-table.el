;;; org-unicode-table.el --- Replace characters in org-table with unicode and vice versa. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Dean Seo

;; Author: Dean Seo <deaniac.seo@gmail.com>
;; Maintainer: Dean Seo <deaniac.seo@gmail.com>
;; Created: July 13, 2025
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.3"))
;; Keywords: convenience
;; URL: https://github.com/Deanseo/emacs-org-unicode-table

;;; Commentary:

;; This package provides two functions to switch between ASCII and Unicode
;; characters for drawing tables in Org mode.  This allows for creating
;; visually appealing tables using box-drawing characters, while maintaining
;; compatibility with systems that do not render Unicode characters correctly.
;;
;; The two main functions are:
;;
;; - `org-unicode-table-unicode`: This function replaces the standard ASCII
;;   characters used for table borders ('+', '-', '|') with their corresponding
;;   Unicode box-drawing counterparts. It also converts '^' and 'v' to '↑'
;;   and '↓' respectively. The function is smart enough to determine the
;;   correct box-drawing character based on the surrounding table structure.
;;
;; - `org-unicode-table-ascii`: This function reverts the changes made by
;;   `org-unicode-table-unicode`, converting the Unicode box-drawing and arrow
;;   characters back to their original ASCII representation.
;;
;; To use, simply select a region containing an Org mode table and call
;; either `M-x org-unicode-table-unicode` or `M-x org-unicode-table-ascii`.

;;; Code:

(defconst box-drawing-map
  (vector ?+ ?─ ?─ ?─ ?│ ?┘ ?└ ?┴ ?│ ?┐ ?┌ ?┬ ?│ ?┤ ?├ ?┼))

(defun org-unicode-table-unicode ()
  "Replace '+', '^', 'v' characters in the selected region with box-drawing or arrow characters.
The replacement for '+' is based on adjacent '-' (horizontal) and '|' (vertical) characters.
For example, '+' with '-' to the right and '|' below becomes '┌'.
'^' becomes '↑' and 'v' becomes '↓'."
  (interactive)
  (save-excursion
    (let ((end (region-end)))
      (goto-char (region-beginning))
      (while (< (point) end)
        (let ((char (char-after)))
          (cond
           ((eq char ?+)
            (let* ((pos (point))
                   (left-char (when (> pos (line-beginning-position)) (char-before pos)))
                   (right-char (when (< (1+ pos) (line-end-position)) (char-after (1+ pos))))
                   (col (current-column))
                   (top-char (save-excursion
                               (when (zerop (forward-line -1))
                                 (move-to-column col)
                                 (when (= (current-column) col)
                                   (char-after)))))
                   (bottom-char (save-excursion
                                  (when (zerop (forward-line 1))
                                    (move-to-column col)
                                    (when (= (current-column) col)
                                      (char-after))))))
              (let ((connect_left (and left-char (eq left-char ?-)))
                    (connect_right (and right-char (eq right-char ?-)))
                    (connect_top (and top-char (eq top-char ?|)))
                    (connect_bottom (and bottom-char (eq bottom-char ?|))))
                (let ((index 0))
                  (when connect_left (setq index (logior index 1)))
                  (when connect_right (setq index (logior index 2)))
                  (when connect_top (setq index (logior index 4)))
                  (when connect_bottom (setq index (logior index 8)))
                  (let ((new-char (aref box-drawing-map index)))
                    (delete-char 1)
                    (insert new-char))))))
           ((eq char ?^)
            (delete-char 1)
            (insert ?↑))
           ((eq char ?v)
            (delete-char 1)
            (insert ?↓))))
        (forward-char 1)))))

(defun org-unicode-table-ascii ()
  "Revert box-drawing and arrow characters back to ASCII in the selected region."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char start)
          (replace-regexp-in-region "[─│┌┐└┘├┤┬┴┼]" "+" start end)
          (goto-char start)
          (replace-regexp-in-region "↑" "^" start end)
          (goto-char start)
          (replace-regexp-in-region "↓" "v" start end)))
    (message "No region selected")))

(provide 'org-unicode-table)
