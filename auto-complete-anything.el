;;; auto-complete-anything.el --- Run anything from auto-complete

;; Copyright (C) 2009  Taiki SUGAWARA

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
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

;; (define-key ac-completing-map (kbd "C-M-i") 'ac-anything)

;;; Code:

(require 'anything)
(require 'auto-complete)

(defvar ac-anything-point nil)
(defvar ac-anything-prefix nil)
(defvar ac-anything-limit 10000)

(defvar ac-anything-advice-list
  '((anything-mark-current-line after ac-anything)
    (asc-display-overlay after ac-anything)))

(defvar ac-anything-source-template
  '((name . "Auto Complete")
    (candidate-transformer ac-anything-candidate-highligher)
    (action . ac-anything-action)))

(when (require 'anything-show-completion nil t)
  (use-anything-show-completion 'ac-anything '(length ac-anything-prefix)))

(defun ac-anything ()
  (interactive)
  (let ((candidates (ac-anything-candidates)))
    (setq ac-anything-point ac-point)
    (setq ac-anything-prefix ac-prefix)
    (ac-abort)
    (unwind-protect
	(progn
	  (ac-anything-enable-advices)
	  (anything (list (ac-anything-source candidates))))
      (ac-anything-disable-advices))))

(defun ac-anything-enable-advices ()
  (dolist (advice ac-anything-advice-list)
    (apply 'ad-enable-advice advice)
    (ad-activate (car advice))))

(defun ac-anything-disable-advices ()
  (dolist (advice ac-anything-advice-list)
    (apply 'ad-disable-advice advice)
    (ad-activate (car advice))))

(defun ac-anything-candidates ()
  (mapcar 
   (lambda (x)
     (let ((text (copy-sequence x)))
       (set-text-properties 0 (length text) nil text)
       (cons text
	     (list
	      (cons 'text text)
	      (cons 'action (ac-get-candidate-action x))
	      (cons 'face (ac-get-candidate-property 'face x))
	      (cons 'selection-face (ac-get-candidate-property 'selection-face x))))))
   (let ((ac-limit ac-anything-limit))
     (ac-candidates))))

(defun ac-anything-candidate-highligher (candidates)
  (mapcar 
   (lambda (x)
     (cons (propertize (assoc-default 'text x) 
		       'face (assoc-default 'face x))
	   x))
   candidates))

(defun ac-anything-source (candidates)
  (cons 
   (cons 'candidates candidates)
   ac-anything-source-template))

(defun ac-anything-action (candidate)
  (delete-region ac-anything-point (point))
  (insert (assoc-default 'text candidate))
  (let ((action (assoc-default 'action candidate)))
    (when action
      (funcall action))))

(defun ac-anything-selection-face (selection)
  (or (assoc-default 'selection-face selection)
      'ac-selection-face))
  

(defadvice anything-mark-current-line (after ac-anything 
					     disable)
  (overlay-put anything-selection-overlay 
	       'face (ac-anything-selection-face (anything-get-selection))))

(defadvice asc-display-overlay (after ac-anything 
				      (selection) 
				      disable)
  (overlay-put asc-overlay
	       'face (ac-anything-selection-face selection)))

(provide 'auto-complete-anything)
;;; auto-complete-anything.el ends here
