;;; highlight-doxygen-test-basic.el --- Test primitive functions.

;; Copyright (C) 2018 Anders Lindgren

;; Author: Anders Lindgren

;; This file is part of the `highlight-doxygen' package.

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

;; Test primitive functions in `highlight-doxygen'.

;;; Code:


;; ------------------------------------------------------------
;; Test `highlight-doxygen-forward-to-indentation'
;;

(defun highlight-doxygen-test-basic--forward-to-indentation (str)
  "Call `highlight-doxygen-forward-to-indentation' with buffer containing STR.

The point is placed on the location of the `|' character and the
character is delected before the function is called.

If `highlight-doxygen-forward-to-indentation' returns non-nil the
current column is returned.  Otherwise, nil is returned."
  (with-temp-buffer
    (c++-mode)
    (save-excursion
      (insert str))
    (when (search-forward "|" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (and (highlight-doxygen-forward-to-indentation)
         (current-column))))


(ert-deftest highlight-doxygen-test-basic--forward-to-indentation ()
  ;; Without indentation.
  (should (equal (highlight-doxygen-test-basic--forward-to-indentation
                  "|//!")
                 nil))
  (should (equal (highlight-doxygen-test-basic--forward-to-indentation
                  "|//! ")
                 nil))
  (should (equal (highlight-doxygen-test-basic--forward-to-indentation
                  "|//!   ")
                 nil))
  (should (equal (highlight-doxygen-test-basic--forward-to-indentation
                  "|//!\t")
                 nil))
  (should (equal (highlight-doxygen-test-basic--forward-to-indentation
                  "|//!\t\t")
                 nil))
  ;; With indentation.
  (should (equal (highlight-doxygen-test-basic--forward-to-indentation
                  "|//!xxx")
                 3))
  (should (equal (highlight-doxygen-test-basic--forward-to-indentation
                  "|//! xxx")
                 4))
  (should (equal (highlight-doxygen-test-basic--forward-to-indentation
                  "|//!  xxx")
                 5))
  ;; C-style comments.
  (should (equal (highlight-doxygen-test-basic--forward-to-indentation
                  "\
|/*!
 */")
                 nil))
  ;; Note: The current implementation incorrectly see the "/" as part
  ;; of the text block.  However, in practice it doesn't matter.  (It
  ;; would be better if it would return nil in this case.)
  (should (equal (highlight-doxygen-test-basic--forward-to-indentation
                  "\
 /*!
 | */")
                 3))
  (should (equal (highlight-doxygen-test-basic--forward-to-indentation
                  "\
/*!
| * /
 */")
                 3))
  nil)


;; ------------------------------------------------------------
;; Test `highlight-doxygen-next-comment'
;;

(defun highlight-doxygen-test-basic--next-comment (str &optional explain)
  ""
  (with-temp-buffer
    (c++-mode)
    (save-excursion
      (insert str))
    ;; Prep the list with `positions' to make the output of the
    ;; explainer nicer when one of the list otherwise would have been
    ;; empty.
    (let ((reference-positions '(positions)))
      (while (search-forward "|" nil t)
        (delete-region (match-beginning 0) (match-end 0))
        (push (point) reference-positions))
      (goto-char (point-min))
      (let ((real-positions '(positions)))
        (while (highlight-doxygen-next-comment (point-max))
          (push (point) real-positions)
          ;; Needed, since `h-d-next-char' leaves point at the
          ;; beginning of a comment, and when reapplied, the point
          ;; doesn't move.  (Maybe this should be changed?)
          (forward-char))
        (funcall
         (if explain
             (get 'equal 'ert-explainer)
           #'equal)
         (reverse reference-positions)
         (reverse real-positions))))))

(put 'highlight-doxygen-test-basic--next-comment
     'ert-explainer
     (lambda (str)
       (highlight-doxygen-test-basic--next-comment str t)))


(ert-deftest highlight-doxygen-test-basic--next-comment ()
  ;; Plain comments.
  (should (highlight-doxygen-test-basic--next-comment ""))
  (should (highlight-doxygen-test-basic--next-comment "|//!"))
  (should (highlight-doxygen-test-basic--next-comment "\
|//! bla
|//! bla"))
  ;; Comments in comments.
  (should (highlight-doxygen-test-basic--next-comment "\
|//! bla //! bla
|//! bla //! bla"))
  (should (highlight-doxygen-test-basic--next-comment "\
// bla //! bla
// bla //! bla"))
  ;; Comments in strings
  (should (highlight-doxygen-test-basic--next-comment "\
foo(\"//! bla\""))
  (should (highlight-doxygen-test-basic--next-comment "\
foo(\"bla //! bla\""))
  nil)


;; ------------------------------------------------------------
;; Test `highlight-doxygen-end-of-paragraph-position'
;;

(defun highlight-doxygen-test-eopp (position limit &optional explain)
  "Check that the end of paragraph is POSITION.

Move point to the next beginning of next line."
  (prog1
      (funcall
       (if explain
           (get 'equal 'ert-explainer)
         #'equal)
       (highlight-doxygen-end-of-paragraph-position limit)
       position)
    (unless explain
      (forward-line))))

(put 'highlight-doxygen-test-eopp
     'ert-explainer
     (lambda (position limit)
       (highlight-doxygen-test-eopp position limit t)))


(ert-deftest highlight-doxygen-test-eopp ()
  "Test `highlight-doxygen-end-of-paragraph-position'."
  (with-temp-buffer
    (c++-mode)
    (let (p1 p2 p3)
      (save-excursion
        (insert "//! XXXXX\n")
        (setq p1 (- (point) 1))
        (insert "//!\n")
        (insert "//! XXXXX\n")
        (insert "//! XXXXX\n")
        (setq p2 (- (point) 1))
        (insert "//!    \n")
        (insert "//! XXXXX\n")
        (setq p3 (- (point) 1))
        (insert "//!\n")
        (insert "//!\n"))
      (should (highlight-doxygen-test-eopp p1 (point-max)))
      (should (highlight-doxygen-test-eopp p2 (point-max)))
      (should (highlight-doxygen-test-eopp p2 (point-max)))
      (should (highlight-doxygen-test-eopp p2 (point-max)))
      (should (highlight-doxygen-test-eopp p3 (point-max)))
      (should (highlight-doxygen-test-eopp p3 (point-max)))
      (should (highlight-doxygen-test-eopp nil (point-max)))
      (should (highlight-doxygen-test-eopp nil (point-max))))))


;; ------------------------------------------------------------
;; The end
;;

(provide 'highlight-doxygen-test-basic)

;;; highlight-doxygen-test-basic.el ends here
