;;; highlight-doxygen-test-files.el --- Regression test for highlight-doxygen.

;; Copyright (C) 2016 Anders Lindgren

;; Author: Anders Lindgren
;; Package-Requires: ((faceup "0.0.4"))

;; This file is part of the `highlight-doxygen' package.

;; This program is free software: you can redistribute it and/or modify
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

;; Regression test of `highlight-doxygen', a package providing
;; font-lock rules for doxygen comment. This module verifies
;; fontification of a number of files. This is done by keeing a text
;; representation of the fontification using `faceup' markup, in
;; addition to the original files.
;;
;; The actual check is performed using `ert', with font-lock test
;; function provided by `faceup'.

;;; Code:

(require 'faceup)

(defvar highlight-doxygen-test-dir (faceup-this-file-directory))

(defun highlight-doxygen-test-file (file)
  "Test that FILE is fontified as the .faceup file describes.

FILE is interpreted as relative to this source directory."
  (faceup-test-font-lock-file '(c++-mode
                                highlight-doxygen-mode)
                              (concat
                               highlight-doxygen-test-dir
                               file)))
(faceup-defexplainer highlight-doxygen-test-file)


(ert-deftest highlight-doxygen-test-files ()
  (should (highlight-doxygen-test-file "files/code.h"))
  (should (highlight-doxygen-test-file "files/example.h"))
  (should (highlight-doxygen-test-file "files/inline.h"))
  (should (highlight-doxygen-test-file "files/known-problems.h"))
  (should (highlight-doxygen-test-file "files/markdown.h"))
  (should (highlight-doxygen-test-file "files/normal.h"))
  (should (highlight-doxygen-test-file "files/other.h"))
  (should (highlight-doxygen-test-file "files/plain.h"))
  (should (highlight-doxygen-test-file "files/sections.h"))
  (should (highlight-doxygen-test-file "files/verbatim.h")))

(provide 'highlight-doxygen-test-files)

;; highlight-doxygen-test-files.el ends here.
