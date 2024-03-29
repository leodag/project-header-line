;;; project-header-line-tests.el --- test -*- lexical-binding:t -*-

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

;; Tests for header line generation.  Should check all corner
;; cases possible.

;;; Code:

(require 'ert)
(require 'project-header-line)

(ert-deftest project-header-line-test/common-case ()
  (should
   (equal-including-properties
    (project-header-line "/proj" "proj" "/proj/dir/file")
    #("[proj]/dir/file"
      1 5 (face project-header-line-project)
      11 15 (face project-header-line-file)))))

(ert-deftest project-header-line-test/root-of-project ()
  (should
   (equal-including-properties
    (project-header-line "/proj" "proj" "/proj/file")
    #("[proj]/file"
      1 5 (face project-header-line-project)
      7 11 (face project-header-line-file)))))

(ert-deftest project-header-line-test/project-at-root ()
  (should
   (equal-including-properties
    (project-header-line "/" "proj" "/dir/file")
    #("[proj]/dir/file"
      1 5 (face project-header-line-project)
      11 15 (face project-header-line-file)))))

(ert-deftest project-header-line-test/root-of-project-at-root ()
  (should
   (equal-including-properties
    (project-header-line "/" "proj" "/file")
    #("[proj]/file"
      1 5 (face project-header-line-project)
      7 11 (face project-header-line-file)))))

(ert-deftest project-header-line-test/fallback-file-at-root ()
  (should
   (equal-including-properties
    (project-header-line--fallback "/file")
    #("/file" 1 5 (face project-header-line-file)))))

(ert-deftest project-header-line-test/fallback-unabbreviated-path ()
  (should
   (equal-including-properties
    (project-header-line--fallback "/bin/sh")
    #("/bin/sh" 5 7 (face project-header-line-file)))))

(ert-deftest project-header-line-test/fallback-abbreviated-path ()
  (should
   (equal-including-properties
    (let ((directory-abbrev-alist '(("\\`/should-abbrev\\(/\\|\\'\\)" . "abbrev"))))
      (project-header-line--fallback "/should-abbrev/file"))
    #("abbrev/file" 7 11 (face project-header-line-file)))))

;;; project-header-line-tests.el ends here
