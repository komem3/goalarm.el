;;; goalarm-test.el --- Tests for goalarm.

;; Copyright (C) 2020 komem3

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(load-file "./goalarm.elc")

(ert-deftest goalarm-convert-args-min-or-time-test ()
  (should (not (goalarm-convert-args-min-or-time "tt")))
  (should (equal (goalarm-convert-args-min-or-time "33")
                 '("-min" "33")))
  (should (equal (goalarm-convert-args-min-or-time "33 ")
                 '("-min" "33")))
  (should (equal (goalarm-convert-args-min-or-time "33l")
                 '("-loop" "-min" "33")))
  (should (equal (goalarm-convert-args-min-or-time "33l ")
                 '("-loop" "-min" "33")))
  (should (equal (goalarm-convert-args-min-or-time "33:33:33")
                 '("-time" "33:33:33")))
  (should (equal (goalarm-convert-args-min-or-time "33:33:33 ")
                 '("-time" "33:33:33")))
  (should (equal (goalarm-convert-args-min-or-time "33:33:33l")
                 '("-loop" "-time" "33:33:33")))
  (should (equal (goalarm-convert-args-min-or-time "33:33:33l ")
                 '("-loop" "-time" "33:33:33"))))

(ert-deftest goalarm-start-test()
  (setq-local goalarm-sound-file nil)
  (should (string= (goalarm-start "10") "yet not setting goalarm-sound-file."))
  (setq-local goalarm-sound-file "dummy")
  (should (string= (goalarm-start "") "args is empty."))
  (should (string= (goalarm-start "10") "start goalarm."))
  (should (string= (goalarm-start "10") "already goalarm running."))
  (should (goalarm-check-process-running))
  (goalarm-stop))

;;; goalarm-test.el ends here
