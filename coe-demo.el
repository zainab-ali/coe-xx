;; Copyright (C) 2022  Zainab Ali

;; Author: Zainab Ali <zainab@kebab-ca.se>
;; Keywords: lisp
;; URL: https://github.com/zainab-ali/coe-xx

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(defconst coe-demo--dir (file-name-directory load-file-name))

(defun coe-demo-xx-view ()
  "Demos the `xx-view' function"
  (interactive)
  (require 'coe-xx)
  (find-file (expand-file-name "logo.xx" coe-demo--dir))
  (read-only-mode -1)
  (normal-mode)
  (coe-xx-mode)
  (coe-xx-ruler-mode)
  (coe-xx-basket-clear)
  (coe-xx-basket-add)
  (coe-xx-basket-view))

(defun coe-demo-xx ()
   (interactive)
   (get-buffer-create "*xx*")
   (switch-to-buffer "*xx*")
   (erase-buffer)
   (coe-xx--insert-line "Skeins")
   (coe-xx--insert-line "======")
   (let* ((strands (coe-demo--read-positive "No. strands: "))
          (stitches (coe-demo--read-positive
                     "No. stitches: "))
          (skeins (coe-demo--num-skeins
                   strands
                   stitches)))
         (coe-xx--insert-line (concat "No. strands: "
                                  (number-to-string strands)))
         (coe-xx--insert-line (concat "No. stitches: "
                                  (number-to-string stitches)))
         (coe-xx--insert-line
          (concat "No. skeins: "
                  (number-to-string skeins)))))

(defun coe-demo--num-skeins (strands stitches)
   (ceiling (/ (float (* strands stitches)) 4800.0)))

(defun coe-demo--read-positive (prompt)
  (let ((input (read-number prompt)))
    (if (and (integerp input) (> input 0))
	input
      (message "Enter an integer greater than 0.")
      (sit-for 1)
      (coe-demo--read-positive prompt))))


(provide 'coe-demo)

;; Local Variables:
;; nameless-current-name: "coe-demo"
;; End:
