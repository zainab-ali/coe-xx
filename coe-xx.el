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

(require 'seq)
(require 'subr-x)
(require 'color)

;; Preparing skeins

(defun coe-xx-basket-add ()
  "Adds a pattern's materials to a 'shopping basket'.
This is used to calculate materials for multiple patterns at once.

See also `coe-xx-basket-clear' and `coe-xx-basket-view'"
  (interactive)
  (let* ((grid (coe-xx--grid coe-xx--grid-region))
         (stitches (coe-xx--stitch-lengths coe-xx--legend grid))
         (threads (coe-xx--threads stitches)))
    (setq coe-xx--threads (append threads coe-xx--threads))))

(defun coe-xx-basket-clear ()
  "Removes all materials from the 'shopping basket'.
See also `coe-xx-basket-add' and `coe-xx-basket-view'. "
  (interactive)
  (setq coe-xx--threads nil))

(defun coe-xx-basket-view ()
  (interactive)
  (let* ((threads (coe-xx--threads coe-xx--threads))
         (skeins-function
          (lambda (item)
            (let ((colour (car item))
                  (length (cdr item)))
              (cons colour (coe-xx--num-skeins length)))))
         (skeins (seq-map skeins-function threads))
         (num-skeins (seq-map 'cdr skeins))
         (total-num-skeins (apply '+ num-skeins)))
    (get-buffer-create "*coe-xx*")
    (switch-to-buffer-other-window "*coe-xx*")
    (erase-buffer)
    (coe-xx--insert-line "Skeins")
    (coe-xx--insert-line "======")
    (coe-xx--insert-line (concat "Total no. skeins: "
                             (number-to-string
                              total-num-skeins)))
    (seq-map (lambda (item)
               (let ((colour (car item))
                     (count (cdr item)))
                 (coe-xx--insert-line
                  (concat "No. "
                          colour
                          " skeins : "
                          (number-to-string count)))))
             skeins)))

(defun coe-xx--stitch-lengths (legend grid)
  (seq-map
   (lambda (item)
     (let ((code (car item))
           (colour (cadr item))
           (num-strands (caddr item)))
       (cons colour (* num-strands
                       (seq-count (lambda (cell) (= code cell)) grid)))))
   legend))


(defvar coe-xx--threads nil
  "An association list of colour and thread length pairs.

Used when calculating the number of skeins to acquire.")

(defun coe-xx--threads (stitches)
  (let ((groups (seq-group-by 'car stitches))
        (group-function
         (lambda (group)
           (let* ((colour (car group))
                  (colour-and-lengths (cdr group))
                  (lengths (seq-map 'cdr colour-and-lengths)))
             (cons colour (apply '+ lengths))))))
    (seq-map group-function groups)))

(defconst coe-xx--skein-length 4800.0
  "The length of a new skein of thread from the local craft shop.")

(defun coe-xx--num-skeins (thread-length)
  (ceiling (/ (float thread-length) coe-xx--skein-length)))

(defun coe-xx--insert-line (text)
  (insert text)
  (newline))

;; Extracting information from the pattern buffer

(defun coe-xx--string (start end)
  (string-trim (buffer-substring-no-properties start end)))

(defun coe-xx--title ()
  (goto-char (point-min))
  (coe-xx--string (line-beginning-position) (line-end-position)))

(defun coe-xx--legend ()
  (goto-char (point-min))
  (search-forward "LEGEND")
  (forward-line 1)
  (let ((start (line-beginning-position)))
    (search-forward "PATTERN")
    (forward-line -1)
    (let* ((text (coe-xx--string start (point)))
          (lines (split-string text "\n")))
      (seq-map 'coe-xx--legend-line-to-item lines))))

(defun coe-xx--legend-line-to-item (line)
  (let* ((parts (split-string line " - "))
         (code-str (car parts))
         (code (string-to-char code-str))
         (colour (string-trim (cadr parts)))
         (strands-str (caddr parts))
         (num-strands-str (car (split-string strands-str " ")))
         (num-strands (string-to-number num-strands-str)))
    (list code colour num-strands)))

(defun coe-xx--grid (region)
  (string-to-list (buffer-substring (car region)
                                    (cdr region))))

(defun coe-xx--grid-size (region)
  (let* ((text (coe-xx--string (car region) (cdr region)))
         (lines (split-string text "\n"))
         (height (length lines))
         (width (seq-max (seq-map 'length lines))))
    (cons width height)))

(defun coe-xx--grid-region ()
  (goto-char (point-min))
  (search-forward "PATTERN")
  (forward-line 1)
  (cons (line-beginning-position) (point-max)))

;; Displaying the pattern buffer

(defun coe-xx--header-line (title size)
  (format "%s (%s wide by %s high)" title (car size) (cdr size)))

(defconst coe-xx-colour-codes
  '((black 0 0 0)
    (pink 244 174 213)
    (magenta 224 40 118)
    (violet 119 107 152)
    (lavender 131 91 139)
    (delft 116 142 182)
    (grey 120 128 164)
    (cream 255 251 239))
  "A list of skein colours. Each colour is represented as a list of
  (CODE R G B) where R G and B are rgb values."
  )

(defconst coe-xx-hex-codes
  (progn
    ;; (require 'xx-dmc-codes)
    (seq-map (lambda (it)
	       (let* ((rgb (cdr it))
		      (normalized (seq-map (lambda (x) (/ x 255.0)) rgb))
		      (stitch-hex (apply 'color-rgb-to-hex normalized))
		      (hsl (apply 'color-rgb-to-hsl normalized))
		      (light-hsl (apply 'color-lighten-hsl (seq-concatenate 'list hsl '(25))))
		      (canvas-hex (apply 'color-rgb-to-hex (apply 'color-hsl-to-rgb light-hsl))))
		 (cons (symbol-name (car it))
		       (cons stitch-hex
			     canvas-hex))))
	   coe-xx-colour-codes)))

(defun coe-xx--grid-display-all-texts (legend)
  (cons coe-xx--grid-display-text-space
        (seq-map 'coe-xx--grid-code-display-text legend)))

(defun coe-xx--grid-display-only-code-texts (legend code)
  (let ((grey-text-function
         (lambda (item)
           (let ((other-code (car item)))
             (cons other-code
                   (coe-xx--grid-display-text ?+ '("grey" . "grey")))))))
    (cons coe-xx--grid-display-text-space
          (cons (coe-xx--grid-code-display-text code)
           (seq-map grey-text-function legend)))))

(defun coe-xx--grid-display-texts (legend)
  (let ((code (assoc coe-xx--code legend)))
    (if code
        (coe-xx--grid-display-only-code-texts legend code)
      (coe-xx--grid-display-all-texts legend))))

(defun coe-xx--grid-display-text (char hex)
  (propertize (char-to-string char)
              'face
              (list ':background (cdr hex)
                    ':foreground (car hex))))

(defun coe-xx--grid-code-display-text (item)
  (let* ((code (car item))
         (colour (cadr item))
         (hex (cdr (assoc-string colour coe-xx-hex-codes))))
    (cons code (coe-xx--grid-display-text ?x hex))))

(defconst coe-xx--grid-display-text-space
  (cons ?\s (coe-xx--grid-display-text ?\s '("#fdf6e3" . "#fdf6e3"))))

(defun coe-xx--grid-prettify (region colours)
  (goto-char (car region))
  (while (< (point) (cdr region))
    (let* ((code (char-after))
           (text (alist-get code colours)))
      (when text
        (put-text-property (point) (1+ (point))
                           'display
                           (concat text))))
    (forward-char 1)))

(defvar coe-xx-mode-map
  (let ((map (make-sparse-keymap 'coe-xx-mode-map)))
    (define-key map (kbd "C-c a") 'coe-xx-basket-add)
    (define-key map (kbd "C-c c") 'coe-xx-code-select)
    (define-key map [mouse-1]  'coe-xx-code-select)
    (define-key map (kbd "C-c r") 'coe-xx-ruler-mode)
    map))

;; Major mode initialization

(define-derived-mode coe-xx-mode nil "coe-xx"
  "Major mode for viewing cross stitch patterns.

Each cross stitch in the pattern is displayed as a 'x' coloured
according to its thread colour.

\\{coe-xx-mode-map}."
  (let* ((title (coe-xx--title))
         (legend (coe-xx--legend))
         (texts (coe-xx--grid-display-texts legend))
         (region (coe-xx--grid-region))
         (size (coe-xx--grid-size region))
         (header (coe-xx--header-line title size)))
    (put-text-property (point-min) (car region) 'invisible t)
    (setq header-line-format header)
    (setq coe-xx--legend legend)
    (setq coe-xx--grid-region region)
    (setq-local truncate-lines t)
    (coe-xx--grid-prettify region texts)))

(add-to-list 'auto-mode-alist '("\\.coe-xx\\'" . coe-xx-mode))

(defvar-local coe-xx--legend nil "The pattern legend")

(defvar-local coe-xx--grid-region nil "The pattern grid region")

(define-key global-map (kbd "C-c x") 'coe-xx-basket-view)

;; Viewing stitches by code

(defvar-local coe-xx--code nil
  "Code representing the currently worked colour and stitch type.")

(defun coe-xx-code-select ()
  "Displays stitches with the thread colour and stitch type at
point. All other stitches are hidden.

Displays the entire pattern if called when a code (a thread
colour and stitch type) is already selected.

Use this to focus on the stitches you care about when filling in the
pattern."
  
  (interactive)
  (let ((code (char-after)))
    (cond
     ((equal coe-xx--code code) (setq coe-xx--code nil))
     ((alist-get code coe-xx--legend) (setq coe-xx--code code)))
    (save-excursion
      (let ((texts (coe-xx--grid-display-texts coe-xx--legend)))
        (coe-xx--grid-prettify coe-xx--grid-region
                           texts)))))

;; Displaying the ruler

(defun coe-xx--ruler-ticks (n)
  (let* ((max (* (/ n coe-xx-ruler-scale) coe-xx-ruler-scale))
         (0-and-ticks (number-sequence 0 max coe-xx-ruler-scale)))
    (cdr 0-and-ticks)))

(defun coe-xx--ruler-overwrite (text)
  (insert text)
  (let ((n-chars-remaining (- (point-max) (point))))
    (delete-char (min n-chars-remaining (length text)))))

(defun coe-xx--ruler-top (padding width)
  (goto-char (point-min))
  (insert (make-string width ?\s))
  (seq-each
   (lambda (n)
     (goto-char n)
     (coe-xx--ruler-overwrite (number-to-string n)))
   (coe-xx--ruler-ticks width))
    (goto-char (point-min))
    (insert (make-string padding ?\s)))

(defun coe-xx--ruler-left (height)
  (let* ((tick-ns (coe-xx--ruler-ticks height))
         (padding-length
          (if (last tick-ns)
              (length (number-to-string (car (last tick-ns))) )
            1))
         (padding (make-string padding-length ?\s))
         (blank-line (concat "\n" padding)))
    (goto-char (point-max))
    (seq-each 'insert (make-list height blank-line))
    (seq-each
     (lambda (n)
       (goto-char (point-min))
       (forward-line n)
       (coe-xx--ruler-overwrite (number-to-string n)))
     tick-ns)
    padding-length))

(defun coe-xx--ruler-focus-ticks (padding-length focus)
  (let ((col (car focus))
        (row (cdr focus)))
    (goto-char (point-min))
    (forward-line (1+ row))
    (forward-char (1- padding-length))
    (coe-xx--ruler-overwrite ">")
    (goto-char (point-min))
    (forward-char (+ padding-length col))
    (coe-xx--ruler-overwrite "v")))

(defun coe-xx--ruler-texts ()
  (goto-char (point-min))
  (forward-line 1)
  (let ((top
         (buffer-substring (point-min)
                           (line-end-position)))
        (left
         (buffer-substring (1+ (line-end-position))
                           (point-max))))
    (cons top (split-string left "\n"))))

(defun coe-xx--ruler-overlays (region texts)
  (goto-char (car region))
  (seq-each (lambda (text)
              (when (< (point) (cdr region))
                (goto-char (line-beginning-position))
                (coe-xx--ruler-overlay (point) text)
                (forward-line 1)))
            texts))

(defun coe-xx--ruler-overlay (point text)
  (let ((o (make-overlay point (1+ point))))
    (overlay-put o 'before-string text)))

(defun coe-xx--ruler-show ()
  (let* ((size (coe-xx--grid-size coe-xx--grid-region))
         (width (car size))
         (height (cdr size))
         (focus coe-xx--ruler-focus)
         (texts
          (with-temp-buffer
            (let ((padding-length (coe-xx--ruler-left height)))
              (coe-xx--ruler-top padding-length width)
              (when focus
                (coe-xx--ruler-focus-ticks padding-length focus)))
            (coe-xx--ruler-texts))))
    (coe-xx--ruler-overlays coe-xx--grid-region texts)))

(defun coe-xx--ruler-hide ()
  (let ((os (overlays-in (point-min) (point-max))))
    (seq-each 'delete-overlay os)))

(defvar-local coe-xx--ruler-focus
  nil
  "The reference point to focus in the ruler.
This is a cons cell of a zero indexed grid column and row.")

(defun coe-xx-ruler-focus ()
  (interactive)
  (when coe-xx-ruler-mode
    (save-excursion
      (let ((column (current-column))
            (row (- (line-number-at-pos)
                    (line-number-at-pos
                     (car coe-xx--grid-region)))))
        (setq coe-xx--ruler-focus (cons column row)))
      (coe-xx-ruler-mode -1)
      (coe-xx-ruler-mode 1))))

(defvar coe-xx-ruler-mode-map
  (let ((map (make-sparse-keymap 'coe-xx-ruler-mode-map)))
    (define-key map (kbd "C-c f") 'coe-xx-ruler-focus)
    (define-key map [mouse-1]  'coe-xx-ruler-focus)
    map))

(define-minor-mode coe-xx-ruler-mode
  "Minor mode for measuring cross stitch patterns."
  :lighter "-|--|-"
  :keymap coe-xx-ruler-mode-map
  (if coe-xx-ruler-mode
      (save-excursion (coe-xx--ruler-show))
    (coe-xx--ruler-hide)))

;; Customizing the ruler scale

(defgroup coe-xx nil
  "The Cross stitch group")

(defcustom coe-xx-ruler-scale
  10
  "The number of divisions between tick marks on the ruler"
  :type '(integer)
  :group 'coe-xx)

;; Reset the display
(defun coe-xx-reset ()
  (interactive)
  (normal-mode)
  (set-text-properties  (point-min) (point-max) '())
  (revert-buffer t t))

(provide 'coe-xx)
