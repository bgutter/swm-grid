;;;; swm-grid.lisp

(in-package #:swm-grid)

(export '(*groups-width*
          *groups-height*
          *group-default*
          create-groups
          left-group
          right-group
          down-group
          up-group))

;;
;; Default to a 3x3 grid, opening at center.
;;

(defvar groups-width   3)
(defvar groups-height  3)
(defvar groups-default 4)

(defun create-groups ()
  "Create enough workspaces to fill the grid."
  (let*
      ((groups  (sort-groups (current-screen)))
       (ngroups (length groups)))
    (dotimes (i (- (* groups-width groups-height) ngroups))
      (gnew (write-to-string i))))
  (let
      ((groups (sort-groups (current-screen))))
    (switch-to-group (nth groups-default groups))))

(defun gnav-get-y (i)
  "Get the row given a group index."
  (floor (/ i groups-width)))

(defun gnav-get-x (i)
  "Get the column given a group index."
  (mod i groups-width))

(defun gnav-get-i (x y)
  "Get the group index of a row, column pair."
  (+ (* y groups-width) x))

(defun gnav-step (i direction)
  "From group index I, step in one direction in '(:right :left :down :up)"
  (let*
      ((curx      (gnav-get-x i))
       (cury      (gnav-get-y i))
       (newx      (case direction
                    (:left     (max 0 (min (- groups-width 1) (- curx 1))))
                    (:right    (max 0 (min (- groups-width 1) (+ curx 1))))
                    (otherwise curx)))
       (newy      (case direction
                    (:up       (max 0 (min (- groups-height 1) (- cury 1))))
                    (:down     (max 0 (min (- groups-height 1) (+ cury 1))))
                    (otherwise cury)))
       (newi      (gnav-get-i newx newy)))
    newi))

(defun gnav-current-index ()
  "Get the current group index."
  (position (current-group) (sort-groups (current-screen))))

(defun navigate-groups (direction)
  "Move groups as though they're in an MxN grid"
  (let*
      ((groups    (sort-groups (current-screen)))
       (curi      (position (current-group) groups))
       (newi      (gnav-step curi direction))
       (new-group (nth newi groups)))
    (progn
      (if (not (eq newi curi))
          (switch-to-group new-group))
      (gnav-echo-graph))))

(defun nl-join (items)
  "Join on new lines."
  (format nil (concatenate 'string "~{~A~^" (string #\newline) "~}") items))

(defun join (delim items)
  "Join strings on delimiter."
  (format nil (concatenate 'string "~{~A~^" (string delim) "~}") items))

(defun gnav-get-fmt-row (y)
  (let*
      ((curi   (gnav-current-index))
       (firsti (gnav-get-i 0 y))
       (lasti  (gnav-get-i groups-width y)))
    (join " " (mapcar
               (lambda (i)
                 (if (eq i curi)
                     (concatenate 'string "^R" (write-to-string i) "^r")
                   i))
               (range lasti :min firsti)))))

(defun gnav-echo-graph ()
  "Echo an ascii workspace indicator."
  (let*
      ((rows (mapcar 'gnav-get-fmt-row (range groups-height)))
       (img  (nl-join rows)))
    (echo img)))

(defun range (max &key (min 0) (step 1))
  "https://stackoverflow.com/questions/13937520/pythons-range-analog-in-common-lisp"
   (loop for n from min below max by step
      collect n))

(defcommand group-right ()
  () ""
  (navigate-groups :right))

(defcommand group-right ()
  () ""
  (navigate-groups :left))

(defcommand group-up ()
  () ""
  (navigate-groups :up))

(defcommand group-down ()
  () ""
  (navigate-groups :down))
