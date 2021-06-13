
; the program minimizes the number of visited stops by finding a way using the bfs algorithm,
; and then it minimizes the numer of different tram lines to be taken on that way.
; in other words, it prefers to go 4 stops by 2 lines than to go 5 stops on the same line.

; this is our edge for the bfs algorithm and later: a single-stop ride from some stop
; which will remember its outward edges to some other stop(target) by the line_no line.
(defstruct edge
  target
  line_no)

; a few functions for loading the database.
(defun add_edge (number stop1 stop2)
  (setf (get stop1 'outward) (concatenate 'list (get stop1 'outward) (list (make-edge :target stop2
										:line_no number)))))

(defun add_edges (number stop1 stop2)
       (add_edge number stop1 stop2)
       (add_edge number stop2 stop1))


(defun load_line (number line)
  (if (and (not (null line)) (not (null (rest line))))
      (progn (add_edges number (first line) (second line))
	     (load_line number (rest line)))))


(defun load_lines (lines)
  (if (not (null lines))
      (progn (load_line (first (first lines)) (rest (first lines)))
	     (load_lines (rest lines)))))


; a proper database compatible with the program will define the symols 'lines' and 'stops'.
(defun load_database ()
  (load_lines lines))


(if (or (not (get 'stops 'stops_defined)) (not (get 'lines 'lines_defined)))
    (progn
      (write "load the database first!\n")
      (assert nil))
    (load_database))


; children of a given node, to be put in the queue.
(defun get_children (outward_edges stop)
  (if (null outward_edges)
      '()
      (if (get (edge-target (first outward_edges)) 'visited)
	  (get_children (rest outward_edges) stop)
	  (progn
	    (setf (get (edge-target (first outward_edges)) 'visited) T)
	    (setf (get (edge-target (first outward_edges)) 'father) stop)
	    (concatenate 'list (list (edge-target (first outward_edges))) (get_children (rest outward_edges) stop))))))


(defun get_unvisited_children (stop)
  (get_children (get stop 'outward) stop))


; admittedly, the queue works in quadratic time now, which is painful.
; I tried to implement a cyclic queue here instead but I created some
; speghetti code that I couldn't debug.
(defun add_children (queue)
  (rest (concatenate 'list queue (get_unvisited_children (first queue)))))


(defun search_bfs (queue target)
  (if (null queue)
      '()
      (if (eq target (first queue))
      target
      (search_bfs (add_children queue) target))))


; the bfs adds some bindings that are redundant after the process, like the boolean
; 'visited' flag and also the information about which node was the 'father' of a
; given node in the BFS tree (for the sake of reconstructing the way afterwards).
; so these few functions handle the cleanup
(defun clean_stop (stop)
  (setf (get stop 'visited) nil)
  (setf (get stop 'father) nil))


(defun clean_stops (stops_arg)
  (if (not (null stops_arg))
      (progn
	(clean_stop (first stops_arg))
	(clean_stops (rest stops_arg)))))


; ignores the result, passes it back to the caller for convenience.
(defun cleanup (result)
  (clean_stops stops)
  result)


; reconstructs the way to the target node.
(defun get_tree (start)
  (if (null start)
      '()
      (concatenate 'list (list start) (get_tree (get start 'father)))))


(defstruct ride
  first_stop
  last_stop
  line
  length)


(defun init_line (origin edge)
  (make-ride :first_stop origin
	     :last_stop (edge-target edge)
	     :line (edge-line_no edge)
	     :length 1))


(defun init_lines (outward_edges origin second_stop)
  (if (null outward_edges)
      '()
      (if (not (eq (edge-target (first outward_edges)) second_stop))
	  (init_lines (rest outward_edges) origin second_stop)
	  (concatenate 'list (list (init_line origin (first outward_edges))) (init_lines (rest outward_edges) origin second_stop)))))


(defun reachable_by_line (line_no source_lines target)
  (if (null source_lines)
      '()
      (if (and (eq (edge-target (first source_lines)) target) (eq (edge-line_no (first source_lines)) line_no))
	  T
	  (reachable_by_line line_no (rest source_lines) target))))


(defun leads_to (ride stop)
  (reachable_by_line (ride-line ride) (get (ride-last_stop ride) 'outward) stop))


(defun lengthen_ride (ride new_stop)
  (make-ride :first_stop (ride-first_stop ride)
	     :last_stop new_stop
	     :line (ride-line ride)
	     :length (+ 1 (ride-length ride))))


(defun develop_one (ride stop_list)
  (if (leads_to ride (first stop_list))
      (develop_one (lengthen_ride ride (first stop_list)) (rest stop_list))
      ride))


(defun develop_each (lines stop_list)
  (if (null lines)
      '()
      (concatenate 'list (list (develop_one (first lines) stop_list)) (develop_each (rest lines) stop_list))))


(defun get_longest (rides)
  (if (null (rest rides))
      (first rides)
      (let ((longest_in_tail (get_longest (rest rides))))
	(if (> (ride-length longest_in_tail) (ride-length (first rides)))
	    longest_in_tail
	    (first rides)))))


(defun get_longest_line (stop_list)
  (get_longest (develop_each (init_lines (get (first stop_list) 'outward) (first stop_list) (second stop_list)) (rest (rest stop_list)))))


(defun cut_start (stop_list longest_line)
  (if (eq (ride-last_stop longest_line) (first stop_list))
      stop_list
      (cut_start (rest stop_list) longest_line)))


(defun minimize_lines (stop_list target)
    (let ((longest_line (get_longest_line stop_list)))
      (if (eq target (ride-last_stop longest_line))
	  (list longest_line)
	  (concatenate 'list (list longest_line) (minimize_lines (cut_start stop_list longest_line) target)))))


(defun get_directions (start)
  (if (null start)
      '()
      (minimize_lines (reverse (get_tree start)) start)))


(defun find_bfs (start target)
  (setf (get start 'visited) T)
  (cleanup (get_directions (search_bfs (list start) target))))


(defun print_first (ride)
  (write (concatenate 'string "First stop: " (string (ride-first_stop ride)) ", get on the tram number "
		      (write-to-string (ride-line ride)) " and go for " (write-to-string (ride-length ride))
		      " stops, then get off at " (string (ride-last_stop ride))))
  (terpri))


(defun stop_or_stops (i)
  (if (eq i 1)
      " stop"
      " stops"))


(defun print_middle (ride)
  (write (concatenate 'string "Get on the tram number " (write-to-string (ride-line ride)) " and go for "
		      (write-to-string (ride-length ride)) (stop_or_stops (ride-length ride)) ", then get off at " (string (ride-last_stop ride))))
  (terpri))


(defun print_rest (rides)
  (if (null rides)
      (write "and you will be there!")
      (progn (print_middle (first rides))
	     (print_rest (rest rides)))))


(defun print_way (rides)
  (if (null rides)
      ()
      (progn 
	(print_first (first rides))
	(print_rest (rest rides)))))


(defun find_way (start target)
  (print_way (find_bfs start target)))


; to use, first load the tram database in your inferior lisp (e. g. (load "base.lisp") to load the example I have provided),
; then load ths file (load "trams.lisp") and call find_way, for example like so:
; (find_way 'stegny 'teatr_polski)
; (find_way 'lotnisko_chopina 'obozna)
