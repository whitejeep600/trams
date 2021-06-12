; przystanek jest symbolem, i ma liste krawedzi wyjsciowychh.
; krawedz jest struktem i ma numer linii (bedacy liczba)
; oraz przystanek docelowy (bedacy symbolem, ofc).
; szukajac z danego przystanku, sprawdzamy czy dotarlismy, i jesli tak,
; to jakos zwracamy przejechana trase, przy czym jesli nie uda sie
; w ogole znalezc to nil. jesli nie to dodajemy co trzeba nieodwiedzone
; do kolejki no i wiadomo.
; jak juz znajdziemy przystankowo optymalna trase to dobieramy potem 
; linie, ktorymi jedziemy, tak zeby bylo jak najmniej przesiadek.
; niech kazdy wezel pamieta kto go wrzucil do kolejki zeby sie dalo zrekonstruowac

; a potem jak zrekonstruujemy no to mozna sie iterowac po kazdej linii ktora wyjezdza
; z poczatkowego przystanku i na sile kazda probowac jechac tak dlugo jak sie da i w razie
; czego sie przesiadac

(defstruct edge
  target
  lane_no)


(defun add_edge (number stop1 stop2)
  (setf (get stop1 'outward) (concatenate 'list (get stop1 'outward) (list (make-edge :target stop2
										:lane_no number)))))

(defun add_edges (number stop1 stop2)
       (add_edge number stop1 stop2)
       (add_edge number stop2 stop1))


(defun load_lane (number lane)
  (if (and (not (null lane)) (not (null (rest lane))))
      (progn (add_edges number (first lane) (second lane))
	     (load_lane number (rest lane)))))


(defun load_lanes (lanes)
  (if (not (null lanes))
      (progn (load_lane (first (first lanes)) (rest (first lanes)))
	     (load_lanes (rest lanes)))))


; chcemy przejsc sie po liscie linii i dodac przystankom krawdzie wyjsciowe
(defun load_database ()
  (load_lanes lanes))


(if (or (not (get 'stops 'stops_defined)) (not (get 'lanes 'lanes_defined)))
    (progn
      (write "load the database first!\n")
      (assert nil))
    (load_database))


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


(defun add_children (queue)
  (rest (concatenate 'list queue (get_unvisited_children (first queue)))))


; jeszcze nul jak sie nie da
(defun search_bfs (queue target)
  (if (eq target (first queue))
      target
      (search_bfs (add_children queue) target)))


(defun clean_stop (stop)
  (setf (get stop 'visited) nil)
  (setf (get stop 'father) nil))


(defun clean_stops (stops_arg)
  (if (not (null stops_arg))
      (progn
	(clean_stop (first stops_arg))
	(clean_stops (rest stops_arg)))))


(defun cleanup (result)
  (clean_stops stops)
  result)


; jeszcze sprzatanie fatherow i visitedow oraz znajdowanie optymalnej przesiadkowo trasy
(defun find_bfs (start target)
  (cleanup (search_bfs (list start) target)))
