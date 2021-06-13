
; it is assumed that all these lanes go both ways
(defparameter stops '(banacha_mimuw lotnisko_chopina lazienki
		      sejm teatr_polski obozna cmentarz_radziecki
		      akademik_uw_zoliborz teatr_dramatyczny wedel_centrum
		      wisla_schodki mordor targ_bakalarska stegny
		      dworzec_zach ogrod_saski ursus_niedzwiadek
		      pole_mokotowskie saska_kepa zlote_tarasy))


(defparameter lines '((0 lotnisko_chopina mordor banacha_mimuw
			zlote_tarasy wedel_centrum)
		     (420 stegny saska_kepa lazienki ogrod_saski
			 akademik_uw_zoliborz)
		     (1488 obozna teatr_polski sejm wedel_centrum
			 mordor stegny)
		     (1013 zlote_tarasy wedel_centrum ogrod_saski
		         teatr_dramatyczny dworzec_zach)
		     (1024 ursus_niedzwiadek lotnisko_chopina
		         targ_bakalarska cmentarz_radziecki pole_mokotowskie)
		     (2137 targ_bakalarska mordor cmentarz_radziecki
		         wisla_schodki sejm lazienki teatr_polski)))


(setf (get 'stops 'stops_defined) T)


(setf (get 'lines 'lines_defined) T)
