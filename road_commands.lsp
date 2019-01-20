;|
Author: Serag Hassouna
Version: 1.0
Release Date: 04/10/2018
Requirements to be met:
1- Draw Right of Way polylines for a road from its known center line (a LWPolyline) [c:rowsdraw]
2- Locate the correspondant points, on the right and left side ROWs, to the station points of the center lines
   then write them to 3 csv files .. one is for the center line, the second one is for the right side ROW
   and the third is for the left side ROW.
|;

;|
DISCALIMER:-
(1) This piece of software is provieded "in source code" for these purposes:-
1- To meet the needed requirements.
2- To facilitate the work for other developers, who can simply reuse some parts from this code.
However, Any reusage of this code as whole or parts must be accompanied by mentioning the original author.
3- There's a common downside with almost every AutoLISP/Visual LISP source code, they simply use comments
near to nothing, and even with no comments as all.
Here, the developer tries his best to make his code as clear and readable as possible.

(2) This piece of software is provided "AS IS", there's a price for being "free of charge",
which is the non existance of any guarantee of total efficiency, being error-free, regular maintenace
and technical support.
The developer bears no liability of any damage or crash that may result from the use of this piece of software.
|;

;[1] Command: rowsdraw
;*GLOBAL VARIABLES*
;1- *row_right_dist*: the default distance from center line to the right side ROW.
;2- *row_left_dist*: the default distance from center line to the left side ROW.
;3- *last_wide_right_dist*: the previously used widening distance at the right side ROW.
;4- *last_wide_left_dist*: the previously used widening distance at the left side ROW.
;5- *station_interval*: station interval on the center line.
;6- *text_height_len*: text hight of the texts that demonstrates the lengths of the 3 polylines.
(defun c:rowsdraw
       ;*LOCAL VARIABLES*
       (/
	doc ;the drawing's document object
	modelspace ;the drawing's modelspace
	
	sscline ;the center line selection set
	cline ;the center line itself
	cl_len ;length of center line
	width_list ;list of widening parts

	cl_spt ;center line's start point
	cl_2nd_pt ;center line's 2nd point
	r_line ;right side ROW
	l_line ;left side ROW
	r_ang ;offsetting angle for right ROW
	l_ang ;offsetting angle for left side

	sysvars ;store user's default values of some system variables
	)
  ;*Get ACAD and DOC objects*
  
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq modelspace (vla-get-modelspace doc))
  
  ;*UTILITY FUNCTIONS*
  
  ;(1) get_dist: a more enhanced getdist function
  (defun get_dist (var msg / var2)
    (if (not var)
      (progn
       (initget (+ 1 2 4));no allowed zero or negative value & accepting Enter
       (setq var (getdist (strcat msg ": ")))
       (princ "\n ")
       );progn [then part]
      (progn
       (setq var2 var) ;var2 , a temporary variable for var's value
       (initget (+ 2 4));no allowed zero or negative value
       (setq var (getdist (strcat msg "[" (rtos var 2 6) "]: ")))
       (if (eq nil var) (setq var var2))
       (princ "\n ")
       );progn [else part]
      );if

    (princ var)
    (princ "\n ")
    (progn var)
    );end defun

  ;(2) get_int: a more enhanced getint function
  (defun get_int (var msg / var2)
    (if (not var)
      (progn
       (initget (+ 1 4));no allowed negative value & accepting Enter
       (setq var (getint (strcat msg ": ")))
       (princ "\n ")
       );progn [then part]
      (progn
       (setq var2 var) ;var2 , a temporary variable for var's value
       (initget 4);no allowed negative value
       (setq var (getint (strcat msg "[" (itoa var) "]: ")))
       (if (eq nil var) (setq var var2))
       (princ "\n ")
       );progn [else part]
      );if

    (princ var)
    (princ "\n ")
    (progn var)
    );end defun

  ;(3) pool-getkword: get keyword from user from a pool of keywords specified as a list,
;the function works like a standard autocad getting keyword, a default value can be supplied, but if
;it equals nil the function will proceed normally also.
;Note: defval must be a real, an integer or a string!
(defun pool-getkword (defval ordmsg kwlist / dymode elem kwstr strdefval masg kw)
  ;construct the DYNMODE string (allows user to choose keyword using cursor)
  (setq dymode "[")
  (foreach elem kwlist
    (progn
      (setq dymode (strcat dymode elem "/"))
      );progn, froeach expr
    );End foreach
  (setq dymode (vl-string-right-trim "/" dymode))
  (setq dymode (strcat dymode "]"))

  ;construct the initget keywords' string
  (setq kwstr "")
  (foreach elem kwlist
    (progn
      (setq kwstr (strcat kwstr " " elem))
      );progn. foreach expr
    );End foreach

  ;construct the message string, and get the keyword
  (if (eq defval nil)
    (progn
      (setq masg (strcat "\n" ordmsg " " dymode ": "))
      (initget 1 kwstr)
      (setq kw (getkword masg))
      );progn, no default value [then part]
    (progn
      ;get the string form of defval
      (cond
	((eq (type defval) (type 1)) (setq strdefval (itoa defval)));defualt is integer
	((eq (type defval) (type 1.0)) (setq strdefval (rtos defval)));default is real
	((eq (type defval) (type "string")) (setq strdefval defval));default is string
	);End conditional branches
      (setq masg (strcat "\n" ordmsg " <" strdefval "> " dymode ": "))
      (initget kwstr)
      (setq kw (getkword masg))
      (if (eq kw nil) (setq kw defval))
      );progn, there's a default value [else part]
    );End if

  
  (progn kw)
  );End defun

  ;| (4) selpoly: this function ensures that you select a lightweight polyline, otherwise it will rewind itself till
the user selects a lightweight polyline, it returns the polyline's entity name. |;
  (defun selpoly (msg / plobj flag)
    (setq flag T)
    (while flag
      (setq plobj (vl-catch-all-apply 'entsel (list msg)))
      (setq flag (vl-catch-all-error-p plobj))

      (if (and (not (eq flag T)) (not (eq plobj nil)))
	(progn
	  (setq plobj (car plobj)) ;"entsel" returns the entity name and the point at which the selection occured
	  (setq flag (not (eq "LWPOLYLINE" (cdr (assoc 0 (entget plobj))))))
	  (if flag
	    (princ "\nObject isn't a polyline\n")
	    (princ "\n")
	    );end if (for that object isn't a polyline)
	);end 2nd if [Then Part]
	(progn
	  (princ "\nInvalid Selection\n")
	  (setq flag T)
	  );End progn [Else part of the 1st if statement]
	);end 1st if
      );end While

    (progn plobj);what the function returns
    );End selpoly

  ;(5) fill_widening_info: fills the widening information for the specified ROW
  (defun fill_widening_info
	 ;*VARIABLES*
	 (
	  side ;right or left
	  n_wides ;number of widening occurences
	  cl_len_var ;length of center line
	  /
	  station ;station
	  i ;incrementor for every widening occurrence
	  wide ;the amount of widening/contraction
	  width ;the resultant actual width due to widening/contraction
	  wrange-p ;the state of width
	  w_c-p ;state of being a widening or a contraction

	  width_list_side ;width list
	  )
    ;*PROGRAM CONTEXT*
    (if (/= 0 n_wides)
      (progn
	(setq i 0)
	(while (< i n_wides)
	  (princ (strcat "\n" side " Side ROW, Widening no." (itoa (1+ i)) "\n")) ;display widening side and number
	  
	  ;specify station
	  (while (or (eq nil station) (> station cl_len_var))
	    (if (> station cl_len_var) (princ "\nInvalid station value, station outside the center line\n"))
	    (setq station (get_dist station "Specify station"))
	    );while [of station specification]

	  ;specify widening value
	  (while (or
		   (eq nil wide) ;1st argument of the "OR" operand
		   (setq wrange-p
		   (> 0
		      (setq
			width
			 (+
			   (if (eq wide nil) 0 wide)
			   (cond
			     ((= side "Right") *row_right_dist*);1st condition
			     ((= side "Left") *row_left_dist*);2nd condition
			     (t nil)
			     );cond
			   );+
			);setq wrange-p
		      );> 
			 );setq [2nd argument of the "OR" operand]
		   );or
	    ;___
	    (if wrange-p (princ "\nInvalid contraction value, contraction is > the actual width\n"))
	    (setq w_c-p (pool-getkword "Widening" "Specify width type" '("Widening" "Contraction")))
	    (cond
	      ((= w_c-p "W") (setq w_c-p "Widening"));1st condition
	      ((= w_c-p "C") (setq w_c-p "Contraction"));2nd condition
	      ((= w_c-p "w") (setq w_c-p "Widening"));3rd condition
	      ((= w_c-p "c") (setq w_c-p "Contraction"));4th condition
	      (t nil)
	      );cond
	    (setq wide (* (get_dist wide (strcat "Specify " w_c-p)) (if (= w_c-p "Widening") 1.0 -1.0)))
	    );while [of widening specification]
	  (setq width_list_side (append width_list_side (list (list side station width))))

	  (setq station nil wide nil)
	  (setq i (1+ i))
	  );while
	);progn [else part]
      );if

    (progn width_list_side)
    );end defun [fill_widening_info]

  ;(6) get_widening_list: takes widening list from user, and returns its eqivalent list
  (defun get_widening_list
	 ;*VARIABLES*
	 (
	  cl_len_var
	  /
	  n_wides_right ;no. of widening occurrences onto right side ROW
	  n_wides_left ;no. of widening occurences onto left side ROW
	  width_list_app ;width list
	  )
    ;*PROGRAM CONTEXT*
    
    (setq n_wides_right (get_int n_wides_right "Specify number of widening occurrences at Right Side ROW")) ;get number of widening occurrences at right side ROW
    (setq n_wides_left (get_int n_wides_left "Specify number of widening occurrences at Left Side ROW"))

    ;If there's any widening occurence, fill its info
    (setq width_list_app (append width_list_app (fill_widening_info "Right" n_wides_right cl_len_var)))
    (setq width_list_app (append width_list_app (fill_widening_info "Left" n_wides_left cl_len_var)))
    );end defun [get_widening_list]

  ;(7) ang_typical: takes two lists of points and a central point, returns a list of 2 points that are aligned with that central point
  ;helps in filtering the intersection points from the drawn circles
  (defun ang_typical
	 ;*VARIABLES*
	 (
	  c_pt ;the central point
	  ptslst1 ;1st points' list
	  ptslst2 ;2nd points' list
	  /
	  lenpts1 ;no. of points in the 1st points' list
	  lenpts2 ;no. of points in the 2nd points' list
	  i ;incrementor
	  pt1 ;a point from ptslst1
	  pt2 ;a point from ptslst2
	  ret ;returned result
	  stop-p ;a boolean value to stop looping, simillar to "break" keyword in other programming languages
	  )
    (setq lenpts1 (/ (length ptslst1) 3) lenpts2 (/ (length ptslst2) 3))
    (setq i 0 j 0 stop-p T)
    
    (while (and (< i lenpts1) stop-p)
      (setq pt1 (list (nth (* 3 i) ptslst1) (nth (+ (* 3 i) 1) ptslst1) (nth (+ (* 3 i) 2) ptslst1)))
      
      (while (and (< j lenpts2) stop-p)
	(setq pt2 (list (nth (* 3 j) ptslst2) (nth (+ (* 3 j) 1) ptslst2) (nth (+ (* 3 j) 2) ptslst2)))
	(if (equal (angle c_pt pt1) (angle c_pt pt2) 1e-6)
	  (setq ret (list pt1 pt2) stop-p nil)
	  );if

	(setq j (1+ j)) ;increment j
	);while j

      (setq i (1+ i) j 0) ;increment i and reset j to be used in the next "i" iteration
      );while i

    ret ;the result of this function
    );end defun [ang_typical]
  
  ;(8) wide_create: takes the center line polyline (sscl) and the previously created ROW polyline, offsets another copy at the widening position,
  ;links between them at the widening station with a line, then trims over both polylines to creat the new ROW polyline.
  ;it returns the vla-object of the resulted ROW polyline.
  ;| How it works:-
    1- Get the station point.
    2- Draw 2 circles, one to the original polyline and the other to the destination polyline.
    3- Get intersection points beween every circle and its correspondant polyline.
    4- Filter the intersection points to get the 2 points (1 point for each polyline)
    that are aligned with the station point (the circles' center).
    5- Break the 2 polylines and connect them together to form the newly modified polyline.
  |;
  (defun wide_create
	 ;*VARIABLES*
	 (
	  side ;right or left
	  last_dist ;previously used widening distance
	  sscl ;selection set of the center line ROW polyline
	  cl ;vla-object of the center line
	  org ;vla-object of the previously created polyline
	  width ;offset width
	  t_pt ;through point
	  station ;station of widening
	  /
	  wide_line ;the product of offsetting
	  station_pt ;point of the specified station
	  org2new_cir ;circle from station point intersects with "wide_line"
	  org_cir ;circle from station point intersects with the previously created polyline
	  new_pt ;the point on the offsetting product polyline that correponds to the specified station
	  org_pt ;the point on the old ROW polyline that corresponds to the specified station
	  pts ;list of refined points from the first results of new_pt & org_pt
	  line_wide ;the widening line
	  )
    ;*PROGRAM CONTEXT*

    (command "._Offset" width sscl t_pt "Exit")
    (setq wide_line (vlax-ename->vla-object (entlast)))
    (setq station_pt (vlax-curve-getpointatdist cl station))
    (command "._circle" station_pt
	     (cond
	       ((= side "Right") (setq *last_wide_right_dist* width));1st condition
	       ((= side "Left") (setq *last_wide_left_dist* width));2nd condition
	       (t nil)
	       );cond
	     );command
    (setq org2new_cir (vlax-ename->vla-object (entlast)))
    (setq new_pt (vlax-safearray->list (vlax-variant-value (vla-intersectwith org2new_cir wide_line acExtendNone))))
    (command "._circle" station_pt last_dist)
    (setq org_cir (vlax-ename->vla-object (entlast)))
    (setq org_pt (vlax-safearray->list (vlax-variant-value (vla-intersectwith org_cir org acExtendNone))))
    (setq pts (ang_typical station_pt org_pt new_pt)) ;refined points

    (setq org_pt (nth 0 pts) new_pt (nth 1 pts))
    (command "._line" org_pt new_pt "")
    (setq line_wide (entlast))
    
    (mapcar 'vla-delete (list org2new_cir org_cir))
    (setq org2new_cir nil org_cir nil)

    (command "._break" (vlax-vla-object->ename org) org_pt (vlax-curve-getendpoint org))
    (command "._break" (setq wide_line (vlax-vla-object->ename wide_line)) (vlax-curve-getstartpoint wide_line) new_pt)

    (command "._join" (vlax-vla-object->ename org) line_wide wide_line "")
    
    org ;the resulted polyline (what the function returns)
    
    );end defun [wide_create]

  ;(9) get_midpt_2d: gets the middle point between 2 specified 2D points
  (defun get_midpt_2d
	 ;*VARIABLES*
	 (
	  pt1
	  pt2
	  )
    (list (+ (/ (- (car pt2) (car pt1)) 2) (car pt1)) (+ (/ (- (cadr pt2) (cadr pt1)) 2) (cadr pt1)))
    );end defun (get_midpt_2d)

  ;|(10) rtos-list: takes a list of real numbers and returns a string of them
  |;
  (defun rtos-list
	 ;*VARIABLES*
	 (
	  lst ;the list of values
	  dlm ;the delimiter character
	  /
	  elem ;element in list
	  result ;the result
	  )
    (setq result "")
    (foreach elem lst
      (progn
	(setq result (strcat result (rtos elem) dlm))
	);progn of foreach
      );foreach
    (setq result (vl-string-right-trim dlm result))
    );defun (rtos-list)
  
  ;|(11) csv_rows: produces 3 csv files
    1- Center line stations with their coordinates.
    2- Center line stations with their correspondant right side ROW coordinates.
    3- Center line stations with their correspondant left side ROW coordinates.
  |;
  (defun csv_rows
	 ;*VARIABLES*
	 (
	  cl ;vla-object of the center line
	  rline ;vla-object of the right side ROW polyline
	  lline ;vla-object of the left side ROW polyline
	  /
	  cl_file ;file of center line points with their stations
	  r_file ;file of right side ROW points with their stations (at center line)
	  l_file ;file of left side ROW points with their stations (at center line)
	  
	  station_pt ;station point
	  station ;station value
	  coords_cl ;coordinates of the center line
	  i ;increment
	  no_ver ;number of center line verteces
	  cl_ver ;center line verteces
	  pt_ver ;a point which is a vertex
	  stations_ver ;stations of the verteces
	  cl_bulges ;center line bulge values
	  cl_len ;length of the center line
	  seg_i ;segment index
	  s_i ;start index (of determining seg_i using the bi-section algorithm)
	  e_i ;end index (of determining seg_i using the bi-section algorithm)
	  is_ver ;a boolean value to indicate if the specified station is on a vertex or not
	  b ;a boolean value to break the loop of segment index determinitation (debugging case)
	  rem_len ;remaining length of a truncated list
	  mem_val ;values taken from the "member" function

	  xl_pt ;the actual needed xline that passes through station point and determines its correspondant points on both ROW polylines
	  mpt1 ;the midpoint of the xline of the 1st sector
	  mpt2 ;the midpoint of the xline of the 2nd sector
	  mpt ;the midpoint of the xline of a linear segment
	  bulge_sign ;sign of bulge
	  bulge_abs ;absolute value of bulge
	  r_pts ;points on the right side ROW correspondant to the station point of the center line
	  l_pts ;points on the left side ROW correspondant to the station point of the center line
	  )
    
    (setq cl_file (strcat (getvar 'DWGPREFIX) (vl-string-right-trim ".dwg" (getvar 'DWGNAME)) "_center_line.csv"))
    (setq r_file (strcat (getvar 'DWGPREFIX) (vl-string-right-trim ".dwg" (getvar 'DWGNAME)) "_right_ROW.csv"))
    (setq l_file (strcat (getvar 'DWGPREFIX) (vl-string-right-trim ".dwg" (getvar 'DWGNAME)) "_LEFT_ROW.csv"))

    (setq cl_file (open cl_file "w"))
    (setq r_file (open r_file "w"))
    (setq l_file (open l_file "w"))

    (write-line "Station,X Coordinate,Y Coordinate,Z Coordinate" cl_file)
    (write-line "Station,X Coordinate,Y Coordinate,Z Coordinate" r_file)
    (write-line "Station,X Coordinate,Y Coordinate,Z Coordinate" l_file)
    
    (setq coords_cl (vlax-safearray->list (vlax-variant-value (vla-get-coordinates cl))))

    (setq no_ver (/ (length coords_cl) 2))
    (setq i 0)
    (while (< i no_ver)
      (setq cl_ver (append cl_ver (list (setq pt_ver (list (nth (* 2 i) coords_cl) (nth (+ 1 (* 2 i)) coords_cl))))))
      (setq stations_ver (append stations_ver (list (vlax-curve-getdistatpoint cl pt_ver))))
      (if (< i (1- no_ver)) (setq cl_bulges (append cl_bulges (list (vla-getbulge cl i)))))
      
      (setq i (1+ i))
      );while (of aquiring center line verteces, their stations & bulge values)

    (setq i 0 cl_len (vla-get-length cl))
    (while (< (setq station (* i *station_interval*)) cl_len)
      (setq station_pt (vlax-curve-getpointatdist cl station))
      (write-line (strcat (rtos station) "," (rtos-list station_pt ",")) cl_file)
      ;determine segement index using the bi-section algorithm
      (setq seg_i 0 s_i 0 e_i (1- no_ver))
      (setq is_ver T b T) ;loop can run
      (while (and (not (and (> station (nth seg_i stations_ver)) (< station (nth (1+ seg_i) stations_ver)))) is_ver b)
	(cond
	  (
	   (setq mem_val (member station stations_ver))
	   (progn
	     (setq rem_len (length mem_val))
	   (if
	     (= (setq seg_i (- no_ver rem_len)) (1- no_ver))
	     (setq seg_i (1- seg_i))
	     );if (the point is the end point)
	  (setq is_ver nil) ;break the loop
	     );progn (of the 0th condition)
	   );0th condition (if the point is a vertex)
	  (
	   (and (< station (nth seg_i stations_ver)) (< station (nth (1+ seg_i) stations_ver)))
	   (progn
	     (setq e_i seg_i)
	     (setq seg_i (- e_i (/ (- e_i s_i) 2)))
	     );progn (of 1st condition)
	   );1st condition
	  (
	   (and (> station (nth seg_i stations_ver)) (> station (nth (1+ seg_i) stations_ver)))
	   (progn
	     (setq s_i seg_i)
	     (setq seg_i (+ s_i (/ (- e_i s_i) 2)))
	     );progn (of 2nd condition)
	   );2nd condition
	  (t (setq b nil)) ;break the loop if no condition is met
	  );cond
	);while (of determining segment index)

      ;|For every station_pt draw a construction line (xl) that passes through its arc's
        center point, or perpendicular to its linear segment.
        Then, write the intersection points to the csv files
      |;
      (cond
	(
	 (not is_ver)
	 (progn
	   (if
	     (= (setq bulge_abs (nth seg_i cl_bulges)) 0)
	     (progn
	       (if
		 (= rem_len 1)
		 (progn
		   (command "._xline" station_pt (polar station_pt (+ (/ pi 2) (angle (nth seg_i cl_ver) station_pt)) 1) "")
	           (setq xl_pt (vlax-ename->vla-object (entlast)))
		   );progn (it's the end point)
		 (progn
		   (command "._xline" station_pt (polar station_pt (+ (/ pi 2) (angle (nth seg_i cl_ver) (nth (1+ seg_i) cl_ver))) 1) "")
	           (setq xl_pt (vlax-ename->vla-object (entlast)))
		   );progn (not the end point)
		 );if (the point is the end point or not)
	       );progn (it's a line)
	     (progn
	       (if
		 (= rem_len 1)
		 (progn
		   (if (< bulge_abs 0) (setq bulge_sign -1.0) (setq bulge_sign 1.0))
		   (setq bulge_abs (abs bulge_abs))
		   (command "._xline" station_pt
			    (polar
			      (nth seg_i cl_ver)
			      (+ (angle (nth seg_i cl_ver) station_pt) (* bulge_sign (- (/ pi 2) (* 2 (atan bulge_abs)))))
			      1
			      );polar
			    ""
			    );command (xline)
		   (setq xl_pt (vlax-ename->vla-object (entlast)))
		   );progn (it's the end point)
		 (progn
		   (if (< bulge_abs 0) (setq bulge_sign -1.0) (setq bulge_sign 1.0))
		   (setq bulge_abs (abs bulge_abs))
		   (command "._xline" station_pt (polar station_pt (+ (angle (nth seg_i cl_ver) (nth (1+ seg_i) cl_ver)) (* bulge_sign (- (/ pi 2) (* 2 (atan bulge_abs))))) 1) "")
		   (setq xl_pt (vlax-ename->vla-object (entlast)))
		   );progn (not the end point)
		 );if (the point is the end point)
	       );progn (it's an arc)
	     );if (whether it's a line or an arc)
	   );progn (if the point is a vertex)
	 );0th condition (if the point is a vertex)
	(
	 is_ver
	 (progn
	   (if
	     (= (nth seg_i cl_bulges) 0)
	     (progn
	       (command "._xline" station_pt (polar station_pt (+ (/ pi 2) (angle (nth seg_i cl_ver) (nth (1+ seg_i) cl_ver))) 1) "")
	       (setq xl_pt (vlax-ename->vla-object (entlast)))
	       );progn (it's a line)
	     (progn
	       (setq mpt1 (get_midpt_2d (nth seg_i cl_ver) station_pt))
	       (setq mpt2 (get_midpt_2d station_pt (nth (1+ seg_i) cl_ver)))
	       (command "._xline" station_pt (inters
					       mpt1
					       (polar mpt1 (+ (/ pi 2) (angle (nth seg_i cl_ver) station_pt)) 1)
					       mpt2
					       (polar mpt2 (+ (/ pi 2) (angle station_pt (nth (1+ seg_i) cl_ver))) 1)
					       nil
					       );inters
			"");command (xline)
	       (setq xl_pt (vlax-ename->vla-object (entlast)))
	       );progn (it's an arc)
	     );if (is it a line or an arc)
	   );progn (if the point isn't a vertex)
	 );1st condition (if the point isn't a vertex)
	);cond (the point is a vertex or not)

      (setq r_pts (rtos-list (vlax-safearray->list (vlax-variant-value (vla-intersectwith xl_pt rline acExtendNone))) ","))
      (setq l_pts (rtos-list (vlax-safearray->list (vlax-variant-value (vla-intersectwith xl_pt lline acExtendNone))) ","))
      (write-line (strcat (rtos station) "," r_pts) r_file)
      (write-line (strcat (rtos station) "," l_pts) l_file)
      (vla-delete xl_pt)
      
      (setq i (1+ i) is_ver T b T s_i 0 e_i (1- no_ver) seg_i 0) ;increment and reset
      );while (of iteration through stations)

    (mapcar 'close (list cl_file r_file l_file))
    );end defun [csv_rows]

  ;|(12) m-text: writes multiple text lines bellow each other
  |;
  (defun m-text
	 ;*VARIABLES*
	 (
	  textlst ;list of strings, every element will be written in a separate line
	  /
	  txth ;text height
	  inspt ;insertion point
	  i ;incrementor
	  )
    (command-s
      "._text"
      (setq inspt (getpoint "Specify text insertion point:\n"))
      (setq txth (get_dist (if *text_height_len* *text_height_len* (/ *row_right_dist* 2)) "Specify text height"))
      0
      (car textlst)
      "");command-s (write the 1st line)

    (setq i 0)
    (foreach txtline textlst
      (progn
	(if (> i 0)
	  (progn
	    (command-s
	      "._text"
	      (setq inspt (list (car inspt) (- (cadr inspt) (+ txth 0.5))))
	      txth
	      0
	      txtline
	      ""
	      );command-s
	    );progn (then part)
	  );if

	(setq i (1+ i))
	);progn of foreach
      );foreach (write the rest of lines)
    );defun (m-text)

  ;*END OF UTILITY FUNCTIONS*
  
  ;*MAIN PROGRAM CONTEXT*
  ;__________________________

  ;Select the center line
  (princ "\n[[Select the center line (LWPolyline only)]]")
  (while
    (not
      (setq sscline (ssget "_:S" '((0 . "LWPOLYLINE"))))
      );not
    (princ "\nInvalid Selection!")
    (princ "\n[[Select the center line (LWPolyline only)]]")
    );while
  (setq cline (ssname sscline 0))

  ;Take user input for ROWs default distances & widening list creation
  (setq *row_right_dist* (get_dist *row_right_dist* "Specify right side ROW default distance"))
  (setq *row_left_dist* (get_dist *row_left_dist* "Specify left side ROW default distance"))

  (setq width_list (get_widening_list (vla-get-length (setq cline (vlax-ename->vla-object cline)))))

  ;Construct ROWs polylines
  (setq cl_spt (vlax-safearray->list (vlax-variant-value (vla-get-coordinate cline 0))))
  (setq cl_2nd_pt (vlax-safearray->list (vlax-variant-value (vla-get-coordinate cline 1))))
  
  (vla-startundomark doc)

  (setq sysvars (mapcar 'getvar (list 'OSMODE 'OSNAPCOORD 'CMDECHO)))
  (mapcar 'setvar (list "OSMODE" "OSNAPCOORD" "CMDECHO") '(0 1 0))
  
  (command "._Offset" *row_right_dist* sscline (polar cl_spt (setq r_ang (+ (/ pi 2) (angle cl_2nd_pt cl_spt))) *row_right_dist*) "Exit") ;draw the initial right side ROW
  (setq r_line (vlax-ename->vla-object (entlast))) ;initialize right side ROW polyline
  (command "._Offset" *row_right_dist* sscline (polar cl_spt (setq l_ang (- (angle cl_2nd_pt cl_spt) (/ pi 2))) *row_left_dist*) "Exit") ;draw the initial left side ROW
  (setq l_line (vlax-ename->vla-object (entlast))) ;initialize left side ROW polyline

  (foreach elem width_list
    (cond
      ((= "Right" (nth 0 elem))
       (setq r_line (wide_create "Right" (if *last_wide_right_dist* *last_wide_right_dist* *row_right_dist*)
		      sscline cline r_line (nth 2 elem) (polar cl_spt r_ang (nth 2 elem)) (nth 1 elem)))
       );1st condition
      ((= "Left" (nth 0 elem))
       (setq l_line (wide_create "Left" (if *last_wide_left_dist* *last_wide_left_dist* *row_left_dist*)
		      sscline cline l_line (nth 2 elem) (polar cl_spt l_ang (nth 2 elem)) (nth 1 elem)))
       );2nd condition
      (t nil)
      );cond
    );foreach
  (setq *last_wide_right_dist* nil *last_wide_left_dist* nil) ;reset

  (princ "WRITE LENGTHS OF THE 3 POLYLINES\n")
  (m-text (list
	    (strcat "Length of Center Line: " (rtos (vla-get-length cline)))
	    (strcat "Length of Right Side ROW: " (rtos (vla-get-length r_line)))
	    (strcat "Length of Left Side ROW: " (rtos (vla-get-length l_line)))
	    );list
	  );m-text (utility function of writing polylines' lengths)
  
  (vla-endundomark doc)

  ;write the csv files
  (princ "WRITING CSV FILES ...\n")
  (setq *station_interval* (get_dist *station_interval* "Specify station interval"))
  (csv_rows cline r_line l_line)
  (princ "CSV FILES ARE WRITTEN SUCCESSFULLY\n")

  (mapcar 'setvar (list "OSMODE" "OSNAPCOORD" "CMDECHO") sysvars) ;reset system variables
  
  (princ);debug line
  );end defun [rowsdraw]