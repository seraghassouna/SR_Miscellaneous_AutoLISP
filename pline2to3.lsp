;Author: Serag Hassouna
;| Purpose: draw two lines of slope 2/3 & -2/3 at the begining and the end of a "sender polyline"
the two lines intersects with a "reciever polyline". |;

(defun C:pline2to3
       (/
	doc
	modelspace
	sender
	reciever
	s_coords
	x1 x2 y1 y2
	l
	complete
	line1 line2
	)
  ;[1]- Get the drawing object and setup activex functions
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq modelspace (vla-get-modelspace doc))
  
  ;[2]- utility functions

  ;| 1- selpoly: this function ensures that you select a lightweight polyline, otherwise it will rewind itself till
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

  ;| 2- pl_open_only: This function allows user to select
a LWPolyline and ensures that this polyline isn't closed, otherwise, it will rewind untill the user selects an
opened LWPolyline.
it returns the list (Pline_vla_object Coordinates) if the polyline isn't closed.
|;
  (defun pl_open_only (msg / flag pl coords l is_opened)
    (setq flag T)
    (While flag
      (setq pl (selpoly msg))
      (setq pl (vlax-ename->vla-object pl))
      (setq coords (vlax-safearray->list (vlax-variant-value (vla-get-coordinates pl))))
      (setq l (length coords))
      (setq is_opened (not (and (= (nth 0 coords) (nth (- l 2) coords)) (= (nth 1 coords) (nth (- l 1) coords)))))
      (if is_opened
	(setq flag nil)
	(princ "\nPolyline is Closed!\n")
	);end if
      );End while

    (progn (list pl coords));what the function returns
    );End pl_open_only

  ;3 -pool_getkword
  ;pool-getkword: get keyword from user from a pool of keywords specified as a list,
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

;****

  ;| 4- draw2to3line: this function draws the inclined lines by the ratio 2 vertical to 3 horizontal,
this ratio is relative to the line segment of the polyline
|;
  (defun draw2to3line
	 (
	  sign ;is 1.0 or -1.0
	  /
	  base_ang_st
	  normal_ang_st final_ang_st
	  incl_ang
	  uv
	  )
    (setq base_ang_st (angle (list x1 y1) (list x2 y2)))
    (setq normal_ang_st (+ pi base_ang_st))
    (if (>= normal_ang_st (* 2 pi)) (- normal_ang_st (* 2 pi)));the local y axis

    (setq incl_ang (angle '(0 0) '(3 2))) ;more improvements can be made here, but keep things simple right now
    (setq final_ang_st (+ normal_ang_st (* sign incl_ang)))

    (setq x2 (+ x1 (cos final_ang_st)) y2 (+ y1 (sin final_ang_st))) ;final unit vector
    
    
    (setq uv (vla-addline modelspace (vlax-3d-point x1 y1 0) (vlax-3d-point x2 y2 0)))
    (setq ints_pt (vlax-safearray->list (vlax-variant-value (vla-intersectwith uv (car reciever) acExtendBoth))))
    (setq x2 (nth 0 ints_pt) y2 (nth 1 ints_pt))
    (vla-delete uv)
    (setq uv (vla-addline modelspace (vlax-3d-point x1 y1 0) (vlax-3d-point x2 y2 0)))
    
    );End draw2to3line
  
  ;End of utility functions

  ;[2]- Execution space

  ;*Select The Sender and Reciever Polylines*
  (setq sender (pl_open_only "Select Sender Polyline: "))
  (setq reciever (pl_open_only "Select Reciever Polyline: "))

  ;*Draw your lines

  (vla-startundomark doc)
    
  (setq s_coords (cadr sender))
  (setq x1 (nth 0 s_coords) y1 (nth 1 s_coords) x2 (nth 2 s_coords) y2 (nth 3 s_coords))
  (setq line1 (draw2to3line 1.0))
  
  (setq l (length s_coords))
  (setq x2 (nth (- l 4) s_coords) y2 (nth (- l 3) s_coords) x1 (nth (- l 2) s_coords) y1 (nth (- l 1) s_coords))
  (setq line2 (draw2to3line -1.0))

  
  ;Note that x1,y1,x2,y2, reciever are accessible to "draw2to3line"

  ;IF the user needs them drawn the other way
  (setq complete (pool-getkword "n" "Draw Lines the other way?" '("y" "n")))
  (if (eq "y" complete)
    (progn
      (mapcar 'vla-delete (list line1 line2))
      
      (setq x1 (nth 0 s_coords) y1 (nth 1 s_coords) x2 (nth 2 s_coords) y2 (nth 3 s_coords))
      (draw2to3line -1.0)
  
      (setq l (length s_coords))
      (setq x2 (nth (- l 4) s_coords) y2 (nth (- l 3) s_coords) x1 (nth (- l 2) s_coords) y1 (nth (- l 1) s_coords))
      (draw2to3line 1.0)
      );end progn
    );end if
    
  (vla-endundomark doc)
  
  );End defun