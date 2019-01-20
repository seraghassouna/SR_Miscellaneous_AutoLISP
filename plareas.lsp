;Autour: Serag Hassouna
;This is a simple lisp file that selects only closed polylines objects.
;Then gets the total area of them.
(defun C:plareas (/ sspls n result i objid objarea)
  (vl-load-com) ;Enable Activex features
  (print "[[Select Polylines]]")
  (setq sspls (ssget '((0 . "LWPOLYLINE")))) ;get the polylines selected
  (setq n (sslength sspls)) ;get number of selected elements
  (setq result 0 i 0)
  (repeat n
    (progn
      (setq objid (ssname sspls i)) ;get the object's entity name
      (setq objid (vlax-ename->vla-object objid)) ;get the object's vla-object address
      ;check whether the polyline is closed or not, if not closed, report to user and abort.
      (if (and (eq :vlax-False (vla-get-closed objid)) (not (equal (vlax-curve-getstartpoint objid) (vlax-curve-getendpoint objid) 1e-6)))
	(progn
	  (print)
	  (alert "At least a polyline is not closed, program terminated")
	  (quit)
	  );Then part (if statement)
	);End if
      (setq objarea (vla-get-area objid)) ;get the object's area
      (setq result (+ result objarea)) ;add its area to the result

      (setq i (1+ i)) ;increment
      );End progn
    );End repeat

  (setq sspls nil)
  (print)
  (princ "Area is: ")
  (progn result);get the needed output
  );End defun