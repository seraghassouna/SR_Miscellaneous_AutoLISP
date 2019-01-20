(defun C:LRR (/ rec c1 c2 wid ht rows cols blk); = Lights in Rectangular [& orthogonal] Room
  ;**UTILITY FUNCTIONS**

  ;|(1) selpoly: this function ensures that you select a lightweight polyline, otherwise it will rewind itself till
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

  ;|(2) selrec: a function that ensures that a rectangle is selected ,it rewinds itself until a 
 lwpolyline rectangle is selected. |;
(defun selrec (msg / recobj flag coords len ver-p a1 a2 a3)
  (vl-load-com)
  (setq flag T)
  (while flag
    (setq recobj (vlax-ename->vla-object (selpoly msg)))
    (setq flag (not (equal (vlax-curve-getstartpoint recobj) (vlax-curve-getendpoint recobj) 1e-6)));ensure start point is the same as end point [flag will be temporarily set to FALSE]
    (setq coords (vlax-safearray->list (vlax-variant-value (vla-get-coordinates recobj)))) ;get coordinates
    ;|There are 2 conditions, the first is when coords list has 8 elements
    the second is when coords list has 10 elements,
    the second one is valid if the 1st point is typical with the last point,
    & the first one is valid when the 1st point is not the same as
    the last point. 
    This is due to a bug in AutoCAD that doesn't coincide the
    start point with the last point if they are typical when the
    command "pl" is issued. (the case of 10 elements in coords list)
    This bug doesn't exist when the command "rec" is issued
    (the case of 8 elements in coords list)|;
    (if (not flag)
      (progn
        (setq len (length coords))
        (cond
          (
            (= len 8)
            (progn
              (setq ver-p (not (and (equal (nth 0 coords) (nth 6 coords) 1e-6) (equal (nth 1 coords) (nth 7 coords) 1e-6)))); 1st point /= last point
              (if ver-p
                (progn
                  (setq a1 (angle (list (nth 0 coords) (nth 1 coords)) (list (nth 2 coords) (nth 3 coords))))
                  (setq a2 (angle (list (nth 2 coords) (nth 3 coords)) (list (nth 4 coords) (nth 5 coords))))
                  (setq a3 (angle (list (nth 4 coords) (nth 5 coords)) (list (nth 6 coords) (nth 7 coords))))

                  (setq flag (not (and (equal (abs (rem (- a2 a1) (/ pi 2))) 0 1e-6) (equal (abs (rem (- a3 a2) (/ pi 2))) 0 1e-6))))
                );progn of if ver-p
		(progn
		  (setq flag T)
		  (princ "\nSelected shape isn't a rectangle\n")
		  );progn (if the shape is triangle) [debugging]
              );if ver-p [passed the previous test], check angle condition
            );progn of the 1st condition
          );coords list has 8 elements
          (
            (= len 10)
            (progn
              (setq ver-p (and (= (nth 0 coords) (nth 8 coords)) (= (nth 1 coords) (nth 9 coords)))); 1st point = last point
              (if ver-p
                (progn
                  (setq a1 (angle (list (nth 0 coords) (nth 1 coords)) (list (nth 2 coords) (nth 3 coords))))
                  (setq a2 (angle (list (nth 2 coords) (nth 3 coords)) (list (nth 4 coords) (nth 5 coords))))
                  (setq a3 (angle (list (nth 4 coords) (nth 5 coords)) (list (nth 6 coords) (nth 7 coords))))

                  (setq flag (not (and (equal (abs (rem (- a2 a1) (/ pi 2))) 0 1e-6) (equal (abs (rem (- a3 a2) (/ pi 2))) 0 1e-6))))
                );progn of if ver-p
		(progn
		  (setq flag T)
		  );progn
              );if ver-p [passed the previous test], check angle condition
            );progn of the 2nd condition
          );coords list has 10 elements
          (t (progn
	       (setq flag T)
	       (princ "\nSelected shape isn't a rectangle\n")
	       (princ "\n")
	       );progn
	   );debug condition
        );cond
      );progn [then part]
      (progn
        (princ "\nSelected shape isn't a rectangle\n")
        (princ "\n")
      );progn [else part]
    );if [only if the shape is closed]
  );while

  (progn recobj);return the rectangle
);defun [selrec]
  
  ;**END OF UTILITY FUNCTIONS**

  ;**MAIN PROGRAM CONTEXT**
  (setq rec (selrec "Select Rectangle: "))
  (setq

    c1 (vlax-safearray->list (vlax-variant-value (vla-get-coordinate rec 0)))

    c2 (vlax-safearray->list (vlax-variant-value (vla-get-coordinate rec 2)))

    wid (abs (- (car c1) (car c2)))

    ht (abs (- (cadr c1) (cadr c2)))

    rows (getint "\nNumber of rows (---): ")

    cols (getint "\nNumber of columns (|||): ")

    blk (cdr (assoc 2 (entget (car (entsel "\n Select Block")))))

  ); setq

  (command

    "_.minsert" blk

    (mapcar '+ ; insertion point

      (list (min (car c1) (car c2)) (min (cadr c1) (cadr c2))); lower left of room

 

      (list (/ wid cols 2) (/ ht rows 2)); fractions of width/height

    ); mapcar

    "" "" "" ; X, Y, rotation defaults -- edit if needed

    rows cols (/ ht rows) (/ wid cols); numbers and spacings

  ); command
  
  );defun [C:LRR]
