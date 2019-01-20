;Author: Serag Hassouna
;Contact email: serag.hassouna@gmail.com
;_________

;Purpose of this lisp:-
;To Calculate the stress according to Boussinesq equation on a meshed area which have an applied constant distributed load.
;The direction of this distributed load is "Gravity".

;_________

;Important Note: The area must be meshed using polyline elements, not 3Dface elements!

;_______________________________________


;boussinesq-stress: a command to get the stress at a specified point from a set of selected polylines
;these polylines are the meshing elements of the principle area
(defun C:boussinesq-stress (/ ms dload depth sspoint po pov sslines n i objid regobj earea q bline cg r term result)
  (vl-load-com) ;load ActiveX functions
  (setq ms (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))) ;get the current modelspace

  (setq dload (getreal "Specify the distributed load value: "))
  (setq depth (getreal "Specify point depth: "))
  (print "[[Select ONLY the point projection on XY Plane]]")
  (setq sspoint (ssget "_:S" '((0 . "POINT"))))
  (setq po (ssname sspoint 0))
  (setq pov (entget po)) ;get the dxf groups list of the point
  (setq pov (cdr (assoc 10 pov))) ;get the point coordinate [3D]
  (setq pov (reverse (cdr (reverse pov)))) ;remove the Z coordinate
  (print "point is selected")
  (setq sspoint nil) ;clear the selection set of the point
  (print "[[Select the polylines elements, execlude the main shape itself]]")
  (setq sslines (ssget '((-4 . "<OR") (0 . "LWPOLYLINE") (0 . "POLYLINE") (-4 . "OR>")))) ;select the meshing elements
  (setq n (sslength sslines)) ;number of selected polylines
  (setq i 0 result 0)
  (repeat n
    (progn
      (setq objid (ssname sslines i))
      (setq objid (vlax-ename->vla-object objid))
      (setq earea (vla-get-area objid))
      (setq q (* dload earea)) ;the force

      ;create region for every meshing element, then get its centroid
      (setq bline (vlax-make-safearray vlax-vbObject '(0 . 0)))
      (vlax-safearray-put-element bline 0 objid)
      (setq regobj (vla-AddRegion ms bline)) ;Add region returns a weird variant, use vlax-variant-value to retrive its real value
      (setq regobj (vlax-variant-value regobj)) ;get the safearray that contains the reigon
      (setq regobj (vlax-safearray-get-element regobj 0))

      (setq cg (vlax-variant-value (vla-get-centroid regobj)))
      (setq cg (vlax-safearray->list cg))
      
      (setq r (distance pov cg))
      (setq term (/ depth (+ (expt r 2) (expt depth 2))))
      (setq term (expt term 5))
      (setq result (+ result (/ (* 3 q term) (* 2 pi (expt depth 2)))))

      (setq i (1+ i))
      );End progn [of repeat]
    );End repeat

  ;release the selection sets
  (setq sslines nil)

  (princ "Boussinesq Stress = ")
  (progn result) ;get the result
  );End defun

;westergaard-stress: the same as boussinesq, but using westergaard equation
(defun C:westergaard-stress (/ ms dload depth pois sspoint po pov sslines n i objid regobj earea q bline cg r term1 term2 term result)
  (vl-load-com) ;load ActiveX functions
  (setq ms (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))) ;get the current modelspace

  (setq dload (getreal "Specify the distributed load value: "))
  (setq depth (getreal "Specify point depth: "))
  (setq pois (getreal "Specify Poisson's ratio: "))
  (print "[[Select ONLY the point projection on XY Plane]]")
  (setq sspoint (ssget "_:S" '((0 . "POINT"))))
  (setq po (ssname sspoint 0))
  (setq pov (entget po)) ;get the dxf groups list of the point
  (setq pov (cdr (assoc 10 pov))) ;get the point coordinate [3D]
  (setq pov (reverse (cdr (reverse pov)))) ;remove the Z coordinate
  (print "point is selected")
  (setq sspoint nil) ;clear the selection set of the point
  (print "[[Select the polylines elements, execlude the main shape itself]]")
  (setq sslines (ssget '((-4 . "<OR") (0 . "LWPOLYLINE") (0 . "POLYLINE") (-4 . "OR>")))) ;select the meshing elements
  (setq n (sslength sslines)) ;number of selected polylines
  (setq i 0 result 0)
  (repeat n
    (progn
      (setq objid (ssname sslines i))
      (setq objid (vlax-ename->vla-object objid))
      (setq earea (vla-get-area objid))
      (setq q (* dload earea)) ;the force

      ;create region for every meshing element, then get its centroid
      (setq bline (vlax-make-safearray vlax-vbObject '(0 . 0)))
      (vlax-safearray-put-element bline 0 objid)
      (setq regobj (vla-AddRegion ms bline)) ;Add region returns a weird variant, use vlax-variant-value to retrive its real value
      (setq regobj (vlax-variant-value regobj)) ;get the safearray that contains the reigon
      (setq regobj (vlax-safearray-get-element regobj 0))

      (setq cg (vlax-variant-value (vla-get-centroid regobj)))
      (setq cg (vlax-safearray->list cg))
      
      (setq r (distance pov cg))
      (setq term1 (sqrt (/ (- 1 (* 2 pois)) (- 2 (* 2 pois)))))
      (setq term2 (expt (+ (/ (- 1 (* 2 pois)) (- 2 pois)) (expt (/ r depth) 2)) 1.5))
      (setq term (/ term1 term2))
      (setq result (+ result (/ (* q term) (* 2 pi (expt depth 2)))))

      (setq i (1+ i))
      );End progn [of repeat]
    );End repeat

  ;release the selection sets
  (setq sslines nil)

  (princ "Westergaard Stress = ")
  (progn result) ;get the result
  );End defun

;del-reg: a command to delete regions only
(defun c:del-reg (/ ssregs n i reg reglist)
  (setq ssregs (ssget '((0 . "REGION"))))
  (setq n (sslength ssregs))
  (setq i 0)
  (repeat n
    (progn
      (setq reg (ssname ssregs i))
      (setq reglist (append reglist (list reg)))
      (setq i (1+ i))
      );End progn [of repeat]
    );End repeat

  (foreach reg reglist
    (progn
      (entdel reg)
      );End progn [of foreach]
    );End foreach

  (setq ssregs nil) ;clear the selection set
  (princ) ;clean end
  );End defun