;Author: Serag Hassouna
;Purpose of this lisp:-
;Selection of all polylines and lines within a specified layer, then return sum of their lengths
;______________________

;*GLOBAL VARIABLES*
(vl-load-com) ;load all Component Object Model functions
(setq doc (vla-get-ActiveDocument (vlax-get-acad-object))) ;get document (drawing)
;______________________

;total-length: a utility function that recieves any layer and returns the sum of lengths of all lines and polylines
(defun total-length (selayer / sslines i len objid aclayer)
  ;select all lines and polylines within the current active layer
  (setq sslines (ssget "_X" (list '(-4 . "<OR") '(0 . "LINE") '(0 . "LWPOLYLINE") '(0 . "POLYLINE") '(-4 . "OR>") (cons 8 selayer))))
  (setq i 0 len 0);the incrementer and the length variable
  (repeat (sslength sslines)
    (progn
      (setq objid (ssname sslines i)) ;get object's entity name
      (setq objid (vlax-ename->vla-object objid)) ;convert its entityname to vla-object
      (setq len (+ len (vla-get-length objid)));add the objects length
      (setq i (1+ i)) ;increase the incrementer
      );End progn [of repeat]
    );End of repeat
  (princ "Sum of Lengths: ")
  (progn len) ;return the sum of lengths
  );End defun

;sumlen-aclayer: from the active layer, return all lengths
(defun C:sumlen-aclayer (/)
  (setq aclayer (vla-get-ActiveLayer doc)) ;its form is vla-object
  (setq aclayer (vlax-vla-object->ename aclayer)) ;get layer's entity name
  (setq aclayer (cdr (assoc 2 (entget aclayer)))) ;get layer's label
  (total-length aclayer)
  )

;sumlen-line: from a single line or polyline, get its layer and return all lengths of its lines and polylines
(defun C:sumlen-line (/ ssline objid slayer)
  ;select a single line or polyline
  (print "[[Select ONLY one line or polyline]]")
  (setq ssline (ssget ":S" '((-4 . "<OR") (0 . "LINE") (0 . "LWPOLYLINE") (0 . "POLYLINE") (-4 . "OR>"))))
  (setq objid (ssname ssline 0))
  (setq slayer (cdr (assoc 8 (entget objid))))
  (total-length slayer)
  );End defun

;C:sumlen-sline : a utility  that recieves any layer and returns the sum of lengths of all lines and polylines
(defun C:sumlen-sline (/ sslines i len objid aclayer)
  ;select all lines and polylines within the current active layer
  (princ "\n[[Select lines]]\n")
  (setq sslines (ssget '((0 . "LINE"))))
  (setq i 0 len 0);the incrementer and the length variable
  (repeat (sslength sslines)
    (progn
      (setq objid (ssname sslines i)) ;get object's entity name
      (setq objid (vlax-ename->vla-object objid)) ;convert its entityname to vla-object
      (setq len (+ len (vla-get-length objid)));add the objects length
      (setq i (1+ i)) ;increase the incrementer
      );End progn [of repeat]
    );End of repeat
  (princ "Sum of Lengths: ")
  (progn len) ;return the sum of lengths
  );End defun