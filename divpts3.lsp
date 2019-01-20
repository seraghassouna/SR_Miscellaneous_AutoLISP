;Author:Serag Hassouna
;Version: 3
;| Purpose: Select 2 points, then create division points according to these conditions:
1- The maximum division length is user defined.
2- There isn't any division points if the distance between them is less than or equal to the allowable
3- The division length must be at its possible maximum length
[Not to draw infinite number of points to fullfil the 1st condition]
|;

;Note: there's a GLOBAL VARIABLE called "divpts_s" that holds the maximum allowable spacing

(defun C:divpts (/ doc cmdstate pt1 pt2 s dist x1 x2 y1 y2 xdif ydif reminder n i x y)
  ;**Utility Function[s]**

  ;ord-getdist: a utility function that gets real value from user who has a default one as integer or real
  (defun ord-getdist (defval ordmsg / masg val)
    ;defval: the default value, should be only integer or real
    ;ordmsg: is the order message, but without "\n", "<" and ">" default display characters
    (setq masg (strcat "\n" ordmsg (if divpts_s (strcat " <" (rtos defval) ">: ") ": ")))

    (initget (if divpts_s (+ 2 4) (+ 1 2 4)))
    (setq val (getdist masg))
    (if (eq val nil) (setq val defval))

    (progn val)
  );End defun
  
  ;**Execution Part**
  
  (setq cmdstate (getvar 'cmdecho))
  (setvar "CMDECHO" 0)
  
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))

  (vla-startundomark doc)
  
  (setq pt1 (getpoint "\nSelect 1st point: "))
  (setq pt2 (getpoint "\nSelect 2nd point: "))
  
  (setq divpts_s (ord-getdist divpts_s "Specify maximum allowable spacing"))
  
  (setq dist (distance pt1 pt2))
  (setq reminder (rem dist divpts_s))

  (setq
    x1 (nth 0 pt1)
    x2 (nth 0 pt2)
    y1 (nth 1 pt1)
    y2 (nth 1 pt2)
    );setq
  (setq xdif (- x2 x1) ydif (- y2 y1))

  (if
    (<= dist divpts_s)
    (progn
    (setq n 0)
    (princ "\nThe specified maximum allowable spacing is bigger than the actual distance")
    (princ "\nCan't draw any division point")
      );progn [then part]
    (if
      (/= reminder 0)
      (setq n (1+ (- (/ dist divpts_s) (/ reminder divpts_s))))
      (setq n (/ dist divpts_s))
      );if 2nd (dist > divpts_s) [else part for the 1st if statement]
    );if 1st

  (if
    (/= n 0)
    (progn
      (setq i 1)
      (while
	(< i n)
	(setq
	  x (+ x1 (* (/ i n) xdif))
	  y (+ y1 (* (/ i n) ydif))
	  );setq
	(command "._point" (list x y))

	(setq i (1+ i))
	);repeat
      );progn [then part], there's no else part here
    );if

  (setvar "CMDECHO" cmdstate)
  (vla-endundomark doc)

  (princ)
  );end defun [divpts]