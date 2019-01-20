;Author: Serag Hassouna

;|Purpose: Rotate objects using one of the following methods:
  1- Rotation Point is the insertion point of a block reference object, a text object or both of them.
  
  2- Rotation point is the geometric center of a selected collection of multible shapes,
  these shapes must be closed splines and polylines, otherwise, the program will ignore any opened polyline
  or spline.
|;

;| GLOBAL VARIABLES
  1- *rotation_pt_type*: a string that specifies the type of rotation point.
  2- *object_type*: specify whether to select texts only, block references only or both of them
|;

(defun C:rotate2center
       ;*LOCAL VARIABLES*
       (/
	doc ;the document object [the drawing file]
	modelspace ;The model space

	cp ;central point [it's actually the target point from any insertion point or geometric center point]
	ss ;the selection set of objects
	sslen ;number of elements within the selection set
	i ;incrementor
	osang ;offset angle in radians
	cpang ;angle from rotation point [insertion or geometric center point] to central point
	ang ;rotation angle in radians ((vla property value))
	cmdecho ;"CMDECHO" system variable default value
	ent ;any entity from a selection set with index "i"
	entcopy ;a copy of the entity
	entreg ;the created region of any specified lwpolyline or spline [to get the geometric center point from it]
	gpt ;geometric center point for an entity
	xdict ;the extension dictionary of the entity
	xrec_rot ;the XRecord that contatins the rotation value
	oldrot ;the previously stored value within the rotation XRecord
	xrec_grcds ;redundant variable that retrives the group code of the XRecord value
	xrec_vals ;a variable that retrieve the rotation value as a safearray
	)
  ;GET DOC & MODELSPACE OBJECTS
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq modelspace (vla-get-modelspace doc))
  
  ;*BEGINING OF UTILITY FUNCTIONS*

    ;(1) pool-getkword: get keyword from user from a pool of keywords specified as a list,
;the function works like a standard autocad getting keyword, a default value can be supplied, but if
;it equals nil the function will proceed normally also.
;Note: defval must be a real, an integer or a string!
(defun pool-getkword (defval ordmsg kwlist / dymode elem kwstr strdefval masg kw k)
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

  (foreach k kwlist
    (progn
      (if (vl-string-search kw k)
	(setq kw k)
	);if
      );progn of foreach
    );foreach [solving a bug of returning just only one word from the whole string value]
  
  (progn kw)
  );End defun

    ;(2) populate-XRecord: a utility function that populates an XRecord with dxf group codes and correspondant values, XRecord is assumed to
;exist
(defun populate-XRecord (xrecobj size dxfgrcd val / dxfgrcd2 val2)
  (setq dxfgrcd2 (vlax-make-safearray vlax-vbInteger (cons 0 (1- size))))
  (setq val2 (vlax-make-safearray vlax-vbVariant (cons 0 (1- size))))
  (vlax-safearray-fill dxfgrcd2 dxfgrcd)
  (vlax-safearray-fill val2 val)
  (vla-SetXRecordData xrecobj dxfgrcd2 val2)

  (princ) ;clean end
  );End defun

  ;(3) add-or-getXRecord: a utility function that creates or gets a Xrecord, parent dictionary is assumed to exist
(defun add-or-getXRecord (pardict kw / errstat xprop)
  (setq errstat (vl-catch-all-apply 'vla-GetObject (list pardict kw)))
  (if (vl-catch-all-error-p errstat)
    (progn
      (setq xprop (vla-AddXRecord pardict kw))
      (populate-XRecord xprop 1 '(1) '(0)) ;initialize the rotation value to 0, and give it a group code of 1
      ) ;progn [then part]
    (setq xprop (vla-GetObject pardict kw)) ;else part
    );End if
  (progn xprop)
  );End defun
  
  ;*END OF UTILITY FUNCTIONS*

  ;*MAIN PROGRAM CONTEXT*
  (setq cp (getpoint "\nSpecify unique central point: \n"))
  (setq ofsang (getangle "\nSpecify offset angle: \n"))
  (setq *rotation_pt_type* (pool-getkword
			     (if *rotation_pt_type* *rotation_pt_type* "Insertion point")
			     "Specify the type of the rotation base point"
			     (list "Insertion point" "Geometric center")
			     );pool-getkword
	);setq *rotation_pt_type*

  (setq cmdecho (getvar 'CMDECHO))
  (setvar "CMDECHO" 0)
  (vla-StartUndoMark doc)
  
  (cond
    (
     (or (wcmatch *rotation_pt_type* "I*") (wcmatch *rotation_pt_type* "i*"))
     (progn
       (setq *object_type* (pool-getkword
			     (if *object_type* *object_type* "bOth")
			     "Specify type of objects"
			     (list "Text only" "Blocks only" "bOth")
			     );pool-getkword
	     );setq *object_type*
       ;select objects
       (cond
	 (
	  (wcmatch *object_type* "Text only")
	  (progn
	    (setq ss (ssget '((0 . "*TEXT"))))
	    );progn [Text only]
	  );Text only
	 (
	  (wcmatch *object_type* "Blocks only")
	  (progn
	    (setq ss (ssget '((0 . "INSERT"))))
	    );progn [Blocks only]
	  );blocks only
	 (
	  (wcmatch *object_type* "bOth")
	  (progn
	    (setq ss (ssget '((-4 . "<OR") (0 . "INSERT") (0 . "*TEXT") (-4 . "OR>"))))
	    );progn [both text & blocks]
	  );both text & blocks
	 );cond [Text only, Blocks only or both of them]

       ;rotate objects
       (setq sslen (sslength ss) i 0)
       (while (< i sslen)
	 (setq ent (vlax-ename->vla-object (ssname ss i)))
	 (setq cpang (angle (vlax-safearray->list (vlax-variant-value (vla-get-insertionpoint ent))) cp))
	 (setq ang (- cpang ofsang))
	 (vla-put-rotation ent ang)

	 (setq i (1+ i))
	 );while [of iteration through objects]
       );progn [1st condition]
     );Insertion Point Method
    (
     (or (wcmatch *rotation_pt_type* "*G*") (wcmatch *rotation_pt_type* "*g*"))
     (progn
       (setq ss (ssget '((-4 . "<OR") (0 . "LWPOLYLINE") (0 . "SPLINE") (-4 . "OR>"))))
       (setq sslen (sslength ss) i 0)
       (while (< i sslen)
	 (setq ent (vlax-ename->vla-object (ssname ss i)))
	 (if (equal (vlax-curve-getstartpoint ent) (vlax-curve-getendpoint ent) 1e-6)
	   (progn
	       (setq entcopy (vla-copy ent))
	       (command "._region" (vlax-vla-object->ename entcopy) "")
	       (setq entreg (vlax-ename->vla-object (entlast)))
	       (setq gpt (vlax-safearray->list (vlax-variant-value (vla-get-centroid entreg))))
	       (vla-delete entreg)
	       (setq cpang (angle gpt cp))
	       (setq ang (- cpang ofsang))
	       ;SET AN XRECORD ATTATCEHED TO THE ENTITY'S EXTENSION DICTIONARY, & STORE THE ROTATION VALUE IN IT
	       ;THIS IS TO MIMIC THE EFFECT OF "VLA-PUT-ROTATION" FUNCTION
	       (setq xdict (vla-getExtensionDictionary ent))
	       (setq xrec_rot (add-or-getXRecord xdict "rotation"))
	       (vla-GetXRecordData xrec_rot 'xrec_grcds 'xrec_vals)
	       (setq oldang (vlax-variant-value (vlax-safearray-get-element xrec_vals 0)))
	       (if (eq (type oldang) (type "string")) (setq oldang (atof oldang))) ;though the value is supplied as a number, it's returned as a string value!

	       (if (/= ang oldang)
	         (progn
		   (command "._rotate" (vlax-vla-object->ename ent) ""
		  	    gpt
			    (* (- ang oldang) (/ 180 pi))
			    );command [rotate]
		   (populate-XRecord xrec_rot 1 '(1) (list ang))
		 );progn [then part]
	       );if [previously assigned rotation value differs from the current one]
	    );progn [then part]
	   );if [closed shape]

	 (setq i (1+ i))
	 );while [of iteration through objects]
       );progn [2rd condition]
     );Geometric Center Method
    (t nil)
    );cond [main]

  (vla-EndUndoMark doc)
  (setvar "CMDECHO" cmdecho)

  (princ)
  );defun [C:rotate2center]