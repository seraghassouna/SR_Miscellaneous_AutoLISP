;|C:ADCC : gets the summation of any selection of texts that have suffixes and prefixes
This version is written by Serag Hassouna
|;
(defun C:ADCC
       ;*VARIABLES*
       (
	/
	ss ;the selection set of texts
	sslen ;length of the selection set of texts
	txtentget ;text entity list from the entget function
	sum ;summation
	i ;incrementor
	)
;*UTILITY FUNCTION[S]*
;|(1) strnumonly: takes a string, removes any non numerical characters except for "."
For Example: "1a2b3d" -> "123" , "24 5c fg" --> "245"
This comes handy in removing both suffixes and prefixes and leaving the number itself
however, it will fail if there're multiple "." characters.
|;
(defun strnumonly
       ;*VARIABLES*
       (
	str ;string input
	/
	strlist ;a sorted list of ASCII codes of the string's charactes
	result ;the needed result
	)
  (setq strlist (vl-string->list str))
  (if (not (wcmatch str ""))
    (progn
      (foreach char strlist
	(progn
	  (if
	    (or (and  (>= char 48) (<= char 57)) (= char 46))
	    (progn
	      (setq result (append result (list char)))
	      );progn [if the character is valid]
	    );if character is valid or not
	  );progn [of foreach]
	);foreach
      );progn [then part]
    );if string isn't ""

  (setq result (vl-list->string result));the needed result
  );defun [strnumonly]

  ;*END OF UTILITY FUNCTION[S]*

  ;*MAIN PROGRAM CONTEXT*

  (princ "\nSelect texts to get their summation: \n")
  (setq ss (ssget '((0 . "*TEXT"))))
  (setq sslen (sslength ss))

  (setq i 0 sum 0)
  (while (< i sslen)
    (setq txtnum (atof (strnumonly (cdr (assoc 1 (setq txtentget (entget (ssname ss i))))))))
    (setq sum (+ sum txtnum))

    (setq i (1+ i))
    );while

  (princ "\nSum = ")
  (princ sum)

  (command-s
      "._text"
      (getpoint "\nSpecify text insertion point:\n")
      (cdr (assoc 40 txtentget)) ;text height
      0
      (strcat "RESULT = " (rtos sum))
      "");command-s (write the 1st line)
  
  (princ)
  
);defun [C:ADCC]
