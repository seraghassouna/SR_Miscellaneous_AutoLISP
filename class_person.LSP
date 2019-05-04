;|
Author: Serag Hassouna
Purpose: Implementation example of the concept of encapsulation from the OOP paradigm.
Design Idea: 1- Define a class object, this object stores the initialization values of the attributes (aka. properties)
and the methods' definitions.
2- Allow instanciation of objects, the object should mainly know how to retrieve its relevant class object.
Instances should store the minimal amount of data, i.e store only the modified values from the initialized ones.

The list of data stored in the class object is as follows
(properies_list methods_list)
properties_list: ((property_name . property_value) ..) [A list of association lists of properties]
methods_list: ((method_name method_value_as_USUBR) ..) [A list of association list of methods]

The list of data stored in the instance object is as follows
(class_symbol mod_only_props)
class_symbol: the symbol of the instance's parent class
mod_only_props: a list of the modified only properties and their values
note that when the object is instanciated, its data list is in the form "(class_symbol)" [hence, its length = 1]
|;

;|
[1]function: def_class_person: a class function that returns a list consisting of:
1- A list of properties and their initialized values
2- A list of defined methods
|;
(defun def_class_person 
;VARIABLES
(
name
height
hair_color
/
proplist ;list of properties
methods ;list of defined methods
put_enc ;"put" method encapsulated
get_enc ;"get" method encapsulated
)
;[1] set the "VALUE" of property list
(setq proplist (list (cons "NAME" name) (cons "HEIGHT" height) (cons "HAIR COLOR" hair_color) (cons "CLASS" "PERSON")))
;notice that the class name is assigned as an attribute.
;[2] define "set" and "get" methods
(setq put_enc
       ;|[1] method: put: this function sets the value of any attribute to the assigned object
       |;
  (defun put (obj_symbol propname propval / obj_vals cls_proplist obj_proplist ret)
    (setq obj_vals (eval obj_symbol))
    (setq cls_proplist (car (eval (car obj_vals)))) ;the list of all class properties
    (if
      (member (assoc propname cls_proplist) cls_proplist);truth condition, check the property existance within the class object
      (if
	(= (length obj_vals) 1) ;truth condition, check if there's no modified property yet
	(set obj_symbol (list (car obj_vals) (list (cons propname propval))))
	(progn
	  (setq obj_proplist (cadr obj_vals))
	  (if
	    (member (assoc propname obj_proplist) obj_proplist);truth condition, check the "modified" property existance within the instance object
	    (set obj_symbol (list (car obj_vals) (subst (cons propname propval) (assoc propname obj_proplist) obj_proplist)));the property has been modified
	    (set obj_symbol (list (car obj_vals) (append obj_proplist (list (cons propname propval)))));the property isn't modified yet
	    );if
	);else part [progn contains another if statement]
	);then part [the property exists within the class object]
      (progn
	(princ "\nError: The property isn't defined in the instance's class.\n")
	(setq ret "not")
	);else part, progn [the property doesn't exist within the class object]
      );if
    
    (if
      (eq ret "not")
      (setq ret nil)
      (setq ret propval)
      );if [what the function returns]
  );defun put
);setq of put_enc

(setq get_enc
       ;|[2] method: get: returns the property value
       |;
  (defun get (obj_symbol propname / obj_vals cls_proplist obj_proplist ret)
    (setq obj_vals (eval obj_symbol))
    (setq cls_proplist (car (eval (car obj_vals)))) ;the list of all class properties
    (if
      (member (assoc propname cls_proplist) cls_proplist);truth condition, check the property existance within the class object
      (if
	(= (length obj_vals) 1) ;truth condition, check if there's no modified property yet
	(setq ret (cdr (assoc propname cls_proplist)))
	(progn
	  (setq obj_proplist (cadr obj_vals))
	  (setq ret (member (assoc propname obj_proplist) obj_proplist))
	  (if
	    ret;truth condition, check the "modified" property existance within the instance object
	    (setq ret (cdr (assoc propname obj_proplist)));the property has been modified
	    (setq ret (cdr (assoc propname cls_proplist)));the property isn't modified yet
	    );if
	);else part [progn contains another if statement]
	);then part [the property exists within the class object]
      (princ "\nError: The property isn't defined in the instance's class.\n");else part [the property doesn't exist within the class object]
      );if
    ret
  );defun get
);setq get_enc

(setq methods (list (cons "PUT" put_enc) (cons "GET" get_enc)))
;[3] return the class list of properties and defined methods
(list proplist methods)
);defun class_person

;|[2] function: instanciate: a function to instanciate the object of class "PERSON"
notice: this function is implemented in a naive way
Because the defined methods [PUT & GET] in this proof of concept are primarily for the editing and retrieval of instances' properties,
the feature of supplying the intended values at the same time of instanciation is omitted.
|;
(defun instanciate (obj_symbol ;the quoted instance symbol
		    cls_symbol ;the quoted class symbol
		    )
  (set obj_symbol (list cls_symbol))
  );defun instanciate

;[3] function: apply_method: a function to apply a method for a specified instance
(defun apply_method (
		     obj_symbol ;the quoted symbol of the instance object
		     method_name
		     arglist ;arguments list
		     /
		     cls_methods ;association list of class methods
		     method ;the method
		     )
  (setq cls_methods (cadr (eval (car (eval obj_symbol)))))
  (if
    (member (assoc method_name cls_methods) cls_methods);truth condition, check if the method is defined within the class
    (apply (cdr (assoc method_name cls_methods)) (append (list obj_symbol) arglist));then part
    (princ "\nError: The method isn't defined within the instance's class.\n");else part
    );if
  );defun apply_method

;the main function
(defun main (/ $person_class samy hany)
  (setq $person_class (def_class_person "Somebody" 172 "Black")) ;define the class object

  (princ "\nCLASS DATA: ")
  (print $person_class)
  (princ "\n____________\n")

  (print ";Apply some operations on an instance called Samy")
  (instanciate 'samy '$person_class) ;instanciate a person instance
  (print samy)
  (apply_method 'samy "PUT" (list "HAIR COLOR" "Ginger"))
  (print samy)
  (apply_method 'samy "PUT" (list "HAIR COLOR" "Blonde"));notice: you can change property value for any number of times
  (print samy)
  (apply_method 'samy "PUT" (list "NAME" "Samy"))
  (print samy)

  (princ "\nNAME: ")
  (princ (apply_method 'samy "GET" '("NAME"))) ;NAME is modified
  (princ "\nHAIR COLOR: ")
  (princ (apply_method 'samy "GET" '("HAIR COLOR"))) ;HAIR COLOR is modified
  (princ "\nHEIGHT: ")
  (princ (apply_method 'samy "GET" '("HEIGHT"))) ;HEIGHT isn't modified
  (princ "\n____________\n")


  (print "Apply some operations on an instance called Hany")
  (instanciate 'hany '$person_class)
  (print hany)
  (apply_method 'hany "PUT" (list "HEIGHT" 165))
  (print hany)
  (apply_method 'hany "PUT" (list "NAME" "Hany"))
  (print hany)

  (princ "\nNAME: ")
  (princ (apply_method 'hany "GET" '("NAME"))) ;NAME is modified
  (princ "\nHAIR COLOR: ")
  (princ (apply_method 'hany "GET" '("HAIR COLOR"))) ;HAIR COLOR isn't modified
  (princ "\nHEIGHT: ")
  (princ (apply_method 'hany "GET" '("HEIGHT"))) ;HEIGHT is modified
  
  );defun main

(main)