
;;;======================================================
;;;     Bicycle repair Expert System
;;;
;;;     This expert system does a proper diagnoses of some of the problems associated with 
;;;     drive,brakes,damper in your bicycle.
;;;
;;;     PROJECT BY :
;;;     
;;;     Adrian Rutkowski
;;;     Marcin Wójcik
;;;     
;;;======================================================

;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction ask-question (?question $?allowed-values)
	(printout t ?question)
	(bind ?answer (read))
	(if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
	(while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
	?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))


;;;***************
;;;* QUERY RULES *
;;;***************


(defrule determine-problem-type ""
	(not (problem-type ?))
	(not (repair ?))
	=>
	(assert (problem-type
		(ask-question "What is the problem type (drive/brakes/damper)? "
                    drive brakes damper))))


;;;*************************
;;;* QUERY RULES FOR DRIVE*
;;;*************************


(defrule determine-drive-condition ""
	(problem-type drive)
	(not (drive-condition  ?))
	(not (repair ?))
	=>
	(assert (drive-condition  (yes-or-no-p "Is the chain working? (yes/no)? "))))
   
(defrule determine-chain ""
	(problem-type drive)
	(drive-condition  yes)
	(not (repair ?))
	=>
	(assert (chain (yes-or-no-p "Is the chain clean and lubricated? (yes/no)? "))))

(defrule determine-cassete ""
	(problem-type drive)
	(drive-condition  yes)
	(chain yes)
	(not (repair ?))   
	=>
	(assert (cassete (yes-or-no-p "Is the bicycle cassette cleaned? (yes/no)? "))))
   
(defrule determine-gears""
	(problem-type drive)
	(drive-condition yes)
	(chain yes)
	(cassete yes)
	(not (repair ?))
	=>
	(assert (gears (yes-or-no-p "Do gears change smoothly? (yes/no)? "))))
   
;;;*************************
;;;* QUERY RULES FOR DAMPER*
;;;*************************


(defrule determine-damper ""
	(problem-type damper)
	(not (damper-starting ?))
	(not (repair ?))
	=>
	(assert (damper (yes-or-no-p "Is the bicycle damper cleaned? (yes/no)? "))))
   
(defrule determine-cleaned ""
	(problem-type damper)
	(damper yes)
	(not (repair ?))
	=>
	(assert (cleaned (yes-or-no-p "Was the bicycle shock absorber lubricated a long time ago? (yes/no)? "))))

(defrule determine-air ""
	(problem-type damper)
	(damper yes)
	(cleaned no)
	(not (repair ?))   
	=>
	(assert (air (yes-or-no-p "Is the bicycle shock absorber air? (yes/no)? "))))
   
(defrule determine-pressure""
	(problem-type damper)
	(damper yes)
	(cleaned no)
	(air yes)
	(not (repair ?))
	=>
	(assert (pressure(yes-or-no-p "Is the pressure condition ok?(yes/no)? "))))
  
;;;**************************
;;;* QUERY RULES FOR BRAKES *
;;;**************************


(defrule determine-type-brakes ""
	(problem-type brakes)
	(not (type-brakes ?))
	(not (repair ?))
	=>
	(assert (type-brakes
      (ask-question "What type of brakes do you have (disc/v-brakes)? "
                    disc v-brakes))))

(defrule determine-disc-brakes ""
	(problem-type brakes)
	(type-brakes disc)
	(not (repair ?))
	=>
	(assert (brakes-disc-brakes (yes-or-no-p "Are the brake discs skewed (yes/no)? "))))
   
(defrule determine-v-brakes ""
	(problem-type brakes)
	(type-brakes v-brakes)
	(not (repair ?))
	=>
	(assert (v-brakes (yes-or-no-p "Do the brakes have good condition pads (yes/no)? "))))
   
(defrule determine-v-brakes-cable ""
	(problem-type brakes)
	(type-brakes v-brakes)
	(v-brakes yes)
	(not (repair ?))
	=>
	(assert (v-brakes-cable (yes-or-no-p "Is the brake cable efficient (yes/no)? "))))
   
(defrule determine-type-of-disc ""
	(problem-type brakes)
	(type-brakes disc)
	(brakes-disc-brakes yes)
	(not (repair ?))
	=>
    (assert (type-of-disc
      (ask-question "What kind of disc do you have? (mechanical/hydraulic)? "
                    mechanical hydraulic))))

(defrule determine-mechanical-problem ""
	(problem-type brakes)
	(type-brakes disc)
	(brakes-disc-brakes yes)
	(type-of-disc mechanical)
	(not (repair ?))
	=>
	(assert (mechanical-problem (yes-or-no-p "Is the brake cable efficient (yes/no)? "))))
   
(defrule determine-hydraulic-problem ""
	(problem-type brakes)
	(type-brakes disc)
	(brakes-disc-brakes yes)
	(type-of-disc hydraulic)
	(not (repair ?))
	=>
	(assert (hydraulic-problem (yes-or-no-p "Is there oil? (yes/no)? "))))
 
(defrule determine-mechanical-problem-pads ""
	(problem-type brakes)
	(type-brakes disc)
	(brakes-disc-brakes yes)
	(type-of-disc mechanical)
	(mechanical-problem yes)
	(not (repair ?))
	=>
	(assert (mechanical-problem-pads (yes-or-no-p "Do the brakes have good condition pads  (yes/no)? "))))
   
(defrule determine-hydraulic-problem-oil ""
	(problem-type brakes)
	(type-brakes disc)
	(brakes-disc-brakes yes)
	(type-of-disc hydraulic)
	(hydraulic-problem yes)
	(not (repair ?))
	=>
	(assert (hydraulic-problem-oil (yes-or-no-p "Was the brake oil changed a long time ago? (yes/no)? "))))
   
(defrule determine-hydraulic-problem-pads "" 
    (problem-type brakes)
	(type-brakes disc)
	(brakes-disc-brakes yes)
	(type-of-disc hydraulic)
	(hydraulic-problem yes)
	(hydraulic-problem-oil no)
	(not (repair ?))
	=>
	(assert (hydraulic-problem-pads (yes-or-no-p "Do the brakes have good condition pads  (yes/no)? "))))

;;;***************************
;;;* REPAIR RULES FOR drive *
;;;***************************


(defrule drive-condition ""
	(problem-type drive)
	(chain yes)
	(cassete yes)
	(gears yes)
	(not (repair ?))
	=>
	(assert (repair "No repair needed.")))
   
(defrule drive-condition-no ""
	(problem-type drive)
	(drive-condition no)
	(not (repair ?))
	=>
	(assert (repair "Replace the chain."))) 
   
(defrule drive-chain-no ""
	(problem-type drive)
	(drive-condition yes)
	(chain no)
	(not (repair ?))
	=>
	(assert (repair "Wash and brush the chain."))) 

(defrule drive-cassete-no ""
	(problem-type drive)
	(drive-condition yes)
	(chain yes)
	(cassete no)
	(not (repair ?))
	=>
	(assert (repair "Clean the cassette.")))   

(defrule drive-gears-no ""
	(problem-type drive)
	(drive-condition yes)
	(chain yes)
	(cassete yes)
	(gears no)
	(not (repair ?))
	=>
	(assert (repair "Adjust the derailleurs.")))   
  
 ;;;*******************************
;;;* REPAIR RULES FOR brakes     *
;;;*******************************

(defrule brakes-disc-brakes-no ""
	(problem-type brakes)
	(type-brakes disc)
	(brakes-disc-brakes no)
	(not (repair ?))
	=>
	(assert (repair "Straighten the brake disc."))) 
   
  
(defrule brakes-v-brakes-no ""
	(problem-type brakes)
	(type-brakes v-brakes)
	(v-brakes  no)
	(not (repair ?))
	=>
	(assert (repair "Replace the brake pads.")))  
   
(defrule brakes-v-brakes-cable-yes ""
	(problem-type brakes)
	(type-brakes v-brakes)
	(v-brakes yes)
	(v-brakes-cable yes)
	(not (repair ?))
	=>
	(assert (repair "No repair needed.")))  
   
(defrule brakes-v-brakes-cable-no ""
	(problem-type brakes)
	(type-brakes v-brakes)
	(v-brakes yes)
    (v-brakes-cable no)
    (not (repair ?))
	=>
	(assert (repair "Replace the brake cables.")))  

   
(defrule brakes-mechanical-problem-no ""
	(problem-type brakes)
	(type-brakes disc)
	(brakes-disc-brakes yes)
    (type-of-disc mechanical)
	(mechanical-problem no)
	(not (repair ?))
	=>
	(assert (repair "Replace the brake cables."))) 
   
(defrule brakes-hydraulic-problem-no ""
	(problem-type brakes)
	(type-brakes disc)
	(brakes-disc-brakes yes)
    (type-of-disc hydraulic)
	(hydraulic-problem no)
	(not (repair ?))
	=>
	(assert (repair "Fill with brake oil.")))  
   
(defrule brakes-hydraulic-problem-oil-yes ""
	(problem-type brakes)
	(type-brakes disc)
	(brakes-disc-brakes yes)
    (type-of-disc hydraulic)
	(hydraulic-problem yes)
	(hydraulic-problem-oil yes)
	(not (repair ?))
	=>
	(assert (repair "Replace the brake oil.")))   
 
(defrule brakes-hydraulic-problem-pads-no ""
	(problem-type brakes)
	(type-brakes disc)
	(brakes-disc-brakes yes)
    (type-of-disc hydraulic)
	(hydraulic-problem yes)
	(hydraulic-problem-oil no)
    (hydraulic-problem-pads no)
	(not (repair ?))
	=>
	(assert (repair "Replace the brake pads.")))
   
(defrule brakes-mechanical-pads-yes ""
	(problem-type brakes)
	(mechanical-problem yes)
	(mechanical-problem-pads yes)
	(not (repair ?))
	=>
	(assert (repair "No repair needed.")))  
   
(defrule brakes-mechanical-pads-no ""
	(problem-type brakes)
	(mechanical-problem yes)
	(mechanical-problem-pads no)
	(not (repair ?))
	=>
	(assert (repair "Replace the brake pads.")))  

(defrule brakes-hydraulic-problem-pads-no ""
	(problem-type brakes)
	(type-brakes disc)
	(brakes-disc-brakes yes)
    (type-of-disc hydraulic)
	(hydraulic-problem yes)
	(hydraulic-problem-oil no)
    (hydraulic-problem-pads yes)
	(not (repair ?))
	=>
	(assert (repair "No repair needed.")))
   
;;;***************************
;;;* REPAIR RULES FOR DAMPER *
;;;***************************
   
(defrule damper-no ""
	(problem-type damper)
	(damper no)
	(not (repair ?))
	=>
	(assert (repair "Clean and lubricate the bicycle damper.")))
   
(defrule cleaned-yes ""
	(problem-type damper)
	(damper yes)
	(cleaned yes)
	(not (repair ?))
	=>
	(assert (repair "Clean and lubricate the bicycle damper."))) 
   
(defrule pressure-yes ""
	(problem-type damper)
	(damper yes)
	(cleaned no)
	(air yes)
	(pressure no)
	(not (repair ?))
	=>
	(assert (repair "Set the air pressure of the bicycle damper")))   
   
 (defrule pressure-no ""
	(problem-type damper)
	(damper yes)
	(cleaned no)
	(air yes)
	(pressure yes)
	(not (repair ?))
	=>
	(assert (repair "No repair needed.")))  

;;;******************************
;;;* REPAIRS NOT MENTIONED      *
;;;******************************
   
(defrule no-repairs ""
  (declare (salience -10))
  (not (repair ?))
  =>
  (assert (repair "Take the bike for service.")))

;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system-banner ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "The BICYCLE DIAGNOSES Expert System")
  (printout t crlf crlf))

(defrule print-repair ""
  (declare (salience 10))
  (repair ?item)
  =>
  (printout t crlf crlf)
  (printout t "Suggested Repair:")
  (printout t crlf crlf)
  (format t " %s%n%n%n" ?item))

