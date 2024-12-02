;;; $Id: ndfsm.lisp,v 1.7 1998/03/05 21:05:12 fool Exp fool $
;;; nd-fsm, lin340 (8am section), chris mccraw


(defun readme ()
  (progn (princ "alphabet? :      ")
         (setq abet (read))
         (princ "set of states? : ")
         (setq allstate (read))
         (princ "initial state? : ")
         (setq istate (if (atom (setq tempo (read)))
                          tempo
                          (car tempo)))
         (princ "set of final states? : ")
         (setq fstates (read)))
         (princ "transitions (one per line, end with nil)? : ")
         (setq woop t)
         (setq transitions nil)
         (loop while woop do
         (if (setq tempo (read))
             (setq transitions (append (list tempo) transitions))
             (setq woop nil)))
         (princ "what string would you like to test? :")
         (setq it (read))
         (la transitions it fstates istate))

;; i find that there is no need for the emulator to know the alphabet.
;; characters for which there is no transition from the current state,
;; whether in the alphabet or not, will not be accepted.  characters not
;; in the alphabet but given transitions suggest an ill-defined FSM
;; which i accept since the program should not judge user input, merely
;; act on it.

(defun la (
  transitions     ; (state1, accepted, state2) [const]
  string          ; (string to test)
  finalstates     ; (list of final states) [const]
  currentstate    ; (atom for passing recursively current state)
  )


(if string        ; check for emptystring--we're done
  (if (setq bob (loop for i in transitions
      when (eq (if (listp currentstate)       ;listp to be certain its unwrapt
                    (car currentstate)
                      currentstate) (car i))
        collect i))
    (dolist (cur bob) (if (eq 'e (cadr cur))  ;cope with epsilon transitions
                          (if (la transitions string finalstates (setq currentstate (cddr cur))) (return t))
                          (if (eq (car string) (cadr cur))
                                 (if (la transitions
                                  (cdr string)    ;we've processed the 1st char
                                  finalstates     ;basically constant
                                  (setq currentstate (cddr cur))) ; save cur
                                  (return t))))))
  ;; since i use a null string to indicate completion, handle any pending
  ;; epsilon trans (and the case of the empty string as the string to check)
    (if (member (setq lala (car currentstate)) finalstates)
        t
         (if (setq bob (loop for i in transitions
                    when (eq (car currentstate) (car i))
         collect i))
    (dolist (cur bob) (if (eq 'e (cadr cur))  ;cope with epsilon transitions
                          (if (la transitions string finalstates (setq currentstate (cddr cur)))
                              (return t))))))))
