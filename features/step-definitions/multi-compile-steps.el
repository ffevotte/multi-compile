;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.


(Given "^Buffer \"\\(.+\\)\" does not exist$"
       (lambda (name)
         (when (get-buffer name)
           (kill-buffer name))))

(Given "^Buffer \"\\(.+\\)\" is empty$"
       (lambda (name)
         (with-current-buffer name
           (let ((buffer-read-only nil))
             (erase-buffer)))))

(When "^I execute command \"\\(.+\\)\"$"
      (lambda (command)
        (call-interactively (intern command))))

(When "^I wait for \\([[:digit:]\.]+\\) seconds$"
      (lambda (number)
        (sleep-for (read number))))

(Then "^I should see pattern \"\\(.+\\)\" in buffer \"\\(.+\\)\"$"
      (lambda (pattern buffer)
        (with-current-buffer buffer
          (Then (format "I should see pattern \"%s\"" pattern)))))

(Then "^I should not see pattern \"\\(.+\\)\" in buffer \"\\(.+\\)\"$"
      (lambda (pattern buffer)
        (with-current-buffer buffer
          (Then (format "I should not see pattern \"%s\"" pattern)))))
