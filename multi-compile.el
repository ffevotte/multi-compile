;;; multi-compile.el --- Define multiple compilation commands

;; Copyright (C) 2012 François Févotte

;; Author: François Févotte <fevotte@gmail.com>
;; Created: 10 Mar 2014
;; Version: 1.0
;; Keywords: compilation
;; X-URL: https://github.com/ffevotte/multi-compile

;; This file is NOT part of Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; The `multi-compile' macro allows to define new, separated compilation
;; commands, similar to `compile' and `recompile', but using different shell
;; commands.
;;
;; For example, the following snippet:
;;    (multi-compile "build" :key (kbd "<f5>"))
;;    (multi-compile "check" :key (kbd "<f6>")  :command "make check")
;;
;; would define new commands named `build' and `check', respectively bound to
;; <f5> and <f6>. The first time you call each of these commands, you will be
;; asked to provide the associated compilation command by editing a default
;; value:
;;
;;    build:  "make -k"    (inherited from `compile-command')
;;    check:  "make check" (setup in the `multi-compile' declaration)
;;
;; Each subsequent call to these commands re-runs the associated shell command in
;; the dedicated compilation buffer.

;; If you make improvements to this code or have suggestions, please do not
;; hesitate to fork the repository or submit bug reports on github. The
;; repository is at:
;;
;;     https://github.com/ffevotte/multi-compile


;;; Code:

(defmacro multi-compile (name &rest args)
  "Define a new compilation command.

The following commands are defined (examples are given for the
case where NAME is \"build\"):

NAME (e.g M-x build)
    similar to `compile'

reNAME (e.g. M-x rebuild)
    similar to `recompile'

Optional arguments:

:key Bind the given key to a new command which executes either
     one of the preceding commands, depending on the prefix
     argument. See the docstring for the key for more details.

:command Define a default shell command associated to the newly
     created `compile' command."
  (let* ((key              (plist-get args :key))
         (default-command  (plist-get args :command))
         (buffer-name      (concat "*" name "*"))
         (compile-symbol   (intern name))
         (recompile-symbol (intern (concat "re" name)))
         (compile-or-recompile-symbol
          (intern (format "%s-or-re%s" name name)))
         (let-form `((compilation-buffer-name-function
                      (lambda (mode) "" ,buffer-name)))))
    (when (fboundp compile-symbol)
      (warn "redefining command `%s'" name))
    (when (fboundp recompile-symbol)
      (warn "redefining command `re%s'" name))
    (when (not (null default-command))
      (setq let-form (cons `(compile-command ,default-command) let-form)))
    `(progn
       (defun ,compile-symbol ()
         ,(format
           "This function behaves similarly to `compile', except it puts compilation
results in the %s buffer." buffer-name)
         (interactive)
         (let ,let-form
           (call-interactively 'compile)))

       (defun ,recompile-symbol ()
         ,(format
           "This function behaves similarly to `recompile', except it reuses the
last compilation parameters from buffer %s." buffer-name)
         (interactive)
         (if (get-buffer ,buffer-name)
             (with-current-buffer ,buffer-name
               (let ,let-form
                 (call-interactively 'recompile)))
           (call-interactively ',compile-symbol)))

       ,(when key
          `(global-set-key ,key
                           (defun ,compile-or-recompile-symbol (arg)
                             ,(format "Run or re-run the compilation in buffer %s.
The exact effect depends on the prefix argument:

Key            Command              Effect
-------------- -------------------- -------------------------------
%-6s         M-x %-12s     re-run last compilation command
                                        (define it the first time)
C-u %-6s     C-u M-x %-12s modify compilation command
C-u C-u %-6s C-u M-x %-12s run compilation command in an
                                        interactive buffer"
                                      buffer-name
                                      (key-description (eval key)) (symbol-name recompile-symbol)
                                      (key-description (eval key)) (symbol-name recompile-symbol)
                                      (key-description (eval key)) (symbol-name compile-symbol))
                             (interactive "P")
                             (cond ((equal arg '(16))
                                    (let ((current-prefix-arg '(4)))
                                      (call-interactively ',compile-symbol)))
                                   (t
                                    (call-interactively ',recompile-symbol)))))))))

(provide 'multi-compile)
;;; multi-compile.el ends here
