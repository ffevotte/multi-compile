;;; multi-compile.el --- Define multiple compilation commands

;; Copyright (C) 2012-2015 François Févotte

;; Author: François Févotte <fevotte@gmail.com>
;; Created: 10 Mar 2014
;; Package-Version: 1.1
;; Package-Requires: ((hydra "0.13.0"))
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
;;    (multi-compile build)
;;    (multi-compile check :command "make check")
;;
;; would define new commands named `build' and `check'. The first time you call
;; each of these commands, you will be asked to provide the associated
;; compilation command by editing a default value:
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


(eval-and-compile
  (require 'hydra))

(defvar multi-compile-dwim-method nil
  "Method used for the menu of possible actions in the
  multi-compile DWIM command.

Available methods are:
- hydra
- helm
")

(defmacro multi-compile (name &rest args)
  "Define a new compilation command.

The following commands are defined (examples are given for the
case where NAME is \"build\"):

NAME (e.g M-x build)
    similar to `compile'

reNAME (e.g. M-x rebuild)
    similar to `recompile'

NAME-dwim (e.g. M-x build-dwim)
    - without prefix argument, run `recompile'
    - with a prefix argument, offer a choice between several
      compilation-related commands

Optional arguments ARGS are composed of a plist with the following keys:

:command CMD
    Define a default shell command associated to the newly
    created `compile' command.
"
  (let* (;; Optional arguments
         (key              (plist-get args :key))
         (default-command  (plist-get args :command))

         ;; Identification of the various buffers and commands
         (buffer-name      (format "*%s*" name))
         (compile-symbol   name)
         (recompile-symbol (intern (format "re%s" name)))
         (dwim-symbol      (intern (format "%s-dwim" name)))
         (menu-symbol      (intern (format "%s-menu" name)))

         ;; A LET-form which will enclose the compile and recompile bodies
         ;; allows to override standard compilation-related variables with
         ;; customized ones
         (let-form `((compilation-buffer-name-function
                      (lambda (mode) "" ,buffer-name))))

         (generated-code   nil)

         ;; Menu actions
         (menu-recompile   `(let ((current-prefix-arg nil))
                              (call-interactively #',recompile-symbol)))
         (menu-interactive `(let ((current-prefix-arg '(4)))
                              (call-interactively #',compile-symbol)))
         (menu-reset       `(let ((current-prefix-arg nil))
                              (call-interactively #',compile-symbol)))
         (menu-change-cmd  `(let ((current-prefix-arg '(4)))
                              (call-interactively #',recompile-symbol)))
         (menu-change-dir  `(with-current-buffer (get-buffer ,buffer-name)
                              (setq current-prefix-arg nil
                                    compilation-directory
                                    (read-directory-name "Compile in directory:"
                                                         compilation-directory))
                              (call-interactively #',recompile-symbol)))
         )
    (when (fboundp compile-symbol)
      (warn "redefining command `%s'" compile-symbol))
    (when (fboundp recompile-symbol)
      (warn "redefining command `%s'" recompile-symbol))
    (when (not (null default-command))
      (push `(compile-command ,default-command) let-form))
    (push `(defun ,compile-symbol ()
             ,(format
               "This command behaves similarly to `compile', except it puts compilation
    results in the %s buffer." buffer-name)
             (interactive)
             (let ,let-form
               (call-interactively #'compile)))
          generated-code)

    (push `(defun ,recompile-symbol ()
             ,(format
               "This command behaves similarly to `recompile', except it reuses the
    last compilation parameters from buffer %s." buffer-name)
             (interactive)
             (if (get-buffer ,buffer-name)
                 (with-current-buffer ,buffer-name
                   (let ,let-form
                     (call-interactively #'recompile)))
               (call-interactively #',compile-symbol)))
          generated-code)

    (push
     (when multi-compile-dwim-method
       (let ((dwim
              `(defun ,dwim-symbol (argp)
                 ,(format
                   "This command behaves similary to `%s', with alternatives.

    When called without argument, call `%s'.

    Otherwise, when ARGP is non-nil, offer a choice between multiple
    compilation-related commands:

    - `%s' using the last compilation parameters;

    - run an interactive `%s' compilation (uses Comint with
      `compilation-shell-minor-mode');

    - re-run the `%s' compilation command, resetting directory and
      command;

    - `%s', changing the compilation command;

    - `%s', changing the compilation directory."
                   recompile-symbol recompile-symbol recompile-symbol
                   compile-symbol   compile-symbol
                   recompile-symbol recompile-symbol)
                 (interactive "P")
                 (if (null argp)
                     (call-interactively #',recompile-symbol)
                   (call-interactively (get ',dwim-symbol :menu)))))

             (menu
              (case multi-compile-dwim-method
                ('hydra
                 `(defhydra ,menu-symbol (:exit t)
                    ,(concat "\n"
                             (format "%s:\n" name)
                             "  _r_ecompile           recompile and change:\n"
                             "  _i_nteractive           _c_ommand\n"
                             "  _R_eset everything      _d_irectory")
                    ("r" ,menu-recompile   nil)
                    ("i" ,menu-interactive nil)
                    ("R" ,menu-reset       nil)
                    ("c" ,menu-change-cmd  nil)
                    ("d" ,menu-change-dir  nil)))
                ('helm
                 `(defun ,menu-symbol ()
                    (interactive)
                    (case
                        (string-to-char (substring
                                         (or (helm :sources (helm-build-sync-source ,(format "%s" name)
                                                              :candidates '("(r) Recompile"
                                                                            "(i) Switch to interactive mode"
                                                                            "(R) Reset everything"
                                                                            "(c) Recompile and change command"
                                                                            "(d) Recompile and change directory")
                                                              :fuzzy-match t)
                                                   :buffer ,(format "*%s menu*" name))
                                             "(?)")
                                         1 2))
                      (?r ,menu-recompile)
                      (?i ,menu-interactive)
                      (?R ,menu-reset)
                      (?c ,menu-change-cmd)
                      (?d ,menu-change-dir)
                      (t  (message ,(format "%s: Unknown action" name))))))
                (t
                 (error (format
                         "Incorrect value for `multi-compile-dwim-method': %s"
                         multi-compile-dwim-method))))))
         `(prog1
              ,dwim
            (put ',dwim-symbol :menu
                 ,menu))
         ))
     generated-code)

    (push 'prog1 generated-code)))

(provide 'multi-compile)
;;; multi-compile.el ends here
