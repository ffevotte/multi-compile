(require 'f)

(defvar multi-compile-support-path
  (f-dirname load-file-name))

(defvar multi-compile-features-path
  (f-parent multi-compile-support-path))

(defvar multi-compile-root-path
  (f-parent multi-compile-features-path))

(add-to-list 'load-path multi-compile-root-path)

(require 'undercover)
(undercover "*.el")

(require 'multi-compile)
(require 'espuds)
(require 'ert)

(Setup
 (setq compile-command "echo build")
 (global-set-key (kbd "<f5>") (multi-compile build))
 (global-set-key (kbd "<f6>") (multi-compile check :command "echo check")))

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
