# multi-compile

`multi-compile` is an Emacs extension allowing to easily work with several
independant compilation commands. Instead of the traditional `compile` and
`recompile` commands, you can define additional pairs of (re-)compilation
commands to easily automate running multiple shell commands.

## Installation

First install the required dependency:
[`hydra`](http://github.com/abo-abo/hydra). It is available from various ELPA
repositories. Then:

1. get this repository:

   ```sh
   $ git clone https://github.com/ffevotte/multi-compile
   ```

2. instruct Emacs where to find the package by adding a snippet like this in
   your init file:

   ```lisp
   (add-to-list 'load-path "/path/to/multi-compile")
   (require 'multi-compile)
   ```

## Usage

### Synopsis

Putting the following snippet in your init file:

```lisp
(multi-compile check
  :command "make check"  ;; optional
  )
```

defines the following commands:

- <kbd>M-x</kbd>`check`: similar to `compile`, except that it has its own shell
  command and execution directory. Compilation results are put in a buffer named `*check*`.

- <kbd>M-x</kbd>`recheck`: similar to `recompile`, except that it reuses the
  last compilation parameters from the buffer `*check*`.

- <kbd>M-x</kbd>`check-dwim`: when called without argument, call `recheck`. With
  a prefix argument, offer a choice between several compilation-related
  commands. This command is returned by `multi-compile`, so that you can bind it
  directly if you like, as in the examples below.


The following optional parameters can be provided:

- `:command` defines a default value for the shell command associated to the new compilation command.


### Example

A C++ developer might setup the following commands to automate the development process:

```lisp
(global-set-key (kbd "<f5>") (multi-compile build))
(global-set-key (kbd "<f6>") (multi-compile check      :command "make check"))
(global-set-key (kbd "<f7>") (multi-compile install    :command "make install"))
(global-set-key (kbd "<f8>") (multi-compile build-doc  :command "make doc"))
```
