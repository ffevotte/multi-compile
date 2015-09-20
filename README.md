# multi-compile
[![Build-status](https://travis-ci.org/ffevotte/multi-compile.svg)](https://travis-ci.org/ffevotte/multi-compile) [![Coverage-status](https://coveralls.io/repos/ffevotte/multi-compile/badge.svg?branch=master&service=github)](https://coveralls.io/github/ffevotte/multi-compile?branch=master) [![Tag](https://img.shields.io/github/tag/ffevotte/multi-compile.svg)](https://github.com/ffevotte/multi-compile/tags) [![License](https://img.shields.io/badge/license-GPL_v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)


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

## Contributing

If you make improvements to this code or have suggestions, please do not hesitate to fork the repository or submit bug reports on [github](https://github.com/ffevotte/multi-compile). The repository's URL is:

    https://github.com/ffevotte/multi-compile.git


## License

Copyright (C) 2014-2015 François Févotte.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
