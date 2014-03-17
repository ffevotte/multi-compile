# multi-compile

`multi-compile` is an Emacs extension allowing to easily work with several
independant compilation commands. Instead of the traditional `compile` and
`recompile` commands, you can define additional pairs of (re-)compilation
commands to easily automate running multiple shell commands.

## Installation

From `git`:

1. get this repository:

   ```sh
   $ git clone https://github.com/ffevotte/multi-compile
   ```

2. add this to your init file:

   ```lisp
   (add-to-list 'load-path "/path/to/multi-compile")
   (require 'multi-compile)
   ```

## Usage

### Synopsis

Putting the following snippet in your init file:

```lisp
(multi-compile "check"
  :key     (kbd "<f5>")  ;; optional
  :command "make check"  ;; optional
  )
```

defines the following commands:

- <kbd>M-x</kbd>`check`: similar to `compile`, except that it has its own shell command and execution directory.

- <kbd>M-x</kbd>`recheck`: similar to `recompile`, except that it shares the compilation command with `check`.


The following optional parameters can be provided:

- `:key` creates a new emacs command (<kbd>M-x</kbd>`check-or-recheck`), bound to the provided key (<kbd>F5</kbd> in the example). This command executes either one of the previous two commands depending on the prefix argument:

  <table>
  <tr>
    <th>Key</th>
    <th>Equivalent&nbsp;command</th>
    <th>Effect</th>
  </tr>
  <tr>
    <td><kbd>F5</kbd>&nbsp;</td>
    <td><kbd>M-x</kbd>&nbsp;<tt>recheck</tt></td>
    <td>Re-run last compilation command (define it the first time)</td>
  </tr>
  <tr>
    <td><kbd>C-u</kbd>&nbsp;<kbd>F5</kbd></td>
    <td><kbd>C-u</kbd>&nbsp;<kbd>M-x</kbd>&nbsp;<tt>recheck</tt></td>
    <td>Modify the compilation command (but not the directory)</td>
  </tr>
  <tr>
    <td><kbd>C-u</kbd>&nbsp;<kbd>C-u</kbd>&nbsp;<kbd>F5</kbd></td>
    <td><kbd>C-u</kbd>&nbsp;<kbd>M-x</kbd>&nbsp;<tt>check</tt></td>
    <td>Run the compilation command in an interactive buffer</td>
  </tr>
  </table>

- `:command` defines a default value for the shell command associated to the new compilation command.


### Example

A C++ developer might setup the following commands to automate the development process:

```lisp
(multi-compile "build"     :key (kbd "<f5>"))
(multi-compile "check"     :key (kbd "<f6>")  :command "make check")
(multi-compile "install"   :key (kbd "<f7>")  :command "make install")
(multi-compile "build-doc" :key (kbd "<f8>")  :command "make doc")
```
