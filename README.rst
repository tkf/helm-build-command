Helm/anything interface for build commands
==========================================

helm-build-command.el helps you to run "build commands" such as
``make`` from everywhere in the project directory tree.  Despite the
prefix helm- in it, anything.el user can also use
helm-build-command.el.

It supports only ``make``, ``waf``, ``python setup.py``, ``tox`` now.
To extend it, see the instruction below.


Install
-------

Manual install
^^^^^^^^^^^^^^

1. Put helm-build-command.el under some directory in your `load-path`.
2. Put ``(require helm-build-command.el)`` in your Emacs setup.

Usage
-----

anything.el user:
    `M-x anything-build-command`

helm.el user:
    `M-x helm-build-command`


How to extend
-------------

helm-build-command.el uses EIEIO to define command task.

Here is a simplified version of setup.py command definition::

   (defclass hbc-command-setup-py (hbc-command-make)
     ((name :initarg :name :initform "setup.py")
      (makefile :initarg :makefile :initform "setup.py")
      (command-format :initarg :command-format :initform "python setup.py %s")))

   (defmethod hbc-list-targets ((task hbc-command-setup-py))
     (list "build" "build_ext" "build_ext --inplace" "clean"))


Required slots:

  `name` : string
    A short name for this task.

  `makefile` : string
    Here, `makefile` slot specifies a file which determines a directory
    to run the build command.  When `helm-build-command` is run,
    `makefile` is searched in current directory and parent directories.

  `command-format` : string
    A format string with one argument.

Required method:

  `hbc-list-targets`
    This function must return a list of string.  Each string will be
    passed to the format string `command-format`.


To use this command class, you need to add it to `hbc-task-classes`::

   (setq hbc-task-classes '(hbc-command-make hbc-command-setup-py))
