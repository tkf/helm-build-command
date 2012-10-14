;;; helm-build-command.el --- Helm/anything interface for build commands

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; helm-build-command.el is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; helm-build-command.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with helm-build-command.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))


(defvar helm-build-command-make-exclude
  "Makefile"
  "Regexp to match against Makefile targets to be excluded.")

(defun helm-build-command-list-make-targets ()
  "List make target. A Makefile must exist under `default-directory'."
  (loop for s in
        (split-string
         (shell-command-to-string
          "make -pn | sed -n '/^[a-zA-Z][a-zA-Z.-]*:.*/p' | cut -d: -f1")
         "\n" t)
        unless (string-match-p helm-build-command-make-exclude s)
        collect s))

(defun* helm-build-command--find-makefile
    (&key (start default-directory) (makefile "Makefile"))
  "Find a parent directory in which MAKEFILE exists."
  (loop with dir = (file-name-as-directory (expand-file-name start))
        with prev = ""
        while (not (string= dir prev))
        do (setq prev dir
                 dir (file-name-directory (directory-file-name dir)))
        when (file-exists-p (expand-file-name makefile dir))
        return dir))

(defvar helm-build-command-make--directory nil
  "Bound to result of `helm-build-command--find-makefile' while
running helm/anything.")

(defun helm-build-command-make--candidates ()
  "List make targets. `helm-build-command-make--directory' must be bound."
  (let ((default-directory helm-build-command-make--directory))
    (when default-directory
      (mapcar (lambda (x) (format "make %s" x))
              (helm-build-command-list-make-targets)))))

(defun helm-build-command-make--compile (command)
  "Run `compile' with COMMAND. `helm-build-command-make--directory'
must be bound."
  (let ((default-directory helm-build-command-make--directory))
    (compile command)))

(defvar helm-build-command-make-source
  '((name . "Build command (make)")
    (candidates . helm-build-command-make--candidates)
    (action . helm-build-command-make--compile)))

(defun* helm-build-command--internal (&optional (helm-or-anything "helm"))
  "Do what `helm-build-command' should do."
  (let (;; helm/anything compatibility
        (func (intern (format "%s-other-buffer" helm-or-anything)))
        (buf (format "*%s build*" helm-or-anything))
        ;; actual setup
        (helm-build-command-make--directory
         (helm-build-command--find-makefile)))
    (funcall func helm-build-command-make-source buf)))

;;;###autoload
(defun anything-build-command ()
  "Run make command."
  (interactive)
  (helm-build-command--internal "anything"))

;;;###autoload
(defun helm-build-command ()
  "Run make command."
  (interactive)
  (helm-build-command--internal))


(provide 'helm-build-command)

;;; helm-build-command.el ends here
