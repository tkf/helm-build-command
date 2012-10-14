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
(require 'eieio)


;;; Interface

(defclass hbc-command-base ()
  ((name :initarg :name :type string)
   (directory :initarg :directory :type string))
  :documentation "Base command task class.")

(defmethod initialize-instance ((task hbc-command-make) &optional slots)
  (call-next-method)
  (hbc-find-directory task))

(defgeneric hbc-find-directory (task directory)
  "Find a base directory to run.

\(fn task directory)")

(defgeneric hbc-candidates (task)
  "Return a list of candidates.

\(fn task)")

(defgeneric hbc-run (task choice)
  "Run command.

\(fn task choice)")

(defvar hbc--get-current-source #'helm-get-current-source)

(defun hbc-source--action (choice)
  (let* ((source (funcall hbc--get-current-source))
         (task (assoc-default 'hbc-task source)))
    (hbc-run choice)))

(defun hbc-source--candidates ()
  (let* ((source (funcall hbc--get-current-source))
         (task (assoc-default 'hbc-task source)))
    (hbc-candidates task)))


;;; Utilities

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


;;; Makefile

(defclass hbc-command-make (hbc-command-base)
  ((name :initarg :name :initform "make" :type string)
   (makefile :initarg :makefile :type string)
   (command-format :initarg :command-format :type string :initform "make %s")
   (exclude
    :initarg :exclude :type string :initform "Makefile"
    :documentation "Regexp to match against Makefile targets to be excluded."))
  :documentation "Command task to run Makefile")

(defmethod hbc-find-directory ((task hbc-command-make) &optional start)
  "Find a parent directory in which Makefile exists."
  (unless start (setq start default-directory))
  (let ((directory (helm-build-command--find-makefile
                    :start start :makefile (oref task :makefile))))
    (oset task :directory directory)))

(defmethod hbc-list-targets ((task hbc-command-make))
  "List make target."
  (loop for s in
        (split-string
         (let ((default-directory (oref task :directory)))
           (shell-command-to-string
            "make -pn | sed -n '/^[a-zA-Z][a-zA-Z.-]*:.*/p' | cut -d: -f1"))
         "\n" t)
        unless (string-match-p (oref task :exclude) s)
        collect s))

(defmethod hbc-candidates ((task hbc-command-make))
  (when (slot-boundp task :directory)
    (mapcar (lambda (x) (format (oref task :command-format) x))
            (hbc-list-targets task))))

(defmethod hbc-run ((task hbc-command-make) choice)
  "Run `compile' with CHOICE as command."
  (let ((default-directory (oref task :directory)))
    (compile choice)))

(defun helm-build-command-sources ()
  "Return a list of helm/anything sources."
  (mapcar
   (lambda (class)
     (let ((task (make-instance class)))
       `((name . ,(format "Build command (%s)" (oref task :name)))
         (hbc-task . ,task)
         (candidates . hbc-source--candidates)
         (action . hbc-source--action))))
   '(hbc-command-make)))

(defun* helm-build-command--internal (&optional (helm-or-anything "helm"))
  "Do what `helm-build-command' should do."
  (let (;; helm/anything compatibility
        (func (intern (format "%s-other-buffer" helm-or-anything)))
        (buf (format "*%s build*" helm-or-anything))
        (hbc--get-current-source
         (intern (format "%s-get-current-source" helm-or-anything))))
    (funcall func (helm-build-command-sources) buf)))

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
