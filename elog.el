;;; elog.el --- logging library for Emacs

;; Copyright (C) 2015  DarkSun

;; Author: DarkSun <lujun9972@gmail.com>
;; Keywords: lisp, tool
;; Package-Version: 20151102.1255
;; Version: 0.1
;; Package-Requires: ((eieio "1.3"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This module provides logging facility for Emacs

;;; Code:

(require 'cl)
(require 'eieio)

(defclass elog-object ()
  ((level :initarg :level :initform nil)
   ;; %I means identify,%T means timestamp,%L means level %M measn message
   (fmt :initarg :fmt :initform "[%I][%T][%L]:%M"))) 

(defmethod elog/insert-log ((log elog-object) format &rest objects)
  "Base implementation, do nothing")

(defmethod elog/should-log-p ((log elog-object) level)
  (let ((l (oref log :level)))
    (and (integerp l)
         (<= level l))))

(defmethod elog/log ((log elog-object) level ident string &rest objects)
  (when (elog/should-log-p log level)
    (let ((fmt (oref log :fmt)))
      (setq fmt (replace-regexp-in-string "%I" (format "%s" ident) fmt))
      (setq fmt (replace-regexp-in-string "%T" (current-time-string) fmt))
      (setq fmt (replace-regexp-in-string "%L" (format "%s" level) fmt))
      (setq fmt (replace-regexp-in-string "%M" string fmt))
      (apply 'elog/insert-log log fmt objects))))

;; (defmethod elog/log (log level ident string &rest objects)
;;   "Fallback implementation, do nothing. This allows in particular
;;   to pass nil as the log object."
;;   nil)

(defmacro elog/open-log (type ident &rest init-args)
  ""
  (declare (indent 'defun))
  (let ((log-obj (gensym))
        (log-type (intern (format "elog-%s-object" type)))
        (log-func (intern (format "elog/%s-log" ident)))
        (log-close-func (intern (format "elog/%s-close-log" ident))))
    `(progn
       (defconst ,log-obj (make-instance ',log-type ,@init-args))
       (defun ,log-func (level format-string &rest objects)
         (apply #'elog/log ,log-obj level ',ident format-string objects))
       (defun ,log-close-func ()
         (elog/close-log ,log-obj)))))

(defmethod elog/close-log ((log elog-object))
  "Base implementation, do nothing")

;; log for message
(defclass elog-message-object (elog-object)
  ())

(defmethod elog/insert-log ((log elog-message-object) format &rest objects)
  (apply 'message format objects))

;; log for buffer
(defclass elog-buffer-object (elog-object)
  ((buffer :initarg :buffer :initform nil)))

(defmethod elog/should-log-p ((log elog-buffer-object) level)
  (and (oref log :buffer)
       (call-next-method)))

(defmethod elog/insert-log ((log elog-buffer-object) format &rest objects)
  (let ((buffer (get-buffer-create (oref log :buffer))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (apply 'format format objects) "\n"))))

(defmethod elog/close-log ((log elog-buffer-object))
  (when (buffer-live-p (get-buffer (oref log :buffer)))
    (kill-buffer (oref log :buffer))))

;; log for file
(defclass elog-file-object (elog-object)
  ((file :initarg :file :initform nil)))

(defmethod elog/should-log-p ((log elog-file-object) level)
  (and (oref log :file)
       (call-next-method)))

(defmethod elog/insert-log ((log elog-file-object) format &rest objects)
  (let ((msg (concat  (apply #'format format objects) "\n"))
        (file (oref log :file)))
    (append-to-file msg nil file)))

;; define log level
(defconst elog-error 0)
(defconst elog-info 5)
(defconst elog-verbose 10)
(defconst elog-debug 15)


(provide 'elog)
;;; elog.el ends here
