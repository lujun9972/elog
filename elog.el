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

;; define log serverity
(defconst elog-emerg 0)
(defconst elog-alert 1)
(defconst elog-critical 2)
(defconst elog-error 3)
(defconst elog-warn 4)
(defconst elog-notice 5)
(defconst elog-info 6)
(defconst elog-debug 7)

(defclass elog-object ()
  ((level :initarg :level :initform 6)
   ;; %I means identify
   ;; %T means timestamp
   ;; %L means level
   ;; %P means pid
   ;; %M means message
   (fmt :initarg :fmt :initform "[%I][%T][%L]:%M")))

(defmethod elog/insert-log ((log elog-object) serverity format &rest objects)
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
      (setq fmt (replace-regexp-in-string "%P" (format "%s" (emacs-pid)) fmt))
      (setq fmt (replace-regexp-in-string "%M" string fmt))
      (apply 'elog/insert-log log level fmt objects))))

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

(defmethod elog/insert-log ((log elog-message-object) serverity format &rest objects)
  (apply 'message format objects))

;; log for buffer
(defclass elog-buffer-object (elog-object)
  ((buffer :initarg :buffer :initform nil)))

(defmethod elog/should-log-p ((log elog-buffer-object) level)
  (and (oref log :buffer)
       (call-next-method)))

(defmethod elog/insert-log ((log elog-buffer-object) serverity format &rest objects)
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

(defmethod elog/insert-log ((log elog-file-object) serverity format &rest objects)
  (let ((msg (concat  (apply #'format format objects) "\n"))
        (file (oref log :file)))
    (append-to-file msg nil file)))

;; log for syslogd
;; define syslog facility
(defconst elog-kern 0)
(defconst elog-user 1)
(defconst elog-mail 2)
(defconst elog-auth 4)
(defconst elog-daemon 3)
(defconst elog-lpr 6)
(defconst elog-news 7)
(defconst elog-uucp 8)
(defconst elog-cron 9)
(defconst elog-local0 16)
(defconst elog-local1 17)
(defconst elog-local2 18)
(defconst elog-local3 19)
(defconst elog-local4 20)
(defconst elog-local5 21)
(defconst elog-local6 22)
(defconst elog-local7 23)

(defclass elog-syslog-object (elog-object)
  ((conn :initarg :conn)
   (facility :initarg :facility)
   (fmt :initarg :fmt :initform "%M")))

(defmethod elog/should-log-p ((log elog-syslog-object) level)
  (let ((conn (oref log :conn)))
    (and (processp conn)
         (eq 'open (process-status conn))
         (call-next-method))))

(defmethod elog/insert-log ((log elog-syslog-object) serverity format &rest objects)
  (let* ((conn (oref log :conn))
         (facility (oref log :facility))
         (pri (format "<%d>" (+ (* 8 facility) serverity)))
         (timestamp (substring  (current-time-string) 4 19))
         (host (format-network-address (process-contact conn :local) t))
         (header (format "%s %s" timestamp host))
         (tag "")
         (content (concat  (apply #'format format objects) "\n"))
         (msg (format "%s:%s" tag content))
         (package (format  "%s%s %s" pri header msg)))
    (process-send-string conn package)))

(defmethod elog/close-log ((log elog-syslog-object))
  (when (processp (oref log :conn))
    (delete-process (oref log :conn))))

(provide 'elog)

;;; elog.el ends here
