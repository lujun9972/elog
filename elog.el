;;; elog.el --- logging library

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
(defconst elog-emerg 0
  "Emergency: system is unusable")
(defconst elog-alert 1
  "Alert: action must be taken immediately")
(defconst elog-critical 2
  "Critical: critical conditions")
(defconst elog-error 3
  "Error: error conditions")
(defconst elog-warn 4
  "Warning: warning conditions")
(defconst elog-notice 5
  "Notice: normal but significant condition")
(defconst elog-info 6
  "Informational: informational messages")
(defconst elog-debug 7
  "Debug: debug-level messages")

(defclass elog-object ()
  ((serverity :initarg :serverity
              :documentation "specify the logging level"
              :type number
              :custom number
              :initform 6)
   ;; %I means identify
   ;; %T means timestamp
   ;; %L means serverity
   ;; %P means pid
   ;; %M means message
   (fmt :initarg :fmt
        :documentation "specify the logging format"
        :type string
        :custom string
        :initform "[%I][%T][%L]:%M"))
  "An interface to special elog-object"
  :abstract t)

(defmethod elog-insert-log ((log elog-object) serverity format &rest objects)
  "do the actual logging job.")

(defmethod elog-should-log-p ((log elog-object) serverity)
  " check if the log item should be recorded."
  (let ((l (oref log :serverity)))
    (and (integerp l)
         (<= serverity l))))

(defmethod elog-close-log ((log elog-object))
  "do the cleanning job after log job is done")

(defmethod elog-log ((log elog-object) serverity ident string &rest objects)
  "do the log job if applicable"
  (when (elog-should-log-p log serverity)
    (let ((fmt (oref log :fmt)))
      (setq fmt (replace-regexp-in-string "%I" (format "%s" ident) fmt t))
      (setq fmt (replace-regexp-in-string "%T" (current-time-string) fmt t))
      (setq fmt (replace-regexp-in-string "%L" (format "%s" serverity) fmt t))
      (setq fmt (replace-regexp-in-string "%P" (format "%s" (emacs-pid)) fmt t))
      (setq fmt (replace-regexp-in-string "%M" string fmt t))
      (apply 'elog-insert-log log serverity fmt objects))))

;; (defmethod elog-log (log serverity ident string &rest objects)
;;   "Fallback implementation, do nothing. This allows in particular
;;   to pass nil as the log object."
;;   nil)

(defmacro elog-open-log (type ident &rest init-args)
  "Create the logging functions.
`TYPE' specify which kind of elog-object is used. Now, elog support four types of elog-object: `message',`buffer',`file' and `syslog'.
It will create two functions: `IDENT-log' used to do the log stuff and `IDENT-close-log' used to do the cleanning job
`INIT-ARGS' is used to construct the elog-object"
  (declare (indent 'defun))
  (let ((log-obj (gensym))
        (log-type (intern (format "elog-%s-object" type)))
        (log-func (intern (format "%s-log" ident)))
        (log-close-func (intern (format "%s-close-log" ident))))
    `(progn
       (defconst ,log-obj (make-instance ',log-type ,@init-args))
       (defun ,log-func (serverity format-string &rest objects)
         "use this function to log stuff"
         (apply #'elog-log ,log-obj serverity ',ident format-string objects))
       (defun ,log-close-func ()
         "use this function to do cleanning job after the log job is done"
         (elog-close-log ,log-obj)))))

;; log for message
(defclass elog-message-object (elog-object)
  ())

(defmethod elog-insert-log ((log elog-message-object) serverity format &rest objects)
  (apply 'message format objects))

;; log for buffer
(defclass elog-buffer-object (elog-object)
  ((buffer :initarg :buffer
           :documentation "specify which buffer is used to record the logging item."
           :type (or null string)
           :custom string
           :initform nil)))

(defmethod elog-should-log-p ((log elog-buffer-object) serverity)
  (and (oref log :buffer)
       (call-next-method)))

(defmethod elog-insert-log ((log elog-buffer-object) serverity format &rest objects)
  (let ((buffer (get-buffer-create (oref log :buffer))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (apply 'format format objects) "\n"))))

(defmethod elog-close-log ((log elog-buffer-object))
  (when (buffer-live-p (get-buffer (oref log :buffer)))
    (kill-buffer (oref log :buffer))))

;; log for file
(defclass elog-file-object (elog-object)
  ((file :initarg :file
         :documentation "specify which file is used to record the logging item"
         :type (or null string)
         :custom string
         :initform nil)
   (max-size :initarg :max-size
         :documentation "specify max size(bytes) of single log file."
         :type (or null number)
         :custom (or null number)
         :initform nil)
   (old-dir :initarg :old-dir
         :documentation "specify which directory the old log file will be located."
         :type (or null string)
         :custom (or null string)
         :initform nil)))

(defmethod elog-should-log-p ((log elog-file-object) serverity)
  (and (oref log :file)
       (call-next-method)))

(defmethod elog-insert-log ((log elog-file-object) serverity format &rest objects)
  (let* ((msg (concat  (apply #'format format objects) "\n"))
        (file (oref log :file))
        (max-size (oref log :max-size))
        (file-size (nth 7 (file-attributes file))))
    (when (and max-size
               (> file-size max-size))
      (let* ((old-dir (or (oref log :old-dir)
                          (file-name-directory file)))
             (old-file (expand-file-name  (format "%s-%s.%s" (file-name-base file) (format-time-string "%FT%T") (file-name-extension file)) old-dir)))
        (rename-file file old-file)))
    (append-to-file msg nil file)))

;; log for syslogd
;; define syslog facility
(defconst elog-kern 0
  "kernel messages")
(defconst elog-user 1
  "user-level messages")
(defconst elog-mail 2
  "mail system")
(defconst elog-daemon 3
  "system daemons")
(defconst elog-auth 4
  "security/authorization messages")
(defconst elog-lpr 6
  "line printer subsystem")
(defconst elog-news 7
  "network news subsystem")
(defconst elog-uucp 8
  "UUCP subsystem")
(defconst elog-cron 9
  "clock daemon")
(defconst elog-local0 16
  "local use 0")
(defconst elog-local1 17
  "local use 1")
(defconst elog-local2 18
  "local use 2")
(defconst elog-local3 19
  "local use 3")
(defconst elog-local4 20
  "local use 4")
(defconst elog-local5 21
  "local use 5")
(defconst elog-local6 22
  "local use 6")
(defconst elog-local7 23
  "local use 7")

(defclass elog-syslog-object (elog-object)
  ((facility :initarg :facility
             :documentation "specify the facility"
             :type number
             :custom number)
   (conn :initarg :conn
         :documentation "the network process that send logging item to the syslogd server")
   (fmt :initarg :fmt :initform "%M")))

(defun elog--plist-remove (plist prop)
  (when plist
    (let ((key (car plist))
          (value (cadr plist))
          (rest (cddr plist)))
      (cond ((equal prop key)
             rest)
            (t (append (list key value) (elog--plist-remove rest prop)))))))

(defmethod initialize-instance ((log elog-syslog-object) &optional args)
  (let* ((host (plist-get args :host))
         (port (plist-get args :port))
         (conn (make-network-process :name (format "%s-%d" host port)
                                     :type 'datagram
                                     :host host
                                     :service port))
         (rest-args (elog--plist-remove (elog--plist-remove args :host) :port))
         (slots (append (list :conn conn) rest-args)))
    (funcall #'call-next-method log slots)))

(defmethod elog-should-log-p ((log elog-syslog-object) serverity)
  (let ((conn (oref log :conn)))
    (and (processp conn)
         (eq 'open (process-status conn))
         (call-next-method))))

(defmethod elog-insert-log ((log elog-syslog-object) serverity format &rest objects)
  (let* ((conn (oref log :conn))
         ;; create pri
         (facility (oref log :facility))
         (pri (format "<%d>" (+ (* 8 facility) serverity)))
         ;; create header
         (timestamp (substring  (current-time-string) 4 19))
         (host (format-network-address (process-contact conn :local) t))
         (header (format "%s %s" timestamp host))
         ;; create msg
         (tag "")
         (content (concat  (apply #'format format objects) "\n"))
         (msg (format "%s:%s" tag content))
         ;; combine to a whole package
         (package (format  "%s%s %s" pri header msg)))
    (process-send-string conn package)))

(defmethod elog-close-log ((log elog-syslog-object))
  (when (processp (oref log :conn))
    (delete-process (oref log :conn))))

(provide 'elog)

;;; elog.el ends here
