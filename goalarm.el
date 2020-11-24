;;; goalarm.el --- manage the goalarm process

;; Copyright (C) 2020 komem3

;; Author: komem3 <komata392@gmail.com>
;; Keywords: languages, go
;; URL: https://github.com/komem3/go-alarm
;; Version: 1.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'subr-x)
(require 'json)
(require 'cl-lib)

(defvar goalarm-process nil
  "Process of Alarm.")

(defvar goalarm-process-response nil
  "Status of goalarm response.")

(defcustom goalarm-read-routine-function 'goalarm-default-read-routine
  "Function of reading routine from minibuffer."
  :type 'function
  :group 'goalarm)

(defcustom goalarm-sound-file nil
  "Sound file of alarm.  File is allowed mp3."
  :type 'string
  :group 'goalarm)

(defcustom goalarm-routine-list '(("pomodro" . (:value
                                                (((name . "working1") (range . 25))
                                                 ((name . "break") (range . 5))
                                                 ((name . "working2") (range . 25))
                                                 ((name . "break") (range . 5))
                                                 ((name . "working3") (range . 25))
                                                 ((name . "break") (range . 15)))
                                                :loop t)))
  "List of routine."
  :type 'alist
  :group 'goalarm)

(defcustom goalarm-start-hook nil
  "List of functions to be called before goalarm has been started."
  :type 'hook
  :group 'goalarm)

(defcustom goalarm-exit-hook nil
  "List of functions to be called before goalarm has been exit."
  :type 'hook
  :group 'goalarm)

(defcustom goalarm-stop-hook nil
  "List of functions to be called before goalarm has been stop."
  :type 'hook
  :group 'goalarm)

(defcustom goalarm-finish-hook nil
  "List of functions to be called before goalarm has been finish."
  :type 'hook
  :group 'goalarm)

(defun goalarm-default-read-routine (routine-list)
  "Reading routine from minibuffer.  ROUTINE-LIST is list of routine."
  (let ((keys (mapcar 'car routine-list)))
    (read-string "Which routine does start? " (car keys))))

(defun goalarm-update-status (process output)
  "Update goalarm status.
PROCESS and OUTPUT is jnjected from 'goalarm-process'."
  (setq goalarm-process-response (json-read-from-string output))
  (let ((status (goalarm-process-status)))
    (cond ((string= status "stop") (progn (run-hooks 'goalarm-stop-hook) (message "goalarm stopped.")))
          ((string= status "finish") (progn (run-hooks 'goalarm-finish-hook) (message "goalarm finished.")))
          ((string= status "error") (message (cdr (assoc 'error
                                                         goalarm-process-response)))))))

(defun goalarm-check-process-running()
  "Check status of process."
  (and goalarm-process (process-live-p goalarm-process)))

(defun goalarm-send-get-signal()
  "Send 'get' signal to goalarm."
  (if (goalarm-check-process-running)
      (process-send-string goalarm-process "get\n")))

(defun goalarm-sentinel (process event)
  "Event handler.  PROCESS and EVENT is inject from 'goalarm-process'."
  (if (not (goalarm-check-process-running))
      (progn
        (run-hooks 'goalarm-exit-hook)
        (setq goalarm-process nil)
        (setq goalarm-process-response nil))))

(defun goalarm-convert-args-min-or-time (args)
  "Determine which time format.  ARGS are 'min' or 'hh:min:sec' or 'minl'."
  (cond ((string-match "^\\([[:digit:]]+\\)\\W*$" args)
         (list "-min" (match-string 1 args)))
        ((string-match "^\\([[:digit:]]+:[[:digit:]]+:[[:digit:]]+\\)\\W*$" args)
         (list "-time" (match-string 1 args)))
        ((string-match "^\\([[:digit:]]+\\)l\\W*$" args)
         (list "-loop" "-min" (match-string 1 args)))
        ((string-match "^\\([[:digit:]]+:[[:digit:]]+:[[:digit:]]+\\)l\\W*$" args)
         (list "-loop" "-time" (match-string 1 args)))))

(defun goalarm-convert-routine-args (routine)
  "Convert routine to json args.  ROUTINE are one of the 'goalarm-routine-list'."
  (let ((value (json-encode-list (plist-get routine :value)))
        (loop (plist-get routine :loop)))
    (if loop
        (list "-routine" value "-loop")
      (list "-routine" value))))

(defun goalarm-start-process (args start-message)
  "Start goalarm process.  ARGS are goalarm args.  START-MESSAGE is message of starting message."
  (cond
   ((not goalarm-sound-file) (message "yet not setting goalarm-sound-file."))
   ((goalarm-check-process-running) (message "already goalarm running."))
   (t (progn
        (run-hooks 'goalarm-start-hook)
        (setq goalarm-process
              (make-process :name "goalarm"
                            :buffer "*goalarm*"
                            :stderr "*goalarm::stderr*"
                            :command (flatten-tree
                                      `("goalarm"
                                        "-file"
                                        ,(shell-quote-argument goalarm-sound-file)
                                        ,args))
                            :connection-type 'pipe
                            :sentinel 'goalarm-sentinel
                            :filter 'goalarm-update-status
                            :noquery t))
        (setq goalarm-process-response nil)
        (goalarm-send-get-signal)       ; first signal
        (message start-message)))))

;;
;; use modeline functions
;;

(defun goalarm-get-time()
  "Get time."
  (goalarm-send-get-signal)
  (let ((status (goalarm-process-status)))
    (if (or (string= status "running") (string= status "pause"))
        (cdr (assoc 'left
                    goalarm-process-response)))))

(defun goalarm-process-status()
  "Get latest process status."
  (if (not goalarm-process-response)
      ""
    (cdr (assoc 'status goalarm-process-response))))

(defun goalarm-get-alarm-name()
  "Get running alarm name."
  (let ((status (goalarm-process-status)))
    (if (or (string= status "running") (string= status "pause"))
        (cdr (assoc 'name (cdr (assoc 'task
                    goalarm-process-response)))))))

;;;###autoload
(defun goalarm-start (args)
  "Start goalarm.  ARGS are time of alarm.
When you want to loop, write 'l' at the args end."
  (interactive "sargs of goalarm command. (min or hh:min:sec or minl): ")
  (if (string-empty-p args) (message "args is empty.")
    (let ((time-args (goalarm-convert-args-min-or-time args)))
      (if time-args
          (goalarm-start-process time-args "start goalarm.")
        (message "args are invalid format.  (min or hh:min:sec or minl)")))))

;;;###autoload
(defun goalarm-routine ()
    "Start goalarm routine.
Read a routine of 'goalarm-routine-list' from minibuffer and execute it.
Read function is 'goalarm-read-routine-function'"
    (interactive)
    (let* ((key (funcall goalarm-read-routine-function goalarm-routine-list))
           (routine (assoc key goalarm-routine-list))
           (routine-args (goalarm-convert-routine-args (cdr routine))))
      (if routine-args
          (goalarm-start-process routine-args (format "start %s." (car routine)))
        (message "no such routine."))))

;;;###autoload
(defun goalarm-stop ()
  "Stop goalarm."
  (interactive)
  (if (goalarm-check-process-running)
      (process-send-string goalarm-process "stop\n")
    (message "already stop.")))

;;;###autoload
(defun goalarm-resume ()
  "Resume goalarm."
  (interactive)
  (if (goalarm-check-process-running)
      (process-send-string goalarm-process "start\n")
    (message "not running goalarm.")))

;;;###autoload
(defun goalarm-pause ()
  "Pause goalarm."
  (interactive)
  (if (goalarm-check-process-running)
      (progn
        (process-send-string goalarm-process "pause\n")
        (message "pause goalarm."))
    (message "not running goalarm.")))

;;;###autoload
(defun goalarm-restart ()
  "Restart goalarm."
  (interactive)
  (if (goalarm-check-process-running)
      (progn
        (process-send-string goalarm-process "restart\n")
        (message "restart goalarm."))
    (message "not running goalarm.")))

(provide 'goalarm)
;;; goalarm.el ends here
