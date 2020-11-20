;; goalarm.el --- manage the goalarm process

;; Author: komem3 <komata392@gmail.com>
;; URL: https://github.com/komem3/go-alarm
;; Keywords: languages, go

;; Copyright (C) 2020 komem3

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

(defcustom goalarm-sound-file nil
  "Sound file of alarm.  File is allowed mp3."
  :type 'string
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

(defun goalarm-process-status()
  "Get latest process status."
  (if (not goalarm-process-response)
      ""
    (cdr (assoc 'status goalarm-process-response))))

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

(defun goalarm-get-time()
  "Get time."
  (goalarm-send-get-signal)
  (let ((status (goalarm-process-status)))
    (if (or (string= status "running") (string= status "pause"))
        (cdr (assoc 'left
                    goalarm-process-response)))))

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

;;;###autoload
(defun goalarm-start (args)
  "Start goalarm.  ARGS are time of alarm.
When you want to loop, write 'l' at the args end."
  (interactive "sargs of goalarm command. (min or hh:min:sec or minl): ")
  (cond
   ((not goalarm-sound-file) ((message "yet not setting goalarm-sound-file.")))
   ((goalarm-check-process-running) (message "already goalarm running"))
   ((string-empty-p args) (message "args is empty"))
   (t (progn
        (let ((time-args (goalarm-convert-args-min-or-time args)))
          (if (not time-args)
              (message "args are invalid format.  (min or hh:min:sec or minl)")
            (run-hooks 'goalarm-start-hook)
            (setq goalarm-process
                  (make-process :name "goalarm"
                                :buffer "*goalarm*"
                                :stderr "*goalarm::stderr*"
                                :command (flatten-tree
                                          `("goalarm"
                                           "-file"
                                           ,(shell-quote-argument goalarm-sound-file)
                                           ,time-args))
                                :connection-type 'pipe
                                :sentinel 'goalarm-sentinel
                                :filter 'goalarm-update-status
                                :noquery t))
            (setq goalarm-process-response nil)
            (goalarm-send-get-signal)       ; first signal
            (message "start goalarm.")))))))

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
