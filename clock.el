;;; clock.el -- an Emacs alarm clock

;; Copyright (C) 2011 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: home automation

;; This file is not part of GNU Emacs.

;; clock.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; clock.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide a convenient keypad based interface for displaying time,
;; and waking myself up in the morning.

;;; Code:

(require 'cl)

(defvar clock-temperatures nil)
(defvar clock-temperature-poll 0)

(defun display-clock ()
  (save-excursion
    (clock-update-temperatures)
    (set-buffer (set-buffer (get-buffer-create "*clock*")))
    (goto-char (point-min))
    (delete-region (point) (line-end-position))
    (insert (format-time-string "%H:%M"))
    (put-text-property (point) (line-beginning-position)
		       'face 'clock-clock-face)

    (when clock-temperatures
      (goto-char (point-min))
      (forward-line 3)
      (delete-region (point) (line-end-position))
      (insert (format "%.1f°" (cadr clock-temperatures)))
      (put-text-property (point) (line-beginning-position)
			 'face 'clock-temperature-face))))

(defun clock-update-temperatures ()
  (when (zerop (mod clock-temperature-poll 30))
    (setq clock-temperatures (clock-get-temperatures)))
  (incf clock-temperature-poll))

(defvar clock-timer nil)

(defun start-clock ()
  (when clock-timer
    (cancel-timer clock-timer))
  (setq clock-timer
	(run-at-time 1 1 #'display-clock)))

(defun clock-get-temperatures ()
  (with-temp-buffer
    (call-process "~/bin/get-temperatures" nil (current-buffer) nil)
    (mapcar #'string-to-number (split-string (buffer-string)))))

(defface clock-alarm-face
  '((((class color))
     (:foreground "#202020" :italic nil)))
  "Alarm")

(defface clock-temperature-face
  '((((class color))
     (:foreground "#202020" :italic nil)))
  "Alarm")

(defface clock-clock-face
  '((((class color))
     (:foreground "#404040" :italic nil)))
  "Alarm")

(defvar clock-mode-map nil)
(unless clock-mode-map
  (setq clock-mode-map (make-sparse-keymap))
  (suppress-keymap clock-mode-map)
  (define-key clock-mode-map "*" 'clock-set-alarm)
  (define-key clock-mode-map "\r" 'clock-cancel-alarm)
  (define-key clock-mode-map "2" 'clock-decrease-volume)
  (define-key clock-mode-map "8" 'clock-increase-volume)
  (define-key clock-mode-map "0" 'clock-pause)
  (define-key clock-mode-map [(XF86Calculator)] 'clock-lights-on)
  (define-key clock-mode-map [(meta tab)] 'clock-lights-off))

(defun clock-lights-on ()
  (interactive)
  (call-process "lights" nil (get-buffer-create " *lights*") nil "1"))

(defun clock-lights-off ()
  (interactive)
  (call-process "lights" nil (get-buffer-create " *lights*") nil "0"))

(defun clock-mode (&optional arg)
  (interactive)
  (setq major-mode 'clock-mode)
  (setq mode-name "Clock")
  (use-local-map clock-mode-map)
  (setq mode-line-buffer-identification
	'("Clock"))
  (setq truncate-lines t)
  (buffer-disable-undo)
  (set-face-background 'fringe "black")
  (setq default-frame-alist
	(nconc (list '(mouse-color . "black")
		     '(cursor-type . box)
		     '(cursor-color . "black"))
	       default-frame-alist))
  (blink-cursor-mode -1))

(defun setup-clock ()
  (fringe-mode 0)
  (let ((system (car (split-string (system-name) "[.]"))))
    (when (member system '("clock"))
      (setq server-use-tcp t
	    server-host (system-name)
	    server-name system)
      (server-start)))
  (switch-to-buffer (set-buffer (get-buffer-create "*clock*")))
  (erase-buffer)
  (insert "\n\n\n\n\n")
  (clock-mode)
  (start-clock))

(defun clock-pause ()
  (interactive)
  (clock-emacsclient "(jukebox-pause)"))

(defvar clock-volume 2)

(defun clock-decrease-volume ()
  (interactive)
  (setq clock-volume (max (- clock-volume 0.1) 0))
  (clock-emacsclient (format "(jukebox-set-vol-volume %s \"bedroom\")"
			     clock-volume)))

(defun clock-increase-volume ()
  (interactive)
  (setq clock-volume (min (+ clock-volume 0.1) 9))
  (clock-emacsclient (format "(jukebox-set-vol-volume %s \"bedroom\")"
			     clock-volume)))

(defvar clock-alarm nil)

(defun clock-set-alarm (time)
  (interactive "sTime: ")
  (setq time
	(cond
	 ((= (length time) 1)
	  (format "0%s:00" time))
	 ((= (length time) 2)
	  (format "%s:00" time))
	 ((= (length time) 3)
	  (format "0%s:%s" (substring time 0 1) (substring time 1)))
	 ((= (length time) 4)
	  (format "%s:%s" (substring time 0 2) (substring time 2)))))
  (clock-cancel-alarm)
  (setq clock-alarm 
	(run-at-time (clock-number-of-seconds-until time)
		     nil #'clock-sound-alarm))
  (save-excursion
    (set-buffer (get-buffer-create "*clock*"))
    (goto-line (point-min))
    (forward-line 1)
    (delete-region (point) (line-end-position))
    (insert time)
    (put-text-property (point) (line-beginning-position)
		       'face 'clock-alarm-face)))

(defun clock-number-of-seconds-until (clock)
  (let ((seconds 0)
	(now (time-to-seconds (current-time))))
    (while (not (string= clock (format-time-string "%H:%M"
						   (seconds-to-time (+ seconds now)))))
      (incf seconds 40))
    (while (string= clock (format-time-string "%H:%M"
					      (seconds-to-time (+ seconds now))))
      (decf seconds))
    seconds))

(defun clock-emacsclient (command)
  (call-process "emacsclient" nil nil nil
		"--server-file=rocket-sam" 
		"--eval" command))

(defun clock-sound-alarm ()
  (clock-emacsclient "(jukebox-volume-mute \"bedroom\")")
  (clock-emacsclient "(jukebox-set-vol-volume 2 \"bedroom\")")
  (sit-for 1)
  (clock-emacsclient "(jukebox-ensure-playing)")
  (clock-cancel-alarm))
  
(defun clock-cancel-alarm ()
  (interactive)
  (ignore-errors
    (cancel-timer clock-alarm))
  (remove-alarm-text))

(defun remove-alarm-text ()
  (save-excursion
    (set-buffer (get-buffer-create "*clock*"))
    (goto-line (point-min))
    (forward-line 1)
    (delete-region (point) (line-end-position))))

(provide 'clock)

;;; clock.el ends here
