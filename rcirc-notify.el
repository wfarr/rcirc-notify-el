;;;; rcirc-notify.el
;; -*- mode: elisp -*-
;; Copyright (c) 2008 Will Farrington <wcfarrington@gmail.com>
;; Copyright (c) 2008 Nathan Weizenbaum <nex342@gmail.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;

(defvar rcirc-notification-title "rcirc: %s [%s]")

(defvar rcirc-notified-nick-alist nil)

(defvar rcirc-notification-timeout 30
  "Number of seconds that will elapse between notifications from the
same person.")

(defvar rcirc-notify-sticky nil
  "If t makes notifications 'stick' and not go away until clicked.")

(defun rcirc-send-notification (title message)
  (cond ((and (or (eq window-system 'mac)
                  (eq window-system 'ns))
              (executable-find "growlnotify"))
         (start-process "rcirc-notify" nil "growlnotify"
                        "-t" title "-m" message "-a" "Emacs.app" (if rcirc-notify-sticky "-s" "")))
        ((and (eq window-system 'x)
              (executable-find "notify-send"))
         (start-process "rcirc-notify" nil
                        ;; 8640000 ms = 1 day
                        "notify-send" "-u" "normal" "-i" "gtk-dialog-info"
                        "-t" (if rcirc-notify-sticky "0" "8640000") title message))))

(defun rcirc-notify (sender text &optional target)
  (setq target (or target "private message"))
  (if window-system
      (let ((default-directory "~/"))
        (rcirc-send-notification (concat (format rcirc-notification-title sender target)) text)))
  (message (concat (format-time-string "%r") " - " (format rcirc-notification-title sender target) ": " text))
  (makunbound 'target))

(defun rcirc-notify-allowed (sender &optional delay)
  (unless delay (setq delay rcirc-notification-timeout))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc sender rcirc-notified-nick-alist))
        (last-time))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons sender cur-time) rcirc-notified-nick-alist)
      t)))

(defun rcirc-notify-channel (proc sender response target text)
  (interactive)
  (when (and (string-match (regexp-quote (rcirc-nick proc)) text)
             (not (string= (rcirc-nick proc) sender))
             (not (string= (rcirc-server-name proc) sender))
             (rcirc-notify-allowed sender))
    (rcirc-notify sender text target)))

(defun rcirc-notify-privmsg (proc sender response target text)
  (interactive)
  (when (and (string= response "PRIVMSG")
             (not (string= sender (rcirc-nick proc)))
             (not (rcirc-channel-p target))
             (rcirc-notify-allowed sender))
    (rcirc-notify sender text)))

;;;###autoload
(defun turn-on-rcirc-notify ()
  "Enables rcirc-notify if it hasn't already been loaded
or required."
  (interactive)
  (unless (featurep 'rcirc-notify)
    (load "rcirc-notify"))
  (add-hook 'rcirc-print-hooks 'rcirc-notify-privmsg)
  (add-hook 'rcirc-print-hooks 'rcirc-notify-channel))

;;;###autoload
(defun turn-off-rcirc-notify ()
  "Removes the rcirc-notify listening methods from
`rcirc-print-hooks'."
  (interactive)
  (remove-hook 'rcirc-print-hooks 'rcirc-notify-channel)
  (remove-hook 'rcirc-print-hooks 'rcirc-notify-privmsg))

(add-hook 'rcirc-print-hooks 'rcirc-notify-privmsg)
(add-hook 'rcirc-print-hooks 'rcirc-notify-channel)

(provide 'rcirc-notify)