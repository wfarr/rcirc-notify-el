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

(defvar rcirc-notification-message "%s is calling your name in %s."
  "Format of message to display in libnotify popup.
'%s' will expand to the nick that notified you.")

(defvar rcirc-notified-nick-alist nil
  "An alist of nicks and the last time they tried to trigger a
notification.")

(defvar rcirc-notification-timeout 30
  "Number of seconds that will elapse between notifications from the
same person.")

(defvar rcirc-privmsg-target "private message"
  "String used as target in rcirc-notify-message for private messages.")

(defun rcirc-send-notification (title message)
  (cond ((eq window-system 'mac)
         (start-process "rcirc-notify" nil "growlnotify"
                        "-t" title "-m" message))
        ((eq window-system 'x)
         (start-process "rcirc-notify" nil
                        ;; 8640000 ms = 1 day
                        "notify-send" "-u" "normal" "-i" "gtk-dialog-info"
                        "-t" "8640000" title message))))

(defun rcirc-notify (sender &optional target)
  (unless target (setq target rcirc-privmsg-target))
  (when window-system
    ;; Set default dir to appease the notification gods
    (let ((default-directory "~/"))
      (rcirc-send-notification "rcirc" (format rcirc-notification-message sender target))))
  ;; Always print a copy to *Messages*
  (message (concat (format-time-string "%r") " - " (format rcirc-notification-message sender target))))

(defun rcirc-notify-allowed (sender &optional delay)
  "Return non-nil if a notification should be made for SENDER.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`rcirc-notification-timeout'."
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
  "Notify the current user when someone sends a message that
matches a regexp in `rcirc-keywords' inside a channel."
  (interactive)
  (when (and (string-match
              (concat "\\(^\\|[^a-zA-Z0-9_\\-]\\)"
                      (regexp-quote (rcirc-nick proc))
                      "\\($\\|[^a-zA-Z0-9_\\-]\\)")
              text)
             (not (string= (rcirc-nick proc) sender))
             (not (string= (rcirc-server-name proc) sender))
             (rcirc-notify-allowed sender))
    (rcirc-notify sender target)))

(defun rcirc-notify-privmsg (proc sender response target text)
  "Notify the current user when someone sends a private message
to them."
  (interactive)
  (when (and (string= response "PRIVMSG")
             (not (string= sender (rcirc-nick proc)))
             (not (rcirc-channel-p target))
             (rcirc-notify-allowed sender))
    (rcirc-notify sender)))

;;;###autoload
(defun turn-on-rcirc-notify ()
  (interactive)
  (unless (load-history-filename-element "\\rcirc-notify.el")
    (load "rcirc-notify"))
  (add-hook 'rcirc-print-hooks 'rcirc-notify-privmsg)
  (add-hook 'rcirc-print-hooks 'rcirc-notify-channel))

;;;###autoload
(defun turn-off-rcirc-notify ()
  (interactive)
  (remove-hook 'rcirc-print-hooks 'rcirc-notify-channel)
  (remove-hook 'rcirc-print-hooks 'rcirc-notify-privmsg))

(provide 'rcirc-notify)
