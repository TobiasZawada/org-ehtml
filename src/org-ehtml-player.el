;;; org-ehtml-play.el --- Player add-on for ehtml    -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Tobias Zawada

;; Author: Tobias Zawada <naehring@smtp.1und1.de>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
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

;; 

;;; Code:
(require 'org-ehtml-server)
(require 'subr-x)
(require 'cl-lib)

(defvar org-ehtml-player-process nil
  "Process for `org-ehtml-play'.")

(defcustom org-ehtml-players
  '((mplayer .
     ("mplayer" "-cache" "8192" "%u"))
    (vlc .
     ("vlc" "-Idummy" "%u")))
  "Mapping of player symbols to command lists.
The first element of the list is the command to be executed.
The others are the arguments for the command.
An %u in the argument list is replaced by the URL to be played."
  :group 'org-export-ehtml
  :type '(repeat (cons symbol (repeat string))))

(defcustom org-ehtml-play-buffer "*ehtml-play*"
  "Name of the process buffer of the player."
  :group 'org-export-ehtml
  :type 'string)

(defmacro org-ehtml-play-with-buffer (&rest body)
  "Run BODY in `org-ehtml-play-buffer'."
  (declare (debug body))
  `(with-current-buffer (get-buffer-create org-ehtml-play-buffer)
     ,@body))

(defconst org-ehtml-play-cdrom-type-alist
  '(("Disc found in drive: data disc" . data)
    ("Disc found in drive: audio disc" . audio)
    ("No disc" . no-disc)
    ("CD tray is open" . open)
    ("Drive is not ready" . not-ready))
  "Alist mapping responses of setcd to cdrom types.")

(defun org-ehtml-play-cdrom-type ()
  "Detect cdrom type with the help of setcd."
  (org-ehtml-play-with-buffer
   (goto-char (point-max))
   (unless (bolp)
     (insert "\n"))
   (let (type
	 (pt (point-max)))
     (while 
	 (progn
	   (when (eq type 'not-ready)
	     (delete-region pt (point-max)))
	   (setq pt (point-max))
	   (call-process "setcd"
			 nil org-ehtml-play-buffer nil
			 "-i")
	   (goto-char pt)
	   (let ((found (re-search-forward (concat "^[[:space:]]*" (regexp-opt (mapcar #'car org-ehtml-play-cdrom-type-alist) t)) nil t)))
	     (cl-assert found nil "Output of setcd not recognized"))
	   (setq type (alist-get (match-string 1) org-ehtml-play-cdrom-type-alist nil nil #'string-equal))
	   (eq type 'not-ready)))
       type)))

(defun org-ehtml-play-cdrom ()
  "Play audio CD and data CD with mp3-files."
  (let ((type (org-ehtml-play-cdrom-type)))
    (cl-case type
      (audio
       (org-ehtml-play 'mplayer "cdda://"))
      (data
       (call-process "mount" nil org-ehtml-play-buffer nil "/dev/cdrom")
       (let ((play-list (mapcar (lambda (file)
				  (format "file://%s" file))
				(directory-files "/mnt/cdrom" t "\\.mp3\\'"))))
	 (when (process-live-p org-ehtml-player-process)
	   (kill-process org-ehtml-player-process))
	 (org-ehtml-play-with-buffer
	  (erase-buffer)
	  (setq org-ehtml-player-process
		(apply #'start-process "ehtml-play" (current-buffer)
		       "mplayer"
		       play-list))
	  (write-region nil nil "/tmp/ehtml.log" t))
	 ))
      (t
       (error "Unexpected cdrom type in `org-ehtml-play-cdrom'")))
    ))

(put 'org-ehtml-play-cdrom 'org-ehtml-safe-form '(t))

(defun org-ehtml-play-send-char (chars)
  "Send string CHARS to org-ehtml-player-process."
  (unless (process-live-p org-ehtml-player-process)
    (error "Player process not running."))
  (cl-assert (stringp chars) "Argument %S is not a character string" chars)
  (process-send-string org-ehtml-player-process chars))

(put 'org-ehtml-play-send-char 'org-ehtml-safe-form '(t stringp))

(defun org-ehtml-play (player url)
  "Start URLS in PLAYER.
PLAYER is mapped by `org-ehtml-players' to
a list of a command with arguments.
%u is replaced by URLS in the argument list."
  (when (process-live-p org-ehtml-player-process)
    (kill-process org-ehtml-player-process))
  (org-ehtml-play-with-buffer
   (erase-buffer))
  (when-let ((fun-list (alist-get player org-ehtml-players)))
    (setq fun-list (mapcar
		    (lambda (str)
		      (format-spec str (list
					(cons ?u url))))
		    fun-list))
    (setq org-ehtml-player-process
	  (apply #'start-process "ehtml-play" org-ehtml-play-buffer
		 fun-list))
    (org-ehtml-play-with-buffer
      (write-region nil nil "/tmp/ehtml.log" t))
    ))

(defun org-ehtml-play-check-backend (backend)
  "Only accept ehtml backend for ehtml links."
  (unless (memq backend '(ehtml html)) ;; This should be (eq backend 'ehtml), but there is actually a bug in `org-html-link'.
    (user-error "Org ehtml links can only be exported to ehtml")))
  
(defun org-ehtml-play-link-export (player path description backend)
  "Export links of type [[ehtml-PLAYERNAME:PATH][DESCRIPTION]].
The PLAYER is mapped to a command by `org-ehtml-players'.
Only ehtml BACKEND is supported."
  (org-ehtml-play-check-backend backend)
  (format "<a href=\"?ehtml_query=%s\">%s</a>"
	  (url-encode-url (format "%S" (list 'org-ehtml-play (list 'quote player) path)))
	  description))

(put 'org-ehtml-play 'org-ehtml-safe-form '(t org-ehtml-qsymbol-p stringp))
;; (org-ehtml-safe-form-p '(org-ehtml-play 'mplayer "http://test"))

(defun org-ehtml-player-button (button label)
  "Define an html button with LABEL sending BUTTON.
BUTTON is a string that is sent to `org-ehtml-player-process'."
  (format
   "<button type=\"button\" onclick=\"run_elisp_form('%s')\">%s</button> "
   (url-encode-url (format "(org-ehtml-play-send-char \"%s\")" button))
   label)
  )
;; Test:
;; (org-ehtml-player-button "a" "→")

(defun org-ehtml-play-cdrom-export (_path description backend)
  "Export ehtml string for cd player.
DESCRIPTION is the label.
Check whether BACKEND is really ehtml."
  (org-ehtml-play-check-backend backend)
  (concat
   "<a href=\"?ehtml_query="
   (url-encode-url "(org-ehtml-play-cdrom)")
   "\">" description "</a> "
   (org-ehtml-player-button "p" "Pause/Play")
   (org-ehtml-player-button "g" "Prev")
   (org-ehtml-player-button "y" "Next")
   (org-ehtml-player-button "\x1b\ D" "←10")
   (org-ehtml-player-button "\x1b\ C" "→10")
   (org-ehtml-player-button "\x1b\ A" "←60")
   (org-ehtml-player-button "\x1b\ B" "→60")
   (org-ehtml-player-button "\x1bS" "←600")
   (org-ehtml-player-button "\x1bT" "→600")
   )
  )

(org-link-set-parameters "ehtml-cdrom"
			 :follow
			 (lambda (_url) (org-ehtml-play-cdrom))
			 :export #'org-ehtml-play-cdrom-export)

(org-link-set-parameters "ehtml-mplayer"
			 :follow
			 (lambda (url) (org-ehtml-play 'mplayer url))
			 :export
			 (lambda (&rest args)
			   (apply #'org-ehtml-play-link-export 'mplayer args))
			 )

(org-link-set-parameters "ehtml-vlc"
			 :follow
			 (lambda (url) (org-ehtml-play 'vlc url))
			 :export
			 (lambda (&rest args)
			   (apply #'org-ehtml-play-link-export 'vlc args))
			 )

(provide 'org-ehtml-player)
;;; org-ehtml-player.el ends here
