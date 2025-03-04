;;; ox-ehtml.el -- export of Org-mode to editable HTML

;; Copyright (C) 2012-2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Keywords: org web-server javascript html

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'ox-html)
(require 'ox-org)
(require 'org-ehtml-util)

(defvar org-ehtml-style
  (concat
   "<style type=\"text/css\">\n<!--/*--><![CDATA[/*><!--*/\n"
   (file-contents (if (file-exists-p (expand-file-name "~/.ox-ehtml-override.css"))
                      (expand-file-name "~/.ox-ehtml-override.css")
                    (expand-file-name "ox-ehtml.css" org-ehtml-base)
                    ))
   "/*]]>*/-->\n</style>"))

(defvar org-ehtml-jquery
  "https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js")

(defvar org-ehtml-js)
(setq org-ehtml-js (file-contents (expand-file-name "ox-ehtml.js" org-ehtml-base)))

(defun org-ehtml-scripts ()
  (concat
   "<script type=\"text/javascript\" src=\""
   org-ehtml-jquery "\"></script>"
   "<script type=\"text/javascript\">\n<!--/*--><![CDATA[/*><!--*/\n"
   org-ehtml-js
   "/*]]>*///-->\n</script>\n"))

(defvar org-ehtml-wrap-template
  (concat
   "<div class=\"edit_in_place\">%html-text</div>"
   "<div class=\"raw-org\" contents-begin=\"%begin\" contents-end=\"%end\">"
   "%org-text</div>"))

(defcustom org-ehtml-everything-editable nil
  "Set to a true value to make everything exported by org-ehtml editable."
  :group 'org-export-ehtml
  :type 'boolean)

(defcustom org-ehtml-editable-headlines nil
  "Set to a true value to make headines exported by org-ehtml editable."
  :group 'org-export-ehtml
  :type 'boolean)

(defcustom org-ehtml-editable-types
  '(paragraph plain-list table verbatim quote-block verse-block)
  "Types of elements whose children should not be editable."
  :group 'org-export-ehtml
  :type '(repeat symbol))

(defcustom org-ehtml-button-format
  "<button type=\"button\" onclick=\"run_src_block('%n')\">%l</button>"
  "Format string for exporting source blocks to buttons.
It should run `run_src_block' on the onclick event.
The format specifier %n is replaced with the source block name
and the specifier %l is replaced with the button label."
  :group 'org-export-ehtml
  :type 'string)

(defvar org-ehtml-headline nil
  "Used to pass headline from `org-ehtml-format-headline-wrap' to
  `org-ehtml-format-headine-function'.")

(defvar org-ehtml-info nil
  "Used to pass info from `org-ehtml-format-headline-wrap' to
  `org-ehtml-format-headine-function'.")

(defun org-ehtml-propertize-buffer-positions (&optional backend)
  "Propertize current buffer with buffer positions.
The buffer positions are stored in the text property ehtml-pos.
Ignore this functions unless BACKEND is nil or ehtml."
  (when (or (null backend) (eq backend 'ehtml))
    (org-with-wide-buffer
     (cl-loop for pos from (point-min) upto (1- (point-max))
	      do (put-text-property pos (1+ pos) 'ehtml-pos pos)))))
;; Test: (with-current-buffer "*scratch*" (org-ehtml-propertize-buffer-positions 'ehtml))

(add-hook 'org-export-before-processing-hook #'org-ehtml-propertize-buffer-positions 90)

(defun org-ehtml-original-buffer-position (point &optional end)
  "Get original buffer position at POINT.
If this should be the end of a region set END to non-nil."
  (or
   (get-text-property point 'ehtml-pos)
   (get-text-property (next-single-property-change point 'ehtml-pos nil (point-max)) 'ehtml-pos) ;; e.g., at text inserted by src-block evaluation
   (let ((new-pt (get-text-property (previous-single-property-change point 'ehtml-pos nil (point-min)) 'ehtml-pos))) ;; e.g., at (point-max)
     (and new-pt
	  (if end (1+ new-pt)
	    new-pt)))
   (user-error "Cannot find original buffer position starting search at position %d" point)))
;; Tests:
;; (with-current-buffer "*scratch*" (org-ehtml-original-buffer-position (point-max) t))
;; (with-current-buffer "*scratch*" (org-ehtml-original-buffer-position (point-min)))

(defun org-ehtml-format-headine-function (&rest args)
  (let*
      ((headline org-ehtml-headline)
       (info org-ehtml-info)
       (html (apply #'org-html-format-headline-default-function args))
       (begin (number-to-string (org-element-property :begin headline)))
       (contents-begin (org-element-property :contents-begin headline))
       (end (number-to-string (if contents-begin
				  contents-begin
				(org-element-property :end headline))))
       (org (org-org-headline headline "" info)))
    (org-fill-template org-ehtml-wrap-template
                       `(("html-text" . ,html)
                         ("org-text"  . ,org)
                         ("begin"     . ,begin)
                         ("end"       . ,end)))))

(defun org-ehtml-format-headline-wrap (headline contents info)
  (if org-ehtml-editable-headlines
      (let ((org-html-format-headline-function
             #'org-ehtml-format-headine-function)
            (org-ehtml-headline headline)
            (org-ehtml-info info))
        (org-html-headline headline contents info))
    (org-html-headline headline contents info)))

(defun org-ehtml-editable-p (element info)
  (let ((parent (org-export-get-parent element)))
    (cond ((eq (car element) 'headline) org-ehtml-editable-headlines)
          ((eq (car parent) 'headline)
           (or org-ehtml-everything-editable
               (member "EDITABLE" (org-export-get-tags parent info))))
          ((eq (car parent) 'org-data)
           (or org-ehtml-everything-editable
               (cl-some
                (lambda (keyword)
                  (let ((key (plist-get (cadr keyword) :key))
                        (val (plist-get (cadr keyword) :value)))
                    (and (string= "PROPERTY" key)
                         (string-match "editable \\(.+\\)" val)
                         (car (read-from-string (match-string 1 val))))))
                (cddr (cl-caddr parent)))))
          ((member (car parent) org-ehtml-editable-types) nil)
          (t (org-ehtml-editable-p parent info)))))

(defmacro def-ehtml-wrap (html-function)
  "Defines and returns an ehtml-wrapped version of HTML-FUNCTION."
  (let ((fname (intern (replace-regexp-in-string
                        "org-html-" "org-ehtml-"
                        (symbol-name html-function)))))
    `(defun ,fname (element contents info)
       ,(format "Editable html wrapper around `%s'." html-function)
       (let* ((original-contents (cl-copy-seq contents))
              (original-info     (cl-copy-seq info))
              (html-text (,html-function element contents info))
              (org-text  (or (org-element-interpret-data element)
                             original-contents
                             (error "no org-text found for %s" (car element)))))
         (if (org-ehtml-editable-p element info)
             (org-fill-template org-ehtml-wrap-template
              `(("html-text" . ,html-text)
                ("org-text"  . ,org-text)
                ("begin"     . ,(number-to-string
				 (org-ehtml-original-buffer-position
                                  (plist-get (cadr element) :begin))))
                ("end"       . ,(number-to-string
				 (org-ehtml-original-buffer-position
                                  (plist-get (cadr element) :end) t)))))
           html-text)))))

(defun org-ehtml-src-block (src-block contents info)
  "Additional to `org-html-src-block' handle source blocks with ATTR_EHTML.
The args SRC-BLOCK, CONTENTS and INFO are
passed through to `org-html-src-block'.
If the source block has the ATTR_EHTML attribute :type
the source block is replaced by a control element of that type.
Currently, only \"button\" is supported as type.
Clicking that butten evaluates the source block."
  (let ((type (org-export-read-attribute
		     :attr_ehtml src-block
		     :type)))
    (if type
	(let* ((name (org-element-property :name src-block)))
	  (unless name
	    (user-error "Source blocks with ehtml-attr must be named"))
	  (pcase type
	    ("button"
	      (format-spec
	       org-ehtml-button-format
	       (list (cons ?n name)
		     (cons ?l (or (org-export-read-attribute :attr_ehtml src-block :label) name)))
	       ))
	    (_
	     (user-error "Unknown ehtml-attr"))))
      (org-html-src-block src-block contents info))))

(org-export-define-derived-backend 'ehtml 'html
  :menu-entry
  '(?e "Export to Editable HTML"
       ((?H "To Temporary buffer" org-ehtml-export-as-html)
        (?h "To file" org-ehtml-export-to-html)
        (?o "To file and open"
	    (lambda (a s v b)
	      (if a (org-ehtml-export-to-html t s v b)
		(org-open-file (org-ehtml-export-to-html nil s v b)))))))
  :options-alist
  '((:ehtml-everything-editable "HTML_EVERYTHING_EDITABLE" nil
                                org-ehtml-everything-editable)
    (:ehtml-editable-headlines "HTML_EDITABLE_HEADLINES" nil
                                org-ehtml-editable-headlines)
    (:ehtml-editable-types nil nil org-ehtml-editable-types))
  :translate-alist
  `((headline    . org-ehtml-format-headline-wrap)
    (paragraph   . ,(def-ehtml-wrap org-html-paragraph))
    (plain-list  . ,(def-ehtml-wrap org-html-plain-list))
    (table       . ,(def-ehtml-wrap org-html-table))
    (verbatim    . ,(def-ehtml-wrap org-html-verbatim))
    (quote-block . ,(def-ehtml-wrap org-html-quote-block))
    (src-block   . org-ehtml-src-block)
    (verse-block . ,(def-ehtml-wrap org-html-verse-block))))

(defun org-ehtml-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.
Based on `org-html-export-as-html'.'"
  (interactive)
  (org-export-to-buffer 'ehtml "*Org eHTML Export*"
    async subtreep visible-only body-only ext-plist (lambda () (nxml-mode))))

(defun org-ehtml-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an editable HTML file.
Based on `org-html-export-to-html'.'"
  (interactive)
  (let* ((extension (concat "." org-html-extension))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-coding-system)
         ;; custom headers
         (org-html-style-default (concat org-html-style-default "\n"
                                         org-ehtml-style))
         (org-html-scripts (concat org-html-scripts "\n"
                                   (org-ehtml-scripts))))
    (org-export-to-file 'ehtml file
      async subtreep visible-only body-only ext-plist)))

(defun org-ehtml-export-file (file)
  "Export FILE's contents to editable HTML."
  (save-window-excursion
    (find-file file)
    (org-ehtml-export-to-html)))

(defun org-ehtml-cached (file)
  "Export FILE to editable HTML if no previous export exists.
If a previous HTML export of FILE exists but is older than FILE
re-export."
  (cl-flet ((age (f)
                 (float-time
                  (time-subtract (current-time)
                                 (nth 5 (or (file-attributes (file-truename f))
                                            (file-attributes f)))))))
    (let* ((dir (file-name-directory file))
           (base (file-name-sans-extension file))
           (html (concat base ".html"))
           (org (concat base ".org")))
      (if (and (file-exists-p org)
               (or (not (file-exists-p html)) (> (age html) (age org))))
          (expand-file-name (org-ehtml-export-file org) dir)
        html))))

(provide 'ox-ehtml)
;;; ox-ehtml.el ends here
