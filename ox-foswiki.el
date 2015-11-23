;;; ox-foswiki --- Foswiki (https://foswiki.org) Back-End for Org Export Engine, 
;;; modeled after the Markdown export engine (ox-md.el)

;; WORK IN PROGRESS

;; Copyright (C) 2015 Alexander Bub

;; Author: Alexander Bub
;; Keywords: org, orgmode, foswiki, twiki, wiki

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; ox-foswiki.el lets you convert Org files to foswiki syntax files
;; using the ox.el export engine.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;	 (require 'ox-foswiki)
;;
;; Export Org files to foswiki:
;; M-x org-fw-export-as-fw RET
;;
;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-html)
(require 'ox-publish)
(require 'ox-md)
(require 'ert)


;;; User-Configurable Variables

(defgroup org-export-foswiki nil
  "Options specific to foswiki export back-end."
  :tag "Org Foswiki"
  :group 'org-export
  :version "0.1"
  :package-version '(Org . "8.0"))



;;; Define Back-End

(org-export-define-derived-backend 'foswiki 'md
  :export-block '("TWIKI" "FOSWIKI")
  ;;:filters-alist '(:filter-parse-tree . org-fw-separate-elements)
  :menu-entry
  '(?f "Export to Foswiki"
       ((?f "To temporary buffer"
	    (lambda (a s v b) (org-fw-export-as-fw a s v)))
	(?F "To file" (lambda (a s v b) (org-fw-export-as-fw a s v)))
	(?O "To file and open"
	    (lambda (a s v b)
	      (if a (org-fw-export-as-fw t s v)
		(org-open-file (org-fw-export-to-markdown nil s v)))))))
  :translate-alist '((bold . org-fw-bold)
		     (code . org-fw-code)
		     (example-block . org-fw-example-block)
		     (export-block . org-fw-export-block)
		     (fixed-width . org-fw-example-block)
		     (headline . org-fw-headline)
		     (horizontal-rule . org-fw-horizontal-rule)
		     (inline-src-block . org-fw-verbatim)
		     (inner-template . org-fw-inner-template)
		     (italic . org-fw-italic)
		     (item . org-fw-item)
		     (keyword . org-fw-keyword)
		     (line-break . org-fw-line-break)
		     (link . org-fw-link)
		     (node-property . org-fw-node-property)
		     (paragraph . org-fw-paragraph)
		     (plain-list . org-fw-plain-list)
		     (plain-text . org-fw-plain-text)
		     (property-drawer . org-fw-property-drawer)
		     (quote-block . org-fw-quote-block)
		     (section . org-fw-section)
		     (src-block . org-fw-example-block)
		     (template . org-fw-template)
		     (verbatim . org-fw-verbatim)
                     (strike-through . org-fw-strike-through))
  :options-alist '((:fw-dummy-option nil "fw-dummy-option" org-fw-dummy-option t)))


(defcustom org-fw-dummy-option t
  "A dummy option."
  :group 'org-export-foswiki
  :type 'string)


;;; Filters


(defun org-fw-separate-elements (tree backend info)
  "Fix blank lines between elements.

TREE is the parse tree being exported.  BACKEND is the export
back-end used.  INFO is a plist used as a communication channel.

Enforce a blank line between elements.  There are two exceptions
to this rule:

  1. Preserve blank lines between sibling items in a plain list,

  2. In an item, remove any blank line before the very first
     paragraph and the next sub-list when the latter ends the
     current item.

Assume BACKEND is `foswiki'."
  (org-element-map tree (remq 'item org-element-all-elements)
    (lambda (e)
      (org-element-put-property
       e :post-blank
       (if (and (eq (org-element-type e) 'paragraph)
		(eq (org-element-type (org-element-property :parent e)) 'item)
		(org-export-first-sibling-p e info)
		(let ((next (org-export-get-next-element e info)))
		  (and (eq (org-element-type next) 'plain-list)
		       (not (org-export-get-next-element next info)))))
	   0
	 1))))
  ;; Return updated tree.
  tree)



;;; Transcode Functions

;;;; Bold

(defun org-fw-bold (bold contents info)
  "Transcode BOLD object into Foswiki format.
CONTENTS is the text within bold markup.  INFO is a plist used as
a communication channel."
  (format "*%s*" contents))

(ert-deftest org-fw-test-bold ()
  "Test the bold format filter"
  (should (equal (org-fw-bold nil "text" nil) "*text*"))
  (should (equal (org-fw-bold nil "3 * 4" nil) "*3 * 4*")))

;;;; Verbatim

(defun org-fw-verbatim (verbatim contents info)
  "Transcode VERBATIM object into Foswiki format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((value (org-element-property :value verbatim)))
    (format "<verbatim>%s</verbatim>" value)))

;;;; Code

(defun org-fw-code (code contents info)
  "Transcode CODE object into Foswiki format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((value (org-element-property :value code)))
    (format "<verbatim>%s</verbatim>" value)))

;;;; Strike-through

(defun org-fw-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH object into Foswiki format.
CONTENTS is the text within strike-through markup.  INFO is a plist used as
a communication channel."
  (format "<strike>%s</strike>" contents))

;;;; Example Block, Src Block and export Block

(defun org-fw-example-block (example-block contents info)
  "Transcode EXAMPLE-BLOCK element into Foswiki format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((value (org-element-property :value example-block)))
    (format "<verbatim>\n%s</verbatim>" value)))

(ert-deftest org-fw-test-example-block () ;; TODO: adjust
  "Test the example block filter for foswiki"
  (should (equal (org-fw-example-block nil "text" nil) 
                 "<verbatim>\ntext\n</verbatim>")))

(defun org-fw-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to Foswiki.
CONTENTS is nil.  INFO is a plist holding contextual information."
    ;; Also include HTML export blocks.
    (org-export-with-backend 'html export-block contents info))

;; (ert-deftest org-fw-test-export-block ()
;; "Test the export block filter"
;; (should (equal (org-fw-export-block nil nil nil) nil))


;;;; Headline

(defun org-fw-headline (headline contents info)
  "Transcode HEADLINE element into Foswiki format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
	   (title (org-export-data (org-element-property :title headline) info))
	   (todo (and (plist-get info :with-todo-keywords)
		      (let ((todo (org-element-property :todo-keyword
							headline)))
			(and todo (concat (org-export-data todo info) " ")))))
	   (tags (and (plist-get info :with-tags)
		      (let ((tag-list (org-export-get-tags headline info)))
			(and tag-list
			     (format "     :%s:"
				     (mapconcat 'identity tag-list ":"))))))
	   (priority
	    (and (plist-get info :with-priority)
		 (let ((char (org-element-property :priority headline)))
		   (and char (format "[#%c] " char)))))
	   ;; Headline text without tags.
	   (heading (concat todo priority title)))

      (cond
       ;; Cannot create a headline.  Fall back to a list.
       ;; ((or (org-export-low-level-p headline info)
       ;;    (> level 6))
       ((> level 6)
	(let ((bullet "   * "))
	  (concat bullet heading tags
		  "\n\n"
		  (and contents
		       (replace-regexp-in-string "^" (make-string (length bullet) ?\s) contents)))))
       ;; Regular headline.
       (t (concat "---" (make-string level ?+) " " heading tags "\n\n"
		  contents))))))


;;;; Horizontal Rule

(defun org-fw-horizontal-rule (horizontal-rule contents info)
  "Transcode HORIZONTAL-RULE element into Foswiki format.
CONTENTS is the horizontal rule contents.  INFO is a plist used
as a communication channel."
  "---")


;;;; Italic

(defun org-fw-italic (italic contents info)
  "Transcode ITALIC object into Foswiki format.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  (format "_%s_" contents))


;;;; Item

(defun org-fw-item (item contents info)
  "Transcode ITEM element into Foswiki format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
	 (struct (org-element-property :structure item))
	 (bullet (if (not (eq type 'ordered)) "   * "
		   (concat (number-to-string
			    (car (last (org-list-get-item-number
					(org-element-property :begin item)
					struct
					(org-list-prevs-alist struct)
					(org-list-parents-alist struct)))))
			   "."))))
    (concat bullet
	    ;;(make-string (- 4 (length bullet)) ?\s)
	    (case (org-element-property :checkbox item)
	      (on    "[X] ")   ;; %ICON{checked}% ?
	      (trans "[-] ")   ;; %ICON{minus}% ?
	      (off   "[ ] "))  ;; %ICON{unchecked}% ?
	    (let ((tag (org-element-property :tag item)))
	      (and tag (format "*%s:* " (org-export-data tag info))))
	    (and contents
	      (org-trim (replace-regexp-in-string "^" "     " contents))))))

;;;; Keyword

;; TODO
(defun org-fw-keyword (keyword contents info)
  "Transcode a KEYWORD element into Foswiki format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cond
     ((member key '("MARKDOWN" "MD"))
      (org-element-property :value keyword))
     ((string= key "TOC")
      "\n%TOC%\n"))))
    ;;(org-export-with-backend 'html keyword contents info))))

;;;; Line Break

;; TODO
(defun org-fw-line-break (line-break contents info)
  "Transcode LINE-BREAK object into Foswiki format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  "  \n")


;;;; Link

(defun org-fw-link (link contents info)
  "Transcode LINE-BREAK object into Foswiki format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  (let ((link-org-files-as-md
	 (lambda (raw-path)
	   ;; Treat links to `file.org' as links to `file.md'.
	   (if (string= ".org" (downcase (file-name-extension raw-path ".")))
	       (concat (file-name-sans-extension raw-path) ".md")
	     raw-path)))
	(type (org-element-property :type link)))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link contents 'md))
     ((member type '("custom-id" "id" "fuzzy"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(case (org-element-type destination)
	  (plain-text			; External file.
	   (let ((path (funcall link-org-files-as-md destination)))
	     (if (not contents) (format "<%s>" path)
	       (format "[%s](%s)" contents path))))
	  (headline
	   (format
	    "[%s](#%s)"
	    ;; Description.
	    (cond ((org-string-nw-p contents))
		  ((org-export-numbered-headline-p destination info)
		   (mapconcat #'number-to-string
			      (org-export-get-headline-number destination info)
			      "."))
		  (t (org-export-data (org-element-property :title destination)
				      info)))
	    ;; Reference.
	    (or (org-element-property :CUSTOM_ID destination)
		(org-export-get-reference destination info))))
	  (t
	   (let ((description
		  (or (org-string-nw-p contents)
		      (let ((number (org-export-get-ordinal destination info)))
			(cond
			 ((not number) nil)
			 ((atom number) (number-to-string number))
			 (t (mapconcat #'number-to-string number ".")))))))
	     (when description
	       (format "[%s](#%s)"
		       description
		       (org-export-get-reference destination info))))))))
     ((org-export-inline-image-p link org-html-inline-image-rules)
      (let ((path (let ((raw-path (org-element-property :path link)))
		    (if (not (file-name-absolute-p raw-path)) raw-path
		      (expand-file-name raw-path))))
	    (caption (org-export-data
		      (org-export-get-caption
		       (org-export-get-parent-element link)) info)))
	(format "![img](%s)"
		(if (not (org-string-nw-p caption)) path
		  (format "%s \"%s\"" path caption)))))
     ((string= type "coderef")
      (let ((ref (org-element-property :path link)))
	(format (org-export-get-coderef-format ref contents)
		(org-export-resolve-coderef ref info))))
     ((equal type "radio") contents)
     (t (let* ((raw-path (org-element-property :path link))
	       (path
		(cond
		 ((member type '("http" "https" "ftp"))
		  (concat type ":" raw-path))
		 ((string= type "file")
		  (org-export-file-uri (funcall link-org-files-as-md raw-path)))
		 (t raw-path))))
	  (if (not contents) (format "<%s>" path)
	    (format "[%s](%s)" contents path)))))))


;;;; Node Property

(defun org-fw-node-property (node-property contents info)
  "Transcode a NODE-PROPERTY element into Foswiki syntax.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "%s:%s"
          (org-element-property :key node-property)
          (let ((value (org-element-property :value node-property)))
            (if value (concat " " value) ""))))


;;;; Paragraph

(defun org-fw-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Foswiki format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (let ((first-object (car (org-element-contents paragraph))))
    ;; If paragraph starts with a #, protect it.
    (if (and (stringp first-object) (string-match "\\`#" first-object))
	(replace-regexp-in-string "\\`#" "\\#" contents nil t)
      contents)))


;;;; Plain List

(defun org-fw-plain-list (plain-list contents info)
  "Transcode PLAIN-LIST element from Org to Foswiki.
CONTENTS is the plain-list contents.  INFO is a plist holding
contextual information." 
  (let* ;; (arg1 ;; (assoc :counter (org-element-map plain-list 'item
       ((type (org-element-property :type plain-list)))
    (format "%s"
	    ;;(org-html-begin-plain-list type)
	    contents ;;(org-html-end-plain-list type)
            )))


;;;; Plain Text

(defun org-fw-plain-text (text info)
  "Transcode a TEXT string into Foswiki format.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (when (plist-get info :with-smart-quotes)
    (setq text (org-export-activate-smart-quotes text :html info)))
  ;; Protect ambiguous #.  This will protect # at the beginning of
  ;; a line, but not at the beginning of a paragraph.  See
  ;; `org-fw-paragraph'.
  (setq text (replace-regexp-in-string "\n#" "\n\\\\#" text))
  ;; Protect ambiguous !
  (setq text (replace-regexp-in-string "\\(!\\)\\[" "\\\\!" text nil nil 1))
  ;; Protect `, *, _ and \
  (setq text (replace-regexp-in-string "[`*_\\]" "\\\\\\&" text))
  ;; Handle special strings, if required.
  (when (plist-get info :with-special-strings)
    (setq text (org-html-convert-special-strings text)))
  ;; Handle break preservation, if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "[ \t]*\n" "  \n" text)))
  ;; Return value.
  text)


;;;; Property Drawer

(defun org-fw-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element into Foswiki format.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (and (org-string-nw-p contents)
       (replace-regexp-in-string "^" "    " contents)))


;;;; Quote Block

(defun org-fw-quote-block (quote-block contents info)
  "Transcode QUOTE-BLOCK element into Foswiki format.
CONTENTS is the quote-block contents.  INFO is a plist used as
a communication channel."
  (replace-regexp-in-string
   "^" "> "
   (replace-regexp-in-string "\n\\'" "" contents)))


;;;; Section

(defun org-fw-section (section contents info)
  "Transcode SECTION element into Foswiki format.
CONTENTS is the section contents.  INFO is a plist used as
a communication channel."
  contents)


;;;; Template

(defun org-fw-inner-template (contents info)
  "Return body of document after converting it to Foswiki syntax.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  ;; Make sure CONTENTS is separated from table of contents and
  ;; footnotes with at least a blank line.
  ;;(org-trim (org-html-inner-template (concat "\n" contents "\n") info)))
  (concat
    ;; Table of contents
    (let ((with-toc (plist-get info :with-toc)))
      (when with-toc "\n%TOC%\n\n"))
    ;; Document contents
    contents))

(defun org-fw-template (contents info)
  "Return complete document string after Foswiki conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  contents)



;;; Interactive function

;;;###autoload
(defun org-fw-export-as-fw (&optional async subtreep visible-only)
  "Export current buffer to a Foswiki buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org MD Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'foswiki "*Org Foswiki Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-fw-convert-region-to-md ()
  "Assume the current region has org-mode syntax, and convert it to Foswiki.
This can be used in any buffer.  For example, you can write an
itemized list in org-mode syntax in a Foswiki buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'md))


;;;###autoload
(defun org-fw-export-to-fw(&optional async subtreep visible-only)
  "Export current buffer to a Foswiki file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".foswiki" subtreep)))
    (org-export-to-file 'foswiki outfile async subtreep visible-only)))

;;;###autoload
(defun org-fw-publish-to-fw (plist filename pub-dir)
  "Publish an org file to Foswiki.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'foswiki filename ".foswiki" plist pub-dir))

(provide 'ox-foswiki)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-foswiki.el ends here
