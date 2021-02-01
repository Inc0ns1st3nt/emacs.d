;;; svg-tag-mode.el --- svg tag rendering for text

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Danny He <o28c14@gmail.com>
;; Package-Requires: ((emacs "24.1"))
;; Version: 1.0.0
;; Keywords: extensions elisp
;; Prefix: svg-tag
;; Separator: -

;;; Commentary:

;; based on the tag.el from u/Nicolas-Rougier on his reddit post "Tags everywhere..."
;; @see https://www.reddit.com/r/emacs/comments/jc4uou/tags_everywhere/
;; u/Nicolas-Rougier's svg text positioning calc didn't work for me which i replaced with
;; u/tampix77's

;;; Code:

(require 'svg)

(defgroup svg-tag-mode nil
  "Replace keywords with SVG rounded box labels"
  :group 'convenience
  :prefix "svg-tag-")

(defcustom svg-tag-default-outer-padding 1
  "Default outer padding (in characters, null or positive)"
  :type 'integer
  :group 'svg-tag-mode)

(defcustom svg-tag-default-inner-padding 1
  "Default inner padding (in characters, null or positive)"
  :type 'integer
  :group 'svg-tag-mode)

(defcustom svg-tag-default-radius 3
  "Default radius  (in pixels, null or positive)"
  :type 'integer
  :group 'svg-tag-mode)

(defcustom svg-tag-default-border 1
  "Default border width (in pixels)"
  :type 'integer
  :group 'svg-tag-mode)

(defcustom svg-tag-vertical-offset 0
  "Vertical offset for text (in pixels).
This should be zero for most fonts but some fonts may need this."
  :type 'integer
  :group 'svg-tag-mode)

(defcustom svg-tag-horizontal-offset 0
  "Horizontal offset for text (in pixels).
This should be zero for most fonts but some fonts may need this."
  :type 'integer
  :group 'svg-tag-mode)

(defface svg-tag-default-face
  `((t :foreground "white"
       :background "orange"
       :box (:line-width 1 :color "orange" :style nil)
       :family ,(face-attribute 'default :family)
       :weight ,(face-attribute 'default :weight)
       :height 120))
  "Default face for tag"
  :group 'svg-tag-mode)

(defun svg-tag (text &optional face inner-padding outer-padding radius border)
  "produce an svg based on the parameters passed in for `text',
which is the matched by the font-keywords. `face' specicies which
face to apply to the svg. `radius' specifies the radius of the
svg. `border' specifies the border width of the svg, currently
is not working."
  (let ((face   (or face 'svg-tag-default-face))
		(inner-padding (or inner-padding svg-tag-default-inner-padding))
		(outer-padding (or outer-padding svg-tag-default-outer-padding))
		(radius (or radius svg-tag-default-radius))
		(border (or border svg-tag-default-border))

		(font-width  (window-font-width))
		(font-height (window-font-height))
		(text (s-trim text)))
	(let* ((tag-width (* (+ (length text) inner-padding)
						 font-width))
           (tag-height (* font-height 0.95))

           (svg-width (+ tag-width (* outer-padding font-width)))

           (tag-x (/ (- svg-width tag-width) 2))

           (svg (svg-create svg-width tag-height)))
	  ;; border
	  (svg-rectangle svg tag-x 0 tag-width tag-height
					 :fill (face-attribute face :box)
					 :rx   radius)
	  ;; background
	  (svg-rectangle svg
					 (+ tag-x border)
					 border
					 (- tag-width (* 2 border))
					 (- tag-height (* 2 border))
					 :fill (face-attribute face :background)
					 :rx   (1- radius))
	  ;; foreground & text
	  (svg-text svg text
				:font-family (face-attribute 'default :family)
				:font-weight (face-attribute face :weight)
				:font-size   (/ (face-attribute face :height) 10)
				:fill        (face-attribute face :foreground)
				:x "50%"
				:y "75%"
				:dominant-baseline "middle"
				:text-anchor "middle")
	  (svg-image svg :ascent 'center))))

(defface svg-tag-note-face
  '((t :foreground "black" :background "yellow" :box "black"
       :family "Roboto Mono" :weight light :height 120))
  "Face for note tag" :group 'svg-tag)

(defface svg-tag-key-face
  '((t :foreground "#333333" :background "#f0f0f0" :box "#999999"
       :family "Roboto Mono" :weight light :height 120))
  "Face for key tag" :group 'svg-tag)

;; (defvar svg-tag-todo (svg-tag "TODO" nil 0 0 3))
;; (defvar svg-tag-note (svg-tag "NOTE" 'svg-tag-note-face 1 1 3))
(defvar svg-tag-font-lock-keywords
  '(("\\(\:TODO\:\\)" 1
	 `(face nil display ,(svg-tag "TODO" 'svg-tag-face 1 1 3)))
	("\\(\:NOTE\:\\)" 1
	 `(face nil display ,(svg-tag "NOTE" 'svg-tag-note-face 1 1 3)))
	("\\(=[0-9a-zA-Z- ]+?=\\)" 1
	 `(face nil display
			,(svg-tag (substring (match-string 0) 1 -1)
					  'svg-tag-key-face 1 1 3 2)))))

;; A tag function using SVG to display a rounded box with outer and inner
;; padding and a controllable box radius. The resulting SVG is perfectly
;; aligned with regular text such that a =TAG= can be inserted and edited
;; anywhere in the text thanks to font-lock and the display property.

(defvar svg-tags nil)
(defvar active-svg-tags nil)

;;;###autoload
(define-minor-mode svg-tag-mode
  "Rendering svg tag mode."
  :lighter " Svg tag"
  (if svg-tag-mode
	  (progn
		(add-to-list 'font-lock-extra-managed-props 'display)
		(if active-svg-tags
			(font-lock-remove-keywords nil active-svg-tags))
		(if svg-tags
			(font-lock-add-keywords nil svg-tags))
		(setq active-svg-tags (copy-sequence svg-tags)))
	(setq font-lock-extra-managed-props
		  (delete 'display font-lock-extra-managed-props))
	(if active-svg-tags
		(font-lock-remove-keywords nil active-svg-tags))
	(setq active-svg-tags nil))
  (font-lock-flush))

;;|:DONE:| Make a minor mode
;;|:NOTE:| Don't know how to do it, help needed…
;;|______| Perfect alignment with regular text
;;
;;  Save ................. =C-x=+=C-s=  Help ............... =C-h=
;;  Save as .............. =C-x=+=C-w=  Cancel ............. =C-g=
;;  Open a new file ...... =C-x=+=C-f=  Undo ............... =C-z=
;;  Open recent .......... =C-x=+=C-r=  Close buffer ....... =C-x=+=k=
;;  Browse directory ......=C-x=+=d=    Quit ............... =C-x=+=C-c=

(provide 'svg-tag-mode)
;;; svg-tag-mode.el ends here