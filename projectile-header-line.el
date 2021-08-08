;;; projectile-header-line.el --- a header-line showing path from project root -*- lexical-binding: t -*-

;; Author: Leonardo Schripsema
;; Created: 2020-05-24
;; Version: 0.1.0
;; Package-Requires: ((projectile "2.1.0") (f "0.20.0"))
;; Keywords: header-line, mode-line, project
;; URL: https://github.com/leodag/projectile-header-line

;; Copyright (C) 2020 Leonardo Schripsema

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Displays a header line showing project name and path inside
;; project.  Falls back to abbreviated path if not in a project,
;; or buffer name if buffer does not have a file name.

;; When enabled as a global mode, will enable itself on every
;; applicable mode (per `projectile-header-line-global-modes'),
;; and try to enable itself when saving.

;;; Code:

(require 'f)
(require 'projectile)


;;; Variables

(defgroup projectile-header-line nil
  "Show header line from project root."
  :prefix "projectile-header-line-"
  :group 'projectile
  :link '(url-link :tag "GitHub" "https://github.com/leodag/projectile-header-line")
  :link '(emacs-commentary-link :tag "Commentary" "projectile-header-line"))

(defcustom projectile-header-line-indent 3
  "The indentation at the start of `projectile-header-line', in columns.
Evaluated safely to a number in `header-line-format'.  Will be
overriden if `projectile-header-line-dynamic-indent' is non-nil."
  :local t
  :type 'sexp
  :group 'projectile-header-line)

(defcustom projectile-header-line-dynamic-indent t
  "Adjust indent according to fringe, margin and line number display."
  :type 'boolean
  :group 'projectile-header-line)

(defface projectile-header-line-project
  '((default
      :weight bold
      :inherit header-line)
    (((class color))
     :foreground "DarkRed"))
  "Face for projectile-header-line's project name"
  :group 'projectile-header-line)

(defface projectile-header-line-file
  '((default
      :weight bold
      :inherit header-line)
    (((class color))
     :foreground "CadetBlue3"))
  "Face for projectile-header-line's file name"
  :group 'projectile-header-line)

(defvar-local projectile-header-line--margin-indent 0
  "Indentation caused by margin.  Used by dynamic indent.")
(defvar-local projectile-header-line--line-number-indent 0
  "Indentation caused by line numbers.  Used by dynamic indent.")
(defvar projectile-header-line--fringe-indent 0
  "Indentation caused by fringe.  Used by dynamic indent.")


;;; Header line generator functions

(defun projectile-header-line (&optional project-name project-root file)
  "Return a header line in the format [project-name]/relative-path/file.
Uses the faces `projectile-header-line-project' and `projectile-project-name-file'"
  (let* ((project-name (or project-name (projectile-project-name)))
         (project-root (or project-root (projectile-project-root)))
         ;; We do this because projectile also canonicalizes paths
         (file (f-canonical (or file (buffer-file-name))))
         (filename (f-filename file))
         (parts (f-dirname (f-relative file project-root)))
         (parts-f (if (not (string= parts "./"))
                      parts
                    ""))
         (project-name-f (concat "["
                                 (propertize project-name
                                             'face 'projectile-header-line-project)
                                 "]"))
         (filename-f (propertize filename 'face 'projectile-header-line-file)))
    (concat project-name-f "/" parts-f filename-f)))

(defun projectile-header-line--fallback (&optional file)
  "Return a header line in the format '/path/to/file', abbreviated.
Uses the face `projectile-header-line-file'."
  (let* ((file (or file (buffer-file-name)))
         (path (abbreviate-file-name (f-dirname file)))
         (filename (f-filename file))
         (path-f (if (not (string= path "/"))
                     path
                   ""))
         (filename-f (propertize filename 'face 'projectile-header-line-file)))
    (concat path-f "/" filename-f)))


;;; Minor mode

;;;###autoload
(define-minor-mode projectile-header-line-mode
  "Shows a header line using `projectile-header-line' if visiting a file in a project,
or using `projectile-header-line--fallback' if in a file outside a project. Else
do not show a header line."
  :group 'projectile-header-line
  (cond
   (projectile-header-line-mode
    (setq header-line-format
          `((:eval (projectile-header-line--update-line-number-indent))
            (:propertize " " display (space :width projectile-header-line-indent))
            (:eval (cond
                    ((and (buffer-file-name) (projectile-project-p))
                     (projectile-header-line))
                    ((buffer-file-name)
                     (projectile-header-line--fallback))
                    (t
                     "%b")))))
    (kill-local-variable 'projectile-header-line-indent)
    (when projectile-header-line-dynamic-indent
      (setq projectile-header-line-indent '(+ projectile-header-line--margin-indent
                                              projectile-header-line--fringe-indent
                                              projectile-header-line--line-number-indent))

      (setq projectile-header-line--fringe-indent (/ (car (window-fringes)) (frame-char-width))
            projectile-header-line--margin-indent (or left-margin-width 0))
      (add-variable-watcher 'fringe-mode 'projectile-header-line--indent-watcher)
      (add-variable-watcher 'left-margin-width 'projectile-header-line--indent-watcher)
      (when (boundp 'display-line-numbers-mode)
        (projectile-header-line--update-line-number-indent))))
   (t
    (kill-local-variable 'header-line-format)
    (remove-variable-watcher 'fringe-mode 'projectile-header-line--indent-watcher)
    (remove-variable-watcher 'left-margin-width 'projectile-header-line--indent-watcher))))


;;; Global mode

(defcustom projectile-header-line-global-modes t
  "Modes for which the minor mode is turned on by its global mode.
If nil, means no modes.  If t, then all major modes have it turned on.
If a list, it should be a list of `major-mode' symbol names for which
`projectile-header-line-mode' should be automatically turned on.  The
sense of the list is negated if it begins with `not'.  For example:
 (c-mode c++-mode)
means that `projectile-header-line-mode' is turned on for buffers in C
and C++ modes only.
 (not message-mode)
means that `projectile-header-line-mode' is always turned on except in
`message-mode' buffers."
  :group 'projectile-header-line
  :type '(choice (const :tag "none" nil)
                 (const :tag "all" t)
                 (set :menu-tag "mode-specific" :tag "modes"
                      :value (not)
                      (const :tag "Except" not)
                      (repeat :inline t (symbol :tag "mode")))))

(defun projectile-header-line--turn-on ()
  "Turn `projectile-header-line-mode' on if applicable."
  (when (and (not projectile-header-line-mode)
             (not (eq (aref (buffer-name) 0) ?\s))
             (buffer-file-name)
             (cond ((eq projectile-header-line-global-modes t)
                    t)
                   ((eq (car-safe projectile-header-line-global-modes) 'not)
                    (not (memq major-mode (cdr projectile-header-line-global-modes))))
                   (t (memq major-mode projectile-header-line-global-modes))))
    (projectile-header-line-mode 1)))

;;;###autoload
(define-global-minor-mode global-projectile-header-line-mode
  projectile-header-line-mode projectile-header-line--turn-on
  (if projectile-header-line-mode
      (add-hook 'after-save-hook 'projectile-header-line--turn-on)
    (remove-hook 'after-save-hook 'projectile-header-line--turn-on)))


;;; Dynamic indent

(defun projectile-header-line--update-line-number-indent ()
  "Update our internal line number width.
Is run every header line update in an (:eval ...) form when ."
  (setq projectile-header-line--line-number-indent (ceiling (line-number-display-width 'columns)))
  ;; returns an empty string so it doesn't actually occupy any
  ;; space in the header line
  "")

(defun projectile-header-line--indent-watcher (symbol newval operation where)
  "Update dynamic indent according to watched variables.
Watches variables `fringe-mode' and `left-margin-width' to update
indentation when `projectile-header-line-dynamic-indent' is
true."
  ;;(message "watch %s %s %s %s" symbol newval operation (and where t))
  (pcase symbol
    ('fringe-mode
     (when (eq operation 'set)
       (let* ((left-fringe-px (or (if (consp newval)
                                      (car newval)
                                    newval)
                                  8))
              ;; summing width-1 we get the ceiling
              (left-fringe (/ (+ left-fringe-px (frame-char-width) -1) (frame-char-width))))
         (setq projectile-header-line--fringe-indent left-fringe))))
    ('left-margin-width
     (when (and (eq operation 'set) where)
       (with-current-buffer where
         (let ((left-margin (or newval 0)))
           (setq projectile-header-line--margin-indent left-margin)))))))

(provide 'projectile-header-line)

;;; projectile-header-line.el ends here
