;;; project-header-line.el --- a header-line showing path from project root -*- lexical-binding: t -*-

;; Author: Leonardo Schripsema
;; Created: 2020-05-24
;; Version: 0.2.0
;; Package-Requires: ((project "0.6.1"))
;; Keywords: header-line, mode-line, project
;; URL: https://github.com/leodag/project-header-line

;; Copyright (C) 2020-2021 Leonardo Schripsema

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
;; project.  Falls back to abbreviated path if not in a project.

;; When enabled as a global mode, will enable itself on every
;; applicable mode (per `project-header-line-global-modes'),
;; and try to enable itself when saving.

;;; Code:

(require 'project)


;;; Variables

(defgroup project-header-line nil
  "Show header line from project root."
  :prefix "project-header-line-"
  :group 'project
  :link '(url-link :tag "GitHub" "https://github.com/leodag/project-header-line")
  :link '(emacs-commentary-link :tag "Commentary" "project-header-line"))

(defcustom project-header-line-dynamic-indent t
  "Indent to align with text area and line number display."
  :type 'boolean
  :group 'project-header-line)

(defface project-header-line-project
  '((default
      :weight bold
      :inherit header-line)
    (((class color))
     :foreground "DarkRed"))
  "Face for project-header-line's project name."
  :group 'project-header-line)

(defface project-header-line-file
  '((default
      :weight bold
      :inherit header-line)
    (((class color))
     :foreground "CadetBlue3"))
  "Face for project-header-line's file name."
  :group 'project-header-line)

(defvar-local project-header-line--project-root nil
  "Current cached project root.
This has to be cached so we don't have to do I/O every header
line update.  May be updated with
`project-header-line-update-project-root'.")

(defvar project-header-line-project-root-function
  'project-header-line-project-root
  "Function that returns the current project root.
Must be a function that receives no arguments and returns the
project root for the current buffer.")

(defvar project-header-line-project-name-function
  'project-header-line-project-directory-name
  "Function that returns the project name for a project.
Must be a function that receives the current project root as the
only argument, and returns the project name.")


;;; Header line generator functions

(defun project-header-line (&optional project-root project-name file)
  "Return a header line in the format [PROJECT-NAME]/relative-path/file.
Uses faces `project-header-line-project' and
`project-header-line-file'.  The path of FILE will be found
relative to PROJECT-ROOT.  If unspecified, the arguments will be
obtained from project and the current buffer."
  (let* ((project-root (or project-root (funcall project-header-line-project-root-function)))
         (project-name (or project-name (funcall project-header-line-project-name-function
                                                 project-root)))
         (file (or file
                   (buffer-file-name)
                   ;; may start with a ~
                   (expand-file-name default-directory)))
         ;; Using file-relative-name leads to I/O and causes
         ;; severe lag with TRAMP, even locally.
         ;; abbreviate-file-name also causes I/O with TRAMP
         (file-relative (if (string-match project-root file)
                            (substring file (match-end 0))
                          (error "Project root did not match the file name, project root is %s and file name is %s" project-root file)))
         (filename (file-name-nondirectory file-relative))
         (parts (file-name-directory file-relative))
         (project-name-f (concat "["
                                 (propertize project-name
                                             'face 'project-header-line-project)
                                 "]"))
         (filename-f (propertize filename 'face 'project-header-line-file)))
    (concat project-name-f "/" parts filename-f)))

(defun project-header-line--fallback (&optional file)
  "Return a header line in the format '/path/to/file', abbreviated.
Uses the face `project-header-line-file'.  An argument FILE
may be passed to make the header line for that path."
  (let* ((file (or file (buffer-file-name)))
         (path (abbreviate-file-name (file-name-directory file)))
         (filename (file-name-nondirectory file))
         (filename-f (propertize filename 'face 'project-header-line-file)))
    (concat path filename-f)))

(defun project-header-line-project-directory-name (project-root)
  "Return a project name for the project at PROJECT-ROOT.
The name will be the project directory's filename."
  (if (string-match "/\\([^/]+\\)/\\'" project-root)
      (match-string 1 project-root)
    project-root))

(defun project-header-line-project-root ()
  "Return project.el's project root for the current file."
  (when-let (pr (project-current))
    ;; filename expanded so filename can be cut by length
    (expand-file-name (project-root pr))))

(defun project-header-line-update-project-root ()
  "Update header line's cached project root.
May be called interactively to detect a change in the project
root, including it starting to exist or ceasing to."
  (interactive)
  (setq project-header-line--project-root
        (funcall project-header-line-project-root-function)))


;;; Minor mode

;;;###autoload
(define-minor-mode project-header-line-mode
  "Shows an appropriate header line on every file buffer.
Shows a header line using `project-header-line' if visiting
a file in a project, or using `project-header-line--fallback'
if in a file outside a project."
  :group 'project-header-line
  (cond
   (project-header-line-mode
    (setq header-line-format
          '(;; aligns to the start of the text area
            ;; considers fringe, margins and scroll bar
            (project-header-line-dynamic-indent (:propertize " " display (space :align-to 0)))
            ;; aligns to line numbers
            (project-header-line-dynamic-indent
             (:eval `(:propertize " " display (space :width (,(line-number-display-width t))))))
            (project-header-line--project-root
             (:eval (project-header-line project-header-line--project-root))
             (:eval (project-header-line--fallback)))))
    (project-header-line-update-project-root))
   (t
    (kill-local-variable 'header-line-format))))


;;; Global mode

(defcustom project-header-line-global-modes t
  "Modes for which the minor mode is turned on by its global mode.
If nil, means no modes.  If t, then all major modes have it turned on.
If a list, it should be a list of `major-mode' symbol names for which
`project-header-line-mode' should be automatically turned on.  The
sense of the list is negated if it begins with `not'.  For example:
 (c-mode c++-mode)
means that `project-header-line-mode' is turned on for buffers in C
and C++ modes only.
 (not message-mode)
means that `project-header-line-mode' is always turned on except in
`message-mode' buffers."
  :group 'project-header-line
  :type '(choice (const :tag "none" nil)
                 (const :tag "all" t)
                 (set :menu-tag "mode-specific" :tag "modes"
                      :value (not)
                      (const :tag "Except" not)
                      (repeat :inline t (symbol :tag "mode")))))

(defun project-header-line--turn-on ()
  "Turn `project-header-line-mode' on if applicable."
  (when (and (not project-header-line-mode)
             (not (eq (aref (buffer-name) 0) ?\s))
             (or (buffer-file-name)
                 (derived-mode-p 'dired-mode))
             (cond ((eq project-header-line-global-modes t)
                    t)
                   ((eq (car-safe project-header-line-global-modes) 'not)
                    (not (memq major-mode (cdr project-header-line-global-modes))))
                   (t (memq major-mode project-header-line-global-modes))))
    (project-header-line-mode 1)))

;;;###autoload
(define-global-minor-mode global-project-header-line-mode
  project-header-line-mode project-header-line--turn-on
  (if project-header-line-mode
      (add-hook 'after-save-hook 'project-header-line--turn-on)
    (remove-hook 'after-save-hook 'project-header-line--turn-on)))



(provide 'project-header-line)

;;; project-header-line.el ends here
