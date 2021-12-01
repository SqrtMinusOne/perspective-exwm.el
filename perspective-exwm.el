;;; perspective-exwm.el --- Yet another Pomodoro timer implementation -*- lexical-binding: t -*-

;; Copyright (C) 2021 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (burly "0.2-pre") (exwm "0.26") (perspective "2.17"))
;; Homepage: https://github.com/SqrtMinusOne/perspective-exwm.el

;; This file is NOT part of GNU Emacs.

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

;; TODO

;;; Code:
(require 'burly)
(require 'exwm)
(require 'perspective)
(require 'cl-lib)

(defgroup perspective-exwm nil
  "Integration between perspective.el and EXWM"
  :group 'frames)

(defun perspective-exwm--get-class ()
  exwm-class-name)

(defun perspective-exwm--get-title ()
  exwm-title)

(defcustom perspective-exwm-get-exwm-buffer-name #'perspective-exwm--get-class
  "A function to get the EXWM buffer title.

Meant to be ran in the context of the target buffer, e.g. with
`with-current-buffer'."
  :group 'perspective-exwm
  :type 'function
  :options '(perspective-exwm--get-class perspective-exwm--get-title))

(defcustom perspective-exwm-override-initial-name nil
  "Set initial perspective name for a particular EXWM workspace."
  :group 'perspective-exwm
  :type '(alist :key-type (integer :tag "EXWM workspace index")
                :value-type (string :tag "Initial perspective name")))

(defun perspective-exwm--cycle-exwm-buffers (dir)
  "Cycle EXWM buffers in the current perspective.

DIR is either 'forward or 'backward. A buffer is skipped if it is
already displayed in some other window of the current
perspective. The buffer name comes from
`perspective-exwm-get-exwm-buffer-name'.

The function prints out the state to the messages.  The current
buffer after the switch is highlighted with `warning', skipped
buffer is highlighted with `persp-selected-face'"
  (let* ((current (current-buffer))
         (ignore-rx (persp--make-ignore-buffer-rx))
         (visible-buffers '())
         (exwm-data
          (cl-loop for buf in (persp-current-buffers)
                   for is-another = (and (get-buffer-window buf) (not (eq current buf)))
                   if (and (buffer-live-p buf)
                           (eq 'exwm-mode (buffer-local-value 'major-mode buf))
                           (not (string-match-p ignore-rx (buffer-name buf))))
                   collect buf into all-buffers
                   and if (not is-another) collect buf into cycle-buffers
                   finally (return (list all-buffers cycle-buffers))))
         (all-buffers (nth 0 exwm-data))
         (cycle-buffers (nth 1 exwm-data))
         (current-pos (or (cl-position current cycle-buffers) -1)))
    (if (seq-empty-p cycle-buffers)
        (message "No EXWM buffers to cycle!")
      (let* ((next-pos (% (+ current-pos (length cycle-buffers)
                             (if (eq dir 'forward) 1 -1))
                          (length cycle-buffers)))
             (next-buffer (nth next-pos cycle-buffers)))
        (switch-to-buffer next-buffer)
        (message
         "%s"
         (cl-loop for buf in all-buffers
                  for name = (with-current-buffer buf (funcall perspective-exwm-get-exwm-buffer-name))
                  for is-current = (eq (current-buffer) buf)
                  for is-skip = (not (member buf cycle-buffers))
                  if is-current
                  concat (concat "[" (propertize name 'face 'warning) "] ") into res
                  else if is-skip
                  concat (concat "[" (propertize name 'face 'persp-selected-face) "] ") into res
                  else
                  concat (format " %s  " name) into res
                  finally return res))))))

;;;###autoload
(defun perspective-exwm-cycle-exwm-buffers-forward ()
  "Cycle EXWM buffers in the current perspective forward.

Take a look at `perspective-exwm--cycle-exwm-buffers' for more
detail."
  (interactive)
  (perspective-exwm--cycle-exwm-buffers 'forward))

;;;###autoload
(defun perspective-exwm-cycle-exwm-buffers-backward ()
  "Cycle EXWM buffers in the current perspective backward.

Take a look at `perspective-exwm--cycle-exwm-buffers' for more
detail."
  (interactive)
  (perspective-exwm--cycle-exwm-buffers 'backward))

;;;###autoload
(defun perspective-exwm-switch-perspective ()
  "Switch to a perspective on any workspace."
  (interactive)
  (let* ((choices
          (cl-loop for i from 0 to (1- (exwm-workspace--count))
                   for frame = (nth i exwm-workspace--list)
                   for workspace-name = (apply exwm-workspace-index-map (list i))
                   with current-workspace-index = exwm-workspace-current-index
                   append
                   (with-selected-frame frame
                     (cl-loop for persp-name being the hash-keys of (perspectives-hash)
                              with current-persp-name = (persp-current-name)
                              collect
                              (cons
                               (format "%-4s %s"
                                       (if (= i current-workspace-index)
                                           (propertize
                                            (format "[%s]" workspace-name)
                                            'face
                                            'warning)
                                         (format "[%s]" workspace-name))
                                       (if (string-equal persp-name current-persp-name)
                                           (propertize
                                            persp-name
                                            'face
                                            'persp-selected-face)
                                         persp-name))
                               (cons i persp-name))))))
         (choice (cdr (assoc (completing-read "Select a perspective: " choices) choices))))
    (exwm-workspace-switch (car choice))
    (persp-switch (cdr choice))))

;;;###autoload
(defun perspective-exwm-copy-to-workspace (&optional move)
  "Copy the current perspective to an EXWM workspace.

If MOVE is t, move the perspective instead."
  (interactive)
  (when (and move (= 1 (hash-table-count (perspectives-hash))))
    (error "Can't move the only workspace"))
  (let* ((target-workspace (exwm-workspace--prompt-for-workspace))
         (persp (persp-curr))
         (persp-name (persp-current-name))
         (url (burly-windows-url)))
    (unless (= (cl-position target-workspace exwm-workspace--list)
               exwm-workspace-current-index)
      (with-selected-frame target-workspace
        (puthash persp-name (copy-tree (copy-perspective persp)) (perspectives-hash))
        (with-perspective persp-name
          ;; (run-hooks 'persp-created-hook)
          (persp-update-modestring)))
      (when move
        (persp-kill persp-name))
      (exwm-workspace-switch target-workspace)
      (persp-switch persp-name)
      (burly-open-url url)
      (persp-update-modestring))))

;;;###autoload
(defun perspective-exwm-move-to-workspace ()
  "Move the current perspective to an EXWM workspace."
  (interactive)
  (perspective-exwm-copy-to-workspace t))

(defun perspective-exwm--delete-frame-around (fun &rest args)
  "An advice around `persp-delete-frame'.

Do not run the function if the frame is floating, because it
occasionally breaks the current perspective in the \"parent\"
frame."
  (unless (and (derived-mode-p 'exwm-mode) exwm--floating-frame)
    (apply fun args)))

(defvar perspective-exwm-workspace--create-index nil
  "Index of an EXWM workspace under creation.")

(defun perspective-exwm--workspace-switch-create-around (fun &rest args)
  "An advice around `exwm-workspace-switch-create'.

This is necessary because the frame is created with `make-frame', and
`exwm-workspace-current-index' is getting set with a hook in
`after-make-frame-functions'.  However, the inital perspective is also
initalized with the same hook, so if perspective.el is loaded before
EXWM (which is generally the case), the advice
`perspective-exwm--init-frame-around' won't have any way to know the
actual index of the current workspace.

So this advice binds the index of the workspace to be created to the
variable `perspective-exwm-workspace--create-index'."
  (let ((perspective-exwm-workspace--create-index (nth 0 args)))
    (apply fun args)))

(defun perspective-exwm--init-frame-around (fun &rest args)
  "An advice around `persp-init-frame'.

Overrides `persp-initial-frame-name' according to `perspective-exwm-override-initial-name'."
  (let* ((workspace-index
          (or (and (numberp perspective-exwm-workspace--create-index)
                   perspective-exwm-workspace--create-index)
              exwm-workspace-current-index))
         (persp-initial-frame-name
          (or
           (cdr (assoc workspace-index
                       perspective-exwm-override-initial-name))
           (format "main-%s" (funcall exwm-workspace-index-map workspace-index)))))
    (apply fun args)))

;;;###autoload
(define-minor-mode perspective-exwm-mode
  "A minor mode for intergrating perspective.el and EXWM."
  :global t
  :after-hook
  (progn
    (if perspective-exwm-mode
        (progn
          (advice-add #'persp-delete-frame
                      :around #'perspective-exwm--delete-frame-around)
          (advice-add #'persp-init-frame
                      :around #'perspective-exwm--init-frame-around)
          (advice-add #'exwm-workspace-switch-create
                      :around #'perspective-exwm--workspace-switch-create-around))
      (advice-remove #'persp-delete-frame
                     #'perspective-exwm--delete-frame-around)
      (advice-remove #'persp-init-frame
                     #'perspective-exwm--init-frame-around)
      (advice-remove #'exwm-workspace-switch-create
                     #'perspective-exwm--workspace-switch-create-around))))

(advice-mapc 'prin1-to-string 'persp-delete-frame)

(provide 'perspective-exwm)
;;; perspective-exwm.el ends here
