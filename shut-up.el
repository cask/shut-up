;;; shut-up.el --- Shut up would you!  -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014 Johan Andersson
;; Copyright (C) 2014 Sebastian Wiesner <lunaryorn@gmail.com>

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Package-Requires: ((cl-lib "0.3"))
;; Version: 0.1.1
;; URL: http://github.com/rejeep/shut-up.el

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(eval-when-compile
  (defvar dired-use-ls-dired))

;; Preserve the original definition of `write-region'
(fset 'shut-up-write-region-original (symbol-function 'write-region))

(defun shut-up-write-region (start end filename
                                   &optional append visit lockname mustbenew)
  "Like `write-region', but try to suppress any messages."
  (unless visit
    (setq visit 'no-message))
  ;; Call our "copy" of `write-region', because if this function is used to
  ;; override `write-region', calling `write-region' directly here would result
  ;; in any endless recursion.
  (shut-up-write-region-original start end filename
                                 append visit lockname mustbenew))

(defmacro shut-up-with-buffer (buffer &rest body)
  "Switch to BUFFER and execute BODY.

If BUFFER does not exists, an empty string is returned."
  (declare (indent 1))
  `(if (buffer-live-p ,buffer)
       (with-current-buffer ,buffer ,@body)
     ""))

;;;###autoload
(defmacro shut-up (&rest body)
  "Evaluate BODY with silenced output.

While BODY is evaluated, all output is redirected to a buffer.
This affects:

- `message'
- All functions using `standard-output' (e.g. `print', `princ', etc.)

Inside BODY, the buffer is bound to the lexical variable
`shut-up-sink'.  Additionally provide a lexical function
`shut-up-current-output', which returns the current contents of
`shut-up-sink' when called with no arguments."
  (declare (indent 0))
  `(let ((shut-up-sink (generate-new-buffer " *shutup*")))
     (cl-flet ((shut-up-current-output () (shut-up-with-buffer shut-up-sink
                                            (buffer-substring-no-properties
                                             (point-min) (point-max)))))
       (unwind-protect
           ;; Override `standard-output', for `print' and friends, and
           ;; monkey-patch `message'
           (cl-letf ((standard-output
                      (lambda (char)
                        (shut-up-with-buffer shut-up-sink (insert-char char))))
                     ((symbol-function 'message)
                      (lambda (fmt &rest args)
                        (shut-up-with-buffer shut-up-sink
                          (insert (apply #'format fmt args))
                          (insert "\n"))))
                     ((symbol-function 'write-region) #'shut-up-write-region))
             ,@body)
         (and (buffer-name shut-up-sink)
              (kill-buffer shut-up-sink))))))

;;;###autoload
(defun shut-up-silence-emacs ()
  "Silence Emacs.

Change Emacs settings to reduce the output.

WARNING: This function has GLOBAL SIDE-EFFECTS.  You should only
call this function in `noninteractive' sessions."
  ;; Loading vc-git...
  (remove-hook 'find-file-hooks 'vc-find-file-hook)

  ;; ls does not support --dired; see `dired-use-ls-dired' for more details.
  (eval-after-load "dired"
    '(setq dired-use-ls-dired nil)))

(provide 'shut-up)

;;; shut-up.el ends here
