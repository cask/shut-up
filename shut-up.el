;;; shut-up.el --- Shut up would you!  -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014 Johan Andersson
;; Copyright (C) 2014 Sebastian Wiesner <lunaryorn@gmail.com>

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Package-Requires: ((cl-lib "0.3"))
;; Version: 0.0.1
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
  `(let ((shut-up-sink (generate-new-buffer " *shutup*")))
     (cl-flet ((shut-up-current-output () (with-current-buffer shut-up-sink
                                            (buffer-substring-no-properties
                                             (point-min) (point-max)))))
       (unwind-protect
           ;; Override `standard-output', for `print' and friends, and
           ;; monkey-patch `message'
           (cl-letf ((standard-output shut-up-sink)
                     ((symbol-function 'message)
                      (lambda (fmt &rest args)
                        (with-current-buffer shut-up-sink
                          (insert (apply #'format fmt args))
                          (insert "\n")))))
             ,@body)
         (and (buffer-name shut-up-sink)
              (kill-buffer shut-up-sink))))))

(when noninteractive
  ;; Loading vc-git...
  (remove-hook 'find-file-hooks 'vc-find-file-hook)

  ;; ls does not support --dired; see `dired-use-ls-dired' for more details.
  (eval-after-load "dired"
    '(setq dired-use-ls-dired nil)))

(provide 'shut-up)

;;; shut-up.el ends here
