;;; shut-up-test.el --- Test suite for shut-up       -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; URL: http://github.com/rejeep/shut-up.el

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

;; Test shut-up

;;; Code:

(defun shut-up-test-message-shown-p (message)
  "Determine whether MESSAGE was shown in the messages buffer."
  (let ((pattern (concat "^" (regexp-quote message) "$")))
    (with-current-buffer "*Messages*"
      (save-excursion
      (goto-char (point-min))
      (re-search-forward pattern nil 'no-error)))))

(ert-deftest shut-up/binds-the-sink-buffer-in-body ()
  (shut-up
    (should (bufferp shut-up-sink))
    (should (buffer-live-p shut-up-sink))))

(ert-deftest shut-up/silences-message ()
  (message "This message shall be visible")
  (shut-up-test-message-shown-p "This message shall be visible")
  (shut-up
    (message "This message shall be hidden")
    (should (string= "This message shall be hidden\n"
                     (shut-up-current-output)))
    (should-not (shut-up-test-message-shown-p "This message shall be hidden")))
  ;; Test that `message' is properly restored
  (message "This message shall be visible again")
  (shut-up-test-message-shown-p "This message shall be visible again"))

(ert-deftest shut-up/directs-standard-output-to-sink ()
  (shut-up
    (should (eq shut-up-sink standard-output))))

(ert-deftest shut-up/silences-princ ()
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (princ "This text is visible. ")
      (should (string= "This text is visible. " (buffer-string)))
      (shut-up
        (princ "This text is hidden. ")
        (should (string= "This text is hidden. "
                         (shut-up-current-output)))
        (should (string= "This text is visible. " (buffer-string))))
      (princ "This text is visible again.")
      (should (string= "This text is visible. This text is visible again."
                       (buffer-string))))))

(ert-deftest shut-up/silences-write-region ()
  (let ((emacs (concat invocation-directory invocation-name))
        (shut-up (symbol-file 'shut-up 'defun))
        (temp-file (make-temp-file "shut-up-test-")))
    (with-temp-buffer
      ;; We must use a sub-process, because we have no way to intercept
      ;; `write-region' messages otherwise
      (call-process emacs nil t nil "-Q" "--batch"
                    "-l" shut-up
                    "--eval" (prin1-to-string
                              `(progn
                                 (message "Start")
                                 (shut-up
                                   (write-region "Silent world" nil ,temp-file))
                                 (message "Done"))))
      (should (string= "Start\nDone\n" (buffer-string)))
      ;; Test that the overridden shut-up did it's work actually
      (with-temp-buffer
        (insert-file-contents temp-file)
        (should (string= "Silent world" (buffer-string)))))))

(provide 'shut-up-test)

;;; shut-up-test.el ends here
