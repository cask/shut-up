;;; shut-up-test.el --- Test suite for shut-up       -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
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

(require 'shut-up)
(require 's)

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
    ;; Cannot use string equality because Emacs 24.3 prints message
    ;; "ad-handle-definition: `message' got redefined".
    (should (s-ends-with? "This message shall be hidden\n"
                         (shut-up-current-output)))
    (should-not (shut-up-test-message-shown-p "This message shall be hidden")))
  ;; Test that `message' is properly restored
  (message "This message shall be visible again")
  (shut-up-test-message-shown-p "This message shall be visible again"))

(ert-deftest shut-up/handles-message-with-nil-argument ()
  (shut-up
    (message nil)
    (should (s-blank? (shut-up-current-output)))))

(ert-deftest shut-up/silences-princ ()
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (princ "This text is visible. ")
      (should (string= "This text is visible. " (buffer-string)))
      (shut-up
        (princ "This text is hidden. ")
        ;; Cannot use string equality because Emacs 24.3 prints message
        ;; "ad-handle-definition: `message' got redefined".
        (should (s-ends-with? "This text is hidden. "
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
      ;; Can not do strict equality because in Emacs-23 this message
      ;; is printed:
      ;; "This `cl-labels' requires `lexical-binding' to be non-nil"
      (should (s-contains? "Start\n" (buffer-string)))
      (should (s-contains? "Done\n" (buffer-string)))
      ;; Test that the overridden shut-up did it's work actually
      (with-temp-buffer
        (insert-file-contents temp-file)
        (should (string= "Silent world" (buffer-string)))))))

(ert-deftest shut-up/kill-sink-buffer ()
  (shut-up
   (kill-buffer shut-up-sink)
   (message "bar")
   (should (string= (shut-up-current-output) "")))
  (shut-up
   (kill-buffer shut-up-sink)
   (print "bar")
   (should (string= (shut-up-current-output) "")))
  (shut-up
   (kill-buffer shut-up-sink)
   (should (string= (shut-up-current-output) ""))))

(ert-deftest shut-up/ignore ()
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (shut-up
        (princ "foo")
        (should (s-ends-with? "foo" (shut-up-current-output)))))
    (should (string= (buffer-string) "")))
  (with-temp-buffer
    (let ((shut-up-ignore t)
          (standard-output (current-buffer)))
      (shut-up
        (princ "foo")
        (should-not (s-ends-with? "foo" (shut-up-current-output)))))
    (should (string= (buffer-string) "foo"))))

(provide 'shut-up-test)

;;; shut-up-test.el ends here
