;; open-test.el - Open and run rails unit tests in emacs

;; Author: Justin Weiss
;; URL: http://github.com/justinweiss/open-test.el
;; Version: 1.0
;; Created: 2010-05-11
;; Keywords: project, convenience
 
;; This file is NOT part of GNU Emacs.
 
;;; License:
 
;; Copyright (c) 2010 Justin Weiss
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary: 

;; Not yet ready for prime time, but it works for me.

;;; Usage:
;; open-test.el provides two main interactive functions:
;;
;; - `open-test' will try to guess the filename of the test covering
;;   `current-buffer''s file and find that file.  
;;
;; - `run-test', when run in a buffer that looks like a ruby test
;;   file, will attempt to run the tests in that file. It runs it in a
;;   compilation buffer, so links to files in backtraces should work.

;;; Configuration:
;; In your .emacs or init.el: 
;;
;; (add-to-list 'load-path "~/.emacs.d/path/to/open-test")
;; (require 'open-test)

;;; TODO: 
;; - real documentation 

;;; Code:


;; Returns true if `pred' is non-nil for all elements in `list', `nil'
;; otherwise.
(defun ot-all-p (pred list)
  (cond
   ((equal list ()) t)
   ((apply pred (list (car list))) (ot-all-p pred (cdr list)))
   (nil)))

;; Predicate returning non-nil if `file-name' refers to a ruby file.
(defun ot-ruby-file-p (file-name)
  (equal (file-name-extension file-name) "rb"))

;; Predicate returning non-nil if `file-name' refers to a ruby test file.
(defun ot-ruby-test-file-p (file-name)
  (string-ends-with-p file-name "_test.rb"))

;; Returns our best guess as to the root directory of the project
;; containing `file-name'.
(defun ot-project-root (file-name)
  (ot-find-dir (lambda (dir-name)
                 (ot-all-p (lambda (e) 
                             (file-exists-p (concat dir-name e)))
                           '("test" "lib"))) file-name))

(defun ot-test-type (file-name)
  (let* ((test-types-alist '(("models" . "unit") ("controllers" . "functional") ("lib" . "unit"))))
    (ot-alist-get test-types-alist 
                  (and file-name 
                       (file-name-nondirectory (ot-trim-directory (ot-dir-in-alist test-types-alist file-name)))))))

(defun ot-dir-in-alist (alist file-name)
  (ot-find-dir (lambda (dir-name)
                 (ot-alist-get alist (file-name-nondirectory (ot-trim-directory dir-name)))) file-name))

(defun ot-alist-get (alist key)
  (cond ((equal nil alist) nil)
        ((equal key (car (car alist))) (cdr (car alist)))
        ((ot-alist-get (cdr alist) key))))

(defun ot-find-dir (pred file-name)
  (let ((dir-name (file-name-directory file-name)))
    (if (and dir-name (apply pred (list dir-name)))
        dir-name
      (and dir-name (ot-find-dir pred (substring dir-name 0 -1))))))

(defun ot-test-name (file-name)
  (concat (ot-test-directory file-name) (file-name-sans-extension (file-name-nondirectory file-name)) "_test.rb"))

(defun ot-test-directory (file-name)
  (concat (ot-project-root file-name) "test/" (ot-test-type file-name) "/")) 

(defun ot-trim-directory (dir-name)
   (if dir-name 
       (substring dir-name 0 -1)
     ""))

(defun open-test ()
  (interactive)
  (when (and (ot-ruby-file-p (buffer-file-name)) (ot-project-root (buffer-file-name)) (ot-test-type (buffer-file-name)))
    (find-file-other-window (ot-test-name (buffer-file-name)))))

(defvar ot-test-process nil)

(defun ot-include-path (file-name)
  (concat (ot-project-root file-name) "test:" (ot-project-root file-name) "lib "))

(defun run-test ()
  (interactive)
  (let* ((buffer-name "*Test Results*")
         (test-buffer-name (buffer-file-name)))
    (if (get-buffer buffer-name)
        (kill-buffer buffer-name))
    (when (and (ot-ruby-test-file-p test-buffer-name))
      (get-buffer-create buffer-name)
      (set-buffer buffer-name)
      (cd (ot-project-root test-buffer-name))
      (compilation-mode)
      (and ot-test-process (equal 'run (process-status ot-test-process)) (interrupt-process ot-test-process))
      (erase-buffer)
      (setq ot-test-process (start-process "ruby-test-process" buffer-name "ruby" (concat "-I" (ot-include-path test-buffer-name)) test-buffer-name ))
      (display-buffer buffer-name))))
 
(provide 'open-test)
