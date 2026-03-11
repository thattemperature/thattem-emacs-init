;;; dependency-helper --- helper of package dependency  -*- lexical-binding: t; -*-

;; Author: That Temperature <2719023332@qq.com>
;; URL: https://github.com/thattemperature/thattem-emacs-init

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

;;; Code:

(require 'dash)
(require 'package)

;;; Deal package version

(defconst thattem--gnu-elpa-source
  "https://elpa.gnu.org/packages/"
  "The URL of gnu elpa.")

(defconst thattem--nongun-elpa-source
  "https://elpa.nongnu.org/nongnu/"
  "The URL of nongnu elpa.")

(defconst thattem--melpa-stable-source
  "https://stable.melpa.org/packages/"
  "The URL of melpa stable.")

(defcustom thattem--package-archives
  (list thattem--melpa-stable-source
        thattem--nongun-elpa-source
        thattem--gnu-elpa-source)
  "Available package archives, the former takes precedence."
  :type '(repeat string)
  :group 'package)

(defun thattem--package-archive-content (archive)
  "Get the content of the ARCHIVE."
  (with-temp-buffer
    (url-insert-file-contents
     (file-name-concat archive
                       "archive-contents"))
    (read (current-buffer))))

(defvar thattem--package-archive-content
  nil
  "Cached archive contents.")

(defun thattem--cache-package-archive ()
  "Cache package archive contents."
  (setq thattem--package-archive-content
        (-map #'thattem--package-archive-content
              thattem--package-archives)))

(defun thattem--archive-package-version (package archive)
  "Return the version of PACKAGE from ARCHIVE as a list.
PACKAGE should be a symbol and ARCHIVE should be the return value of
\\='thattem--package-archive-content\\='."
  (let ((pkg-entry (assq package (cdr archive))))
    (when pkg-entry
      (package-desc-version (package--from-builtin pkg-entry)))))

(defun thattem--package-version (package)
  "Return the version of PACKAGE."
  (unless thattem--package-archive-content
    (thattem--cache-package-archive))
  (--some (thattem--archive-package-version package it)
          thattem--package-archive-content))

;;; Deal dependencies

(defun thattem--get-package-dependencies (package)
  "Return the dependency list of PACKAGE."
  (when (atom package)
    (setq package (cons package '((0)))))
  (let ((item (assq (car package) package-alist)))
    (when item (package-desc-reqs (cadr item)))))

(defun thattem--get-extended-package-dependencies (package)
  "Return the dependency list of PACKAGE, with PACKAGE itself added."
  (when (atom package)
    (setq package (cons package '((0)))))
  (let ((dependency-list
         (thattem--get-package-dependencies package)))
    (if (assq (car package) dependency-list)
        dependency-list
      (cons package dependency-list))))

(defun thattem--get-full-dependencies (package-list)
  "Return the full dependency list of PACKAGE-LIST.
i.e. including dependency of dependency."
  (let ((new-list
         (let ((extended-list
                (apply #'append
                       (-map
                        #'thattem--get-extended-package-dependencies
                        package-list))))
           (--reduce-from
            (if-let (existing (-find
                               (lambda (it2) (eq (car it) (car it2)))
                               acc))
                (if (version-list-< (cadr existing) (cadr it))
                    (-replace existing it acc)
                  acc)
              (cons it acc))
            nil
            extended-list))))
    (let ((new-sorted (--sort (string< (car it) (car other)) new-list)))
      (if (equal new-sorted package-list)
          package-list
        (thattem--get-full-dependencies new-sorted)))))

(defun thattem--simplify-dependency-list (package-list)
  "Simplify the PACKAGE-LIST by removing the dependency of dependency."
  (let ((simple-list nil)
        (full-list nil)
        (package-list (--map
                       (if (atom it) it (car it))
                       package-list)))
    (dolist (package package-list)
      (when (assq package package-alist)
        (unless (assq package full-list)
          (let ((package-full
                 (thattem--get-full-dependencies (list (list package)))))
            (setq simple-list
                  (-snoc (--remove (assq it package-full) simple-list)
                         package))
            (setq full-list (append package-full full-list))))))
    (sort simple-list)))

(defun thattem--dependency-list-add-version (package-list)
  "Add the newest version of each package in PACKAGE-LIST if possible."
  (--map
   (if-let* ((version (thattem--package-version it))
             (string (package-version-join version)))
       (list it string)
     it)
   package-list))


;;;###autoload
(defun thattem-deal-package-requires ()
  "Find the \"Package-Requires\" of current buffer and modify it."
  (interactive)
  (if-let*
      ((start-point
        (save-excursion
          (goto-char (point-min))
          (re-search-forward "^;+ +Package-Requires: *" nil t)))
       (end-point
        (scan-sexps start-point 1))
       (origin-list
        (read (buffer-substring-no-properties
               start-point end-point))))
      (let* ((simplified-list
              (thattem--simplify-dependency-list origin-list))
             (final-list
              (thattem--dependency-list-add-version simplified-list)))
        (if (equal origin-list final-list)
            (message "Already done!")
          (delete-region start-point end-point)
          (goto-char start-point)
          (insert (prin1-to-string final-list))))
    (user-error "Cannot get \"Package-Requires\"")))


(provide 'thattem-emacs-init-dependency-helper)
;;; thattem-emacs-init-dependency-helper.el ends here
