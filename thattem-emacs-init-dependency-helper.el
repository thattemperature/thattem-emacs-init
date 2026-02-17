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
  "Simplify the dependency list by removing the dependency of dependency.
PACKAGE-LIST should be a list of package (symbol)."
  (let ((simple-list nil)
        (full-list nil))
    (dolist (package package-list)
      (when (assq package package-alist)
        (unless (assq package full-list)
          (let ((package-full
                 (thattem--get-full-dependencies (list (list package)))))
            (setq simple-list
                  (-snoc (--remove (assq it package-full) simple-list)
                         package))
            (setq full-list (append package-full full-list))))))
    simple-list))


(provide 'thattem-emacs-init-dependency-helper)
;;; thattem-emacs-init-dependency-helper.el ends here
