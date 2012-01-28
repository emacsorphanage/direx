;;; direx-imenu.el --- Imenu Module for Direx

;; Copyright (C) 2012  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <tomo@cx4a.org>
;; Keywords: convenience

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

;; 

;;; Code:

(require 'cl)
(require 'direx)

(defun direx-imenu:make-point-node (point)
  (direx:make-node :type 'direx-imenu:point-node
                   :point point
                   :leaf t))

(defun direx-imenu:node-start (node)
  (direx:node-op node :start))

(defun direx-imenu:node-end (node)
  (direx:node-op node :end))

(defun direx-imenu:node-point (node)
  (direx:node-op node :point))

(defun direx-imenu:node-contain-p (node child)
  (direx:aif (direx:node-children node)
      (some (lambda (node-child) (direx-imenu:node-contain-p node-child child))
            it)
    (and (eq (direx:node-type child) 'direx-imenu:point-node)
         (number-or-marker-p (direx-imenu:node-start node))
         (number-or-marker-p (direx-imenu:node-end node))
         (<= (direx-imenu:node-start node) (direx-imenu:node-point child))
         (< (direx-imenu:node-point child) (direx-imenu:node-end node)))))

(defun direx-imenu:node-action (node op)
  (let ((buffer (direx:node-get node :buffer))
        (start (direx-imenu:node-start node)))
    (ecase op
      (:find-node
       (switch-to-buffer buffer)
       (goto-char start))
      (:find-node-other-window
       (switch-to-buffer-other-window buffer)
       (goto-char start)))))

(defun direx-imenu:make-segments (imenu min max)
  (let (segments)
    (direx:walk-tree (lambda (object)
                       (when (number-or-marker-p object)
                         (push object segments)))
                     imenu)
    (sort (cons min (nreverse (cons max segments))) '<)))

(defun direx-imenu:make-node (imenu buffer segments)
  (let ((name (car imenu))
        (info (cdr imenu)))
    (if (consp info)
        (direx:make-node :type 'direx-imenu:node
                         :name name
                         :children (direx-imenu:make-node-children info buffer segments)
                         :contain 'direx-imenu:node-contain-p)
      (direx:make-node :type 'direx-imenu:leaf
                       :name name
                       :leaf t
                       :start info
                       :end (second (memq info segments))
                       :buffer buffer
                       :contain 'direx-imenu:node-contain-p
                       :action 'direx-imenu:node-action))))

(defun direx-imenu:make-node-children (imenus buffer segments)
  (mapcar (lambda (sub-imenu) (direx-imenu:make-node sub-imenu buffer segments))
          imenus))

(defun direx-imenu:make-root (buffer)
  (with-current-buffer buffer
    (let* ((name (format "Imenu %s" (buffer-name buffer)))
           (imenu (imenu--make-index-alist))
           (segments (direx-imenu:make-segments imenu (point-min) (point-max)))
           (root (direx-imenu:make-node (cons name imenu) buffer segments)))
      (direx:node-set root :buffer buffer)
      root)))

(defun direx-imenu:maybe-goto-current-node (point buffer)
  (with-current-buffer buffer
    (ignore-errors
      (direx:goto-node (direx-imenu:make-point-node point)))))

(defun direx-imenu:imenu-noselect ()
  (interactive)
  (let* ((root-item
          (direx:find-root-item-if
           (lambda (root-item)
             (let ((root (direx:item-node root-item)))
               (and root
                    (eq (direx:node-type root) 'direx-imenu:node)
                    (eq (direx:node-get root :buffer) (current-buffer)))))))
         (buffer (if root-item
                     (direx:item-buffer root-item)
                   (direx:ensure-buffer-for-root
                    (direx-imenu:make-root (current-buffer))))))
    (direx-imenu:maybe-goto-current-node (point) buffer)
    buffer))

(defun direx-imenu:imenu ()
  (interactive)
  (switch-to-buffer (direx-imenu:imenu-noselect)))

(defun direx-imenu:imenu-other-window ()
  (interactive)
  (switch-to-buffer-other-window (direx-imenu:imenu-noselect)))

(provide 'direx-imenu)
;;; direx-imenu.el ends here
