;;; direx.el --- Simple Directory Explorer

;; Copyright (C) 2011, 2012  Tomohiro Matsuyama

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

(require 'dired)

(require 'cl)

(defgroup direx nil
  "Directory Explorer."
  :group 'convenience
  :prefix "direx:")

(defcustom direx:leaf-icon "   "
  ""
  :type 'string
  :group 'direx)
  
(defcustom direx:open-icon "[-]"
  ""
  :type 'string
  :group 'direx)

(defcustom direx:closed-icon "[+]"
  ""
  :type 'string
  :group 'direx)



;;; Utilities

(defmacro direx:aif (test then &rest else)
  (declare (indent 2))
  `(let ((it ,test)) (if it ,then ,@else)))

(defmacro direx:awhen (test &rest body)
  (declare (indent 1))
  `(let ((it ,test)) (when it ,@body)))

(defun direx:partial (fun &rest args)
  (lexical-let ((fun fun) (args args))
    (lambda (&rest restargs)
      (apply fun (append args restargs)))))

(defun direx:starts-with (x y)
  (and (<= (length y) (length x))
       (equal (substring x 0 (length y)) y)))

(defun direx:canonical-filename (filename)
  (expand-file-name filename))

(defun direx:canonical-dirname (dirname)
  (file-name-as-directory (expand-file-name dirname)))

(defun direx:directory-basename (dirname)
  (file-name-nondirectory
   (directory-file-name
    (direx:canonical-dirname dirname))))

(defun direx:directory-dirname (dirname)
  (file-name-directory
   (directory-file-name
    (direx:canonical-dirname dirname))))

(defun direx:directory-parents (filename)
  (loop for current-dirname = (direx:canonical-filename filename) then parent-dirname
        for parent-dirname = (direx:directory-dirname current-dirname)
        while (and parent-dirname (< (length parent-dirname) (length current-dirname)))
        collect parent-dirname))



;;; Node

(defun direx:node-get (node prop)
  (plist-get node prop))

(defun direx:node-op (node op &rest args)
  (let ((obj (direx:node-get node op)))
    (if (functionp obj)
        (apply obj node args)
      obj)))

(defun direx:node-type (node)
  (direx:node-op node :type))

(defun direx:node-same-type-p (x y)
  (eq (direx:node-type x) (direx:node-type y)))

(defun direx:node-name (node)
  (direx:node-op node :name))

(defun direx:node-face (node)
  (direx:node-op node :face))

(defun direx:node-leaf-p (node)
  (direx:node-op node :leaf))

(defun direx:node-equal (x y)
  (or (eq x y) (direx:node-op x :equal y)))

(defun direx:node-contain-p (node child)
  (direx:node-op node :contain child))

(defun direx:node-children (node)
  (direx:node-op node :children))

(defun direx:node-action (node op)
  (direx:node-op node :action op))



;;; File node

(defun direx:make-filenode (filename)
  (list :type      'direx:filenode
        :name      (file-name-nondirectory filename)
        :full-name (direx:canonical-filename filename)
        :leaf      t
        :equal     'direx:filenode-equal
        :action    'direx:filenode-action))

(defun direx:filenode-full-name (filenode)
  (direx:node-op filenode :full-name))

(defun direx:filenode-action (filenode op)
  (ecase op
    (:find-node
     (find-file (direx:filenode-full-name filenode)))
    (:find-node-other-window
     (find-file-other-window (direx:filenode-full-name filenode)))))

(defun direx:filenode-equal (x y)
  (and (eq (direx:node-type x) 'direx:filenode)
       (eq (direx:node-type y) 'direx:filenode)
       (equal (direx:filenode-full-name x)
              (direx:filenode-full-name y))))



;;; Directory node

(defun direx:make-dirnode (dirname)
  (let* ((full-name (direx:canonical-dirname dirname))
         (basename (direx:directory-basename dirname))
         (name (if (zerop (length basename))
                   full-name
                 basename)))
    (list :type      'direx:dirnode
          :name      name
          :full-name full-name
          :face      'dired-directory
          :equal     'direx:dirnode-equal
          :contain   'direx:dirnode-contain-p
          :children  'direx:dirnode-children
          :action    'direx:dirnode-action)))

(defun direx:dirnode-full-name (dirnode)
  (direx:node-op dirnode :full-name))

(defun direx:dirnode-equal (x y)
  (and (eq (direx:node-type x) 'direx:dirnode)
       (eq (direx:node-type y) 'direx:dirnode)
       (equal (direx:dirnode-full-name x)
              (direx:dirnode-full-name y))))

(defun direx:dirnode-contain-p (dirnode child)
  (and (memq (direx:node-type child) '(direx:filenode direx:dirnode))
       (direx:starts-with (direx:filenode-full-name child)
                          (direx:dirnode-full-name dirnode))))

(defun direx:dirnode-children (dirnode)
  (loop with dirname = (direx:dirnode-full-name dirnode)
        for filename in (directory-files dirname t)
        for basename = (file-name-nondirectory filename)
        unless (string-match dired-trivial-filenames basename)
        if (file-directory-p filename)
        collect (direx:make-dirnode filename)
        else
        collect (direx:make-filenode filename)))

(defun direx:dirnode-action (dirnode op)
  (ecase op
    (:find-node
     (dired (direx:dirnode-full-name dirnode)))
    (:find-node-other-window
     (dired-other-window (direx:dirnode-full-name dirnode)))))



;;; Tree widget

(defstruct (direx:item (:constructor direx:make-item (node &key parent)))
  node open parent children overlay)

(defun direx:item-name (item)
  (direx:node-name (direx:item-node item)))

(defun direx:item-leaf-p (item)
  (direx:node-leaf-p (direx:item-node item)))

(defun direx:item-equal (x y)
  (direx:node-equal (direx:item-node x) (direx:item-node y)))

(defun direx:item-face (item)
  (direx:node-face (direx:item-node item)))

(defun direx:item-depth (item)
  (direx:aif (direx:item-parent item)
      (1+ (direx:item-depth it))
    0))

(defun direx:item-root (item)
  (direx:aif (direx:item-parent item)
      (direx:item-root it)
    item))

(defun direx:item-action (item op)
  (direx:node-action (direx:item-node item) op))

(defun direx:item-icon-part-offset (item)
  (* (direx:item-depth item)
     (length direx:open-icon)))

(defun direx:item-name-part-offset (item)
  (+ (direx:item-icon-part-offset item)
     (length direx:open-icon)))

(defun direx:item-at-point (&optional position)
  (get-char-property (or position (point)) 'direx:item))

(defun direx:item-at-event (event)
  (direx:awhen (posn-window (event-end event))
    (with-selected-window it
      (direx:item-at-point (posn-point (event-end event))))))

(defun direx:item-make-children (item)
  (mapcar (lambda (child-node) (direx:make-item child-node :parent item))
          (direx:node-children (direx:item-node item))))

(defun direx:item-make-indent (item)
  (make-string (direx:item-icon-part-offset item) ? ))

(defun direx:item-make-icon-part (item)
  (if (direx:item-leaf-p item)
      direx:leaf-icon
    direx:closed-icon))

(defun direx:item-make-name-part (item)
  (propertize (direx:item-name item)
              'face (direx:item-face item)
              'mouse-face 'hightlight
              'help-echo "mouse-1: toggle or find this node
mouse-2: find this node in other window"))

(defun direx:item-insert (item)
  (let ((start (point))
        (buffer-read-only nil))
    (insert
     (concat
      (direx:item-make-indent item)
      (direx:item-make-icon-part item)
      (direx:item-make-name-part item)
      "\n"))
    (let ((overlay (make-overlay start (point))))
      (overlay-put overlay 'direx:item item)
      (setf (direx:item-overlay item) overlay))
    item))

(defun* direx:item-delete (item &key recursive)
  (let* ((overlay (direx:item-overlay item))
         (start (overlay-start overlay))
         (end (overlay-end overlay))
         (buffer-read-only nil))
    (delete-overlay overlay)
    (delete-region start end)
    (when (and recursive
               (not (direx:item-leaf-p item)))
      (dolist (child (direx:item-children item))
        (direx:item-delete child :recursive t)))))

(defun direx:item-change-icon (item new-icon)
  (let ((depth (direx:item-depth item))
        (buffer-read-only nil))
    (save-excursion
      (goto-char (+ (line-beginning-position)
                    (* depth (string-width direx:open-icon))))
      (delete-char (length new-icon))
      (insert new-icon))))

(defun direx:item-show (item)
  (overlay-put (direx:item-overlay item) 'invisible nil))

(defun direx:item-hide (item)
  (overlay-put (direx:item-overlay item) 'invisible t))

(defun direx:item-show-children (item)
  (unless (direx:item-leaf-p item)
    (dolist (child (direx:item-children item))
      (direx:item-show child)
      (direx:item-show-children child))))

(defun direx:item-hide-children (item)
  (unless (direx:item-leaf-p item)
    (dolist (child (direx:item-children item))
      (direx:item-hide child)
      (direx:item-hide-children child))))

(defun direx:item-expand (item)
  (unless (direx:item-leaf-p item)
    (let ((children (direx:item-children item)))
      (unless children
        (setf children (direx:item-make-children item)
              (direx:item-children item) children)
        (save-excursion
          (goto-char (overlay-end (direx:item-overlay item)))
          (dolist (child children)
            (direx:item-insert child)))))
    (setf (direx:item-open item) t)
    (direx:item-show-children item)
    (direx:item-change-icon item direx:open-icon)))

(defun direx:item-collapse (item)
  (unless (direx:item-leaf-p item)
    (setf (direx:item-open item) nil)
    (direx:item-hide-children item)
    (direx:item-change-icon item direx:closed-icon)))

(defun direx:item-toggle (item)
  (if (direx:item-open item)
      (direx:item-collapse item)
    (direx:item-expand item)))

(defun* direx:item-refresh (item &key recursive)
  (when (and (not (direx:item-leaf-p item))
             (direx:item-children item))
    (loop with point = (overlay-end (direx:item-overlay item))
          with old-children = (direx:item-children item)
          for child in (direx:item-make-children item)
          for old-child = (find-if (direx:partial 'direx:item-equal child) old-children)
          if old-child
          do (setq child old-child
                   old-children (delq old-child old-children))
          else
          do (save-excursion
               (goto-char point)
               (direx:item-insert child))
          do (setq point (overlay-end (direx:item-overlay child)))
          collect child into new-children
          finally
          (dolist (old-child old-children)
            (direx:item-delete old-child :recursive t))
          (setf (direx:item-children item) new-children)
          (when recursive
            (dolist (new-child new-children)
              (direx:item-refresh new-child :recursive t))))))



;;; Major mode

(defun direx:make-buffer (name)
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer (direx:direx-mode))
    buffer))

(defun direx:buffer-list ()
  (loop for buffer in (buffer-list)
        if (eq (buffer-local-value 'major-mode buffer)
               'direx:direx-mode)
        collect buffer))

(defun direx:add-root-into-buffer (root buffer)
  (with-current-buffer buffer
    (save-excursion
      (let ((root-item (direx:make-item root))
            (buffer-read-only nil))
        (goto-char (point-max))
        (direx:item-insert root-item)
        (push root-item direx:root-items)))
    (direx:move-to-name-part)))

(defun direx:find-root-in-buffer (root buffer)
  (with-current-buffer buffer
    (find-if (lambda (root-item) (direx:node-equal root (direx:item-node root-item)))
             direx:root-items)))

(defun direx:find-buffers-for-root (root)
  (remove-if-not (direx:partial 'direx:find-root-in-buffer root)
                 (direx:buffer-list)))

(defun direx:find-buffer-for-root (root)
  (first (direx:find-buffers-for-root root)))

(defvar direx:direx-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n")           'direx:next-node)
    (define-key map (kbd "C-n")         'direx:next-node)
    (define-key map (kbd "<down>")      'direx:next-node)
    (define-key map (kbd "p")           'direx:previous-node)
    (define-key map (kbd "C-p")         'direx:previous-node)
    (define-key map (kbd "<up>")        'direx:previous-node)
    (define-key map (kbd "C-M-n")       'direx:next-sibling)
    (define-key map (kbd "C-M-<down>")  'direx:next-sibling)
    (define-key map (kbd "C-M-p")       'direx:previous-sibling)
    (define-key map (kbd "C-M-<up>")    'direx:previous-sibling)
    (define-key map (kbd "^")           'direx:up-node)
    (define-key map (kbd "C-M-u")       'direx:up-node)
    (define-key map (kbd "C-M-<left>")  'direx:up-node)
    (define-key map (kbd "C-M-d")       'direx:down-node)
    (define-key map (kbd "C-M-<right>") 'direx:up-node)
    (define-key map (kbd "f")           'direx:find-node)
    (define-key map (kbd "o")           'direx:find-node-other-window)
    (define-key map (kbd "RET")         'direx:maybe-find-node)
    (define-key map (kbd "TAB")         'direx:maybe-find-node)
    (define-key map (kbd "q")           'quit-window)
    (define-key map (kbd "g")           'direx:refresh-tree)
    (define-key map [mouse-1]           'direx:mouse-1)
    (define-key map [mouse-2]           'direx:mouse-2)
    map))

(defun direx:ensure-name-part-visible ())

(defun direx:move-to-name-part ()
  (direx:awhen (direx:item-at-point)
    (goto-char (+ (line-beginning-position)
                  (direx:item-name-part-offset it)))
    (direx:ensure-name-part-visible)))

(defun direx:next-node ()
  (interactive)
  (next-line)
  (direx:move-to-name-part))

(defun direx:previous-node ()
  (interactive)
  (previous-line)
  (direx:move-to-name-part))

(defun direx:up-node ()
  (interactive)
  (direx:aif (direx:item-at-point)
      (loop with parent = (direx:item-parent it)
            while (and (zerop (forward-line -1))
                       (not (eq (direx:item-at-point) parent)))
            finally (direx:move-to-name-part))
    (goto-char (point-min))))

(defun direx:down-node ()
  (interactive)
  (direx:aif (direx:item-at-point)
      (if (direx:item-leaf-p it)
          (error "No children")
        (unless (direx:item-open it)
          (direx:item-expand it))
        (if (direx:item-children it)
            (direx:next-node)
          (error "No children")))
    (direx:next-node)))

(defun direx:next-sibling (&optional arg)
  (interactive "p")
  (setq arg (if (or (null arg) (> arg 0)) 1 -1))
  (direx:aif (direx:item-at-point)
      (let* ((parent (direx:item-parent it))
             (siblings (if parent
                           (direx:item-children parent)
                         direx:root-items))
             (siblings (if (plusp arg) siblings (reverse siblings)))
             (sibling (second (memq it siblings))))
        (loop with point = (point)
              with item
              while (and sibling
                         (zerop (forward-line arg))
                         (setq item (direx:item-at-point)))
              if (eq item sibling)
              return (direx:move-to-name-part)
              finally
              (goto-char point)
              (error "No sibling")))
    (if (> arg 0)
        (direx:next-node)
      (direx:previous-node))))

(defun direx:previous-sibling (&optional arg)
  (interactive "p")
  (direx:next-sibling (if (or (null arg) (> arg 0)) -1 1)))

(defun direx:goto-node (node)
  (let ((point (point)))
    (goto-char (point-min))
    (condition-case nil
        (loop for item = (direx:item-at-point)
              for item-node = (and item (direx:item-node item))
              while item-node
              if (direx:node-equal item-node node)
              return (direx:move-to-name-part)
              else if (direx:node-contain-p item-node node)
              do (direx:down-node)
              else
              do (direx:next-sibling)
              finally (goto-char point))
      (error (goto-char point)))))

(defun direx:find-node (&optional item)
  (interactive)
  (direx:awhen (or item (direx:item-at-point))
    (direx:item-action it :find-node)))

(defun direx:find-node-other-window (&optional item)
  (interactive)
  (direx:awhen (or item (direx:item-at-point))
    (direx:item-action it :find-node-other-window)))

(defun direx:maybe-find-node (&optional item)
  (interactive)
  (direx:awhen (or item (direx:item-at-point))
    (if (direx:item-leaf-p it)
        (direx:item-action it :find-node)
      (direx:item-toggle it)
      (direx:move-to-name-part))))

(defun direx:refresh-tree (&optional item)
  (interactive)
  (direx:awhen (or item (direx:item-at-point))
    (direx:item-refresh (direx:item-root it))))

(defun direx:mouse-1 (event)
  (interactive "e")
  (direx:awhen (direx:item-at-event event)
    (direx:maybe-find-node it)))

(defun direx:mouse-2 (event)
  (interactive "e")
  (direx:awhen (direx:item-at-event event)
    (direx:find-node-other-window it)))

(define-derived-mode direx:direx-mode nil "Direx"
  ""
  (set (make-local-variable 'direx:root-items) nil)
  (setq buffer-read-only t
        truncate-lines t)
  (use-local-map direx:direx-mode-map))



;;; Directory

(defun direx:find-directory-noselect (dirname)
  (interactive "DDirex (directory): ")
  (let* ((root (direx:make-dirnode dirname))
         (buffer (direx:find-buffer-for-root root)))
    (unless buffer
      (setq buffer (direx:make-buffer (direx:node-name root)))
      (direx:add-root-into-buffer root buffer))
    buffer))

(defun direx:find-directory (dirname)
  (interactive "DDirex (directory): ")
  (switch-to-buffer (direx:find-directory-noselect dirname)))

(defun direx:find-directory-other-window (dirname)
  (interactive "DDirex (directory): ")
  (switch-to-buffer-other-window (direx:find-directory-noselect dirname)))

(defun direx:find-directory-reuse-noselect (dirname)
  (interactive "DDirex (directory): ")
  (loop for current-dirname = dirname then parent-dirname
        for parent-dirname in (direx:directory-parents dirname)
        for dirnode = (direx:make-dirnode current-dirname)
        for buffer = (direx:find-buffer-for-root dirnode)
        if buffer return buffer
        finally return (direx:find-directory-noselect dirname)))

(defun direx:find-directory-reuse (dirname)
  (interactive "DDirex (directory): ")
  (switch-to-buffer (direx:find-directory-reuse-noselect dirname)))

(defun direx:find-directory-reuse-other-window (dirname)
  (interactive "DDirex (directory): ")
  (switch-to-buffer-other-window (direx:find-directory-reuse-noselect dirname)))

(defun direx:maybe-goto-current-node-in-directory (buffer)
  (let ((filename buffer-file-name)
        (dirname default-directory))
    (with-current-buffer buffer
      (ignore-errors
        (cond (filename
               (direx:goto-node (direx:make-filenode filename)))
              (dirname
               (direx:goto-node (direx:make-dirnode dirname))))))))

(defun direx:jump-to-directory-noselect ()
  (interactive)
  (let ((buffer (direx:find-directory-reuse-noselect default-directory)))
    (direx:maybe-goto-current-node-in-directory buffer)
    buffer))

(defun direx:jump-to-directory ()
  (interactive)
  (switch-to-buffer (direx:jump-to-directory-noselect)))

(defun direx:jump-to-directory-other-window ()
  (interactive)
  (switch-to-buffer-other-window (direx:jump-to-directory-noselect)))

(provide 'direx)
;;; direx.el ends here
