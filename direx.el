;;; direx.el --- Directory Explorer

;; Copyright (C) 2011  Tomohiro Matsuyama

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

(eval-when-compile (require 'cl))

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
  (or (eq x y)
      (direx:node-op x :equal y)))

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
                          (direx:dirnode-full-name dirnode))
       t))

(defun direx:dirnode-children (dirnode)
  (let ((dirname (direx:dirnode-full-name dirnode)))
    (loop for filename in (directory-files dirname t)
          for basename = (file-name-nondirectory filename)
          unless (string-match dired-trivial-filenames basename)
          if (file-directory-p filename)
          collect (direx:make-dirnode filename)
          else
          collect (direx:make-filenode filename))))

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

(defun direx:item-face (item)
  (direx:node-face (direx:item-node item)))

(defun direx:item-depth (item)
  (let ((parent (direx:item-parent item)))
    (if parent
        (1+ (direx:item-depth parent))
      0)))

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

(defun direx:make-item-indent (item)
  (make-string (direx:item-icon-part-offset item) ? ))

(defun direx:make-item-icon-part (item)
  (if (direx:item-leaf-p item)
      direx:leaf-icon
    direx:closed-icon))

(defun direx:make-item-name-part (item)
  (propertize (direx:item-name item)
              'face (direx:item-face item)
              'mouse-face 'hightlight
              'help-echo "mouse-1: toggle or find this node
mouse-2: find this node in other window"))

(defun direx:insert-item (item)
  (let ((start (point))
        (buffer-read-only nil))
    (insert
     (concat
      (direx:make-item-indent item)
      (direx:make-item-icon-part item)
      (direx:make-item-name-part item)
      "\n"))
    (let ((overlay (make-overlay start (point))))
      (overlay-put overlay 'direx:item item)
      (setf (direx:item-overlay item) overlay))
    item))

(defun direx:change-item-icon (item new-icon)
  (let ((depth (direx:item-depth item))
        (buffer-read-only nil))
    (save-excursion
      (goto-char (+ (line-beginning-position) ; line-beginning-position?
                    (* depth (string-width direx:open-icon))))
      (delete-char (length new-icon))
      (insert new-icon))))

(defun direx:show-item (item)
  (overlay-put (direx:item-overlay item) 'invisible nil))

(defun direx:hide-item (item)
  (overlay-put (direx:item-overlay item) 'invisible t))

(defun direx:show-item-children (item)
  (when (and (not (direx:item-leaf-p item))
             (direx:item-open item))
    (dolist (child (direx:item-children item))
      (direx:show-item child)
      (direx:show-item-children child))))

(defun direx:hide-item-children (item)
  (when (and (not (direx:item-leaf-p item))
             (not (direx:item-open item)))
    (dolist (child (direx:item-children item))
      (direx:hide-item child)
      (direx:hide-item-children child))))

(defun direx:expand-item (item)
  (unless (direx:item-leaf-p item)
    (let ((children (direx:item-children item)))
      (unless children
        (setf children (mapcar (lambda (child-node)
                                 (direx:make-item child-node :parent item))
                               (direx:node-children (direx:item-node item)))
              (direx:item-children item) children)
        (save-excursion
          (goto-char (overlay-end (direx:item-overlay item)))
          (dolist (child children)
            (direx:insert-item child)))))
    (setf (direx:item-open item) t)
    (direx:show-item-children item)
    (direx:change-item-icon item direx:open-icon)))

(defun direx:collapse-item (item)
  (when (and (not (direx:item-leaf-p item))
             (direx:item-open item))
    (setf (direx:item-open item) nil)
    (direx:hide-item-children item)
    (direx:change-item-icon item direx:closed-icon)))

(defun direx:toggle-item (item)
  (if (direx:item-open item)
      (direx:collapse-item item)
    (direx:expand-item item)))



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
        (direx:insert-item root-item)
        (push root-item direx:root-items)))
    (direx:move-to-name-part)))

(defun direx:find-root-in-buffer (root buffer)
  (with-current-buffer buffer
    (loop for root-item in direx:root-items
          if (direx:node-equal root (direx:item-node root-item))
          return root-item)))

(defun direx:find-buffers-for-root (root)
  (loop for buffer in (direx:buffer-list)
        if (direx:find-root-in-buffer root buffer)
        collect buffer))

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
    (define-key map (kbd "q")           'quit-window)
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
      (progn
        (loop with parent = (direx:item-parent it)
              while (and (zerop (forward-line -1))
                         (not (eq (direx:item-at-point) parent))))
        (direx:move-to-name-part))
    (goto-char (point-min))))

(defun direx:down-node ()
  (interactive)
  (direx:aif (direx:item-at-point)
      (if (direx:item-leaf-p it)
          (error "No children")
        (unless (direx:item-open it)
          (direx:expand-item it))
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
      (error () (goto-char point)))))
    
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
      (direx:toggle-item it)
      (direx:move-to-name-part))))

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
  (loop repeat 16                       ; make sure finite loop
        for current-dirname = dirname then parent-dirname
        for parent-dirname = (direx:directory-dirname current-dirname)
        for dirnode = (direx:make-dirnode current-dirname)
        for buffer = (direx:find-buffer-for-root dirnode)
        if buffer
        return buffer
        if (equal current-dirname parent-dirname)
        return (direx:find-directory-noselect dirname)))

(defun direx:find-directory-reuse (dirname)
  (interactive "DDirex (directory): ")
  (switch-to-buffer (direx:find-directory-reuse-noselect dirname)))

(defun direx:find-directory-reuse-other-window (dirname)
  (interactive "DDirex (directory): ")
  (switch-to-buffer-other-window (direx:find-directory-reuse-noselect dirname)))

(defun direx:jump-to-directory-noselect ()
  (interactive)
  (let* ((dirname default-directory)
         (filename buffer-file-name)
         (buffer (direx:find-directory-reuse-noselect dirname)))
    (with-current-buffer buffer
      (ignore-errors
        (direx:goto-node
         (if filename
             (direx:make-filenode filename)
           (direx:make-dirnode dirname)))))
    buffer))

(defun direx:jump-to-directory ()
  (interactive)
  (switch-to-buffer (direx:jump-to-directory-noselect)))

(defun direx:jump-to-directory-other-window ()
  (interactive)
  (switch-to-buffer-other-window (direx:jump-to-directory-noselect)))

(provide 'direx)
;;; direx.el ends here
