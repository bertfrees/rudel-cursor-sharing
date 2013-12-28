;;; rudel-obby-cursor.el --- Obby extension for cursor position sharing.

;;; License:

;; Copyright (C) 2013 Bert Frees <bertfrees@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extends the Obby protocol with messages for sharing cursor positions.

;;; Code:

(require 'rudel)
(require 'rudel-operation-hooks)

;; server
(eval-after-load 'rudel-obby-server
  '(progn
     
     (defmethod rudel-obby/obby_document/move_cursor
       ((this rudel-obby-server-state-idle) document point)
       "Handle 'move_cursor' submessage of obby 'document' message."
       (with-slots (connection) this
         (with-slots (user) connection
           (with-slots (user-id) user
             (with-slots (owner-id (doc-id :id)) document
               (rudel-broadcast this (list 'exclude connection)
                                "obby_document"
                                (format "%x %x" owner-id doc-id)
                                "move_cursor"
                                (format "%x" user-id)
                                point)))))
       nil)))

;; client
(eval-after-load 'rudel-obby-client
  '(progn
     
     (defun rudel-obby-handle-cursor-move ()
       "Handle cursor movements in the current buffer"
       (let* ((connection (oref rudel-current-session :connection))
              (document (rudel-buffer-document)))
         (when document
           (with-slots (owner-id (doc-id :id)) document
             (rudel-send connection
                         "obby_document"
                         (format "%x %x" owner-id doc-id)
                         "move_cursor"
                         (format "%x" (point)))))))
     
     (defun rudel-obby-handle-cursor-move-after-local-operation (document operation)
       (with-current-buffer (oref document :buffer)
         (rudel-obby-handle-cursor-move)))
     
     (add-hook 'rudel-local-operation-hook #'rudel-obby-handle-cursor-move-after-local-operation)
     
     (defun rudel-obby-handle-cursor-move-after-command ()
       "Check for the last command and decide to send new cursor point or not."
       (when (memq this-command '(right-char left-char
                                             next-line previous-line
                                             forward-char backward-char
                                             forward-sexp backward-sexp
                                             forward-word backward-word
                                             scroll-up-command scroll-down-command))
         (rudel-obby-handle-cursor-move)))
     
     (add-hook 'rudel-document-attach-hook
               (lambda (document buffer)
                 (with-current-buffer buffer
                   (add-hook 'post-command-hook #'rudel-obby-handle-cursor-move-after-command nil t))))
     
     (add-hook 'rudel-document-detach-hook
               (lambda (document buffer)
                 (with-current-buffer buffer
                   (remove-hook 'post-command-hook #'rudel-obby-handle-cursor-move-after-command t))))
     
     (defmethod rudel-obby/obby_document/move_cursor
       ((this rudel-obby-client-state-idle) document user-id point)
       "Handle obby 'move_cursor' message."
       (with-parsed-arguments ((user-id number)
                               (point number))
                              (with-slots (session) (oref this :connection)
                                (let ((user (rudel-find-user session user-id #'= #'rudel-id)))
                                  (with-slots (buffer) document
                                    (when buffer
                                      (with-current-buffer buffer
                                        (rudel-move-cursor user point)))))))
       nil)
     
     (defun rudel-move-cursor (user point)
       "Move the cursor belonging to `user' to `point' in the current buffer"
       (with-slots (user-id) user
         (let ((overlay (cdr (assoc user-id rudel-cursor-overlays))))
           (unless overlay
             (setq overlay (rudel-make-cursor-overlay user))
             (add-to-list 'rudel-cursor-overlays (cons user-id overlay)))
           (rudel-redraw-cursor-overlay overlay point))))
     
     (require 'rudel-color)
     
     (defun rudel-make-cursor-overlay (user)
       (with-slots ((name :object-name) color) user
         (let ((overlay (make-overlay (point-min) (point-min)))
               (face (rudel-overlay-make-face-symbol 'cursor name)))
           (make-face face)
           (set-face-attribute face nil ':background color)
           (when (< 1.6 (/ (float (color-distance "white" color))
                           (float (color-distance "black" color))))
             (set-face-attribute face nil ':foreground "white"))
           (overlay-put overlay 'face face)
           (overlay-put overlay 'help-echo (format "%s's cursor" name))
           overlay)))
     
     (defun rudel-redraw-cursor-overlay (overlay &optional point)
       (let ((point (or point (overlay-start overlay))))
         (if (or (>= point (point-max))
                 (= (char-after point) ?\n))
             (progn
               (overlay-put overlay 'after-string (propertize " " 'face (overlay-get overlay 'face)))
               (move-overlay overlay point point))
           (overlay-put overlay 'after-string nil)
           (move-overlay overlay point (1+ point)))))
     
     (defun rudel-redraw-cursor-overlays-after-remote-operation (document user operation)
       (with-current-buffer (oref document :buffer)
         (with-slots (user-id) user
           (loop for (id . overlay) in rudel-cursor-overlays
                 when (/= id user-id)
                 do (rudel-redraw-cursor-overlay overlay)))))
     
     (defun rudel-redraw-cursor-overlays-after-local-operation (document operation)
       (with-current-buffer (oref document :buffer)
         (loop for (id . overlay) in rudel-cursor-overlays
               do (rudel-redraw-cursor-overlay overlay))))
     
     (add-hook 'rudel-remote-operation-hook #'rudel-redraw-cursor-overlays-after-remote-operation)
     (add-hook 'rudel-local-operation-hook #'rudel-redraw-cursor-overlays-after-local-operation)
     
     (defvar rudel-cursor-overlays)
     
     (defun rudel-init-cursor-overlays (document buffer)
       (with-current-buffer buffer
         (set (make-local-variable 'rudel-cursor-overlays) nil)))
     
     (add-hook 'rudel-document-attach-hook #'rudel-init-cursor-overlays)))

(provide 'rudel-obby-cursor)
