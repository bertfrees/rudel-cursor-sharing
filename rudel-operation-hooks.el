;;; rudel-operation-hooks.el --- Hooks for local and remote operations.

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

;; Hooks for local and remote operations.

;;; Code:

(require 'rudel)

(defvar rudel-remote-operation-hook nil)
(defvar rudel-local-operation-hook nil)

(defmethod rudel-remote-operation :after ((this rudel-document) user operation)
  (with-slots (buffer) document
    (when (and buffer (not (string-match-p "^ \\*.*\\*$" (buffer-name buffer))))
      (run-hook-with-args 'rudel-remote-operation-hook this user operation))))

(defmethod rudel-local-operation :after ((this rudel-document) operation)
  (run-hook-with-args 'rudel-local-operation-hook this operation))

(provide 'rudel-operation-hooks)
