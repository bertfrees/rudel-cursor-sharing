;;; rudel-cursor-sharing.el --- Enables sharing of cursor positions.

;;; License:

;; Copyright (C) 2013 Bert Frees <bertfrees@gmail.com>
;; Author: Bert Frees <bertfrees@gmail.com>
;; URL: https://github.com/bertfrees/rudel-cursor-sharing
;; Keywords: rudel, obby
;; Package-Requires: ((rudel "0.3alpha1"))

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

;; Enables sharing of cursor positions with Obby backend.

;;; Installation:

;; Clone rudel-cursor-sharing and add the root directory to your load-path:
;;
;;     (add-to-list 'load-path "/path/to/rudel-cursor-sharing")
;;
;; el-get users can install rudel-cursor-sharing with the following recipe:
;;
;;     (:name rudel-cursor-sharing
;;            :type git
;;            :url "http://github.com/bertfrees/rudel-cursor-sharing.git"
;;            :checkout "alpha"
;;            :depends (rudel))
;;
;; rudel-cursor-sharing is also available as an ELPA package. First you need
;; to add `bertfrees.github.com/elpa' to the list of repositories:
;;
;;     (add-to-list 'package-archives '("bertfrees" . "http://bertfrees.github.com/elpa/packages/"))
;;     (package-install 'rudel-cursor-sharing)

;;; Usage:

;;     (eval-after-load 'rudel
;;       '(load "rudel-cursor-sharing.el"))

;;; Code:

(require 'rudel-obby-cursor)
