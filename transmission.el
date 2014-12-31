;;; transmission.el --- Interface to Transmission session

;; Copyright (C) 2014  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Keywords: comm, tools

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

;; Based on the JSON RPC library written by Christopher Wellons,
;; available online here: <https://github.com/skeeto/elisp-json-rpc>

;;; Code:

(require 'cl-lib)
(require 'json)

(defconst transmission-session-header "X-Transmission-Session-Id"
  "The \"X-Transmission-Session-Id\" header key.")

(defvar transmission-session-id nil
  "The \"X-Transmission-Session-Id\" header value.")

(define-error 'transmission-conflict
  "Wrong or missing header \"X-Transmission-Session-Id\"" 'error)

(defun transmission--move-to-content ()
  "Move the point to beginning of content after the headers."
  (setf (point) (point-min))
  (re-search-forward "\r?\n\r?\n" nil t))

(defun transmission--content-finished-p ()
  "Return non-nil if all of the content has arrived."
  (setf (point) (point-min))
  (when (search-forward "Content-Length: " nil t)
    (let ((length (read (current-buffer))))
      (and (transmission--move-to-content)
           (<= length (- (position-bytes (point-max))
                         (position-bytes (point))))))))

(defun transmission--status ()
  "Check the HTTP status code.  A 409 response from a
Transmission session includes the \"X-Transmission-Session-Id\"
header.  If a 409 is received, update `transmission-session-id'
and signal the error."
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward "HTTP/")
    (skip-chars-forward "[0-9].")
    (let* ((buffer (current-buffer))
           (status (read buffer)))
      (pcase status
        (409 (when (search-forward (format "%s: " transmission-session-header))
               (setq transmission-session-id (read buffer))
               (signal 'transmission-conflict status)))))))

(defun transmission-http-post (process content)
  (with-current-buffer (process-buffer process)
    (erase-buffer))
  (let ((path "/transmission/rpc") ; XXX: hardcoding
        (headers `((,transmission-session-header . ,transmission-session-id)
                   ("Content-length" . ,(string-bytes content)))))
    (with-temp-buffer
      (insert (format "POST %s HTTP/1.1\r\n" path))
      (dolist (elt headers)
        (insert (format "%s: %s\r\n" (car elt) (cdr elt))))
      (insert "\r\n")
      (insert content)
      (process-send-string process (buffer-string)))))

(defun transmission-wait (process)
  (with-current-buffer (process-buffer process)
    (cl-block nil
      (while t
        (when (or (transmission--content-finished-p)
                  (not (process-live-p process)))
          (transmission--status)
          (transmission--move-to-content)
          (cl-return (json-read)))
        (accept-process-output)))))

(defun transmission-ensure-process ()
  (let* ((name "transmission")
         (process (get-process name)))
    (if (and process (process-live-p process))
        process
      ;; XXX: hardcoding
      (open-network-stream name (format "*%s*" name) "localhost" 9091))))

(defun transmission-request (method &optional arguments tag)
  "Send a request to Transmission.

Details regarding the Transmission RPC can be found here:
<https://trac.transmissionbt.com/browser/trunk/extras/rpc-spec.txt>"
  (let ((process (transmission-ensure-process))
        (content (json-encode `(:method ,method :arguments ,arguments :tag ,tag))))
    (transmission-http-post process content)
    (condition-case nil
        (transmission-wait process)
      (transmission-conflict
       (transmission-http-post process content)
       (transmission-wait process)))))

(defun transmission-add (torrent)
  "Add a torrent by filename, URL, or magnet link."
  (interactive
   (let* ((prompt "Add torrent: "))
     (list (read-file-name prompt))))
  ;; perhaps test if (torrent?) file then encode it into :metainfo
  (let* ((response (transmission-request "torrent-add" `(:filename ,torrent)))
         (status (cdr (assq 'result response)))
         (arguments (cadr (assq 'arguments response))))
    (pcase status
      ("success"
       (let ((object (car-safe arguments))
             (name (cdr-safe (assq 'name arguments)))
             (id (cdr-safe (assq 'id arguments))))
         (pcase object
           ('torrent-added (message "Added #%s: %s" id name))
           ('torrent-duplicate (user-error "Already added %s" name)))))
      (_ (user-error status)))))

(defvar transmission-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "a" 'transmission-add)
    (define-key map "q" 'quit-window)
    map)
  "Keymap used in `transmission-mode' buffers.")

(define-derived-mode transmission-mode nil "Transmission"
  "Major mode for interfacing with a Transmission daemon. See
https://trac.transmissionbt.com/ for more information about
transmission.  The hook `transmission-mode-hook' is run at mode
initialization.

Key bindings:
\\{transmission-mode-map}"
  (setq buffer-read-only t)
  (run-mode-hooks 'transmission-mode-hook))

;;;###autoload
(defun transmission ()
  "Open a Transmission buffer."
  (interactive)
  (let* ((name "*Transmission*")
         (buffer (or (get-buffer name)
                     (generate-new-buffer name))))
    (switch-to-buffer-other-window buffer)
    (transmission-mode)))

(provide 'transmission)

;;; transmission.el ends here
