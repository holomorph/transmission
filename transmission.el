;;; transmission.el --- Interface to Transmission session -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015  Mark Oteiza <mvoteiza@udel.edu>

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

(defgroup transmission nil
  "Interface to a Transmission session."
  :group 'external)

(defcustom transmission-host "localhost"
  "Host name or IP address of the Transmission session."
  :type 'string
  :group 'transmission)

(defcustom transmission-service 9091
  "Port or name of the service for the Transmission session."
  :type '(choice (string :tag "Service")
                 (integer :tag "Port"))
  :group 'transmission)

(defcustom transmission-path "/transmission/rpc"
  "Path to the Transmission session RPC interface."
  :type 'string
  :group 'transmission)

(defcustom transmission-file-size-units nil
  "The flavor of units used to display file sizes using
`file-size-human-readable'."
  :type '(choice (const :tag "Default" nil)
                 (symbol :tag "SI" si)
                 (symbol :tag "IEC" iec))
  :group 'transmission)

(defconst transmission-status-alist
  '((0 . "stopped")
    (1 . "checkwait")
    (2 . "check")
    (3 . "downwait")
    (4 . "downloading")
    (5 . "seedwait")
    (6 . "seeding"))
  "Alist of possible Transmission torrent statuses.")

(defconst transmission-torrent-get-fields
  '("id" "name" "status" "eta"
    "rateDownload" "rateUpload"
    "percentDone" "sizeWhenDone"
    "uploadRatio"))

(defconst transmission-session-header "X-Transmission-Session-Id"
  "The \"X-Transmission-Session-Id\" header key.")

(defvar transmission-session-id nil
  "The \"X-Transmission-Session-Id\" header value.")

(define-error 'transmission-conflict
  "Wrong or missing header \"X-Transmission-Session-Id\"" 'error)


;; JSON RPC

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
  (let ((path transmission-path)
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

(defun transmission-send (process content)
  (transmission-http-post process content)
  (transmission-wait process))

(defun transmission-ensure-process ()
  (let* ((name "transmission")
         (process (get-process name)))
    (if (and process (process-live-p process))
        process
      (open-network-stream name (format "*%s" name)
                           transmission-host
                           transmission-service))))

(defun transmission-request (method &optional arguments tag)
  "Send a request to Transmission.

Details regarding the Transmission RPC can be found here:
<https://trac.transmissionbt.com/browser/trunk/extras/rpc-spec.txt>"
  (let ((process (transmission-ensure-process))
        (content (json-encode `(:method ,method :arguments ,arguments :tag ,tag))))
    (unwind-protect
        (condition-case nil
            (transmission-send process content)
          (transmission-conflict
           (transmission-send process content)))
      (when (and process (process-live-p process))
        (delete-process process)
        (kill-buffer (process-buffer process))))))


;; Response parsing

(defun transmission-value (response field)
  "Return the value associated with FIELD in the \"arguments\"
array of RESPONSE."
  (let* ((arguments (assq 'arguments response))
         (element (assq field arguments)))
    (cdr element)))

(defun transmission-torrents (response)
  "Return the \"torrents\" vector associated with the response
from a \"torrent-get\" request."
  (cdr (cadr (assq 'arguments response))))

(defun transmission-torrents-value (torrents index field)
  "Return value in FIELD of elt INDEX in TORRENTS, the
\"torrents\" vector returned by `transmission-torrents'."
  (cdr (assq field (elt torrents index))))


;; Other

(defun transmission-eta (seconds)
  "Return a string showing SECONDS in human-readable form;
otherwise \"Done\" if SECONDS is non-positive."
  (if (<= seconds 0) "Done"
    (let* ((minute (float 60))
           (hour (float 3600))
           (day (float 86400))
           (month (* 29.53 day))
           (year (* 365.25 day)))
      (apply 'format "%3.0f%s"
             (pcase seconds
               ((pred (> minute)) (list seconds "s"))
               ((pred (> hour)) (list (/ seconds minute) "m"))
               ((pred (> day)) (list (/ seconds hour) "h"))
               ((pred (> month)) (list (/ seconds day) "d"))
               ((pred (> year)) (list (/ seconds month) "M"))
               (_ (list (/ seconds year) "y")))))))

(defun transmission-prompt-speed-limit (upload)
  "Make a prompt to set transfer speed limit.  If UPLOAD is
non-nil, make a prompt for upload rate, otherwise for download
rate."
  (let* ((response (transmission-request "session-get"))
         (limit (transmission-value response (if upload 'speed-limit-up 'speed-limit-down)))
         (enabled (eq t (transmission-value response (if upload 'speed-limit-up-enabled 'speed-limit-down-enabled)))))
    (list (read-number (concat "Set global " (if upload "upload" "download") " limit ("
                               (if enabled (format "%d KB/s" limit)
                                 "disabled")
                               "): ")))))


;; Interactive

(defun transmission-next ()
  (interactive)
  (pcase (get-text-property (point) 'transmission)
    ('torrent (transmission-next-torrent))
    (_ (forward-line))))

(defun transmission-previous ()
  (interactive)
  (pcase (get-text-property (point) 'transmission)
    ('torrent (transmission-previous-torrent))
    (_ (forward-line -1))))

(defun transmission-next-torrent ()
  "Skip to the next torrent."
  (interactive)
  (let* ((id (get-text-property (point) 'id))
         (skip (text-property-any (point) (point-max) 'id id)))
    (if (or (eobp)
            (not (setq skip (text-property-not-all skip (point-max)
                                                   'id id))))
        (message "No next torrent")
      (when (not (get-text-property skip 'id))
          (save-excursion
            (goto-char skip)
            (setq skip (text-property-not-all skip (point-max)
                                              'id nil))))
      (if skip (goto-char skip)
        (message "No next torrent")))))

(defun transmission-previous-torrent ()
  "Skip to the previous torrent."
  (interactive)
  (let ((id (get-text-property (point) 'id))
        (start (point))
        (found nil))
    ;; Skip past the current link.
    (while (and (not (bobp))
                (numberp id)
                (eq id (get-text-property (point) 'id)))
      (forward-char -1))
    ;; Find the previous link.
    (while (and (not (bobp))
                (or (not (numberp (get-text-property (point) 'id)))
                    (not (setq found (= id (get-text-property (point) 'id))))))
      (forward-char -1)
      (setq id (get-text-property (point) 'id)))
    (if (not found)
        (progn
          (message "No previous torrent")
          (goto-char start))
      ;; Put point at the start of the link.
      (while (and (not (bobp))
                  (eq id (get-text-property (point) 'id)))
        (forward-char -1))
      (and (not (bobp)) (forward-char 1)))))

(defun transmission-add (torrent)
  "Add a torrent by filename, URL, or magnet link."
  (interactive
   (let* ((prompt "Add torrent: "))
     (list (read-file-name prompt))))
  (let* ((response (transmission-request "torrent-add" `(:filename ,torrent)))
         (result (cdr (assq 'result response)))
         (arguments (cadr (assq 'arguments response))))
    (pcase result
      ("success"
       (let ((object (car-safe arguments))
             (name (cdr-safe (assq 'name arguments))))
         (pcase object
           ('torrent-added (message "Added %s" name))
           ('torrent-duplicate (user-error "Already added %s" name)))))
      (_ (user-error result)))))

(defun transmission-set-download (limit)
  "Set global download speed limit in KB/s."
  (interactive (transmission-prompt-speed-limit nil))
  (let ((arguments (if (<= limit 0) '(:speed-limit-down-enabled :json-false)
                     `(:speed-limit-down-enabled t :speed-limit-down ,limit))))
    (transmission-request "session-set" arguments)))

(defun transmission-set-upload (limit)
  "Set global upload speed limit in KB/s."
  (interactive (transmission-prompt-speed-limit t))
  (let ((arguments (if (<= limit 0) '(:speed-limit-up-enabled :json-false)
                     `(:speed-limit-up-enabled t :speed-limit-up ,limit))))
    (transmission-request "session-set" arguments)))

(defun transmission-toggle ()
  "Toggle torrent between started and stopped."
  (interactive)
  (let ((id (get-char-property (point) 'id)))
    (if id
        (let* ((request `("torrent-get" (:ids ,id :fields ("status"))))
               (response (apply 'transmission-request request))
               (torrents (transmission-torrents response))
               (status (transmission-torrents-value torrents 0 'status)))
          (pcase status
            (0 (transmission-request "torrent-start" `(:ids ,id)))
            ((or 4 6) (transmission-request "torrent-stop" `(:ids ,id)))))
      (user-error "No torrent selected"))))

(defun transmission-add-properties (start end property value)
  (add-text-properties start end property)
  (put-text-property start end property value))

(defun transmission-status (status up down)
  (let ((state (cdr (assq status transmission-status-alist)))
        (idle (propertize "idle" 'face 'shadow)))
    (pcase status
      (0 (propertize state 'face 'warning))
      (4 (if (> down 0) (propertize state 'face 'highlight) idle))
      (6 (if (> up 0) (propertize state 'face 'success) idle))
      (_ state))))

(defun transmission-draw ()
  (let* ((request `("torrent-get" (:fields ,transmission-torrent-get-fields)))
         (response (apply 'transmission-request request))
         (torrents (transmission-torrents response))
         (old-point (point))
         (index 0))
    (erase-buffer)
    (while (< index (length torrents))
      (let ((id (transmission-torrents-value torrents index 'id))
            (eta (transmission-torrents-value torrents index 'eta))
            (status (transmission-torrents-value torrents index 'status))
            (up (transmission-torrents-value torrents index 'rateUpload))
            (down (transmission-torrents-value torrents index 'rateDownload))
            (ratio (transmission-torrents-value torrents index 'uploadRatio))
            (fraction (transmission-torrents-value torrents index 'percentDone))
            (have (transmission-torrents-value torrents index 'sizeWhenDone))
            (name (transmission-torrents-value torrents index 'name))
            list)
        (push (format "%-4s" (transmission-eta eta)) list)
        (push (format (if (eq 'iec transmission-file-size-units) "%8s" "%6s")
                      (file-size-human-readable have transmission-file-size-units))
              list)
        (push (format "%3d%%" (* 100 fraction)) list)
        (push (format "%4d" (/ down (if (eq 'iec transmission-file-size-units) 1024 1000)))
              list)
        (push (format "%3d" (/ up (if (eq 'iec transmission-file-size-units) 1024 1000)))
              list)
        (push (format "%4.1f" (if (> ratio 0) ratio 0)) list)
        (push (format "%-11s" (transmission-status status up down)) list)
        (push (concat name "\n") list)
        (let* ((entry (mapconcat 'identity (reverse list) " "))
               (start (point))
               (end (+ start (length entry))))
          (insert entry)
          (transmission-add-properties start end 'id id)))
      (setq index (1+ index)))
    (goto-char old-point))
  (transmission-add-properties (point-min) (point-max) 'transmission 'torrent))

(defun transmission-draw-files ()
  (let ((id (get-char-property (point) 'id)))
    (if id
      (let* ((request `("torrent-get" (:ids ,id :fields ("name" "files"))))
             (response (apply 'transmission-request request))
             (torrents (transmission-torrents response))
             (files (transmission-torrents-value torrents 0 'files))
             (index 0))
        (erase-buffer)
        (while (< index (length files))
          (let ((name (transmission-torrents-value files index 'name))
                (len (transmission-torrents-value files index 'length))
                (have (transmission-torrents-value files index 'bytesCompleted))
                list)
            (push (format "%3d" index) list)
            (push (format (if (eq 'iec transmission-file-size-units) "%8s" "%6s")
                      (file-size-human-readable len transmission-file-size-units))
                  list)
            (push (format "%3d%%" (* 100 (/ have len))) list)
            (push (concat name "\n") list)
            (let* ((entry (mapconcat 'identity (reverse list) " "))
                   (start (point))
                   (end (+ start (length entry))))
              (insert entry)
              (add-text-properties start end 'index)
              (put-text-property start end 'index index)))
          (setq index (1+ index))))
      (user-error "No torrent selected"))))

(defun transmission-view ()
  (interactive)
  (setq buffer-read-only nil)
  (transmission-draw-files)
  (set-buffer-modified-p nil)
  (setq buffer-read-only t))

(defun transmission-refresh ()
  (interactive)
  (setq buffer-read-only nil)
  (transmission-draw)
  (set-buffer-modified-p nil)
  (setq buffer-read-only t))

(defvar transmission-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "RET") 'transmission-view)
    (define-key map "\t" 'transmission-next)
    (define-key map [backtab] 'transmission-previous)
    (define-key map "\e\t" 'transmission-previous)
    (define-key map "p" 'transmission-previous)
    (define-key map "n" 'transmission-next)
    (define-key map "?" 'describe-mode)
    (define-key map "a" 'transmission-add)
    (define-key map "d" 'transmission-set-download)
    (define-key map "g" 'transmission-refresh)
    (define-key map "s" 'transmission-toggle)
    (define-key map "u" 'transmission-set-upload)
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
    (transmission-mode)
    (transmission-refresh)))

(provide 'transmission)

;;; transmission.el ends here
