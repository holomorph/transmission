;;; transmission.el --- Interface to a Transmission session -*- lexical-binding: t -*-

;; Copyright (C) 2014-2016  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Version: 0.10
;; Package-Requires: ((emacs "24.4") (let-alist "1.0.3"))
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

;; Interface to a Transmission session.

;; Originally based on the JSON RPC library written by Christopher
;; Wellons, available online at
;; <https://github.com/skeeto/elisp-json-rpc>

;; Entry points are the `transmission' and `transmission-add'
;; commands.  A variety of commands are available for manipulating
;; torrents and their contents, some of which can be applied over
;; multiple items by selecting them within a region.  The menus for
;; each context provide good exposure.

;; "M-x transmission RET" pops up a torrent list.  One can add,
;; start/stop, verify, remove torrents, set speed limits, ratio
;; limits, bandwidth priorities, trackers, etc.  Also, one can
;; navigate to the corresponding file list, torrent info, or peer info
;; contexts.  In the file list, individual files can be toggled for
;; download, and their priorities set.

;; Customize-able are: the session address components, RPC
;; credentials, the display format of dates, file sizes and transfer
;; rates, pieces display, automatic refreshing of the torrent
;; list, etc.  See the `transmission' customization group.

;; The design draws from a number of sources, including the command
;; line utility transmission-remote(1), the ncurses interface
;; transmission-remote-cli(1), and the rtorrent(1) client.  These can
;; be found respectively at the following:
;; <https://trac.transmissionbt.com/browser/trunk/daemon/remote.c>
;; <https://github.com/fagga/transmission-remote-cli>
;; <https://rakshasa.github.io/rtorrent/>

;;; Code:

(require 'auth-source)
(require 'calc-bin)
(require 'calc-ext)
(require 'color)
(require 'diary-lib)
(require 'json)
(require 'mailcap)
(require 'tabulated-list)
(require 'url-util)

(eval-when-compile
  (require 'cl-lib)
  (require 'let-alist)
  (require 'subr-x))

(defgroup transmission nil
  "Interface to a Transmission session."
  :link '(url-link "https://trac.transmissionbt.com/")
  :group 'external)

(defcustom transmission-host "localhost"
  "Host name, IP address, or socket address of the Transmission session."
  :type 'string)

(defcustom transmission-service 9091
  "Port or name of the service for the Transmission session."
  :type '(choice (const :tag "Default" 9091)
                 (string :tag "Service")
                 (integer :tag "Port")))

(defcustom transmission-rpc-path "/transmission/rpc"
  "Path to the Transmission session RPC interface."
  :type '(choice (const :tag "Default" "/transmission/rpc")
                 (string :tag "Other path")))

(defcustom transmission-rpc-auth nil
  "Authentication (username, password, etc.) for the RPC interface.
Its value is a specification of the type used in `auth-source-search'.
If no password is set, `auth-sources' is searched using the
username, `transmission-host', and `transmission-service'."
  :type '(choice (const :tag "None" nil)
                 (plist :tag "Username/password"
                        :options ((:username string)
                                  (:password string))))
  :link '(info-link "(auth) Help for users")
  :link '(function-link auth-source-search))

(defcustom transmission-digit-delimiter ","
  "String used to delimit digits in numbers.
The variable `calc-group-char' is bound to this in `transmission-group-digits'."
  :type '(choice (const :tag "Comma" ",")
                 (const :tag "Full Stop" ".")
                 (const :tag "None" nil)
                 (string :tag "Other char"))
  :link '(variable-link calc-group-char)
  :link '(function-link transmission-group-digits))

(defcustom transmission-pieces-function #'transmission-format-pieces
  "Function used to show pieces of incomplete torrents.
The function takes a string (bitfield) representing the torrent
pieces and the number of pieces as arguments, and should return a string."
  :type '(radio (const :tag "None" nil)
                (function-item transmission-format-pieces)
                (function-item transmission-format-pieces-brief)
                (function :tag "Function")))

(defcustom transmission-trackers '()
  "List of tracker URLs.
These are used for completion in `transmission-trackers-add' and
`transmission-trackers-replace'."
  :type '(repeat (string :tag "URL")))

(defcustom transmission-units nil
  "The flavor of units used to display file sizes.
See `file-size-human-readable'."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "SI" si)
                 (const :tag "IEC" iec))
  :link '(function-link file-size-human-readable))

(defcustom transmission-refresh-modes '()
  "List of major modes in which to refresh the buffer automatically."
  :type 'hook
  :options '(transmission-mode
             transmission-files-mode
             transmission-info-mode
             transmission-peers-mode))

(defcustom transmission-refresh-interval 2
  "Period in seconds of the refresh timer."
  :type '(number :validate (lambda (w)
                             (unless (> (widget-value w) 0)
                               (widget-put w :error "Value must be positive")
                               w))))

(defcustom transmission-time-format "%a %b %e %T %Y %z"
  "Format string used to display dates.
See `format-time-string'."
  :type 'string
  :link '(function-link format-time-string))

(defcustom transmission-time-zone nil
  "Time zone of formatted dates.
See `format-time-string'."
  :type '(choice (const :tag "Local time" nil)
                 (const :tag "Universal Time (UTC)" t)
                 (const :tag "System Wall Clock" wall)
                 (string :tag "Time Zone Identifier"))
  :link '(function-link format-time-string))

(defcustom transmission-torrent-functions '(transmission-ffap)
  "List of functions to use for guessing torrents for `transmission-add'.
Each function should accept no arguments, and return a string or nil."
  :type 'hook
  :options '(transmission-ffap
             transmission-ffap-selection
             transmission-ffap-last-killed))

(defcustom transmission-geoip-function nil
  "Function used to translate an IP address into a location name.
The function should accept an IP address and return a string or nil."
  :type '(radio (const :tag "None" nil)
                (function-item transmission-geoiplookup)
                (function :tag "Function")))

(defcustom transmission-geoip-use-cache nil
  "Whether to cache IP address/location name associations.
If non-nil, associations are stored in `transmission-geoip-hash'.
Useful if `transmission-geoip-function' does not have its own
caching built in or is otherwise slow."
  :type 'boolean)

(defconst transmission-schedules
  (eval-when-compile
    (pcase-let*
        ((`(,sun ,mon ,tues ,wed ,thurs ,fri ,sat)
          (mapcar (lambda (x) (lsh 1 x)) (number-sequence 0 6)))
         (weekday (logior mon tues wed thurs fri))
         (weekend (logior sat sun))
         (all (logior weekday weekend)))
      `((sun . ,sun)
        (mon . ,mon)
        (tues . ,tues)
        (wed . ,wed)
        (thurs . ,thurs)
        (fri . ,fri)
        (sat . ,sat)
        (weekday . ,weekday)
        (weekend . ,weekend)
        (all . ,all))))
  "Alist of Transmission turtle mode schedules.")

(defconst transmission-mode-alist
  '((session . 0)
    (torrent . 1)
    (unlimited . 2))
  "Alist of threshold mode enumerations.")

(defconst transmission-priority-alist
  '((low . -1)
    (normal . 0)
    (high . 1))
  "Alist of names to priority values.")

(defconst transmission-status-alist
  '((stopped . 0)
    (verifywait . 1)
    (verifying . 2)
    (downwait . 3)
    (downloading . 4)
    (seedwait . 5)
    (seeding . 6))
  "Alist of possible Transmission torrent statuses.")

(defconst transmission-draw-torrents-keys
  '("id" "name" "status" "eta" "error"
    "rateDownload" "rateUpload"
    "percentDone" "sizeWhenDone"
    "uploadRatio"))

(defconst transmission-draw-files-keys
  '("name" "files" "fileStats" "downloadDir"))

(defconst transmission-draw-info-keys
  '("name" "hashString" "magnetLink" "activityDate" "addedDate"
    "dateCreated" "doneDate" "startDate" "peers" "pieces" "pieceCount"
    "pieceSize" "trackerStats" "peersConnected" "peersGettingFromUs" "peersFrom"
    "peersSendingToUs" "sizeWhenDone" "error" "errorString" "uploadRatio"
    "downloadedEver" "corruptEver" "haveValid" "totalSize" "percentDone"
    "seedRatioLimit" "seedRatioMode" "bandwidthPriority" "downloadDir"
    "uploadLimit" "uploadLimited" "downloadLimit" "downloadLimited"
    "honorsSessionLimits"  "rateDownload" "rateUpload"))

(defconst transmission-file-symbols
  '(:files-wanted :files-unwanted :priority-high :priority-low :priority-normal)
  "List of \"torrent-set\" method arguments for operating on files.")

(defconst transmission-session-header "X-Transmission-Session-Id"
  "The \"X-Transmission-Session-Id\" header key.")

(defvar transmission-session-id nil
  "The \"X-Transmission-Session-Id\" header value.")

(defvar-local transmission-torrent-vector nil
  "Vector of Transmission torrent data.")

(defvar-local transmission-torrent-id nil
  "The Transmission torrent ID integer.")

(defvar-local transmission-refresh-function nil
  "The name of the function used to redraw a buffer.
Should accept the torrent ID as an argument, e.g. `transmission-torrent-id'.")

(define-error 'transmission-conflict
  "Wrong or missing header \"X-Transmission-Session-Id\"")

(define-error 'transmission-unauthorized
  "Unauthorized user.  Check `transmission-rpc-auth'")

(define-error 'transmission-wrong-rpc-path
  "Bad RPC path.  Check `transmission-rpc-path'")

(defvar transmission-timer nil
  "Timer for repeating `revert-buffer' in a visible Transmission buffer.")

(defconst transmission-hash-table (make-hash-table :test 'equal)
  "Hash table used as initial value of `transmission-geoip-hash'.")

(defvar transmission-geoip-hash (copy-hash-table transmission-hash-table)
  "Hash table storing associations between IP addresses and location names.")


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
  "Check the HTTP status code.
A 409 response from a Transmission session includes the
\"X-Transmission-Session-Id\" header.  If a 409 is received,
update `transmission-session-id' and signal the error."
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward "HTTP/")
    (skip-chars-forward "[0-9].")
    (let* ((buffer (current-buffer))
           (status (read buffer)))
      (pcase status
        ((or 301 404 405) (signal 'transmission-wrong-rpc-path status))
        (401 (signal 'transmission-unauthorized status))
        (409 (when (search-forward (format "%s: " transmission-session-header))
               (setq transmission-session-id (read buffer))
               (signal 'transmission-conflict status)))))))

(defun transmission--auth-source-secret (user)
  "Return the secret for USER at found in `auth-sources'.
Unless otherwise specified in `transmission-rpc-auth', the host
and port default to `transmission-host' and
`transmission-service', respectively."
  (let ((spec (copy-sequence transmission-rpc-auth)))
    (unless (plist-get spec :host) (plist-put spec :host transmission-host))
    (unless (plist-get spec :port) (plist-put spec :port transmission-service))
    (apply #'auth-source-pick-first-password (nconc `(:user ,user) spec))))

(defun transmission--auth-string ()
  "HTTP \"Authorization\" header value if `transmission-rpc-auth' is populated."
  (when transmission-rpc-auth
    (let* ((user (plist-get transmission-rpc-auth :username))
           (pass (and user (or (plist-get transmission-rpc-auth :password)
                               (transmission--auth-source-secret user)))))
      (concat "Basic " (base64-encode-string (concat user ":" pass))))))

(defun transmission-http-post (process content)
  "Send to PROCESS an HTTP POST request containing CONTENT."
  (with-current-buffer (process-buffer process)
    (erase-buffer))
  (let ((headers (list (cons transmission-session-header transmission-session-id)
                       (cons "Content-length" (string-bytes content)))))
    (let ((auth (transmission--auth-string)))
      (if auth (push (cons "Authorization" auth) headers)))
    (with-temp-buffer
      (insert (format "POST %s HTTP/1.1\r\n" transmission-rpc-path))
      (mapc (lambda (elt)
              (insert (format "%s: %s\r\n" (car elt) (cdr elt))))
            headers)
      (insert "\r\n" content)
      (process-send-string process (buffer-string)))))

(defun transmission-wait (process)
  "Wait to receive HTTP response from PROCESS.
Return JSON object parsed from content."
  (with-current-buffer (process-buffer process)
    (while (and (not (transmission--content-finished-p))
                (process-live-p process))
      (accept-process-output process 1))
    (transmission--status)
    (transmission--move-to-content)
    (json-read)))

(defun transmission-send (process content)
  "Send PROCESS string CONTENT and wait for response synchronously."
  (transmission-http-post process content)
  (transmission-wait process))

(defun transmission-make-network-process ()
  "Return a network client process connected to a transmission daemon.
When creating a new connection, the address is determined by the
custom variables `transmission-host' and `transmission-service'."
  (let ((socket (if (file-name-absolute-p transmission-host)
                    (expand-file-name transmission-host)))
        buffer process)
    (unwind-protect
        (prog1
            (setq buffer (generate-new-buffer " *transmission*")
                  process
                  (make-network-process
                   :name "transmission" :buffer buffer
                   :host (unless socket transmission-host)
                   :service (or socket transmission-service)
                   :family (if socket 'local) :noquery t))
          (setq buffer nil process nil))
      (if (process-live-p process) (kill-process process))
      (if (buffer-live-p buffer) (kill-buffer buffer)))))

(defun transmission-request (method &optional arguments tag)
  "Send a request to Transmission.

METHOD is a string.
ARGUMENTS is a plist having keys corresponding to METHOD.
TAG is an integer and ignored.

Details regarding the Transmission RPC can be found here:
<https://trac.transmissionbt.com/browser/trunk/extras/rpc-spec.txt>"
  (let ((process (transmission-make-network-process))
        (content (json-encode `(:method ,method :arguments ,arguments :tag ,tag))))
    (unwind-protect
        (condition-case nil
            (transmission-send process content)
          (transmission-conflict
           (transmission-send process content)))
      (when (process-live-p process)
        (delete-process process)
        (kill-buffer (process-buffer process))))))


;; Asynchronous calls

(defun transmission-process-filter (process _string)
  "Function used as a supplement to the default filter function for PROCESS."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (when (transmission--content-finished-p)
        (condition-case e
            (progn (transmission--status)
                   (delete-process process))
          (transmission-conflict
           (let ((content (process-get process :request)))
             (transmission-http-post process content)))
          (error
           (process-put process :callback nil)
           (delete-process process)
           (message "%s" (error-message-string e))))))))

(defun transmission-process-sentinel (process _message)
  "Dispatch callback function for PROCESS and kill the process buffer."
  (when (buffer-live-p (process-buffer process))
    (unwind-protect
        (let* ((callback (process-get process :callback))
               (content (and callback
                             (with-current-buffer (process-buffer process)
                               (transmission--move-to-content)
                               (buffer-substring (point) (point-max))))))
          (if callback (run-at-time 0 nil callback content)))
      (kill-buffer (process-buffer process)))))

(defun transmission-request-async (callback method &optional arguments tag)
  "Send a request to Transmission asynchronously.

CALLBACK accepts one argument, the HTTP response content.
METHOD, ARGUMENTS, and TAG are the same as in `transmission-request'."
  (let ((process (transmission-make-network-process))
        (content (json-encode `(:method ,method :arguments ,arguments :tag ,tag))))
    (set-process-sentinel process #'transmission-process-sentinel)
    (add-function :after (process-filter process) 'transmission-process-filter)
    (process-put process :request content)
    (process-put process :callback callback)
    (transmission-http-post process content)
    process))


;; Response parsing

(defun transmission-torrents (response)
  "Return the \"torrents\" array in RESPONSE.
Each element is an alist with keys corresponding to the elements
of \"fields\" in the arguments of the \"torrent-get\" request."
  (cdr (assq 'torrents (cdr (assq 'arguments response)))))


;; Timer management

(defun transmission-timer-revert ()
  "Revert the buffer or cancel `transmission-timer'."
  (if (and (memq major-mode transmission-refresh-modes)
           (not (or isearch-mode (buffer-narrowed-p) (use-region-p))))
      (revert-buffer)
    (cancel-timer transmission-timer)))

(defun transmission-timer-run ()
  "Run the timer `transmission-timer'."
  (when transmission-timer (cancel-timer transmission-timer))
  (setq
   transmission-timer
   (run-at-time t transmission-refresh-interval #'transmission-timer-revert)))

(defun transmission-timer-check ()
  "Check if current buffer should run a refresh timer."
  (when (memq major-mode transmission-refresh-modes)
    (transmission-timer-run)))


;; Other

(defun transmission-refs (sequence key)
  "Make a list of the values of KEY in each element of SEQUENCE."
  (mapcar (lambda (x) (cdr (assq key x))) sequence))

(defun transmission-size (bytes)
  "Return string showing size BYTES in human-readable form."
  (file-size-human-readable bytes transmission-units))

(defun transmission-percent (have total)
  "Return the percentage of HAVE by TOTAL."
  (if (zerop total) 0 (/ (* 100.0 have) total)))

(defun transmission-files-directory-base (filename)
  "Return the top-most parent directory in string FILENAME."
  (let ((index (and (stringp filename)
                    (string-match-p "/" filename))))
    (if index (substring filename 0 (1+ index)))))

(defun transmission-every-prefix-p (prefix list)
  "Return t if PREFIX is a prefix to every string in LIST, otherwise nil."
  (not (cl-loop for string in list
                if (not (string-prefix-p prefix string)) return t)))

(defun transmission-slice (str k)
  "Slice STRING into K strings of somewhat equal size.
The result can have no more elements than STRING.
\n(fn STRING K)"
  (let ((len (length str)))
    (let ((quotient (/ len k))
          (remainder (% len k))
          (i 0)
          slice result)
      (while (and (/= 0 (setq len (length str))) (< i k))
        (setq slice (if (< i remainder) (1+ quotient) quotient))
        (push (substring str 0 (min slice len)) result)
        (setq str (substring str (min slice len) len))
        (cl-incf i))
      (nreverse result))))

(defun transmission-prop-values-in-region (prop)
  "Return a list of truthy values of text property PROP in region or at point.
If none are found, return nil."
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end))
            (list '()))
        (save-excursion
          (goto-char beg)
          (while (> end (point))
            (push (get-text-property (point) prop) list)
            (let ((pos (text-property-not-all (point) end prop (car-safe list))))
              (goto-char (or pos end)))))
        (and (car-safe list) list))
    (let ((value (get-text-property (point) prop)))
      (if value (list value)))))

(defun transmission-eta (seconds percent)
  "Return a string showing SECONDS in human-readable form;
otherwise some other estimate indicated by SECONDS and PERCENT."
  (if (<= seconds 0)
      (if (eql percent 1) "Done"
        (if (char-displayable-p ?∞) (eval-when-compile (char-to-string ?∞)) "Inf"))
    (let* ((minute 60.0)
           (hour 3600.0)
           (day 86400.0)
           (month (* 29.53 day))
           (year (* 365.25 day)))
      (apply #'format "%.0f%s"
             (cond
              ((> minute seconds) (list seconds "s"))
              ((> hour seconds) (list (/ seconds minute) "m"))
              ((> day seconds) (list (/ seconds hour) "h"))
              ((> month seconds) (list (/ seconds day) "d"))
              ((> year seconds) (list (/ seconds month) "mo"))
              (t (list (/ seconds year) "y")))))))

(defun transmission-when (seconds)
  "The `transmission-eta' of time between `current-time' and SECONDS."
  (if (<= seconds 0) "never"
    (let ((secs (- seconds (time-to-seconds (current-time)))))
      (format (if (< secs 0) "%s ago" "in %s")
              (transmission-eta (abs secs) nil)))))

(defun transmission-rate (bytes)
  "Return a rate in units kilobytes per second.
The rate is calculated from BYTES according to `transmission-units'."
  (/ bytes (if (eq 'iec transmission-units) 1024 1000)))

(defun transmission-throttle-torrent (ids limit n)
  "Set transfer speed limit for IDS.
LIMIT is a symbol; either uploadLimit or downloadLimit.
N is the desired threshold.  A negative value of N means to disable the limit."
  (cl-assert (memq limit '(uploadLimit downloadLimit)))
  (let* ((limit (intern (concat ":" (symbol-name limit))))
         (limited (intern (concat (symbol-name limit) "ed")))
         (arguments `(:ids ,ids ,@(if (< n 0) `(,limited :json-false)
                                    `(,limited t ,limit ,n)))))
    (transmission-request-async nil "torrent-set" arguments)))

(defun transmission-set-torrent-speed-limit (ids d)
  "Set speed limit of torrents IDS.
Direction D should be a symbol, either \"up\" or \"down\"."
  (cl-assert (memq d '(up down)))
  (let* ((str (concat (symbol-name d) "loadLimit"))
         (limit (intern str))
         (limited (intern (concat str "ed"))))
    (if (cdr ids)
        (let ((prompt (concat "Set torrents' " (symbol-name d) "load limit: ")))
          (transmission-throttle-torrent ids limit (read-number prompt)))
      (transmission-request-async
       (lambda (content)
         (let* ((torrents (transmission-torrents (json-read-from-string content)))
                (torrent (elt torrents 0))
                (n (cdr (assq limit torrent)))
                (throttle (eq t (cdr (assq limited torrent))))
                (prompt (concat "Set torrent's " (symbol-name d) "load limit ("
                                (if throttle (format "%d kB/s" n) "disabled") "): ")))
           (transmission-throttle-torrent ids limit (read-number prompt))))
       "torrent-get" `(:ids ,ids :fields (,str ,(concat str "ed")))))))

(defun transmission-prompt-speed-limit (upload)
  "Make a prompt to set transfer speed limit.
If UPLOAD is non-nil, make a prompt for upload rate, otherwise
for download rate."
  (let-alist (cdr (assq 'arguments (transmission-request "session-get")))
    (let ((limit (if upload .speed-limit-up .speed-limit-down))
          (enabled (eq t (if upload .speed-limit-up-enabled
                           .speed-limit-down-enabled))))
      (list (read-number (concat "Set global " (if upload "up" "down") "load limit ("
                                 (if enabled (format "%d kB/s" limit) "disabled")
                                 "): "))))))

(defun transmission-prompt-ratio-limit ()
  "Make a prompt to set global seed ratio limit."
  (let-alist (cdr (assq 'arguments (transmission-request "session-get")))
    (let ((limit .seedRatioLimit)
          (enabled (eq t .seedRatioLimited)))
      (list (read-number (concat "Set global seed ratio limit ("
                                 (if enabled (format "%.1f" limit) "disabled")
                                 "): "))))))

(defun transmission-read-strings (prompt &optional collection)
  "Read strings until an input is blank, with optional completion.
PROMPT and COLLECTION are the same as in `completing-read'.
Returns a list of non-blank inputs."
  (let (res entry)
    (catch :finished
      (while t
        (setq entry (if (not collection) (read-string prompt)
                      (completing-read prompt collection nil)))
        (if (and (not (string-empty-p entry))
                 (not (string-blank-p entry)))
            (progn (push entry res)
                   (setq collection (delete entry collection)))
          (throw :finished (nreverse res)))))))

(defun transmission-read-time (prompt)
  "Read an expression for time, prompting with string PROMPT.
Uses `diary-entry-time' to parse user input.
Returns minutes from midnight, otherwise nil."
  (let ((hhmm (diary-entry-time (read-string prompt))))
    (if (>= hhmm 0) (+ (% hhmm 100) (* 60 (/ hhmm 100))))))

(defun transmission-format-minutes (minutes)
  "Return a formatted string from MINUTES from midnight."
  (format-time-string "%H:%M" (seconds-to-time (* 60 (+ 300 minutes)))))

(defun transmission-n->days (n)
  "Return days corresponding to bitfield N.
Days are the keys of `transmission-schedules'."
  (cond
   ((let ((cell (rassq n transmission-schedules)))
      (when cell (list (car cell)))))
   ((let (res)
      (pcase-dolist (`(,k . ,v) transmission-schedules)
        (unless (zerop (logand n v))
          (push k res)
          (cl-decf n v)))
      (nreverse res)))))

(defun transmission-list-trackers (id)
  "Return the \"trackerStats\" array for torrent id ID."
  (let* ((arguments `(:ids ,id :fields ("trackerStats")))
         (response (transmission-request "torrent-get" arguments))
         (torrents (transmission-torrents response)))
    (cdr (assq 'trackerStats (elt torrents 0)))))

(defun transmission-list-unique-announce-urls ()
  "Return a list of unique announce URLs from all current torrents."
  (let* ((response (transmission-request "torrent-get" '(:fields ("trackers"))))
         (trackers (transmission-refs (transmission-torrents response) 'trackers))
         (urls (mapcar (lambda (vector) (transmission-refs vector 'announce))
                       trackers)))
    (delete-dups (apply #'append (delq nil urls)))))

(defun transmission-btih-p (string)
  "Return non-nil if STRING is a BitTorrent info hash, otherwise nil."
  (if (and string (string-match-p "\\`[[:xdigit:]]\\{40\\}\\'" string)) string))

(defun transmission-directory-name-p (name)
  "Return non-nil if NAME ends with a directory separator character."
  (let ((len (length name))
        (last ?.))
    (if (> len 0) (setq last (aref name (1- len))))
    (or (= last ?/)
        (and (memq system-type '(windows-nt ms-dos))
             (= last ?\\)))))

(defun transmission-ffap ()
  "Return a file name, URL, or info hash at point, otherwise nil."
  (or (get-text-property (point) 'shr-url)
      (get-text-property (point) :nt-link)
      (let ((fn (run-hook-with-args-until-success 'file-name-at-point-functions)))
        (unless (transmission-directory-name-p fn) fn))
      (url-get-url-at-point)
      (transmission-btih-p (thing-at-point 'word))))

(defun transmission-ffap-string (string)
  "Apply `transmission-ffap' to the beginning of STRING."
  (when string
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (transmission-ffap))))

(defun transmission-ffap-last-killed ()
  "Apply `transmission-ffap' to the most recent `kill-ring' entry."
  (transmission-ffap-string (car kill-ring)))

(defun transmission-ffap-selection ()
  "Apply `transmission-ffap' to the graphical selection."
  (transmission-ffap-string (with-no-warnings (x-get-selection))))

(defun transmission-files-do (action)
  "Apply ACTION to files in `transmission-files-mode' buffers."
  (cl-assert (memq action transmission-file-symbols))
  (let ((id transmission-torrent-id)
        (indices (mapcar (lambda (id) (cdr (assq 'index id)))
                         (transmission-prop-values-in-region 'tabulated-list-id))))
    (if (and id indices)
        (let ((arguments (list :ids id action indices)))
          (transmission-request-async nil "torrent-set" arguments))
      (user-error "No files selected or at point"))))

(defun transmission-files-file-at-point ()
  "Return the absolute path of the torrent file at point, or nil.
If the file named \"foo\" does not exist, try \"foo.part\" before returning."
  (let* ((dir (cdr (assq 'downloadDir (elt transmission-torrent-vector 0))))
         (base (or (and dir (cdr (assq 'name (tabulated-list-get-id))))
                   (user-error "No file at point")))
         (filename (and base (expand-file-name base dir))))
    (setq filename (or (and (file-exists-p filename) filename)
                       (let ((part (concat filename ".part")))
                         (and (file-exists-p part) part))))
    (if filename (abbreviate-file-name filename)
      (user-error "File does not exist"))))

(defun transmission-files-sort (torrent)
  "Return a list derived from the \"files\" and \"fileStats\" arrays in TORRENT.
The two are spliced together with indices for each file, sorted by file name."
  (let* ((alist (elt torrent 0))
         (files (cdr (assq 'files alist)))
         (stats (cdr (assq 'fileStats alist))))
    (sort (cl-loop for f across files
                   for s across stats
                   for i below (length files)
                   collect (append f s (list (cons 'index i))))
          (lambda (a b)
            (string< (cdr (assq 'name a))
                     (cdr (assq 'name b)))))))

(defun transmission-geoiplookup (ip)
  "Return country name associated with IP using geoiplookup(1)."
  (let ((program (if (string-match-p ":" ip) "geoiplookup6" "geoiplookup")))
    (when (executable-find program)
      (with-temp-buffer
        (call-process program nil t nil ip)
        (car (last (split-string (buffer-string) ": " t "[ \t\r\n]*")))))))

(defun transmission-geoip-retrieve (ip)
  "Retrieve value of IP in `transmission-geoip-hash'.
If IP is not a key, add it with the value from `transmission-geoip-function'.
If `transmission-geoip-function' has changed, reset `transmission-geoip-hash'
from `transmission-hash-table'."
  (when (functionp transmission-geoip-function)
    (if (not transmission-geoip-use-cache)
        (funcall transmission-geoip-function ip)
      (let ((fn (get 'transmission-geoip-hash :fn)))
        (if (eq fn transmission-geoip-function)
            (or (gethash ip transmission-geoip-hash)
                (setf (gethash ip transmission-geoip-hash)
                      (funcall transmission-geoip-function ip)))
          (setq transmission-geoip-hash
                (copy-hash-table transmission-hash-table))
          (put 'transmission-geoip-hash :fn transmission-geoip-function)
          (setf (gethash ip transmission-geoip-hash)
                (funcall transmission-geoip-function ip)))))))

(defun transmission-time (seconds)
  "Format a time string, given SECONDS from the epoch."
  (if (= 0 seconds) "Never"
    (format-time-string transmission-time-format (seconds-to-time seconds)
                        transmission-time-zone)))

(defun transmission-hamming-weight (x)
  "Calculate the Hamming weight of X."
  (let ((m1 #x555555555555555)
        (m2 #x333333333333333)
        (m4 #x0f0f0f0f0f0f0f0f)
        (h01 #x0101010101010101))
    (setq x (- x (logand (lsh x -1) m1)))
    (setq x (+ (logand x m2) (logand (lsh x -2) m2)))
    (setq x (logand (+ x (lsh x -4)) m4))
    (lsh (* x h01) -56)))

(defun transmission-count-bits (bytearray)
  "Calculate sum of Hamming weight of each byte in BYTEARRAY."
  (cl-loop for x across bytearray sum (transmission-hamming-weight x)))

(defun transmission-byte->string (byte)
  "Format integer BYTE into a string."
  (let* ((calc-number-radix 2)
         (string (math-format-binary byte)))
    (concat (make-string (- 8 (length string)) ?0) string)))

(defun transmission-ratio->glyph (ratio)
  "Return a single-char string representing RATIO."
  (char-to-string
   (cond
    ((= 0 ratio) #x20)
    ((< ratio 0.333) #x2591)
    ((< ratio 0.667) #x2592)
    ((< ratio 1) #x2593)
    ((= 1 ratio) #x2588))))

(defun transmission-ratio->256 (ratio)
  "Return a grey font-locked single-space string according to RATIO.
Uses color names for the 256 color palette."
  (let ((n (if (= 1 ratio) 231 (+ 236 (* 19 ratio)))))
    (propertize " " 'font-lock-face `(:background ,(format "color-%d" n)))))

(defun transmission-ratio->grey (ratio)
  "Return a grey font-locked single-space string according to RATIO."
  (let ((l (+ 0.2 (* 0.8 ratio))))
    (propertize " " 'font-lock-face `(:background ,(color-rgb-to-hex l l l))
                'help-echo (format "%.2f" ratio))))

(defun transmission-torrent-seed-ratio (mode tlimit)
  "String showing a torrent's seed ratio limit.
MODE is which seed ratio to use; TLIMIT is the torrent-level limit."
  (pcase mode
    (0 "session limit")
    (1 (format "%.2f (torrent-specific limit)" tlimit))
    (2 "unlimited")))

(defun transmission-group-digits (n)
  "Group digits of positive number N with `transmission-digit-delimiter'."
  (if (< n 10000) (number-to-string n)
    (let ((calc-group-char transmission-digit-delimiter))
      (math-group-float (number-to-string n)))))

(defun transmission-plural (n s)
  "Return a pluralized string expressing quantity N of thing S.
Done in the spirit of `dired-plural-s'."
  (let ((m (if (= -1 n) 0 n)))
    (concat (transmission-group-digits m) " " s (unless (= m 1) "s"))))

(defun transmission-format-size (bytes)
  "Format size BYTES into a more readable string."
  (format "%s (%s bytes)" (transmission-size bytes)
          (transmission-group-digits bytes)))

(defun transmission-tabulated-list-pred (key)
  "Return a sorting predicate comparing values of KEY.
KEY should be a key in an element of `tabulated-list-entries'."
  (lambda (a b)
    (> (cdr (assq key (car a)))
       (cdr (assq key (car b))))))

(defmacro transmission-let*-ids (bindings &rest body)
  "Conditionally bind variables according to BINDINGS and eval BODY.
If anaphoric binding of \"ids\"--to the list of torrent IDs at
point or in region--is non-nil, then BINDINGS and BODY are fed to
`let*'.  Else, a `user-error' is signalled."
  (declare (indent 1) (debug let*))
  `(let ((ids (or (and transmission-torrent-id (list transmission-torrent-id))
                  (mapcar (lambda (id) (cdr (assq 'id id)))
                          (transmission-prop-values-in-region 'tabulated-list-id)))))
     (if ids
         (let* (,@bindings)
           ,@body)
       (user-error "No torrent selected"))))

(defun transmission-collect-hook (hook)
  "Run HOOK and return a list of non-nil results from calling its elements."
  (let (res)
    (run-hook-wrapped
     hook
     (lambda (fun)
       (let ((val (funcall fun)))
         (when val (push val res)))
       nil))
    (nreverse res)))

(defmacro transmission-with-window-maybe (window &rest body)
  "If WINDOW is non-nil, execute BODY with WINDOW current.
Otherwise, just execute BODY."
  (declare (indent 1) (debug t))
  `(if (null ,window) (progn ,@body)
     (with-selected-window ,window
       ,@body)))

(defun transmission-window->state (window)
  "Return a list containing some state of WINDOW.
A simplification of `window-state-get', the list associates
WINDOW with `window-start' and the line/column coordinates of `point'."
  (transmission-with-window-maybe window
    (save-restriction
      (widen)
      (list window (window-start) (line-number-at-pos) (current-column)))))

(defun transmission-restore-state (state)
  "Set `window-start' and `window-point' according to STATE."
  (pcase-let ((`(,window ,start ,line ,column) state))
    (transmission-with-window-maybe window
      (goto-char (point-min))
      (goto-char (point-at-bol line))
      (move-to-column column)
      (setf (window-start) start))))


;; Interactive

;;;###autoload
(defun transmission-add (torrent &optional directory)
  "Add TORRENT by filename, URL, magnet link, or info hash.
When called with a prefix, prompt for DIRECTORY."
  (interactive
   (let* ((f (transmission-collect-hook 'transmission-torrent-functions))
          (def (mapcar #'file-relative-name f))
          (prompt (concat "Add torrent" (if def (format " [%s]" (car def))) ": ")))
     (list (read-file-name prompt nil def)
           (if current-prefix-arg
               (read-directory-name "Target directory: ")))))
  (transmission-request-async
   (lambda (content)
     (let-alist (json-read-from-string content)
       (pcase .result
         ("success"
          (or (and .arguments.torrent-added.name
                   (message "Added %s" .arguments.torrent-added.name))
              (and .arguments.torrent-duplicate.name
                   (message "Already added %s" .arguments.torrent-duplicate.name))))
         (_ (message .result)))))
   "torrent-add"
   (append (if (and (file-readable-p torrent) (not (file-directory-p torrent)))
               `(:metainfo ,(with-temp-buffer
                              (insert-file-contents torrent)
                              (base64-encode-string (buffer-string))))
             (setq torrent (string-trim torrent))
             `(:filename ,(if (transmission-btih-p torrent)
                              (concat "magnet:?xt=urn:btih:" torrent)
                            torrent)))
           (if directory (list :download-dir (expand-file-name directory))))))

(defun transmission-free (directory)
  "Show in the echo area how much free space is in DIRECTORY."
  (interactive (list (read-directory-name "Directory: " nil nil t)))
  (transmission-request-async
   (lambda (content)
     (let-alist (cdr (assq 'arguments (json-read-from-string content)))
       (message "%s free in %s" (transmission-format-size .size-bytes)
                (abbreviate-file-name .path))))
   "free-space" (list :path (expand-file-name directory))))

(defun transmission-stats ()
  "Message some information about the session."
  (interactive)
  (transmission-request-async
   (lambda (content)
     (let-alist (cdr (assq 'arguments (json-read-from-string content)))
       (message (concat "%d kB/s down, %d kB/s up; %d/%d torrents active; "
                        "%s received, %s sent; uptime %s")
                (transmission-rate .downloadSpeed)
                (transmission-rate .uploadSpeed)
                .activeTorrentCount .torrentCount
                (transmission-size .current-stats.downloadedBytes)
                (transmission-size .current-stats.uploadedBytes)
                (transmission-eta .current-stats.secondsActive nil))))
   "session-stats"))

(defun transmission-move (location)
  "Move torrent at point or in region to a new LOCATION."
  (interactive (list (read-directory-name "New directory: ")))
  (transmission-let*-ids
      ((arguments (list :ids ids :move t :location (expand-file-name location)))
       (prompt (format "Move torrent%s to %s? " (if (cdr ids) "s" "") location)))
    (when (y-or-n-p prompt)
      (setq deactivate-mark t)
      (transmission-request-async nil "torrent-set-location" arguments))))

(defun transmission-reannounce ()
  "Reannounce torrent at point or in region."
  (interactive)
  (transmission-let*-ids nil
    (setq deactivate-mark t)
    (transmission-request-async nil "torrent-reannounce" (list :ids ids))))

(defun transmission-remove (&optional unlink)
  "Prompt to remove torrent at point or torrents in region.
When called with a prefix UNLINK, also unlink torrent data on disk."
  (interactive "P")
  (transmission-let*-ids ((arguments `(:ids ,ids :delete-local-data ,(and unlink t))))
    (when (yes-or-no-p (concat "Remove " (and unlink "and unlink ")
                               "torrent" (and (< 1 (length ids)) "s") "? "))
      (setq deactivate-mark t)
      (transmission-request-async nil "torrent-remove" arguments))))

(defun transmission-set-bandwidth-priority ()
  "Set bandwidth priority of torrent(s) at point or in region."
  (interactive)
  (transmission-let*-ids
      ((prompt "Set bandwidth priority: ")
       (priority (completing-read prompt transmission-priority-alist nil t))
       (number (cdr (assoc-string priority transmission-priority-alist)))
       (arguments `(:ids ,ids :bandwidthPriority ,number)))
    (transmission-request-async nil "torrent-set" arguments)))

(defun transmission-set-download (limit)
  "Set global download speed LIMIT in kB/s."
  (interactive (transmission-prompt-speed-limit nil))
  (let ((arguments (if (<= limit 0) '(:speed-limit-down-enabled :json-false)
                     `(:speed-limit-down-enabled t :speed-limit-down ,limit))))
    (transmission-request-async nil "session-set" arguments)))

(defun transmission-set-upload (limit)
  "Set global upload speed LIMIT in kB/s."
  (interactive (transmission-prompt-speed-limit t))
  (let ((arguments (if (< limit 0) '(:speed-limit-up-enabled :json-false)
                     `(:speed-limit-up-enabled t :speed-limit-up ,limit))))
    (transmission-request-async nil "session-set" arguments)))

(defun transmission-set-cache-size (size)
  "Set global cache size in MB."
  (interactive
   (let-alist (cdr (assq 'arguments (transmission-request "session-get")))
     (list (read-number (format "Set global cache size (%d MB): " .cache-size-mb)))))
  (let ((arguments (list :cache-size-mb (max size 1))))
    (transmission-request-async nil "session-set" arguments)))

(defun transmission-set-ratio (limit)
  "Set global seed ratio LIMIT."
  (interactive (transmission-prompt-ratio-limit))
  (let ((arguments (if (< limit 0) '(:seedRatioLimited :json-false)
                     `(:seedRatioLimited t :seedRatioLimit ,limit))))
    (transmission-request-async nil "session-set" arguments)))

(defun transmission-set-torrent-download ()
  "Set download limit of torrent(s) at point in kB/s."
  (interactive)
  (transmission-let*-ids nil
    (transmission-set-torrent-speed-limit ids 'down)))

(defun transmission-set-torrent-upload ()
  "Set upload limit of torrent(s) at point in kB/s."
  (interactive)
  (transmission-let*-ids nil
    (transmission-set-torrent-speed-limit ids 'up)))

(defun transmission-set-torrent-ratio ()
  "Set seed ratio limit of torrent(s) at point."
  (interactive)
  (transmission-let*-ids
      ((prompt (concat "Set torrent" (if (cdr ids) "s'" "'s") " ratio mode: "))
       (mode (completing-read prompt transmission-mode-alist nil t))
       (n (cdr (assoc-string mode transmission-mode-alist)))
       (arguments `(:ids ,ids :seedRatioMode ,n)))
    (when (= n 1)
      (let ((limit (read-number "Set torrent ratio limit: ")))
        (setq arguments (append arguments `(:seedRatioLimit ,limit)))))
    (transmission-request-async nil "torrent-set" arguments)))

(defun transmission-toggle-limits ()
  "Toggle whether torrent(s) at point honor session speed limits."
  (interactive)
  (transmission-let*-ids nil
    (transmission-request-async
     (lambda (content)
       (let* ((torrents (transmission-torrents (json-read-from-string content)))
              (honor (pcase (cdr (assq 'honorsSessionLimits (elt torrents 0)))
                       (:json-false t) (_ :json-false))))
         (transmission-request-async nil "torrent-set"
                                     `(:ids ,ids :honorsSessionLimits ,honor))))
     "torrent-get" `(:ids ,ids :fields ("honorsSessionLimits")))))

(defun transmission-toggle ()
  "Toggle torrent between started and stopped."
  (interactive)
  (transmission-let*-ids nil
    (setq deactivate-mark t)
    (transmission-request-async
     (lambda (content)
       (let* ((torrents (transmission-torrents (json-read-from-string content)))
              (status (cdr (assq 'status (elt torrents 0))))
              (method (if (zerop status) "torrent-start" "torrent-stop")))
         (transmission-request-async nil method (list :ids ids))))
     "torrent-get" (list :ids ids :fields '("status")))))

(defun transmission-trackers-add ()
  "Add announce URLs to torrent or torrents."
  (interactive)
  (transmission-let*-ids
      ((trackers (transmission-refs (transmission-list-trackers ids) 'announce))
       (urls (or (transmission-read-strings
                  "Add announce URLs: "
                  (cl-loop for url in
                           (append transmission-trackers
                                   (transmission-list-unique-announce-urls))
                           unless (member url trackers) collect url))
                 (user-error "No trackers to add")))
       (arguments (list :ids ids :trackerAdd
                        ;; Don't add trackers that are already there
                        (cl-loop for url in urls
                                 unless (member url trackers) collect url))))
    (transmission-request-async
     (lambda (content)
       (let-alist (json-read-from-string content) (message .result)))
     "torrent-set" arguments)))

(defun transmission-trackers-remove ()
  "Remove trackers from torrent at point by ID or announce URL."
  (interactive)
  (let* ((id (or transmission-torrent-id (user-error "No torrent selected")))
         (array (or (transmission-list-trackers id)
                    (user-error "No trackers to remove")))
         (prompt (format "Remove tracker (%d trackers): " (length array)))
         (trackers (mapcar (lambda (x) (cons (cdr (assq 'announce x))
                                             (cdr (assq 'id x))))
                           array))
         (completion-extra-properties
          `(:annotation-function
            (lambda (x) (format " ID# %d" (cdr (assoc x ',trackers))))))
         (urls (or (transmission-read-strings prompt trackers)
                   (user-error "No trackers selected for removal")))
         (tids (cl-loop for alist across array
                        if (or (member (cdr (assq 'announce alist)) urls)
                               (member (number-to-string (cdr (assq 'id alist))) urls))
                        collect (cdr (assq 'id alist))))
         (arguments (list :ids id :trackerRemove tids)))
    (transmission-request-async
     (lambda (content)
       (let-alist (json-read-from-string content) (message .result)))
     "torrent-set" arguments)))

(defun transmission-trackers-replace ()
  "Replace tracker by ID or announce URL."
  (interactive)
  (let* ((id (or transmission-torrent-id (user-error "No torrent selected")))
         (trackers (or (mapcar (lambda (x)
                                 (cons (cdr (assq 'announce x))
                                       (cdr (assq 'id x))))
                               (transmission-list-trackers id))
                       (user-error "No trackers to replace")))
         (prompt (format "Replace tracker (%d trackers): " (length trackers)))
         (tid (or (let* ((completion-extra-properties
                          `(:annotation-function
                            (lambda (x)
                              (format " ID# %d" (cdr (assoc x ',trackers))))))
                         (tracker (completing-read prompt trackers)))
                    (cl-loop for cell in trackers
                             if (member tracker (list (car cell)
                                                      (number-to-string (cdr cell))))
                             return (cdr cell)))
                  (user-error "No tracker selected for substitution")))
         (replacement
          (completing-read "Replacement tracker? "
                           (append transmission-trackers
                                   (transmission-list-unique-announce-urls))))
         (arguments (list :ids id :trackerReplace (vector tid replacement))))
    (transmission-request-async
     (lambda (content)
       (let-alist (json-read-from-string content) (message .result)))
     "torrent-set" arguments)))

(defun transmission-turtle-set-days (days)
  "Set DAYS on which turtle mode will be active.
DAYS is a bitfield, the associations of which are in `transmission-schedules'.
If DAYS is nil, disable turtle mode schedule."
  (interactive
   (let-alist (cdr (assq 'arguments (transmission-request "session-get")))
     (let* ((prompt
             (format "Days %s: "
                     (if (not (eq t .alt-speed-time-enabled)) "(disabled)"
                       (or (transmission-n->days .alt-speed-time-day) "(none)"))))
            (names (transmission-read-strings prompt transmission-schedules))
            (bits (mapcar (lambda (x) (cdr (assoc-string x transmission-schedules)))
                          names)))
       (list (apply #'logior bits)))))
  (let ((arguments
         (append `(:alt-speed-time-enabled ,(if (zerop days) json-false t))
                 (unless (zerop days) `(:alt-speed-time-day ,days)))))
    (transmission-request-async nil "session-set" arguments)))

(defun transmission-turtle-set-times (begin end)
  "Set BEGIN and END times for turtle mode.
See `transmission-read-time' for details on time input."
  (interactive
   (let-alist (cdr (assq 'arguments (transmission-request "session-get")))
     (let* ((begs (transmission-format-minutes .alt-speed-time-begin))
            (ends (transmission-format-minutes .alt-speed-time-end))
            (start (or (transmission-read-time (format "Begin (%s): " begs))
                       .alt-speed-time-begin))
            (stop (or (transmission-read-time (format "End (%s): " ends))
                      .alt-speed-time-end)))
       (when (and (= start .alt-speed-time-begin) (= stop .alt-speed-time-end))
         (user-error "No change in schedule"))
       (if (y-or-n-p (format "Set active time from %s to %s? "
                             (transmission-format-minutes start)
                             (transmission-format-minutes stop)))
           (list start stop) '(nil nil)))))
  (when (or begin end)
    (let ((arguments
           (append (if begin (list :alt-speed-time-begin begin))
                   (if end (list :alt-speed-time-end end)))))
      (transmission-request-async nil "session-set" arguments))))

(defun transmission-turtle-set-speeds (up down)
  "Set UP and DOWN speed limits (kB/s) for turtle mode."
  (interactive
   (let-alist (cdr (assq 'arguments (transmission-request "session-get")))
     (let ((p1 (format "Set turtle upload limit (%d kB/s): " .alt-speed-up))
           (p2 (format "Set turtle download limit (%d kB/s): " .alt-speed-down)))
       (list (read-number p1) (read-number p2)))))
  (let ((arguments
         (append (if down (list :alt-speed-down down))
                 (if up (list :alt-speed-up up)))))
    (transmission-request-async nil "session-set" arguments)))

(defun transmission-turtle-toggle ()
  "Toggle turtle mode."
  (interactive)
  (transmission-request-async
   (lambda (content)
     (let* ((arguments (cdr (assq 'arguments (json-read-from-string content))))
            (enable (equal json-false (cdr (assq 'alt-speed-enabled arguments)))))
       (when (y-or-n-p (concat (if enable "En" "Dis") "able turtle mode? "))
         (transmission-request-async
          (lambda (content)
            (message (cdr (assq 'result (json-read-from-string content)))))
          "session-set" `(:alt-speed-enabled ,(or enable json-false))))))
   "session-get"))

(defun transmission-verify ()
  "Verify torrent at point or in region."
  (interactive)
  (transmission-let*-ids nil
    (when (y-or-n-p (concat "Verify torrent" (if (cdr ids) "s") "? "))
      (setq deactivate-mark t)
      (transmission-request-async nil "torrent-verify" (list :ids ids)))))

(defun transmission-quit ()
  "Quit and bury the buffer."
  (interactive)
  (let ((cur (current-buffer)))
    (if (cl-loop for list in (window-prev-buffers)
                 if (not (eq cur (car list))) return t)
        (quit-window)
      (if (one-window-p)
          (bury-buffer)
        (delete-window)))))

(defun transmission-files-unwant ()
  "Mark file(s) at point or in region as unwanted."
  (interactive)
  (transmission-files-do :files-unwanted))

(defun transmission-files-want ()
  "Mark file(s) at point or in region as wanted."
  (interactive)
  (transmission-files-do :files-wanted))

(defun transmission-files-priority (priority)
  "Set bandwidth PRIORITY on file(s) at point or in region."
  (interactive
   (list (completing-read "Set priority: " transmission-priority-alist nil t)))
  (transmission-files-do (intern (concat ":priority-" priority))))

(defun transmission-files-command (command file)
  "Run a command COMMAND on the FILE at point."
  (interactive
   (let* ((fap (run-hook-with-args-until-success 'file-name-at-point-functions))
          (def (mailcap-file-default-commands
                (list (replace-regexp-in-string "\\.part\\'" "" fap))))
          (prompt (and fap (concat "! on " (file-name-nondirectory fap)
                                   (if def (format " (default %s)" (car def)))
                                   ": ")))
          (input (read-shell-command prompt nil nil def t)))
     (if fap (list (if (string-empty-p input) (or (car def) "") input) fap)
       (user-error "File does not exist"))))
  (let* ((args (nconc (split-string command) (list (expand-file-name file))))
         (prog (car args)))
    (apply #'start-process prog nil args)))

(defun transmission-find-file ()
  "Visit the file at point with `find-file-read-only'."
  (interactive)
  (let ((file (run-hook-with-args-until-success 'file-name-at-point-functions)))
    (if file (find-file-read-only file)
      (user-error "File does not exist"))))

(defun transmission-copy-magnet ()
  "Copy magnet link of current torrent."
  (interactive)
  (let ((magnet (cdr (assq 'magnetLink (elt transmission-torrent-vector 0)))))
    (when magnet
      (kill-new magnet)
      (message "Copied %s" magnet))))


;; Formatting

(defun transmission-format-status (status up down)
  "Return a propertized string describing torrent status.
STATUS is a value in `transmission-status-alist'.  UP and DOWN are
transmission rates."
  (let ((state (symbol-name (car (rassq status transmission-status-alist))))
        (idle (propertize "idle" 'font-lock-face 'shadow))
        (uploading
         (propertize "uploading" 'font-lock-face 'font-lock-constant-face)))
    (pcase status
      (0 (propertize state 'font-lock-face 'warning))
      ((or 1 3 5) (propertize state 'font-lock-face '(bold shadow)))
      (2 (propertize state 'font-lock-face 'font-lock-function-name-face))
      (4 (if (> down 0) (propertize state 'font-lock-face 'highlight)
           (if (> up 0) uploading idle)))
      (6 (if (> up 0) (propertize state 'font-lock-face 'success) idle))
      (_ state))))

(defun transmission-format-pieces (pieces count)
  "Format into a string the bitfield PIECES holding COUNT boolean flags."
  (let* ((bytes (base64-decode-string pieces))
         (bits (mapconcat #'transmission-byte->string bytes "")))
    (cl-flet ((string-partition (s n)
                (let ((res '()))
                  (while (not (string-empty-p s))
                    (let* ((last (length s))
                           (middle (min n last)))
                      (push (substring s 0 middle) res)
                      (setq s (substring s middle last))))
                  (nreverse res))))
      (string-join (string-partition (substring bits 0 count) 72) "\n"))))

(defun transmission-format-pieces-brief (pieces count)
  "Format pieces into a one-line greyscale representation.
PIECES and COUNT are the same as in `transmission-format-pieces'."
  (let* ((bytes (base64-decode-string pieces))
         (slices (transmission-slice bytes 72))
         (ratios
          (cl-loop for bv in slices with div = nil
                   do (cl-decf count (setq div (min count (* 8 (length bv)))))
                   collect (/ (transmission-count-bits bv) (float div)))))
    (mapconcat (pcase (display-color-cells)
                 ((pred (< 256)) #'transmission-ratio->grey)
                 (256 #'transmission-ratio->256)
                 (_ #'transmission-ratio->glyph))
               ratios "")))

(defun transmission-format-pieces-internal (pieces count size)
  "Format piece data into a string.
PIECES and COUNT are the same as in `transmission-format-pieces'.
SIZE is the file size in bytes of a single piece."
  (let ((have (apply #'+ (mapcar #'transmission-hamming-weight
                                 (base64-decode-string pieces)))))
    (concat
     "Piece count: " (transmission-group-digits have)
     " / " (transmission-group-digits count)
     " (" (format "%.1f" (transmission-percent have count)) "%) * "
     (transmission-format-size size) " each"
     (when (and (functionp transmission-pieces-function)
                (/= have 0) (< have count))
       (let ((str (funcall transmission-pieces-function pieces count)))
         (concat "\nPieces:\n\n" str))))))

(defun transmission-format-peers (peers origins connected sending receiving)
  "Format peer information into a string.
PEERS is an array of peer-specific data.
ORIGINS is an alist giving counts of peers from different swarms.
CONNECTED, SENDING, RECEIVING are numbers."
  (cl-macrolet ((accumulate (array key)
                  `(cl-loop for alist across ,array
                            if (eq t (cdr (assq ,key alist))) sum 1)))
    (if (zerop connected) "Peers: none connected\n"
      (concat
       (format "Peers: %d connected, uploading to %d, downloading from %d"
               connected sending receiving)
       (format " (%d unchoked, %d interested)\n"
               (- connected (accumulate peers 'clientIsChoked))
               (accumulate peers 'peerIsInterested))
       (format
        "Peer origins: %s\n"
        (string-join
         (cl-loop with x = 0 for cell in origins for src across
                  ["cache" "DHT" "incoming" "LPD" "LTEP" "PEX" "tracker(s)"]
                  if (not (zerop (setq x (cdr cell))))
                  collect (format "%d from %s" x src))
         ", "))))))

(defun transmission-format-tracker (tracker)
  "Format alist TRACKER into a string of tracker info."
  (let-alist tracker
    (let* ((label (format "Tracker %d" .id))
           (col (length label))
           (fill (propertize (make-string col ?\s) 'display `(space :align-to ,col)))
           (result (pcase .lastAnnounceResult
                     ((or "Success" (pred string-empty-p)) nil)
                     (_ (concat "\n" fill ": "
                                (propertize .lastAnnounceResult
                                            'font-lock-face 'warning))))))
      (format
       (concat label ": %s (Tier %d)\n"
               fill ": %s %s. Announcing %s\n"
               fill ": %s, %s, %s %s. Scraping %s"
               result)
       .announce .tier
       (transmission-plural .lastAnnouncePeerCount "peer")
       (transmission-when .lastAnnounceTime) (transmission-when .nextAnnounceTime)
       (transmission-plural .seederCount "seeder")
       (transmission-plural .leecherCount "leecher")
       (transmission-plural .downloadCount "download")
       (transmission-when .lastScrapeTime) (transmission-when .nextScrapeTime)))))

(defun transmission-format-trackers (trackers)
  "Format tracker information into a string.
TRACKERS should be the \"trackerStats\" array."
  (if (zerop (length trackers)) "Trackers: none\n"
    (concat (mapconcat #'transmission-format-tracker trackers "\n") "\n")))

(defun transmission-format-speed-limit (speed limit limited)
  "Format speed limit data into a string.
SPEED and LIMIT are rates in bytes per second.  LIMITED, if t,
indicates that the speed limit is enabled."
  (cond
   ((not (eq limited t)) (format "%d kB/s" (transmission-rate speed)))
   (t (format "%d / %d kB/s" (transmission-rate speed) limit))))

(defun transmission-format-limits (session rx tx rx-lim tx-lim rx-thr tx-thr)
  "Format download and upload rate and limits into a string."
  (concat (transmission-format-speed-limit rx rx-lim rx-thr) " down, "
          (transmission-format-speed-limit tx tx-lim tx-thr) " up"
          (if (eq session t) ", session limited")))


;; Drawing

(defun transmission-tabulated-list-format (&optional _arg _noconfirm)
  "Initialize tabulated-list header or update `tabulated-list-format'."
  (let ((idx (cl-loop for format across tabulated-list-format
                      if (plist-get (nthcdr 3 format) :transmission-size)
                      return format)))
    (if (eq (cadr idx) (if (eq 'iec transmission-units) 9 7))
        (or header-line-format (tabulated-list-init-header))
      (setf (cadr idx) (if (eq 'iec transmission-units) 9 7))
      (tabulated-list-init-header))))

(defmacro transmission-do-entries (seq &rest body)
  "Map over SEQ, pushing each element to `tabulated-list-entries'.
Each form in BODY is a column descriptor."
  (declare (indent 1) (debug t))
  `(mapc (lambda (x)
           (let-alist x
             (push (list x (vector ,@body)) tabulated-list-entries)))
         ,seq))

(defun transmission-draw-torrents (_id)
  (let* ((arguments `(:fields ,transmission-draw-torrents-keys))
         (response (transmission-request "torrent-get" arguments)))
    (setq transmission-torrent-vector (transmission-torrents response)))
  (setq tabulated-list-entries nil)
  (transmission-do-entries transmission-torrent-vector
    (transmission-eta .eta .percentDone)
    (transmission-size .sizeWhenDone)
    (format "%d%%" (* 100 .percentDone))
    (format "%d" (transmission-rate .rateDownload))
    (format "%d" (transmission-rate .rateUpload))
    (format "%.1f" (if (> .uploadRatio 0) .uploadRatio 0))
    (if (not (zerop .error)) (propertize "error" 'font-lock-face 'error)
      (transmission-format-status .status .rateUpload .rateDownload))
    .name)
  (setq tabulated-list-entries (reverse tabulated-list-entries))
  (tabulated-list-print))

(defun transmission-draw-files (id)
  (let* ((arguments `(:ids ,id :fields ,transmission-draw-files-keys))
         (response (transmission-request "torrent-get" arguments)))
    (setq transmission-torrent-vector (transmission-torrents response)))
  (let* ((files (transmission-files-sort transmission-torrent-vector))
         (names (transmission-refs files 'name))
         (directory (transmission-files-directory-base (car names)))
         (truncate (if directory (transmission-every-prefix-p directory names))))
    (setq tabulated-list-entries nil)
    (transmission-do-entries files
      (format "%d%%" (transmission-percent .bytesCompleted .length))
      (symbol-name (car (rassoc .priority transmission-priority-alist)))
      (if (eq .wanted :json-false) "no" "yes")
      (transmission-size .length)
      (if truncate (string-remove-prefix directory .name) .name)))
  (setq tabulated-list-entries (reverse tabulated-list-entries))
  (tabulated-list-print))

(defun transmission-draw-info (id)
  (let* ((arguments `(:ids ,id :fields ,transmission-draw-info-keys))
         (response (transmission-request "torrent-get" arguments)))
    (setq transmission-torrent-vector (transmission-torrents response)))
  (erase-buffer)
  (let-alist (elt transmission-torrent-vector 0)
    (mapc
     (lambda (s) (if s (insert s "\n")))
     (vector
      (format "ID: %d" id)
      (concat "Name: " .name)
      (concat "Hash: " .hashString)
      (concat "Magnet: " (propertize .magnetLink 'font-lock-face 'link) "\n")
      (concat "Location: " (abbreviate-file-name .downloadDir))
      (let* ((percent (* 100 .percentDone))
             (fmt (if (zerop (mod percent 1)) "%d" "%.2f")))
        (concat "Percent done: " (format fmt percent) "%"))
      (format "Bandwidth priority: %s"
              (car (rassoc .bandwidthPriority transmission-priority-alist)))
      (concat "Speed: "
              (transmission-format-limits
               .honorsSessionLimits .rateDownload .rateUpload
               .downloadLimit .uploadLimit .downloadLimited .uploadLimited))
      (format "Ratio: %.3f / %s" (if (= .uploadRatio -1) 0 .uploadRatio)
              (transmission-torrent-seed-ratio .seedRatioMode .seedRatioLimit))
      (unless (zerop .error)
        (concat "Error: " (propertize .errorString 'font-lock-face 'error)))
      (transmission-format-peers .peers .peersFrom .peersConnected
                                 .peersGettingFromUs .peersSendingToUs)
      (concat "Date created:    " (transmission-time .dateCreated))
      (concat "Date added:      " (transmission-time .addedDate))
      (concat "Date finished:   " (transmission-time .doneDate))
      (concat "Latest Activity: " (transmission-time .activityDate) "\n")
      (transmission-format-trackers .trackerStats)
      (concat "Wanted: " (transmission-format-size .sizeWhenDone))
      (concat "Downloaded: " (transmission-format-size .downloadedEver))
      (concat "Verified: " (transmission-format-size .haveValid))
      (unless (zerop .corruptEver)
        (concat "Corrupt: " (transmission-format-size .corruptEver)))
      (concat "Total size: " (transmission-format-size .totalSize))
      (transmission-format-pieces-internal .pieces .pieceCount .pieceSize)))))

(defun transmission-draw-peers (id)
  (let* ((arguments `(:ids ,id :fields ("peers")))
         (response (transmission-request "torrent-get" arguments)))
    (setq transmission-torrent-vector (transmission-torrents response)))
  (setq tabulated-list-entries nil)
  (transmission-do-entries (cdr (assq 'peers (elt transmission-torrent-vector 0)))
    .address
    .flagStr
    (format "%d%%" (transmission-percent .progress 1.0))
    (format "%d" (transmission-rate .rateToClient))
    (format "%d" (transmission-rate .rateToPeer))
    .clientName
    (or (transmission-geoip-retrieve .address) ""))
  (setq tabulated-list-entries (reverse tabulated-list-entries))
  (tabulated-list-print))

(defun transmission-draw ()
  "Draw the buffer with new contents via `transmission-refresh-function'."
  (with-silent-modifications
    (funcall transmission-refresh-function transmission-torrent-id)))

(defun transmission-refresh (&optional _arg _noconfirm)
  "Refresh the current buffer, restoring window position, point, and mark.
Also run the timer for timer object `transmission-timer'."
  (let* ((old-states (or (mapcar #'transmission-window->state
                                 (get-buffer-window-list nil nil t))
                         (list (transmission-window->state nil))))
         (old-mark (if (not (region-active-p)) (mark)
                     (let ((beg (region-beginning)))
                       (if (= (window-point) beg) (region-end) beg))))
         (old-mark-active mark-active))
    (run-hooks 'before-revert-hook)
    (transmission-draw)
    (run-hooks 'after-revert-hook)
    (mapc #'transmission-restore-state old-states)
    (and old-mark (set-mark old-mark))
    (unless old-mark-active (deactivate-mark)))
  (transmission-timer-check))

(defmacro transmission-context (mode)
  "Switch to a context buffer of major mode MODE."
  (cl-assert (string-suffix-p "-mode" (symbol-name mode)))
  (let ((name (make-symbol "name")))
    `(let ((id (or transmission-torrent-id
                   (cdr (assq 'id (tabulated-list-get-id)))))
           (,name ,(format "*%s*" (string-remove-suffix "-mode" (symbol-name mode)))))
       (if (not id) (user-error "No torrent selected")
         (let ((buffer (or (get-buffer ,name)
                           (generate-new-buffer ,name))))
           (with-current-buffer buffer
             (let ((old-id (or transmission-torrent-id
                               (cdr (assq 'id (tabulated-list-get-id))))))
               (unless (eq major-mode ',mode)
                 (funcall #',mode))
               (if (and old-id (eq old-id id))
                   (revert-buffer)
                 (setq transmission-torrent-id id)
                 (transmission-draw)
                 (goto-char (point-min)))))
           (pop-to-buffer-same-window buffer))))))


;; Major mode definitions

(defvar transmission-peers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" 'transmission-info)
    map)
  "Keymap used in `transmission-peers-mode' buffers.")

(easy-menu-define transmission-peers-mode-menu transmission-peers-mode-map
  "Menu used in `transmission-peers-mode' buffers."
  '("Transmission-Peers"
    ["View Torrent Files" transmission-files]
    ["View Torrent Info" transmission-info]
    "--"
    ["Refresh" revert-buffer]
    ["Quit" quit-window]))

(define-derived-mode transmission-peers-mode tabulated-list-mode "Transmission-Peers"
  "Major mode for viewing peer information.
See https://trac.transmissionbt.com/wiki/PeerStatusText
for explanation of the peer flags.

In addition to any hooks its parent mode might have run, this
mode runs the hook `transmission-peers-mode-hook' at mode
initialization.

Key bindings:
\\{transmission-peers-mode-map}"
  :group 'transmission
  (setq-local line-move-visual nil)
  (setq tabulated-list-format
        `[("Address" 15 nil)
          ("Flags" 6 t)
          ("Has" 4 nil :right-align t)
          ("Down" 4 ,(transmission-tabulated-list-pred 'rateToClient)
           :right-align t)
          ("Up" 3 ,(transmission-tabulated-list-pred 'rateToPeer)
           :right-align t :pad-right 2)
          ("Client" 20 t)
          ("Location" 0 t)])
  (tabulated-list-init-header)
  (setq transmission-refresh-function #'transmission-draw-peers)
  (add-hook 'post-command-hook #'transmission-timer-check nil t)
  (setq-local revert-buffer-function #'transmission-refresh))

(defun transmission-peers ()
  "Open a `transmission-peers-mode' buffer for torrent at point."
  (interactive)
  (transmission-context transmission-peers-mode))

(defvar transmission-info-font-lock-keywords
  (eval-when-compile
    `((,(rx bol (group (*? nonl) ":") (* blank) (group (* nonl)) eol)
       (1 'font-lock-type-face)
       (2 'font-lock-keyword-face))))
  "Default expressions to highlight in `transmission-info-mode' buffers.")

(defvar transmission-info-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "c" 'transmission-copy-magnet)
    (define-key map "d" 'transmission-set-torrent-download)
    (define-key map "e" 'transmission-peers)
    (define-key map "l" 'transmission-set-torrent-ratio)
    (define-key map "m" 'transmission-move)
    (define-key map "t" 'transmission-trackers-add)
    (define-key map "T" 'transmission-trackers-remove)
    (define-key map "u" 'transmission-set-torrent-upload)
    (define-key map "y" 'transmission-set-bandwidth-priority)
    map)
  "Keymap used in `transmission-info-mode' buffers.")

(easy-menu-define transmission-info-mode-menu transmission-info-mode-map
  "Menu used in `transmission-info-mode' buffers."
  '("Transmission-Info"
    ["Add Tracker URLs" transmission-trackers-add]
    ["Remove Trackers" transmission-trackers-remove]
    ["Replace Tracker" transmission-trackers-replace]
    ["Copy Magnet Link" transmission-copy-magnet]
    ["Move Torrent" transmission-move]
    ["Reannounce Torrent" transmission-reannounce]
    ["Set Bandwidth Priority" transmission-set-bandwidth-priority]
    ("Set Torrent Limits"
     ["Set Torrent Download Limit" transmission-set-torrent-download]
     ["Set Torrent Upload Limit" transmission-set-torrent-upload]
     ["Toggle Torrent Speed Limits" transmission-toggle-limits
      :help "Toggle whether torrent honors session limits."]
     ["Set Torrent Seed Ratio Limit" transmission-set-torrent-ratio])
    ["Verify Torrent" transmission-verify]
    "--"
    ["View Torrent Files" transmission-files]
    ["View Torrent Peers" transmission-peers]
    "--"
    ["Refresh" revert-buffer]
    ["Quit" quit-window]))

(define-derived-mode transmission-info-mode special-mode "Transmission-Info"
  "Major mode for viewing and manipulating torrent attributes.

In addition to any hooks its parent mode might have run, this
mode runs the hook `transmission-info-mode-hook' at mode
initialization.

Key bindings:
\\{transmission-info-mode-map}"
  :group 'transmission
  (setq buffer-undo-list t)
  (setq font-lock-defaults '(transmission-info-font-lock-keywords t))
  (setq transmission-refresh-function #'transmission-draw-info)
  (add-hook 'post-command-hook #'transmission-timer-check nil t)
  (setq-local revert-buffer-function #'transmission-refresh))

(defun transmission-info ()
  "Open a `transmission-info-mode' buffer for torrent at point."
  (interactive)
  (transmission-context transmission-info-mode))

(defvar transmission-files-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'transmission-find-file)
    (define-key map "!" 'transmission-files-command)
    (define-key map "e" 'transmission-peers)
    (define-key map "i" 'transmission-info)
    (define-key map "m" 'transmission-move)
    (define-key map "u" 'transmission-files-unwant)
    (define-key map "w" 'transmission-files-want)
    (define-key map "y" 'transmission-files-priority)
    map)
  "Keymap used in `transmission-files-mode' buffers.")

(easy-menu-define transmission-files-mode-menu transmission-files-mode-map
  "Menu used in `transmission-files-mode' buffers."
  '("Transmission-Files"
    ["Run Command On File" transmission-files-command]
    ["Visit File" transmission-find-file
     "Switch to a read-only buffer visiting file at point"]
    ["Mark Files Unwanted" transmission-files-unwant]
    ["Mark Files Wanted" transmission-files-want]
    ["Set Files' Bandwidth Priority" transmission-files-priority]
    "--"
    ["View Torrent Info" transmission-info]
    ["View Torrent Peers" transmission-peers]
    "--"
    ["Refresh" revert-buffer]
    ["Quit" quit-window]))

(define-derived-mode transmission-files-mode tabulated-list-mode "Transmission-Files"
  "Major mode for a torrent's file list.

In addition to any hooks its parent mode might have run, this
mode runs the hook `transmission-files-mode-hook' at mode
initialization.

Key bindings:
\\{transmission-files-mode-map}"
  :group 'transmission
  (setq-local line-move-visual nil)
  (setq tabulated-list-format
        `[("Have" 4 nil :right-align t)
          ("Priority" 8 t)
          ("Want" 4 t :right-align t)
          ("Size" 9 ,(transmission-tabulated-list-pred 'length)
           :right-align t :transmission-size t)
          ("Name" 0 t)])
  (transmission-tabulated-list-format)
  (setq-local file-name-at-point-functions #'transmission-files-file-at-point)
  (setq transmission-refresh-function #'transmission-draw-files)
  (setq-local revert-buffer-function #'transmission-refresh)
  (add-hook 'post-command-hook #'transmission-timer-check nil t)
  (add-hook 'before-revert-hook #'transmission-tabulated-list-format nil t))

(defun transmission-files ()
  "Open a `transmission-files-mode' buffer for torrent at point."
  (interactive)
  (transmission-context transmission-files-mode))

(defvar transmission-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'transmission-files)
    (define-key map "a" 'transmission-add)
    (define-key map "d" 'transmission-set-download)
    (define-key map "e" 'transmission-peers)
    (define-key map "i" 'transmission-info)
    (define-key map "l" 'transmission-set-ratio)
    (define-key map "m" 'transmission-move)
    (define-key map "r" 'transmission-remove)
    (define-key map "s" 'transmission-toggle)
    (define-key map "t" 'transmission-trackers-add)
    (define-key map "u" 'transmission-set-upload)
    (define-key map "v" 'transmission-verify)
    (define-key map "q" 'transmission-quit)
    (define-key map "y" 'transmission-set-bandwidth-priority)
    map)
  "Keymap used in `transmission-mode' buffers.")

(easy-menu-define transmission-mode-menu transmission-mode-map
  "Menu used in `transmission-mode' buffers."
  '("Transmission"
    ["Add Torrent" transmission-add]
    ["Start/Stop Torrent" transmission-toggle
     :help "Toggle pause on torrents at point or in region"]
    ["Set Bandwidth Priority" transmission-set-bandwidth-priority]
    ("Set Global/Session Limits"
     ["Set Global Download Limit" transmission-set-download]
     ["Set Global Upload Limit" transmission-set-upload]
     ["Set Global Seed Ratio Limit" transmission-set-ratio])
    ("Set Torrent Limits"
     ["Set Torrent Download Limit" transmission-set-torrent-download]
     ["Set Torrent Upload Limit" transmission-set-torrent-upload]
     ["Toggle Torrent Speed Limits" transmission-toggle-limits
      :help "Toggle whether torrent honors session limits."]
     ["Set Torrent Seed Ratio Limit" transmission-set-torrent-ratio])
    ["Move Torrent" transmission-move]
    ["Reannounce Torrent" transmission-reannounce]
    ["Verify Torrent" transmission-verify]
    "--"
    ["Query Free Space" transmission-free]
    ["Session Statistics" transmission-stats]
    ("Turtle Mode" :help "Set and schedule alternative speed limits"
     ["Toggle Turtle Mode" transmission-turtle-toggle]
     ["Set Active Days" transmission-turtle-set-days]
     ["Set Active Time Span" transmission-turtle-set-times]
     ["Set Turtle Speed Limits" transmission-turtle-set-speeds])
    "--"
    ["View Torrent Files" transmission-files]
    ["View Torrent Info" transmission-info]
    ["View Torrent Peers" transmission-peers]
    "--"
    ["Refresh" revert-buffer]
    ["Quit" transmission-quit]))

(define-derived-mode transmission-mode tabulated-list-mode "Transmission"
  "Major mode for the list of torrents in a Transmission session.
See https://trac.transmissionbt.com/ for more information about
Transmission.

In addition to any hooks its parent mode might have run, this
mode runs the hook `transmission-mode-hook' at mode
initialization.

Key bindings:
\\{transmission-mode-map}"
  :group 'transmission
  (setq-local line-move-visual nil)
  (setq tabulated-list-format
        `[("ETA" 4 ,(transmission-tabulated-list-pred 'eta)
           :right-align t)
          ("Size" 9 ,(transmission-tabulated-list-pred 'sizeWhenDone)
           :right-align t :transmission-size t)
          ("Have" 4 ,(transmission-tabulated-list-pred 'percentDone)
           :right-align t)
          ("Down" 4 nil :right-align t)
          ("Up" 3 nil :right-align t)
          ("Ratio" 5 ,(transmission-tabulated-list-pred 'uploadRatio)
           :right-align t)
          ("Status" 11 t)
          ("Name" 0 t)])
  (transmission-tabulated-list-format)
  (setq transmission-refresh-function #'transmission-draw-torrents)
  (setq-local revert-buffer-function #'transmission-refresh)
  (add-hook 'post-command-hook #'transmission-timer-check nil t)
  (add-hook 'before-revert-hook #'transmission-tabulated-list-format nil t))

;;;###autoload
(defun transmission ()
  "Open a `transmission-mode' buffer."
  (interactive)
  (let* ((name "*transmission*")
         (buffer (or (get-buffer name)
                     (generate-new-buffer name))))
    (unless (eq buffer (current-buffer))
      (with-current-buffer buffer
        (unless (eq major-mode 'transmission-mode)
          (condition-case e
              (progn
                (transmission-mode)
                (transmission-draw)
                (goto-char (point-min)))
            (error
             (kill-buffer buffer)
             (signal (car e) (cdr e))))))
      (switch-to-buffer-other-window buffer))))

(provide 'transmission)

;;; transmission.el ends here
