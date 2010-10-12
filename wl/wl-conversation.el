(defvar wl-all-folder-alist '(("" . "+all"))
  "Alist of (regex . folder-name). Regex is matched against the current folder. First match is the name of folder containing all mail.")

(defun wl-summary-get-all-folders ()
  (if (not wl-summary-buffer-folder-name)
      (mapcar 'cdr wl-all-folder-alist)
    (if wl-all-folder-alist
        (let ((rest (cdr wl-all-folder-alist))
              (next (car wl-all-folder-alist))
              (retval '()))
          (while next
            (if (string-match (car next) wl-summary-buffer-folder-name)
                (setq retval (cons (cdr next) retval)))
            (setq next (car rest)
                  rest (cdr rest)))
          retval))))

(defvar wl-summary-prev-folder-name nil)
(defvar wl-summary-prev-message-id nil)
(defvar wl-summary-prev-sticky nil)

(defun wl-current-thread-location ()
  "Return a pair consisting of the message-id of the current
message and of the root of its thread (both surrounded by <...>)"
  (save-excursion
    (wl-summary-set-message-buffer-or-redisplay)
    (set-buffer (wl-message-get-original-buffer))

    (let ((message-id (std11-field-body "Message-Id")))
     ;; The thread root is the first UID in References, if any, or
     ;; else is the current message
      (cons message-id
            (car (split-string (or (std11-field-body "References") message-id) 
                               "[ \f\t\n\r\v,]+"))))
    ))

(defun wl-make-all-folder-filter (filter)
  (let ((all-folders (wl-summary-get-all-folders)))
    (if (listp all-folders)
        (concat "*" (mapconcat (lambda (folder)
                                 (concat "/" filter "/" folder)) all-folders ","))
      (concat "/" filter "/" all-folders))))
  
(defun wl-thread-root-folder (thread-root)
  (let ((root-uid (substring thread-root 1 -1))
        (filter (concat "message-id:\"" thread-root
                        "\"|references:\"" thread-root
                        "\"")))
    (wl-make-all-folder-filter filter)))

(defun wl-summary-visit-conversation (&optional close)
 (interactive "P")
 (if close
     (if wl-summary-prev-folder-name
         (let ((scan-type (if wl-summary-prev-sticky 'no-sync 'update)))
           (wl-summary-goto-folder-subr wl-summary-prev-folder-name
                                        scan-type nil nil t)
           (wl-summary-jump-to-msg-by-message-id wl-summary-prev-message-id))
       (message "No previous folder to visit."))

   (let* ((thread-location (wl-current-thread-location))
          (cur-message-id (car thread-location))
          (prev-folder-name wl-summary-buffer-folder-name)
          (sticky (wl-summary-sticky-p)))
     (wl-open-conversation-view (cdr thread-location))
     (wl-summary-jump-to-msg-by-message-id cur-message-id)
     (setq wl-summary-prev-folder-name prev-folder-name
           wl-summary-prev-message-id cur-message-id
           wl-summary-prev-sticky sticky)
     (make-local-variable 'wl-summary-prev-folder-name)
     (make-local-variable 'wl-summary-prev-message-id)
     (make-local-variable 'wl-summary-prev-sticky)
     )))

(defun wl-open-conversation-view (root)
  (wl-summary-goto-folder-subr (wl-thread-root-folder root) 'update nil nil t))

(defun wl-open-message-view (mid)
  (let ((filter (concat "message-id:\"" mid "\"")))
    (wl-summary-goto-folder-subr (wl-make-all-folder-filter filter) 'update nil nil t)))

(define-key wl-summary-mode-map "X" 'wl-summary-visit-conversation)
