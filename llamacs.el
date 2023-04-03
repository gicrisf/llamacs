;;; llamacs.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 gicrisf
;;
;; Author: gicrisf <giovanni.crisalfi@protonmail.com>
;; Maintainer: gicrisf <giovanni.crisalfi@protonmail.com>
;; Created: aprile 02, 2023
;; Modified: aprile 02, 2023
;; Version: 0.0.1
;; Keywords: ai
;; Homepage: https://github.com/cromo/llamacs
;; Package-Requires: ((emacs "25.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'deferred)
(require 'epc)
(require 'python)

(defgroup llamacs nil
  "Configuration for llamacs."
  :prefix "llamacs-"
  :group 'ai)

;; TODO implement gradual text display animation
(defcustom llamacs-gradual-text t
  "Show text gradually."
  :type 'boolean
  :group 'llamacs)

(defcustom llamacs-text-fps 5
  "Frame per seconds to display text."
  :type 'integer
  :group 'llamacs)

(defcustom llamacs-enable-loading-ellipsis t
  "Whether the ellipsis animation is displayed in *LLaMAcs*."
  :type 'boolean
  :group 'llamacs)

(defcustom llamacs-display-on-query t
  "Whether *LLaMAcs* is displayed when a query is sent."
  :type 'boolean
  :group 'llamacs)

(defcustom llamacs-display-on-response t
  "Whether *ChatGPT* is displayed when a response is received."
  :type 'boolean
  :group 'llamacs)

(defcustom llamacs-model-path nil
  "Model to use for generating content."
  :type 'string
  :group 'llamacs)

(defcustom llamacs-repo-path nil
  "The path of llamacs repository."
  :type 'string
  :group 'llamacs)

(defvar llamacs-process nil
  "The LLaMA process.")

(defvar llamacs-wait-timers (make-hash-table)
  "Timers to update the waiting message in the LLaMAcs buffer.")

(defvar llamacs-id 0
  "Tracks responses in the background.")

;;;###autoload
(defun llamacs-init ()
  "Initialize the LLaMAcs buffer.
A message is displayed to indicate that the initialization was
successful."
  (interactive)
  ;; fastLLaMa can be imported but is not in pip list for some reason
  ;; (when (equal (shell-command-to-string "pip list | grep fastLLaMa") "")
    ;; Should I check for and install requirements before?
    ;; (shell-command "pip install git+https://github.com/PotatoSpudowski/fastLLaMa")
    ;; (message "llama.cpp wrapper installed through pip."))

  (when (null llamacs-repo-path)
    (error "Oh no! llamacs-repo-path is nil. Please set llamacs-repo-path"))

  ;; "python" string is the python-interpreter
  (setq llamacs-process (epc:start-epc "python" (list (expand-file-name
                                                       (format "%sllamacs.py"
                                                               llamacs-repo-path)))))

  (with-current-buffer (get-buffer-create "*LLaMAcs*")
    (visual-line-mode 1))
  (message "LLaMAcs initialized."))

;;;###autoload
(defun llamacs-stop ()
  "Stops the LLaMA server."
  (interactive)
  (dolist (id (hash-table-keys llamacs-wait-timers))
    (llamacs--stop-wait id))
  (epc:stop-epc llamacs-process)
  (setq llamacs-process nil)
  (message "Stop LLaMa process."))

;;;###autoload
(defun llamacs-reset ()
  "Reset the LLaMAcs server."
  (interactive)
  (llamacs-stop)
  (llamacs-init))

;;;###autoload
(defun llamacs-display ()
  "Displays *LLaMAcs*."
  (interactive)
  (display-buffer "*LLaMAcs*")
  (when-let ((saved-win (get-buffer-window (current-buffer)))
             (win (get-buffer-window "*LLaMAcs*")))
    (unless (equal (current-buffer) (get-buffer "*LLaMAcs*"))
      (select-window win)
      (goto-char (point-max))
      (unless (pos-visible-in-window-p (point-max) win)
        (goto-char (point-max))
        (recenter))
      (select-window saved-win))))

(defun llamacs--clear-line ()
  "Clear line in *LLaMAcs*."
  (cl-assert (equal (current-buffer) (get-buffer "*LLaMAcs*")))
  (delete-region (save-excursion (beginning-of-line)
                                 (point))
                 (save-excursion (end-of-line)
                                 (point))))

(defun llamacs--identifier-string (id)
  "Identifier string corresponding to ID."
  (format "cg?[%s]" id))

(defun llamacs--regex-string (id)
  "Regex corresponding to ID."
  (format "cg\\?\\[%s\\]" id))

(defun llamacs--goto-identifier (id)
  "Go to response of ID."
  (cl-assert (equal (current-buffer) (get-buffer "*LLaMAcs*")))
  (goto-char (point-min))
  (re-search-forward (llamacs--regex-string id))
  (forward-line))

(defun llamacs--insert-query (query id)
  "Insert QUERY with ID into *LLaMAcs*."
  (with-current-buffer (get-buffer-create "*LLaMAcs*")
    (save-excursion
      (goto-char (point-max))
      (insert (format "%s%s\n%s\n%s"
                      (if (= (point-min) (point))
                          ""
                        "\n\n")
                      (propertize query 'face 'bold)
                      (propertize
                       (llamacs--identifier-string id)
                       'invisible t)
                      (if llamacs-enable-loading-ellipsis
                          ""
                        (concat "Waiting for LLaMAcs...")))))))

(defun llamacs--insert-response (response id)
  "Insert RESPONSE into *LLaMAcs* for ID."
  (with-current-buffer (get-buffer-create "*LLaMAcs*")
    (save-excursion
      (llamacs--goto-identifier id)
      (llamacs--clear-line)
      (insert response))))

(defun llamacs--insert-error (error-msg id)
  "Insert ERROR-MSG into *LLaMAcs* for ID."
  (with-current-buffer (get-buffer-create "*LLaMAcs*")
    (save-excursion
      (llamacs--goto-identifier id)
      (llamacs--clear-line)
      (insert (format "ERROR: %s" error-msg)))))

(defun llamacs--add-timer (id)
  "Add timer for ID to 'llamacs-wait-timers'."
  (cl-assert (null (gethash id llamacs-wait-timers)))
  (puthash id
           (run-with-timer 0.5 0.5
                           (eval
                            `(lambda ()
                               (with-current-buffer (get-buffer-create "*LLaMAcs*")
                                 (save-excursion
                                   (llamacs--goto-identifier ,id)
                                   (let ((line (thing-at-point 'line)))
                                     (when (>= (+ (length line)
                                                  (if (eq (substring line -1) "\n")
                                                      0
                                                    1))
                                               4)
                                       (llamacs--clear-line))
                                     (insert ".")))))))
           llamacs-wait-timers))

(defun llamacs--stop-wait (id)
  "Stop waiting for a response from ID."
  (when-let (timer (gethash id llamacs-wait-timers))
    (cancel-timer timer)
    (remhash id llamacs-wait-timers)))

(defun llamacs--query (query)
  "Send QUERY to the LLaMA process.
The query is inserted into the *LLaMAcs* buffer with bold text,
and the response from the LLaMA process is appended to the
buffer. If `llamacs-enable-loading-ellipsis' is non-nil, a loading
ellipsis is displayed in the buffer while waiting for the
response.
This function is intended to be called internally by the
`llamacs-query' function, and should not be called directly by
users."
  (unless llamacs-process
    (llamacs-init))
  (let ((saved-id (cl-incf llamacs-id)))
    (llamacs--insert-query query saved-id)
    (when llamacs-enable-loading-ellipsis
      (llamacs--add-timer saved-id))
    (when llamacs-display-on-query
      (llamacs-display))
    (deferred:$
      (epc:call-deferred llamacs-process 'query (list query))
      (deferred:nextc it
        (lambda (response)
          (llamacs--stop-wait saved-id)
          (llamacs--insert-response response saved-id)
          (when llamacs-display-on-response
            (llamacs-display)))))))

;;;###autoload
(defun llamacs-query (query)
  "Query LLaMA with QUERY."
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "LLaMAcs Query: "))))

  ;; TODO add query types
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (read-from-minibuffer "ChatGPT Query: "))
  (llamacs--query query))

(provide 'llamacs)
;;; llamacs.el ends here
