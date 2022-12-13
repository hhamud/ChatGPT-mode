;;; chatgpt.el --- Description -*- lexical-binding: t; -*-
;;
;; Author: Hamza Hamud
;; Created: December 05, 2022
;; Modified: December 05, 2022
;; Version: 0.0.1
;; Keywords: chatgpt-mode
;; Homepage: https://github.com/hhamud/chatgpt-mode
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


;;;; packages
(require 'request)


;;;; Parameters
(defconst chatgpt-user-agent "Mozilla/5.0 (X11; Linux x86_64; rv:107.0) Gecko/20100101 Firefox/107.0"
  "User agent set for requests.")

(defconst chatgpt-url-auth "https://chat.openai.com/api/auth/session"
  "Authentication URL for Chatgpt API.")

(defconst chatgpt-url-backend "https://chat.openai.com/backend-api/conversation"
  "ChatGPT backend API.")

;;;;;Tokens
(defvar chatgpt-auth-token ""
  "Auth token.")

(defvar chatgpt-session-token ""
  "Session token.")


;;;; Functions
(defun chatgpt-place-session-token (session-token)
  "Takes SESSION-TOKEN input from browser cookies."
  (interactive "sInput session token:")
  (setq chatgpt-session-token session-token))

(defun chatgpt--conversation-headers ()
  "Constructs the header parameter for the POST request."
  (let ((headers
         `(("Authorization" . ,(format "Bearer %s" chatgpt-auth-token))
           ("Content-Type" . "application/json")
           ("user-agent" . ,chatgpt-user-agent))))
    headers))


(defun chatgpt--uuid ()
  "Create UUID using shell."
  (shell-command-to-string "printf %s \"$(uuidgen)\""))

(defun chatgpt--conversation-data (prompt)
  "Constructs the body parameter for the POST request with PROMPT."
  (let ((data `(("action" . "next")
                ("messages" (("id" . ,(chatgpt--uuid))
                             ("role" . "user")
                             ("content" . (("content_type" . "text")
                                           ("parts" . ,(list prompt))))))
                ("parent_message_id" . ,(chatgpt--uuid))
                ("model" . "text-davinci-002-render")
                )))
    data))

(defun chatgpt--auth-headers ()
  "Create the headers for the auth session."
  (let ((headers `(("cookie" . ,(format "__Secure-next-auth.session-token=%s" chatgpt-session-token))
                   ("user-agent" . ,chatgpt-user-agent))))
    headers))

(defun chatgpt--fetch-auth-token ()
  (interactive)
  "Send the auth request to fetch the auth token."
  (request chatgpt-url-auth
    :type "GET"
    :headers (chatgpt--auth-headers )
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((auth-token (assoc-default 'accessToken data)))
                  (setq chatgpt-auth-token auth-token))))
    :error (cl-function
            (lambda (&key data &key error-thrown &allow-other-keys)
              (print (format "Auth-Error: %S, code: %S" (json-read-from-string data) error-thrown))))))


(defun chatgpt-input-auth-token (auth-token)
  (interactive "sInput Auth Token:")
  (setq chatgpt-auth-token auth-token))

(defun chatgpt--md-to-org (text)
  "Convert TEXT from markdown to org using pandoc."
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "pandoc" t t nil "-f" "markdown" "-t" "org")
    (goto-char (point-min))
    (replace-regexp "begin_example" "BEGIN_SRC")
    (goto-char (point-min))
    (replace-regexp "end_example" "END_SRC")
    (buffer-string)))


(defun chatgpt--success ()
  (cl-function
   (lambda (&key data &allow-other-keys)
     (when data
       (with-temp-buffer
         (insert data)
         (re-search-backward "data: \{")
         (end-of-line)
         (let* ((json-data
                 (buffer-substring (+ (line-beginning-position) 5) (point)))
                (json-string (json-read-from-string json-data))
                (resp (cdr (assoc 'parts (cdr (assoc 'content (cdr (assoc 'message json-string))))))))
           (with-current-buffer (get-buffer-create "*chatgpt*")
             (erase-buffer)
             (insert (chatgpt--md-to-org (substring (format "%s" resp) 1 -1)))
             (org-mode)
             (pop-to-buffer (current-buffer)))))))))


(defun chatgpt--error ()
  (cl-function
   (lambda (&key data &key error-thrown &allow-other-keys)
     (print (format "Chat-Error: %s, code: %s" data error-thrown)))))


(defun chatgpt--construct-response (prompt)
  "Construct the final request response to the chatgpt servers with user PROMPT."
  (request chatgpt-url-backend
    :type "POST"
    :data (json-encode (chatgpt--conversation-data prompt))
    :headers (chatgpt--conversation-headers )
    :parser 'buffer-string
    :success (chatgpt--success)
    :error (chatgpt--error) ))


(defun chatgpt-run (prompt)
  "Fetches the response from the prompt (as PROMPT) given to ChatGPT."
  (interactive "sInput Prompt:")
  (chatgpt--construct-response prompt))


(provide 'chatgpt)
;;; chatgpt.el ends here


