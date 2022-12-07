;;; chatgpt.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Hamza Hamud
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
;;; Commentary:
;;
;;  Description
;;
;;; Code:


;;;; packages
(require 'request)


;;;; Parameters
(defconst chatgpt-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/107.0.0.0 Safari/537.36"
  "User agent set for requests.")

(defconst chatgpt-url-auth "https://chat.openai.com/api/auth/session"
  "Authentication URL for Chatgpt API.")

(defconst chatgpt-url-backend "https://chat.openai.com/backend-api/conversation"
  "ChatGPT backend API.")

(defconst chatgpt-api "https://chat.openai.com/api"
  "ChatGPT URL API.")

(defconst chatgpt-env "~/Documents/projects/upg-tool/.env"
  "Env file destination.")

;;;; Functions
(defun chatgpt-place-session-token (session-token)
  "Takes session_token input from browser cookies and stores it into .env file.
    and overwrites previous token if its old"
  (interactive "sInput session token:")
  (write-region (format "SESSION_TOKEN=%s" session-token) nil chatgpt-env))

(defun chatgpt--read-env-file (file-name)
  "Read the contents of an .env file and return a list of (key . value) pairs."
  (with-temp-buffer
    (insert-file-contents file-name)
    (goto-char (point-min))
    (let (env-pairs)
      (while (re-search-forward "^\\([^=\n]+\\)=\\(.+\\)" nil t)
        (push (cons (match-string 1) (match-string 2)) env-pairs))
      env-pairs)))


(defun chatgpt--conversation-headers (auth_token)
  "Constructs the header parameter for the POST request."
  (let ((headers
          `(("Accept" . "application/json")
          ("Authorization" . ,(format "Bearer %s" auth_token))
          ("Content-Type" . "application/json"))))
  headers))

(defun chatgpt--conversation-data (prompt parent-message-id)
  "Constructs the data parameter for the POST request."
  (let ((data `(("action" . "next")
               ("messages" . '(("id" . ,(shell-command-to-string "uuidgen"))
                               ("role" . "user")
                               ("content" . '(("content-type" . "text")
                                              ("parts" . ,prompt)))))
               ("model" . "text-davinci-002-render")
               ("parent_message_id" . ,parent-message-id))))
    data))

(defun chatgpt--auth-headers (session-token)
  (let ((headers `(("cookie" . (format "__Secure-next-auth.session-token=%s" ,session-token))
                   ("user-agent" . chatgpt-user-agent))))
    headers))


(defun chatgpt--json-read ()
  (let* ((json-object-type `plist )
       (json-read))))

(defun chatgpt-construct-response (prompt)
  (let* ((env (chatgpt--read-env-file chatgpt-env))
        (session_token (cdr (assoc "SESSION_TOKEN" env))))
  (request chatgpt-url
  :type "POST"
  :data (chatgpt--conversation-data prompt)
  :headers (chatgpt--conversation-headers session_token)
  :parser (chatgpt--json-read )
  :success (cl-function
            (lambda (&key data &allow-other-keys)
              (message "I sent: %S" (assoc-default 'form data))))
  :error
  (cl-function
          (lambda (&key data &allow-other-keys)
            (print (format "Error: %s" data)))))))


(defun chatgpt-fetch-response (prompt)
  (interactive "sInput Prompt:")
  (chatgpt-construct-response prompt))

(global-set-key (kbd "C-c C-c b") 'chatgpt-fetch-response)
(global-set-key (kbd "C-c C-c c") 'chatgpt-place-session-token)

(provide 'chatgpt)
;;; chatgpt.el ends here

