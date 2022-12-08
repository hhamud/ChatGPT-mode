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
(defconst chatgpt-user-agent "Mozilla/5.0 (X11; Linux x86_64; rv:107.0) Gecko/20100101 Firefox/107.0"
  "User agent set for requests.")

(defconst chatgpt-url-auth "https://chat.openai.com/api/auth/session"
  "Authentication URL for Chatgpt API.")

(defconst chatgpt-url-backend "https://chat.openai.com/backend-api/conversation"
  "ChatGPT backend API.")

(defconst chatgpt-env "~/Documents/projects/chatgpt-mode/.env"
  "Env file destination.")

(defvar chatgpt-auth-token ""
  "Auth token.")

(defvar chatgpt-session-token ""
  "Session token.")
;;;; Functions
(defun chatgpt-place-session-token (session-token)
  "Takes session_token input from browser cookies and stores it into .env file.
    and overwrites previous token if its old"
  (interactive "sInput session token:")
  (setq chatgpt-session-token session-token))

(defun chatgpt--conversation-headers (auth_token)
  "Constructs the header parameter for the POST request."
  (let ((headers
          `(("Authorization" . ,(format "Bearer %s" auth_token))
          ("Content-Type" . "application/json")
          ("user-agent" . chatgpt-user-agent)
          )))
  headers))

(defun chatgpt--conversation-data (prompt)
  "Constructs the data parameter for the POST request."
  (let ((data `(("action" . "next")
               ("messages" . '(("id" . ,(shell-command-to-string "uuidgen"))
                               ("role" . "user")
                               ("content" . '(("content_type" . "text")
                                              ("parts" . '(,prompt))))))
               ("model" . "text-davinci-002-render")
               ("parent_message_id" . ,(shell-command-to-string "uuidgen")))))
    data))

(defun chatgpt--auth-headers (session-token)
  (let ((headers `(("cookie" . ,(format "__Secure-next-auth.session-token=%s" session-token))
                   ("user-agent" . ,chatgpt-user-agent))))
    headers))

(defun chatgpt--fetch-auth-token ()
    (request chatgpt-url-auth
      :type "GET"
      :headers (chatgpt--auth-headers chatgpt-session-token)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let ((auth-token (assoc-default 'accessToken data)))
                   (setq chatgpt-auth-token auth-token))))
      :error (cl-function
                (lambda (&key data &allow-other-keys)
                (print (format "Error: %s" data))))))


(defun chatgpt--construct-response (prompt)
  (chatgpt--fetch-auth-token)
  (request chatgpt-url-backend
  :type "POST"
  :data (json-encode (chatgpt--conversation-data prompt))
  :headers (chatgpt--conversation-headers chatgpt-auth-token)
  :parser 'json-read
  :sync t
  :success (cl-function
            (lambda (&key data &allow-other-keys)
              (message "%S" data)))
  :error (cl-function
          (lambda (&key data &allow-other-keys)
            (print (format "Error: %S" data))))))

(defun chatgpt-fetch-response (prompt)
  (interactive "sInput Prompt:")
  (chatgpt--construct-response prompt))

;; move these to README or the config file
(global-set-key (kbd "C-c C-c b") 'chatgpt-fetch-response)
(global-set-key (kbd "C-c C-c c") 'chatgpt-place-session-token)

(provide 'chatgpt)
;;; chatgpt.el ends here

