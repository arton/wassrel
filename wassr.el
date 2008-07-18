(load-library "base64")
(defvar wassr-mode-map nil "Local keymap for wassr-mode buffers.")
 
(defun byte-length (s)
  (let ((fun #'(lambda (lis) 
                 (if (null lis) 
                   0
                   (let ((n (string-to-char (car lis))))
                     ; (display-message-or-buffer (format "char(%c)=%d" n n))
                     (+ (cond ((>= n #x1000000) 4)
                              ((>= n #x10000) 3)
                              ((>= n #x100) 2)
                              (t 1))
                        (funcall fun (cdr lis)))))))
        )
    (funcall fun (split-string s "")))
)
    
(defun wassr-get (func credential)
  (let ((conn (open-network-stream "wassr" "*wassr*" "api.wassr.jp" 80)))
    (set-process-coding-system conn 'utf-8 'utf-8)
    (process-send-string conn
                         (concat "GET /statuses/" func " HTTP/1.0\r\n"))
    (process-send-string conn "Host: api.wassr.jp\r\n")
    (process-send-string conn "Accept: */*\r\n")
    (process-send-string conn "Connection: close\r\n")
    (process-send-string conn "User-Agent: elisp-wassr 0.8\r\n")
    (process-send-string conn (concat "Authorization: Basic " credential "\r\n"))
    (process-send-string conn "\r\n")
    ))
 
(defun wassr-post (credential unibytes)
  ; (display-message-or-buffer (concat "post data=" unibytes))
  (let ((conn (open-network-stream "wassr" "*wassr*" "api.wassr.jp" 80))
;  (let ((conn (open-network-stream "wassr" "*wassr*" "localhost" 1033))
        )
    
    (set-process-coding-system conn 'utf-8 'utf-8)
    (process-send-string conn "POST /statuses/update.json HTTP/1.1\r\n")
    (process-send-string conn "Host: api.wassr.jp\r\n")
    (process-send-string conn "Accept: */*\r\n")
    (process-send-string conn "Connection: close\r\n")
    (process-send-string conn "User-Agent: elisp-wassr 0.8\r\n")
    (process-send-string conn (concat "Authorization: Basic " credential "\r\n"))
    (process-send-string conn "Content-Type: application/x-www-form-urlencoded\r\n")
    (process-send-string conn 
                         (concat "Content-Length: " 
                                 (number-to-string (length unibytes)) "\r\n"))
    (process-send-string conn "\r\n")
    (process-send-string conn unibytes)
    ))
 
(if wassr-mode-map
    nil
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "\C-l" 'wassr-friend-timeline)
    (define-key map "\C-s" 'wassr-update-status)
    (setq wassr-mode-map map)))
 
(defun wassr-advertise ()
  (get-buffer-create "*wassr*")
)
 
(defun wassr-mode ()
  "Mode for post status to Wassr."
   (kill-all-local-variables)
   (wassr-advertise)
   (set-buffer "*wassr*")
   (use-local-map wassr-mode-map)
   (setq major-mode 'wassr-mode
         mode-name "Wassr"
         buffer-read-only t)
   (set (make-local-variable 'wassr-credential)
        (let ((user (read-from-minibuffer "user-id: "))
              (pass (read-passwd "password: ")))
          (base64-encode-string (concat user ":" pass))))
   (set (make-local-variable 'unsafe-string)
        (remove "" (split-string " ;/?:@&=+$<>#%\"," "")))
   (run-hooks 'wassr-mode-hook))
 
(defun url-encode (unibytes)
  (let ((fun #'(lambda (c lis) (cond ((null lis) nil)
                                     ((equal c (car lis)) c)
                                     (t (funcall fun c (cdr lis))))))
        (unsafe-p #'(lambda (c)
                      (funcall fun c unsafe-string)))
        )
    (mapconcat '(lambda (c) (if (funcall unsafe-p c)
                                (format "%%%02X" (string-to-char c))
                              c))
               (remove "" (split-string unibytes "")) ""))
)
(defun wassr-friend-timeline ()
  (interactive)
  (wassr-get "friends_timeline.json" wassr-credential))
(defun wassr-update-status ()
  (interactive)
  (wassr-post wassr-credential
              (concat "status=" 
                      (url-encode 
                       (encode-coding-string 
                        (read-from-minibuffer "msg: ") 'utf-8)) 
                      "&reply_status_rid=&source=wassr.el"))
)
