(require 'org)

(defcustom mint-webapp-desktop-dirs (list (expand-file-name "~/.local/share/applications/"))
  "List of directories containing the .desktop files for WebApps."
  :type '(repeat directory)
  :group 'mint-webapp)

(defun org-webapp-open (path)
  "Open a webapp link from Org mode."
  (let ((components (split-string path "::")))
    (if (= (length components) 2)
        (mint-webapp-browse-url (nth 1 components) (nth 0 components))
      (error "Invalid webapp link format. Should be 'app::url'"))))

(org-link-set-parameters "webapp"
                         :follow #'org-webapp-open
                         :description "Open URL in specified WebApp")

(defun mint-webapp-list-directory (dir)
  (mapcar (lambda (f) (cons (file-name-sans-extension f) (concat dir f)))
          (directory-files dir nil "\\.desktop$")))

(defun mint-webapp-list-flatten ()
  (apply #'append (mapcar #'mint-webapp-list-directory mint-webapp-desktop-dirs)))

(defun mint-webapp-browse-url (url &optional app)
  "Launch a WebApp with the given URL.
If APP is provided, use it directly without prompting the user.
APP should be the name of the .desktop file without the .desktop extension."
  (interactive "sEnter URL: ")

  (cl-labels ((fallback (msg &rest args)
                (browse-url url)
                (apply #'message msg args)))
    (condition-case err
        (let* ((desktop-files (mint-webapp-list-flatten))
               (chosen-app (assoc app desktop-files)))
          (cond ((null chosen-app) (fallback "Falling back to default browser"))
                (t (let ((desktop-file-path (cdr chosen-app))
                         (buffer-name (format "*mint-webapp-%s*" (car chosen-app))))
                     (with-temp-buffer
                       (insert-file-contents desktop-file-path)
                       (goto-char (point-min))
                       (when (search-forward-regexp "^Exec=\\(.*\\)" nil t)
                         (let* ((original-cmd (match-string 1))
                                (exec-cmd (-some->> original-cmd
                                            (replace-regexp-in-string "https?://[^ \"]+" url))))
                           (if (not (string-equal original-cmd exec-cmd))
                               (let ((process (start-process "mint-webapp-process" buffer-name "sh" "-c" exec-cmd)))
                                 (set-process-sentinel
                                  process
                                  (lambda (proc _)
                                    (when (eq (process-status proc) 'exit)
                                      (kill-buffer (process-buffer proc))))))
                             (fallback "No URL replacements were applied. Seems the chosen application is not a proper web app.")))))))))
      (error (fallback "Error in mint-webapp-browse-url: %s" (error-message-string err))))))

(provide 'mint-webapps)
