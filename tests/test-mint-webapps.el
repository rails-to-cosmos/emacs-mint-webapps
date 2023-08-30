(require 'buttercup)

(require 'mint-webapps)

(describe "mint-webapp-browse-url"
  (it "Opens the provided URL in the specified app"
    (spy-on 'mint-webapp-browse-url)
    (org-webapp-open "firefox::http://example.com")
    (expect (spy-calls-all-args 'mint-webapp-browse-url))
    :to-equal '(("http://example.com" "firefox"))))

;; (describe "mint-webapp-browse-url without app parameter"
;;   (it "prompts the user for an app"
;;     (spy-on 'completing-read :and-return-value "firefox")
;;     (mint-webapp-browse-url "http://example.com")
;;     (expect 'completing-read :to-have-been-called)
;;     (expect 'completing-read :to-throw 'error)))

;; (describe "mint-webapp-browse-url with incorrect URL format in chosen app"
;;   (it "throws an error"
;;     (spy-on 'mint-webapp-browse-url :and-call-through)
;;     (expect (lambda () (mint-webapp-browse-url "http://example.com" "brokenapp"))
;;             :to-throw 'error)))

;; (describe "mint-webapp-browse-url process creation"
;;   (it "creates a new process"
;;     (spy-on 'start-process)
;;     ;; Assuming 'firefox' is a valid choice and has a proper URL format in the Exec field.
;;     (mint-webapp-browse-url "http://example.com" "firefox")
;;     (expect 'start-process :to-have-been-called)))

;; (describe "mint-webapp-browse-url buffer naming"
;;   (it "names the buffer correctly"
;;     (let ((buffer-name (format "*mint-webapp-%s*" "firefox")))
;;       ;; Create buffer if not present
;;       (unless (get-buffer buffer-name)
;;         (with-current-buffer (generate-new-buffer buffer-name)
;;           (mint-webapp-browse-url "http://example.com" "firefox")))
;;       (expect (get-buffer buffer-name) :not :to-be nil))))

;; (describe "mint-webapp-browse-url process command"
;;   (it "uses the correct command"
;;     (spy-on 'start-process :and-call-through)
;;     (mint-webapp-browse-url "http://example.com" "firefox")
;;     ;; Assuming the command for 'firefox' with the provided URL is 'firefox http://example.com'.
;;     (expect (spy-calls-args-for 'start-process 0)
;;             :to-equal '("mint-webapp-process" "*mint-webapp-firefox*" "sh" "-c" "firefox http://example.com"))))

(provide 'test-mint-webapps)
