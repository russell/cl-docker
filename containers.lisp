(defpackage :docker/containers
  (:use :common-lisp :docker/request)
  (:import-from :uiop #:copy-stream-to-stream)
  (:export #:list-containers
           #:inspect-container
           #:list-container-processes
           #:inspect-container-changes
           #:create-container
           #:remove-container
           #:wait-container
           #:export-container-to-stream
           #:export-container-to-pathname
           #:start-container
           #:stop-container
           #:restart-container
           #:kill-container
           #:pause-container
           #:unpause-container
           #:container-logs))

(in-package :docker/containers)

(defun list-containers (&key limit since before size all filters)
  (request-json "/containers/json"
                :parameters
                `(("limit" . ,limit)
                  ("since" . ,since)
                  ("before" . ,before)
                  ("size" . ,(and size 1))
                  ("all" . ,(and all 1))
                  ("filters" . ,(and filters
                                     (json:encode-json-to-string
                                       filters))))))



(defun inspect-container (id)
  (request-json (format nil "/containers/~a/json" id)))

(defun list-container-processes (id &key ps-args)
  (request-json (format nil "/containers/~a/top" id)
                :parameters `(("ps_args" . ,ps-args))))

(defun inspect-container-changes (id)
  (request-json (format nil "/containers/~a/changes" id)))

(defun create-container (image &key name json)
  (request-json "/containers/create"
                :method :post
                :content-type "application/json"
                :parameters `(("name" . ,name))
                :content (json:encode-json-to-string
                           (cons
                             `("Image" . ,image)
                               json))))

(defun remove-container (id &key force remove-volumes)
  (request-json (format nil "/containers/~a" id)
                :method :delete
                :parameters `(("v" . ,(and remove-volumes 1))
                              ("force" . ,(and force 1)))))


(defun wait-container (id)
  (request-json (format nil "/containers/~a/wait" id) :method :post))

(defun export-container-to-stream (id stream)
  "Export the container ID as a tar archive to STREAM."
  (with-open-stream (tar (request (format nil "/containers/~a/export" id)))
    (copy-stream-to-stream tar stream :element-type '(unsigned-byte 8))))

(defun export-container-to-pathname (id pathname &rest args &key &allow-other-keys)
  "Export the container ID as a tar archive to PATHNAME. Keyword
arguments are passed to the function OPEN."
  (with-open-stream (out (apply #'open pathname
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                args))
    (export-container-to-stream id out)))


(defun start-container (id)
  (request-json (format nil "/containers/~a/start" id)
                :method :post))


(defun stop-container (id &key timeout)
  (request-json (format nil "/containers/~a/stop" id)
                :method :post
                :parameters `(("t" . ,timeout))))


(defun restart-container (id &key timeout)
  (request-json (format nil "/containers/~a/restart" id)
                :method :post
                :parameters `(("t" . ,timeout))))


(defun kill-container (id &key signal)
  (request-json (format nil "/containers/~a/kill" id)
                :method :post
                :parameters `(("signal" . ,signal))))


(defun pause-container (id)
  (request-json (format nil "/containers/~a/pause" id)
                :method :post))

(defun unpause-container (id)
  (request-json (format nil "/containers/~a/unpause" id)
                :method :post))

(defun container-logs (id &key (details 0) (follow 0)
                               (stdout 1)  (stderr 1)
                               (scince 0) (timestamps 0)
                               (tail "all"))
  "returns the log stream for the specified container"
  (request-stream (format nil "/containers/~a/logs" id)
                  :method :get
                  :parameters `(("details" . ,details)
                                ("follow" . ,follow)
                                ("stdout" . ,stdout)
                                ("stderr" . ,stderr)
                                ("scince" . ,scince)
                                ("timestamps" . ,timestamps)
                                ("tail". ,tail))))
