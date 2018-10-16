(defpackage :docker/stream
  (:use :common-lisp)
  (:import-from :uiop #:copy-stream-to-stream)
  (:import-from :trivial-gray-streams)
  (:import-from :flexi-streams)
  (:export #:docker-line-stream
           #:read-docker-line))

(in-package :docker/stream)

(defclass docker-line-stream (trivial-gray-streams:fundamental-input-stream)
  ((dstream :initarg :stream
            :reader docker-line-stream-s)))

(defmethod trivial-gray-streams:stream-read-sequence
    ((stream docker-line-stream) seq start end &key)
  (setf end (min (or end (length seq))
                 (length seq)))
  (loop
    for i from start to (1- end)
    do (let ((line (read-docker-line stream)))
         (if line
           (setf (elt seq i) line)
           (return i)))
    finally (return end)))

(defparameter +FD-NAME+ #(:stdin :stdout :stderr))

(defun read-docker-line (dstream)
  (let ((arr (make-array 8 :element-type 'flex:octet)))
    (unless (= 0 (read-sequence arr (docker-line-stream-s dstream) :start 0 :end 8))
      (let* ((len (big-endian (subseq arr 4 8)))
             (str (make-string len)))
        (read-sequence str (docker-line-stream-s dstream)
                              :start 0 :end len)
        (list
          (elt +FD-Name+ (elt arr 0))
          len
          str)))))

(defun big-endian (vec)
  (logior
    (ash (elt vec 0) 24)
    (ash (elt vec 1) 16)
    (ash (elt vec 2) 8)
    (elt vec 3)))
