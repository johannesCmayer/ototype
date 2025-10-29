(in-package :ototype)

(defparameter *temporary-directory*
  (uiop:subpathname (uiop:temporary-directory) "ototype/"))

(ensure-directories-exist *temporary-directory*)

(defparameter out-file (uiop:subpathname *temporary-directory* #P"transcription.wav"))

(defmacro comment (&body body)
  (declare (ignore body))
  nil)

(defun clamp (x &optional (minimum 0.0) (maximum 1.0))
  (max (min x maximum) minimum))

(defun float-to-int16 (value)
  (declare (inline))
  (truncate (* (clamp value -1.0 1.0)
               (/ (- (expt 2 16) 2) 2.0))))

(defparameter *recording* t)

(defun record-start (output-path)
  (setf *recording* t)
  (let* ((frames-per-buffer 256)
         (sample-rate 44100d0)
         (bits-per-frame 16)
         (conversion-buffer (make-array frames-per-buffer :element-type '(signed-byte 16))))
    (with-audio
        (with-default-audio-stream (audio-stream
                                    1
                                    1
                                    :sample-format :float
                                    :sample-rate sample-rate
                                    :frames-per-buffer frames-per-buffer)
          ;; Write placeholder header
          (with-open-file (file-stream output-path
                                       :direction :output
                                       :element-type `(unsigned-byte 8)
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
            (write-wav-header-placeholder file-stream))
          (let ((number-of-frames 0))
            ;; Stream audio data to file
            (with-open-file (file-stream output-path
                                         :direction :output
                                         :element-type `(signed-byte ,bits-per-frame)
                                         :if-exists :append)
              (loop while *recording* do
                (let ((data (read-stream audio-stream)))
                  (dotimes (i frames-per-buffer)
                    (setf (aref conversion-buffer i) (float-to-int16 (aref data i))))
                  (write-sequence conversion-buffer file-stream))
                (incf number-of-frames frames-per-buffer)))
            ;; Write proper header
            (with-open-file (file-stream output-path
                                         :direction :output
                                         :element-type '(unsigned-byte 8)
                                         :if-exists :overwrite)
              (write-wav-header file-stream
                                (truncate sample-rate)
                                1
                                bits-per-frame
                                (* (/ bits-per-frame 8) number-of-frames))))))))

(defun record-stop ()
  (setf *recording* nil))

(defun transcribe-start ()
  (record-start out-file))

(defun transcribe-get ()
  (record-stop)
  ;; HACK use proper syncronisation
  (sleep 0.1)
  (let ((transcription (transcribe-file out-file)))
    transcription))

(defun transcribe-with-keypress-interupt ()
  "Start recording, press any key to stop"
  ;; (format t "Recording... Press ENTER to stop.~%")
  (bt:make-thread 
   (lambda () (record-start out-file))
   :initial-bindings `((*standart-ouput* . nil)))
  (read-line)
  (record-stop)
  (format t "~A~%" (transcribe-file out-file)))

(defun save-core ()
  (sb-ext:save-lisp-and-die "ototype-bin"
                            :toplevel 'transcribe-with-keypress-interupt
                            :executable t
                            :compression t))

(comment
  (save-core)
  
  (transcribe-start)
  (transcribe-get)
  
  (defparameter x (record-start out-file))
  (record-stop)

  (transcribe-file out-file)

  
  (defparameter y (reverse (mapcar #'float-to-int16 (apply #'append
                                                        (mapcar (lambda (vec)
                                                                  (coerce vec 'list))
                                                                x)))))

  (write-wav #P"output-4.wav" y)
  
  (defun write-wav (file-path data &optional (sample-rate 44100) (channels 1) (bits-per-sample 16))
    (with-open-file (file-stream file-path
                                 :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists :supersede)
      (write-wav-header file-stream
                        sample-rate
                        channels
                        bits-per-sample
                        (length data)))
    (with-open-file (file-stream file-path
                                 :direction :output
                                 :element-type `(signed-byte ,bits-per-sample)
                                 :if-exists :append)
      (write-sequence data
                      file-stream))
    t)


  (defun gen-test-test-sin ()
    (let* ((frequency 440.0)
           (bits 16)
           (size (expt 2 bits))
           (sample-rate 44100))
      (mapcar (lambda (x)
                (coerce (truncate (* (min 1.0 (expt (/ x 1000) 2))
                                     (- 1.0 (min 1.0 (expt (/ x 20000) 2)))
                                     ;;(- 1.0 (min 1.0 (/ (max 0.0 (- (+ x 100) 1000)) (- sample-rate 1000))))
                                     (sin (* (/ x sample-rate)
                                             (* 2 pi frequency)))
                                     (/ (- size 2) 2)))
                        `(signed-byte ,bits)))
              (a:iota sample-rate))))


  (comment
    (write-wav #P"output_test2.wav" (gen-test-test-sin))
    )

  (/ (expt 2 16) 2)

  (test-read-write-converted-echo)

  (comment
  ;; Groq is consistently 0.3 seconds. Open IA varies heavily from 1.3s-2.5s.
  ;; It also costs 3x less!
  (time (transcribe-file #P"mic_test.ogg"))
  (time (transcribe-file-groq #P"mic_test.ogg"))
  )
)
