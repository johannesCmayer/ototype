(defpackage :ototype.wave
  (:use #:cl)
  (:export #:write-wav-header
           #:write-wav-header-placeholder))

(defpackage :ototype.transcription
  (:use #:cl)
  (:export #:transcribe-file))

(defpackage :ototype
  (:use #:cl)
  (:export #:transcribe-with-keypress-interupt)
  (:local-nicknames (#:a #:alexandria)
                    (#:bt #:bordeaux-threads))
  (:import-from #:portaudio
                #:with-audio
                #:with-default-audio-stream
                #:read-stream
                #:write-stream
                #:merge-channels-into-array
                #:separate-array-to-channels)
  (:import-from #:ototype.transcription
                #:transcribe-file)
  (:import-from #:ototype.wave
                #:write-wav-header
                #:write-wav-header-placeholder))
