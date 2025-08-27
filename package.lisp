(defpackage :mic-rec
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
                #:separate-array-to-channels))
