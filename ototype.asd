#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression t))

(asdf:defsystem system-wide-stt
  :components ((:file "packages")
               (:file "transcription")
               (:file "wave")
               (:file "ototype"))
  :depends-on (:cl-portaudio
               :dexador
               :jonathan
               :str
               :alexandria
               :bordeaux-threads)
  :build-operation "program-op"
  :build-pathname "ototype-bin"
  :entry-point "ototype:transcribe-with-keypress-interupt")
