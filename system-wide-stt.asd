#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression t))

(asdf:defsystem system-wide-stt
  :components ((:file "package")
               (:file "main"))
  :depends-on (:cl-portaudio
               :dexador
               :jonathan
               :str
               :alexandria
               :bordeaux-threads)
  :build-operation "program-op"
  :build-pathname "system-wide-stt-bin"
  :entry-point "mic-rec:transcribe-with-keypress-interupt")
