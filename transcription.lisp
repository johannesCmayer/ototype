(in-package :ototype.transcription)

(defparameter api-key-groq
  (str:trim (uiop:read-file-string "~/.config/secrets/gptel-groq-api-key.txt")))

(defun transcribe-file (file-path)
  "Transcribe audio file using groq"
  (let ((response (dex:post "https://api.groq.com/openai/v1/audio/transcriptions"
                           :headers `(("Authorization" . ,(format nil "Bearer ~A" api-key-groq)))
                           :content `(("model" . "whisper-large-v3")
                                      ("file" . ,file-path)))))
    (gethash "text" (jonathan:parse response :as :hash-table))))
