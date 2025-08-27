(in-package :ototype.wave)

(defun write-16bit-le (value stream)
  "Write 16-bit little-endian integer"
  (write-byte (logand value #xff) stream)
  (write-byte (logand (ash value -8) #xff) stream))

(defun write-32bit-le (value stream)
  "Write 32-bit little-endian integer"
  (write-byte (logand value #xff) stream)
  (write-byte (logand (ash value -8) #xff) stream)
  (write-byte (logand (ash value -16) #xff) stream)
  (write-byte (logand (ash value -24) #xff) stream))

(defun write-wav-header-placeholder (stream)
  (loop repeat 44 do (write-byte 0 stream)))

(defun write-wav-header (stream sample-rate channels bits-per-sample data-size)
  "Write a WAV file header to a binary stream"
  (let* ((byte-rate (* sample-rate channels (/ bits-per-sample 8)))
         (block-align (* channels (/ bits-per-sample 8)))
         (file-size (+ data-size 36)))
    
    ;; RIFF header
    (write-sequence (map 'vector #'char-code "RIFF") stream) ; ChunkID
    (write-32bit-le file-size stream)                        ; ChunkSize
    (write-sequence (map 'vector #'char-code "WAVE") stream) ; Format
    
    ;; fmt chunk
    (write-sequence (map 'vector #'char-code "fmt ") stream) ; Header
    (write-32bit-le 16 stream)                               ; chunk size
    (write-16bit-le 1 stream)                                ; audio format (PCM)
    (write-16bit-le channels stream)                         ; number of channels
    (write-32bit-le sample-rate stream)                      ; sample rate
    (write-32bit-le byte-rate stream)                        ; byte rate
    (write-16bit-le block-align stream)                      ; block align
    (write-16bit-le bits-per-sample stream)                  ; bits per sample
    
    ;; data chunk header
    (write-sequence (map 'vector #'char-code "data") stream)
    (write-32bit-le data-size stream)))
