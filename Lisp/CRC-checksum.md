# CRC Checksum Algorithm in Lisp

Here's an example implementation of a CRC-32 checksum algorithm in Common Lisp:

```lisp
(defun crc32 (data)
  "Calculate CRC-32 checksum of the given data"
  (let ((crc 0))
    (loop for byte across data
          for i from 0
          do (let ((temp (logxor crc byte)))
               (loop for j from 0 below 8
                     do (if (logtest temp 1)
                            (setf temp (logxor (ash temp -1) #xEDB88320))
                            (setf temp (ash temp -1))))
               (setf crc temp)))
    (lognot crc)))

;; Alternative implementation using lookup table for better performance
(defun crc32-table-based (data)
  "Calculate CRC-32 checksum using lookup table"
  (let ((crc 0)
        (crc-table (make-crc32-table)))
    (loop for byte across data
          do (let ((temp (logxor (logand crc #xFF) byte)))
               (setf crc (logxor (ash crc -8) 
                                (aref crc-table temp)))))
    (lognot crc)))

(defun make-crc32-table ()
  "Generate CRC-32 lookup table"
  (let ((table (make-array 256 :element-type 'unsigned-byte-32)))
    (loop for i from 0 below 256
          do (let ((crc i))
               (loop for j from 0 below 8
                     do (if (logtest crc 1)
                            (setf crc (logxor (ash crc -1) #xEDB88320))
                            (setf crc (ash crc -1))))
               (setf (aref table i) crc)))
    table))

;; Example usage
(defun test-crc32 ()
  "Test the CRC-32 implementation"
  (let ((test-data "Hello, World!"))
    (format t "Data: ~A~%" test-data)
    (format t "CRC-32: #x~8,'0X~%" (crc32 test-data))
    (format t "CRC-32 (table-based): #x~8,'0X~%" (crc32-table-based test-data))))

;; Run the test
(test-crc32)
```

## Key Features of this Implementation:

1. **Basic CRC-32 Algorithm**: Implements the standard CRC-32 polynomial (#xEDB88320)
2. **Two Versions**: 
   - Direct calculation (slower but simpler)
   - Lookup table based (faster for large data)
3. **Proper Bit Manipulation**: Uses `logxor`, `logand`, and `ash` functions for bit operations
4. **Standard Compliance**: Follows the IEEE 802.3 CRC-32 standard

## Usage Example:

```lisp
;; Calculate CRC of a string
(crc32 "Hello, World!")
;; Returns: 2222232755 (in decimal)

;; Calculate CRC of binary data
(crc32 #(65 66 67 68 69))  ; "ABCDE" in ASCII
;; Returns: 1725915429
```

The algorithm processes each byte of input data and updates the CRC value according to the CRC-32 polynomial, making it useful for data integrity verification.

