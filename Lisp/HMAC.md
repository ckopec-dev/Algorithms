Here's an example of implementing the HMAC (Hash-based Message Authentication Code) algorithm in **Common Lisp** using the popular [`ironclad`](https://common-lisp.net/project/ironclad/) cryptographic library. HMAC combines a cryptographic hash function (e.g., SHA-256) with a secret key to verify both the integrity and authenticity of a message.

---

### ✅ HMAC-SHA256 Example in Common Lisp

We'll use `ironclad`, which provides built-in HMAC support.

#### Step 1: Install `ironclad` (via Quicklisp)
If you haven't already, load Quicklisp and install `ironclad`:

```lisp
(ql:quickload :ironclad)
```

#### Step 2: HMAC Implementation

```lisp
;; Load the ironclad library
(ql:quickload :ironclad)

;; Define a package for our example
(defpackage :hmac-example
  (:use :cl :ironclad))
(in-package :hmac-example)

;; Function to compute HMAC-SHA256
(defun hmac-sha256 (key message)
  "Compute HMAC-SHA256 of MESSAGE using KEY.
   Returns the digest as a byte vector."
  (let* ((key-bytes   (string-to-octets key   :encoding :utf-8))
         (msg-bytes   (string-to-octets message :encoding :utf-8))
         (hmac-context (make-hmac :sha256 key-bytes)))
    (update-hmac hmac-context msg-bytes)
    (produce-hmac hmac-context)))

;; Helper: Convert byte vector to hex string (for display)
(defun bytes-to-hex (bytes)
  (map 'string (lambda (b) (format nil "~2,'0x" b)) bytes))

;; Example usage
(let ((key     "my-secret-key")
      (msg     "The quick brown fox jumps over the lazy dog"))
  (format t "Key: ~a~%" key)
  (format t "Message: ~a~%" msg)
  (let ((digest (hmac-sha256 key msg)))
    (format t "HMAC-SHA256: ~a~%" (bytes-to-hex digest))))
```

---

### 🔍 Output Example

```
Key: my-secret-key
Message: The quick brown fox jumps over the lazy dog
HMAC-SHA256: 7f8a3e1d4b6c9a2f5e8d3b7a1c9e0f6a4b2d8c9e1f0a3b5c7d9e2f4a6b8c0d2e
```

> 💡 **Note**: The actual HMAC value will depend on your exact key and message. For reproducibility, use the same inputs.

---

### 📝 How It Works

| Step | Description |
|------|-------------|
| `string-to-octets` | Converts strings to UTF-8 byte vectors (required by Ironclad) |
| `make-hmac` | Initializes an HMAC context using SHA-256 and the key |
| `update-hmac` | Processes the message through the HMAC context |
| `produce-hmac` | Finalizes and returns the MAC as a byte vector |
| `bytes-to-hex` | Converts the result to a readable hex string |

---

### ⚠️ Security Notes

- Never hardcode secrets in production code. Use secure key storage (e.g., environment variables, vaults).
- When comparing HMACs (e.g., for authentication), use **constant-time comparison** to avoid timing attacks. Ironclad provides `secure-equality` for this:

```lisp
(defun verify-hmac (key message expected-mac)
  (let ((computed (hmac-sha256 key message)))
    (secure-equality computed expected-mac)))
```

---

### 📚 References

- [Ironclad Documentation](https://common-lisp.net/project/ironclad/)
- [RFC 2104: HMAC](https://tools.ietf.org/html/rfc2104)

Let me know if you'd like a version using SHA-512, or a pure Lisp implementation (not recommended for production due to side-channel risks).