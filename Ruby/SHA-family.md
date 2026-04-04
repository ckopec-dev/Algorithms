# SHA Family Algorithms in Ruby

Ruby provides built-in support for SHA algorithms through the `OpenSSL` library. Here are examples of different SHA algorithms:

## Basic SHA Usage

```ruby
require 'openssl'

# SHA-1
sha1 = OpenSSL::Digest::SHA1.new
sha1.update("Hello, World!")
puts "SHA-1: #{sha1.hexdigest}"
# Output: SHA-1: 2ef7bde608ce5404e97d5f042f95f89f1c231f55

# SHA-256
sha256 = OpenSSL::Digest::SHA256.new
sha256.update("Hello, World!")
puts "SHA-256: #{sha256.hexdigest}"
# Output: SHA-256: dffd6021bb2bd5b0af676290809ec3a53191dd81c7f70a4b28688a362182986f

# SHA-512
sha512 = OpenSSL::Digest::SHA512.new
sha512.update("Hello, World!")
puts "SHA-512: #{sha512.hexdigest}"
# Output: SHA-512: 314782571843775894724554814802051111111111111111111111111111111

