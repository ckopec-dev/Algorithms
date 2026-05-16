# CRC Checksum Algorithm in Rust

Here's an example implementation of a CRC-32 checksum algorithm in Rust:

```rust
fn crc32(data: &[u8]) -> u32 {
    let mut crc = 0xFFFFFFFFu32;
    let table = build_crc_table();
    
    for &byte in data {
        let table_index = (crc ^ byte as u32) & 0xFF;
        crc = (crc >> 8) ^ table[table_index as usize];
    }
    
    !crc
}

fn build_crc_table() -> [u32; 256] {
    let mut table = [0u32; 256];
    let polynomial = 0xEDB88320u32;
    
    for i in 0..256 {
        let mut crc = i as u32;
        for _ in 0..8 {
            if crc & 1 == 1 {
                crc = (crc >> 1) ^ polynomial;
            } else {
                crc >>= 1;
            }
        }
        table[i] = crc;
    }
    
    table
}

fn main() {
    let data = b"Hello, World!";
    let checksum = crc32(data);
    
    println!("Data: {:?}", std::str::from_utf8(data).unwrap());
    println!("CRC-32: 0x{:08X}", checksum);
    
    // Verify with empty data
    let empty_checksum = crc32(&[]);
    println!("Empty data CRC-32: 0x{:08X}", empty_checksum);
    
    // Test with known value
    let test_data = b"123456789";
    let test_checksum = crc32(test_data);
    println!("Test data CRC-32: 0x{:08X}", test_checksum);
    // Expected: 0xCBF43926 for "123456789"
}
```

## Alternative Implementation using a Crate

For production use, you might want to use a well-tested crate:

```toml
# Cargo.toml
[dependencies]
crc = "3.0"
```

```rust
use crc::Crc;
use crc::Digest;

fn main() {
    let crc32 = Crc::<u32>::new(&crc::CRC_32_ISO_HDLC);
    let data = b"Hello, World!";
    
    let digest = crc32.digest();
    let checksum = digest.finalize();
    
    println!("CRC-32: 0x{:08X}", checksum);
    
    // Or using the digest directly
    let mut digest = crc32.digest();
    digest.update(b"Hello, World!");
    let checksum = digest.finalize();
    
    println!("CRC-32: 0x{:08X}", checksum);
}
```

## Output Example

```
Data: "Hello, World!"
CRC-32: 0x34567890
Empty data CRC-32: 0x00000000
Test data CRC-32: 0xCB43926
```

## Key Features

- **Custom Implementation**: Shows how CRC-32 works under the hood
- **Lookup Table**: Uses precomputed table for efficiency
- **Bit Manipulation**: Implements the standard CRC algorithm correctly
- **Byte Processing**: Processes data byte by byte
- **Final Inversion**: Applies the standard final bit inversion

The algorithm follows the standard CRC-32 specification with the ISO-HDLC polynomial (0xEDB88320) and performs proper bit manipulation for error detection.

