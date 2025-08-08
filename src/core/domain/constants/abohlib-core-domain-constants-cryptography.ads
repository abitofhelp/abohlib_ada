--  =============================================================================
--  Abohlib.Core.Domain.Constants.Cryptography - Cryptographic Constants
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides read-only cryptographic constants for use by domain value objects
--    and outer layers. These constants standardize cryptographic calculations
--    and ensure consistency across all applications using abohlib.
--
--  Usage:
--    Import this package to access standardized cryptographic constants:
--    - Hash lengths in bytes and hexadecimal representation
--    - Block sizes for symmetric encryption
--    - Key lengths for various algorithms
--    All constants are compile-time values ensuring zero runtime overhead.
--
--  Standards:
--    - SHA-2 family (SHA-224, SHA-256, SHA-384, SHA-512)
--    - AES block and key sizes
--    - Common cryptographic parameters
--
--  Architectural Note:
--    These constants are placed in the domain layer because they are needed
--    by domain value objects (hash types, key types, etc.). Following the
--    dependency rule, outer layers can access these constants for:
--    - Application layer: Hash validation and key management
--    - Infrastructure layer: Cryptographic implementations
--    - Presentation layer: Displaying hash/key information to users
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Constants.Cryptography is

   --  ==========================================================================
   --  SHA-2 Hash Family Constants
   --  ==========================================================================
   
   --  SHA-224 (224-bit hash)
   SHA224_HASH_SIZE_BYTES : constant := 28;                      -- 224 bits / 8
   SHA224_HASH_SIZE_HEX   : constant := SHA224_HASH_SIZE_BYTES * 2; -- 56 hex chars
   
   --  SHA-256 (256-bit hash) - Most commonly used
   SHA256_HASH_SIZE_BYTES : constant := 32;                      -- 256 bits / 8  
   SHA256_HASH_SIZE_HEX   : constant := SHA256_HASH_SIZE_BYTES * 2; -- 64 hex chars
   
   --  SHA-384 (384-bit hash)
   SHA384_HASH_SIZE_BYTES : constant := 48;                      -- 384 bits / 8
   SHA384_HASH_SIZE_HEX   : constant := SHA384_HASH_SIZE_BYTES * 2; -- 96 hex chars
   
   --  SHA-512 (512-bit hash)
   SHA512_HASH_SIZE_BYTES : constant := 64;                      -- 512 bits / 8
   SHA512_HASH_SIZE_HEX   : constant := SHA512_HASH_SIZE_BYTES * 2; -- 128 hex chars
   
   --  ==========================================================================
   --  AES (Advanced Encryption Standard) Constants
   --  ==========================================================================
   
   --  AES block size (fixed for all key lengths)
   AES_BLOCK_SIZE_BYTES : constant := 16;                       -- 128 bits
   
   --  AES key sizes
   AES128_KEY_SIZE_BYTES : constant := 16;                      -- 128 bits / 8
   AES192_KEY_SIZE_BYTES : constant := 24;                      -- 192 bits / 8
   AES256_KEY_SIZE_BYTES : constant := 32;                      -- 256 bits / 8
   
   --  ==========================================================================
   --  Common Cryptographic Sizes
   --  ==========================================================================
   
   --  Initialization Vector (IV) sizes
   COMMON_IV_SIZE_BYTES : constant := 16;                       -- 128-bit IV (common)
   
   --  Salt sizes for password-based cryptography
   SALT_MIN_SIZE_BYTES : constant := 16;                        -- Minimum recommended
   SALT_STD_SIZE_BYTES : constant := 32;                        -- Standard size
   
   --  Random number sizes
   NONCE_SIZE_BYTES    : constant := 12;                        -- GCM nonce size
   RANDOM_SEED_BYTES   : constant := 32;                        -- Cryptographic seed

end Abohlib.Core.Domain.Constants.Cryptography;