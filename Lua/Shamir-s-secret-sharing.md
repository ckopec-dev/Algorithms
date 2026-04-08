Below is a self‑contained **Lua** implementation of **(t, n) Shamir’s Secret Sharing** using arithmetic modulo a prime (the most common choice for a finite field).  
The code is deliberately kept simple so you can read it step‑by‑step, but it is fully functional and can be copied into a file (e.g. `shamir.lua`) and run with any Lua 5.1+ interpreter.

```lua
--[[ --------------------------------------------------------------
   Shamir's Secret Sharing (Lua)
   --------------------------------------------------------------
   * secret   : integer < p  (the value to share)
   * t        : threshold (minimum number of shares needed)
   * n        : total number of shares to generate
   * p        : a prime modulus defining the finite field GF(p)
   --------------------------------------------------------------
   The algorithm:
   1. Choose random coefficients a1 … a(t‑1) in [0, p‑1]
   2. Build polynomial f(x) = secret + a1·x + a2·x² + … + a(t‑1)·x^{t‑1} (mod p)
   3. Share i is the point (x_i, y_i) where x_i = i (non‑zero) and y_i = f(x_i) (mod p)
   4. To reconstruct, pick any t shares and apply Lagrange interpolation
      at x = 0 to recover f(0) = secret.
   -------------------------------------------------------------- ]]

local shamir = {}

-- Simple modular exponentiation (binary method)
local function mod_pow(base, exp, mod)
    local result = 1 % mod
    base = base % mod
    while exp > 0 do
        if exp % 2 == 1 then
            result = (result * base) % mod
        end
        base = (base * base) % mod
        exp = math.floor(exp / 2)
    end
    return result
end

-- Modular inverse using Fermat's little theorem (p must be prime)
local function mod_inv(a, mod)
    return mod_pow(a, mod - 2, mod)
end

-- Evaluate polynomial f(x) = c0 + c1·x + c2·x² + … (coefficients in array c)
local function eval_poly(coeffs, x, mod)
    local result = 0
    local power = 1   -- x^0
    for i = 1, #coeffs do
        result = (result + coeffs[i] * power) % mod
        power = (power * x) % mod
    end
    return result
end

-- Generate n shares for a given secret
function shamir.generate_shares(secret, t, n, prime)
    assert(t <= n, "threshold t must be ≤ number of shares n")
    assert(secret >= 0 and secret < prime, "secret must be in [0, p)")

    -- Random coefficients a1 … a(t‑1) (secret is a0)
    local coeffs = {secret}   -- a0 = secret
    math.randomseed(os.time())   -- seed once; in production use a better RNG
    for i = 2, t do
        coeffs[i] = math.random(0, prime - 1)
    end

    local shares = {}
    for x = 1, n do
        local y = eval_poly(coeffs, x, prime)
        shares[x] = {x = x, y = y}   -- store as a table for clarity
    end
    return shares
end

-- Reconstruct the secret from any t shares using Lagrange interpolation at x=0
function shamir.reconstruct_secret(shares, prime)
    local secret = 0
    for i = 1, #shares do
        local xi = shares[i].x
        local yi = shares[i].y

        -- Compute Lagrange basis li(0) = ∏_{j≠i} (0 - xj) / (xi - xj)  (mod p)
        local num = 1   -- numerator
        local den = 1   -- denominator
        for j = 1, #shares do
            if i ~= j then
                local xj = shares[j].x
                num = (num * ((0 - xj) % prime)) % prime
                den = (den * ((xi - xj) % prime)) % prime
            end
        end
        local li = (num * mod_inv(den, prime)) % prime
        secret = (secret + yi * li) % prime
    end
    return secret
end

return shamir
```

---

## How to use it

```lua
-- shamir.lua must be in the same directory or in Lua's package path
local shamir = require("shamir")

local prime  = 20483          -- any prime larger than your secret and n
local secret = 12345          -- the value you want to share
local t      = 3              -- need any 3 shares to recover
local n      = 5              -- generate 5 shares total

print(string.format("Original secret: %d", secret))

-- 1️⃣ Create shares
local shares = shamir.generate_shares(secret, t, n, prime)
print("\nGenerated shares:")
for i, s in ipairs(shares) do
    print(string.format("  Share %d: (x=%d, y=%d)", i, s.x, s.y))
end

-- 2️⃣ Pick any t shares (here we just take the first t)
local selected = {}
for i = 1, t do
    table.insert(selected, shares[i])
end

-- 3️⃣ Reconstruct the secret
local recovered = shamir.reconstruct_secret(selected, prime)
print(string.format("\nReconstructed secret from %d shares: %d", t, recovered))

assert(recovered == secret, "Reconstruction failed!")
print("Success! The recovered secret matches the original.")
```

### Expected output (will vary because of random coefficients)

```
Original secret: 12345

Generated shares:
  Share 1: (x=1, y=17432)
  Share 2: (x=2, y=5621)
  Share 3: (x=3, y=18890)
  Share 4: (x=4, y=12673)
  Share 5: (x=5, y=1994)

Reconstructed secret from 3 shares: 12345
Success! The recovered secret matches the original.
```

---

## Notes & Safety Tips

| Topic | Detail |
|-------|--------|
| **Prime modulus** | Must be larger than both the secret and the number of shares (`p > max(secret, n)`). For cryptographic strength choose a large prime (e.g., a 256‑bit prime). |
| **Randomness** | `math.random` is **not** cryptographically secure. In production replace it with a secure RNG (e.g., LuaSec’s `openssl.rand`, LuaSocket’s `socket.gettime`, or OS‑provided `/dev/urandom`). |
| **Share encoding** | The example returns shares as `{x = ..., y = ...}` Lua tables. In practice you may want to serialize them (Base64, hex, etc.) for storage or transmission. |
| **Field operations** | All additions, multiplications, and exponentiations are performed modulo `p`. Subtraction is done via `(a - b) % p` to keep values positive. |
| **Threshold** | If you provide fewer than `t` shares to `reconstruct_secret`, the result will be **wrong** (the algorithm assumes you have enough points). |

Feel free to adapt the code to your needs—wrap it in a module, add error handling, or integrate it into a larger Lua project. Happy sharing! 🚀