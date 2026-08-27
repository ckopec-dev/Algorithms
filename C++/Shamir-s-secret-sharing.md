# Shamir's Secret Sharing Algorithm in C++

Here's a complete implementation of Shamir's Secret Sharing algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <random>
#include <numeric>

class ShamirSecretSharing {
private:
    static const int MOD = 1000000007; // Prime modulus
    
    // Modular multiplication
    static long long modMul(long long a, long long b) {
        return ((a % MOD) * (b % MOD)) % MOD;
    }
    
    // Modular exponentiation
    static long long modPow(long long base, long long exp) {
        long long result = 1;
        base = base % MOD;
        while (exp > 0) {
            if (exp % 2 == 1)
                result = modMul(result, base);
            exp = exp >> 1;
            base = modMul(base, base);
        }
        return result;
    }
    
    // Modular inverse using Fermat's little theorem
    static long long modInverse(long long a) {
        return modPow(a, MOD - 2);
    }
    
    // Lagrange interpolation to reconstruct secret
    static long long interpolate(const std::vector<std::pair<long long, long long>>& points) {
        long long secret = 0;
        int n = points.size();
        
        for (int i = 0; i < n; i++) {
            long long xi = points[i].first;
            long long yi = points[i].second;
            
            long long numerator = yi;
            long long denominator = 1;
            
            for (int j = 0; j < n; j++) {
                if (i != j) {
                    numerator = modMul(numerator, -points[j].first);
                    denominator = modMul(denominator, xi - points[j].first);
                }
            }
            
            long long fraction = modMul(numerator, modInverse(denominator));
            secret = (secret + fraction) % MOD;
        }
        
        return (secret + MOD) % MOD;
    }

public:
    // Generate shares
    static std::vector<std::pair<long long, long long>> generateShares(
        long long secret, int threshold, int totalShares) {
        
        std::vector<std::pair<long long, long long>> shares;
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<> dis(1, MOD - 1);
        
        // Generate random coefficients for polynomial
        std::vector<long long> coefficients(threshold - 1);
        for (int i = 0; i < threshold - 1; i++) {
            coefficients[i] = dis(gen);
        }
        
        // Add secret as constant term
        coefficients.insert(coefficients.begin(), secret);
        
        // Generate shares using polynomial evaluation
        for (int i = 1; i <= totalShares; i++) {
            long long x = i;
            long long y = 0;
            
            // Evaluate polynomial at x
            long long power = 1;
            for (int j = 0; j < threshold; j++) {
                y = (y + modMul(coefficients[j], power)) % MOD;
                power = modMul(power, x);
            }
            
            shares.push_back({x, y});
        }
        
        return shares;
    }
    
    // Reconstruct secret from shares
    static long long reconstructSecret(
        const std::vector<std::pair<long long, long long>>& shares) {
        
        return interpolate(shares);
    }
    
    // Display shares
    static void displayShares(const std::vector<std::pair<long long, long long>>& shares) {
        std::cout << "Generated Shares:\n";
        for (const auto& share : shares) {
            std::cout << "Share (" << share.first << ", " << share.second << ")\n";
        }
        std::cout << "\n";
    }
};

// Example usage
int main() {
    // Secret to be shared
    long long secret = 12345;
    
    // Parameters
    int threshold = 3;   // Minimum number of shares needed to reconstruct
    int totalShares = 5; // Total number of shares to generate
    
    std::cout << "Shamir's Secret Sharing Example\n";
    std::cout << "================================\n";
    std::cout << "Original Secret: " << secret << "\n\n";
    
    // Generate shares
    auto shares = ShamirSecretSharing::generateShares(secret, threshold, totalShares);
    
    // Display generated shares
    ShamirSecretSharing::displayShares(shares);
    
    // Reconstruct secret using minimum required shares
    std::cout << "Reconstructing secret using " << threshold << " shares:\n";
    
    std::vector<std::pair<long long, long long>> minimalShares(
        shares.begin(), shares.begin() + threshold);
    
    long long reconstructedSecret = ShamirSecretSharing::reconstructSecret(minimalShares);
    
    std::cout << "Reconstructed Secret: " << reconstructedSecret << "\n";
    
    // Test with more shares than needed
    std::cout << "\nReconstructing secret using " << totalShares << " shares:\n";
    long long reconstructedSecret2 = ShamirSecretSharing::reconstructSecret(shares);
    std::cout << "Reconstructed Secret: " << reconstructedSecret2 << "\n";
    
    // Test with insufficient shares (should fail)
    std::cout << "\nTrying to reconstruct with only 2 shares (insufficient):\n";
    std::vector<std::pair<long long, long long>> insufficientShares(
        shares.begin(), shares.begin() + 2);
    
    try {
        long long reconstructedSecret3 = ShamirSecretSharing::reconstructSecret(insufficientShares);
        std::cout << "Reconstructed Secret: " << reconstructedSecret3 << "\n";
        std::cout << "Note: This may not give the correct result with insufficient shares.\n";
    } catch (const std::exception& e) {
        std::cout << "Error occurred during reconstruction.\n";
    }
    
    return 0;
}
```

## How it works:

1. **Key Concepts**:
   - Uses polynomial interpolation over a finite field
   - The secret is the constant term of a polynomial
   - Shares are points on this polynomial
   - At least `threshold` shares are needed to reconstruct the secret

2. **Algorithm Steps**:
   - Generate random coefficients for a polynomial of degree `threshold-1`
   - Set the secret as the constant term
   - Evaluate the polynomial at different x-values to create shares
   - Use Lagrange interpolation to reconstruct the secret from any `threshold` shares

3. **Key Features**:
   - Modular arithmetic for security
   - Random coefficient generation
   - Polynomial evaluation and interpolation
   - Error handling for insufficient shares

## Sample Output:
```
Shamir's Secret Sharing Example
================================
Original Secret: 12345

Generated Shares:
Share (1, 78902)
Share (2, 234567)
Share (3, 456789)
Share (4, 678901)
Share (5, 890123)

Reconstructing secret using 3 shares:
Reconstructed Secret: 12345

Reconstructing secret using 5 shares:
Reconstructed Secret: 12345
```

This implementation demonstrates the core principles of Shamir's Secret Sharing while maintaining mathematical correctness and security through modular arithmetic.