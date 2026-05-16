# Yao's Garbled Circuit Protocol Implementation in Lua

```lua
-- Yao's Garbled Circuit Protocol Implementation
-- This is a simplified example demonstrating the core concepts

local YaoProtocol = {}

-- Helper function to generate random keys
local function generate_random_key()
    math.randomseed(os.time())
    return math.random(1000000, 9999999)
end

-- Helper function to XOR two numbers (simulating garbling)
local function xor_keys(key1, key2)
    return key1 ~ key2
end

-- Generate garbled table for a gate
function YaoProtocol.generate_garbled_table(input1, input2, gate_type)
    local garbled_table = {}
    
    -- Simulate 4 possible input combinations (2^2)
    for i = 0, 3 do
        local input_a = bit32.band(i, 1) == 1
        local input_b = bit32.band(bit32.rshift(i, 1), 1) == 1
        
        -- Simulate gate operation (AND gate example)
        local output
        if gate_type == "AND" then
            output = input_a and input_b
        elseif gate_type == "OR" then
            output = input_a or input_b
        else
            output = input_a ~= input_b  -- XOR gate
        end
        
        -- Generate garbled values
        local key_a = generate_random_key()
        local key_b = generate_random_key()
        local key_output = generate_random_key()
        
        -- Store garbled values
        garbled_table[i] = {
            input_a = key_a,
            input_b = key_b,
            output = output and key_output or key_output + 1  -- Simplified
        }
    end
    
    return garbled_table
end

-- Generate garbled circuit
function YaoProtocol.generate_garbled_circuit(inputs, gates)
    local garbled_circuit = {}
    
    -- Generate garbled tables for each gate
    for i, gate in ipairs(gates) do
        local gate_table = YaoProtocol.generate_garbled_table(
            gate.input1, gate.input2, gate.type
        )
        garbled_circuit[i] = {
            type = gate.type,
            table = gate_table,
            output_wire = gate.output
        }
    end
    
    return garbled_circuit
end

-- Evaluate garbled circuit
function YaoProtocol.evaluate_circuit(garbled_circuit, input_keys)
    local results = {}
    
    for i, gate in ipairs(garbled_circuit) do
        -- Simplified evaluation - in practice, this would use the garbled table
        local input1_key = input_keys[gate.input1] or 0
        local input2_key = input_keys[gate.input2] or 0
        
        -- This is where the actual garbled evaluation would happen
        -- For demonstration, we'll just return the input keys
        results[gate.output_wire] = {
            key = input1_key + input2_key,
            type = gate.type
        }
    end
    
    return results
end

-- Main function to demonstrate Yao's protocol
function YaoProtocol.run_demo()
    print("=== Yao's Garbled Circuit Protocol Demo ===")
    
    -- Define inputs (simulated)
    local inputs = {
        {name = "input1", value = 1},
        {name = "input2", value = 0}
    }
    
    -- Define circuit gates (AND gate example)
    local gates = {
        {
            type = "AND",
            input1 = "input1",
            input2 = "input2",
            output = "output1"
        }
    }
    
    -- Generate garbled circuit
    local garbled_circuit = YaoProtocol.generate_garbled_circuit(inputs, gates)
    
    -- Generate input keys (these would be securely shared in practice)
    local input_keys = {
        input1 = generate_random_key(),
        input2 = generate_random_key()
    }
    
    print("Generated garbled circuit with " .. #garbled_circuit .. " gates")
    print("Input keys:")
    for key, value in pairs(input_keys) do
        print("  " .. key .. ": " .. value)
    end
    
    -- Evaluate circuit
    local results = YaoProtocol.evaluate_circuit(garbled_circuit, input_keys)
    
    print("\nEvaluation results:")
    for key, value in pairs(results) do
        print("  " .. key .. ": " .. value.key .. " (" .. value.type .. ")")
    end
    
    print("\nProtocol completed successfully!")
end

-- Run the demo
YaoProtocol.run_demo()

-- Additional utility functions for secure computation

-- Function to encrypt using garbled keys
function YaoProtocol.encrypt_with_key(plaintext, key)
    return plaintext + key
end

-- Function to decrypt using garbled keys
function YaoProtocol.decrypt_with_key(ciphertext, key)
    return ciphertext - key
end

-- Function to verify garbled circuit correctness
function YaoProtocol.verify_circuit(garbled_circuit, expected_outputs)
    local is_valid = true
    
    -- In a real implementation, this would verify the consistency
    -- of the garbled tables and their cryptographic properties
    
    print("Circuit verification: " .. (is_valid and "PASSED" or "FAILED"))
    return is_valid
end

-- Example usage with multiple gates
function YaoProtocol.run_complex_demo()
    print("\n=== Complex Circuit Demo ===")
    
    local complex_gates = {
        {type = "AND", input1 = "a", input2 = "b", output = "and_out"},
        {type = "OR", input1 = "a", input2 = "c", output = "or_out"},
        {type = "XOR", input1 = "b", input2 = "c", output = "xor_out"}
    }
    
    local circuit = YaoProtocol.generate_garbled_circuit({}, complex_gates)
    print("Generated complex circuit with " .. #circuit .. " gates")
end

-- Run complex demo
YaoProtocol.run_complex_demo()
```

This implementation demonstrates:

1. **Core Components**:
   - Garbled table generation for logic gates
   - Circuit generation with multiple gates
   - Circuit evaluation
   - Key generation and management

2. **Key Features**:
   - AND, OR, and XOR gate support
   - Random key generation
   - Garbled table construction
   - Circuit evaluation simulation

3. **Security Concepts**:
   - Key-based garbling
   - Input masking
   - Output computation

**Note**: This is a simplified educational implementation. A full cryptographic implementation would include:
- Proper cryptographic hash functions
- Secure key distribution
- Actual garbled table construction with cryptographic commitments
- Proper oblivious transfer protocols
- Advanced encryption schemes

The actual Yao's protocol requires sophisticated cryptographic techniques beyond this basic demonstration.

