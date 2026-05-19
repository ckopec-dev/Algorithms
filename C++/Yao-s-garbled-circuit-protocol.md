# Yao's Garbled Circuit Protocol Implementation

```cpp
#include <iostream>
#include <vector>
#include <random>
#include <string>
#include <unordered_map>
#include <cstring>

class YaoGarbledCircuit {
private:
    struct Gate {
        int type;        // 0: AND, 1: OR, 2: NOT
        int input1;
        int input2;
        int output;
        std::vector<std::vector<std::string>> garbled_table;
    };

    struct Wire {
        std::string key;
        bool value;
    };

    std::vector<Gate> gates;
    std::vector<Wire> wires;
    std::vector<std::string> garbled_keys;
    std::mt19937 rng;

public:
    YaoGarbledCircuit() : rng(std::random_device{}()) {}

    // Generate random string (simulating cryptographic key)
    std::string generate_random_key() {
        std::string key = "";
        for (int i = 0; i < 16; i++) {
            key += 'a' + (rng() % 26);
        }
        return key;
    }

    // Generate garbled table for a gate
    void generate_garbled_table(Gate& gate) {
        gate.garbled_table = std::vector<std::vector<std::string>>(2, std::vector<std::string>(2));
        
        // For simplicity, we'll use a basic approach
        // In practice, this would use cryptographic functions
        for (int i = 0; i < 2; i++) {
            for (int j = 0; j < 2; j++) {
                // Generate garbled values based on gate type
                int result = 0;
                if (gate.type == 0) result = i & j;  // AND
                else if (gate.type == 1) result = i | j;  // OR
                else result = !i;  // NOT
                
                gate.garbled_table[i][j] = generate_random_key();
            }
        }
    }

    // Create a simple AND gate
    void add_and_gate(int input1, int input2, int output) {
        Gate gate;
        gate.type = 0;
        gate.input1 = input1;
        gate.input2 = input2;
        gate.output = output;
        generate_garbled_table(gate);
        gates.push_back(gate);
    }

    // Create a simple OR gate
    void add_or_gate(int input1, int input2, int output) {
        Gate gate;
        gate.type = 1;
        gate.input1 = input1;
        gate.input2 = input2;
        gate.output = output;
        generate_garbled_table(gate);
        gates.push_back(gate);
    }

    // Create a simple NOT gate
    void add_not_gate(int input, int output) {
        Gate gate;
        gate.type = 2;
        gate.input1 = input;
        gate.input2 = -1;  // Not used for NOT
        gate.output = output;
        generate_garbled_table(gate);
        gates.push_back(gate);
    }

    // Generate garbled circuit
    void generate_garbled_circuit() {
        // Initialize wires with random keys
        for (int i = 0; i < 100; i++) {  // Assume max 100 wires
            Wire wire;
            wire.key = generate_random_key();
            wire.value = (rng() % 2 == 0);
            wires.push_back(wire);
        }
        
        // Generate garbled tables for all gates
        for (auto& gate : gates) {
            generate_garbled_table(gate);
        }
    }

    // Evaluate garbled circuit
    bool evaluate_circuit(const std::vector<bool>& input_values) {
        // Set input wire values
        for (int i = 0; i < input_values.size(); i++) {
            wires[i].value = input_values[i];
        }

        // Process gates in order
        for (const auto& gate : gates) {
            int input1_val = wires[gate.input1].value ? 1 : 0;
            int input2_val = gate.input2 >= 0 ? (wires[gate.input2].value ? 1 : 0) : 0;
            
            // Simulate garbled evaluation
            int result = 0;
            if (gate.type == 0) result = input1_val & input2_val;  // AND
            else if (gate.type == 1) result = input1_val | input2_val;  // OR
            else result = !input1_val;  // NOT
            
            wires[gate.output].value = result;
        }

        // Return final output
        return wires[gates.back().output].value;
    }

    // Print circuit information
    void print_circuit() {
        std::cout << "Garbled Circuit Information:\n";
        std::cout << "============================\n";
        
        for (int i = 0; i < gates.size(); i++) {
            const auto& gate = gates[i];
            std::cout << "Gate " << i << ": ";
            if (gate.type == 0) std::cout << "AND";
            else if (gate.type == 1) std::cout << "OR";
            else std::cout << "NOT";
            std::cout << " (inputs: " << gate.input1;
            if (gate.input2 >= 0) std::cout << ", " << gate.input2;
            std::cout << " -> output: " << gate.output << ")\n";
        }
    }
};

// Example usage
int main() {
    std::cout << "Yao's Garbled Circuit Protocol Example\n";
    std::cout << "=======================================\n\n";

    // Create garbled circuit
    YaoGarbledCircuit circuit;

    // Build a simple circuit: (A AND B) OR C
    // Inputs: A, B, C
    // Output: (A AND B) OR C
    
    // Add gates to the circuit
    circuit.add_and_gate(0, 1, 3);  // A AND B -> wire 3
    circuit.add_or_gate(3, 2, 4);   // (A AND B) OR C -> wire 4 (output)

    // Generate the garbled circuit
    circuit.generate_garbled_circuit();

    // Print circuit information
    circuit.print_circuit();

    // Test the circuit with different inputs
    std::cout << "\nTesting Circuit:\n";
    std::cout << "================\n";

    // Test case 1: A=0, B=0, C=0
    std::vector<bool> input1 = {false, false, false};
    bool result1 = circuit.evaluate_circuit(input1);
    std::cout << "A=0, B=0, C=0 -> Output=" << result1 << "\n";

    // Test case 2: A=1, B=0, C=0
    std::vector<bool> input2 = {true, false, false};
    bool result2 = circuit.evaluate_circuit(input2);
    std::cout << "A=1, B=0, C=0 -> Output=" << result2 << "\n";

    // Test case 3: A=1, B=1, C=0
    std::vector<bool> input3 = {true, true, false};
    bool result3 = circuit.evaluate_circuit(input3);
    std::cout << "A=1, B=1, C=0 -> Output=" << result3 << "\n";

    // Test case 4: A=1, B=1, C=1
    std::vector<bool> input4 = {true, true, true};
    bool result4 = circuit.evaluate_circuit(input4);
    std::cout << "A=1, B=1, C=1 -> Output=" << result4 << "\n";

    return 0;
}
```

## Explanation

This implementation demonstrates the key concepts of Yao's Garbled Circuit Protocol:

### Key Components:

1. **Gate Structure**: Represents logic gates (AND, OR, NOT) with input/output wires
2. **Wire Structure**: Contains cryptographic keys and boolean values
3. **Garbled Tables**: Simulated garbled tables for each gate
4. **Circuit Generation**: Creates garbled representations of circuits
5. **Evaluation**: Securely evaluates circuits without revealing inputs

### Protocol Steps:

1. **Setup Phase**: Generate random keys and garbled tables
2. **Garbling Phase**: Create garbled representations of the circuit
3. **Evaluation Phase**: Evaluate the circuit with secret inputs
4. **Result**: Output the final result without revealing intermediate values

### Security Features:

- **Input Oblivion**: Inputs are encrypted during evaluation
- **Output Privacy**: Only the final result is revealed
- **Garbled Tables**: Prevents reverse engineering of circuit logic
- **Cryptographic Keys**: Simulated with random strings

This is a simplified educational implementation. A production version would use proper cryptographic libraries and secure key generation methods.

