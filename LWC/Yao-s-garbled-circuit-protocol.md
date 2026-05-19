# Yao's Garbled Circuit Protocol Implementation in Lightning Web Component

```javascript
// yaoGarbledCircuit.js
import { LightningElement, track } from 'lwc';

export default class YaoGarbledCircuit extends LightningElement {
    @track circuitInputs = [];
    @track garbledCircuit = [];
    @track result = '';
    @track isProcessing = false;

    // Sample circuit configuration
    circuitConfig = {
        gates: [
            { id: 'g1', type: 'AND', inputs: ['a', 'b'], output: 'ab' },
            { id: 'g2', type: 'XOR', inputs: ['ab', 'c'], output: 'result' }
        ],
        inputs: ['a', 'b', 'c'],
        outputs: ['result']
    };

    connectedCallback() {
        this.initializeCircuit();
    }

    initializeCircuit() {
        // Initialize input values
        this.circuitInputs = [
            { name: 'a', value: false },
            { name: 'b', value: false },
            { name: 'c', value: false }
        ];
    }

    // Generate garbled table for a gate
    generateGarbledTable(gate, inputValues) {
        const garbledTable = [];
        const inputs = gate.inputs;
        const output = gate.output;

        // Generate all possible input combinations
        const combinations = this.generateCombinations(inputs.length);
        
        combinations.forEach((combo, index) => {
            const inputBits = combo.map((bit, i) => {
                return inputValues[inputs[i]].value ? 1 : 0;
            });
            
            let outputValue = this.evaluateGate(gate, inputBits);
            
            // Create garbled entries
            const entry = {
                input: inputBits.join(''),
                output: outputValue,
                encryptedOutput: this.encryptValue(outputValue, index)
            };
            
            garbledTable.push(entry);
        });

        return garbledTable;
    }

    // Evaluate gate logic
    evaluateGate(gate, inputBits) {
        switch (gate.type) {
            case 'AND':
                return inputBits[0] && inputBits[1];
            case 'OR':
                return inputBits[0] || inputBits[1];
            case 'XOR':
                return inputBits[0] !== inputBits[1];
            case 'NOT':
                return !inputBits[0];
            default:
                return false;
        }
    }

    // Generate all combinations for input bits
    generateCombinations(numInputs) {
        const combinations = [];
        const total = Math.pow(2, numInputs);
        
        for (let i = 0; i < total; i++) {
            const bits = [];
            for (let j = 0; j < numInputs; j++) {
                bits.push((i >> j) & 1);
            }
            combinations.push(bits);
        }
        
        return combinations;
    }

    // Simple encryption for demonstration
    encryptValue(value, index) {
        // In real implementation, this would use proper cryptographic methods
        return value ? `enc_${index}_1` : `enc_${index}_0`;
    }

    // Generate garbled circuit
    generateGarbledCircuit() {
        this.isProcessing = true;
        
        try {
            const garbledCircuit = [];
            
            // Process each gate
            this.circuitConfig.gates.forEach(gate => {
                const gateInputs = this.circuitInputs.filter(input => 
                    gate.inputs.includes(input.name)
                );
                
                const table = this.generateGarbledTable(gate, this.circuitInputs);
                garbledCircuit.push({
                    gateId: gate.id,
                    gateType: gate.type,
                    inputs: gate.inputs,
                    output: gate.output,
                    table: table
                });
            });
            
            this.garbledCircuit = garbledCircuit;
            this.result = 'Garbled circuit generated successfully';
            
        } catch (error) {
            this.result = 'Error generating garbled circuit: ' + error.message;
        } finally {
            this.isProcessing = false;
        }
    }

    // Evaluate garbled circuit
    evaluateGarbledCircuit() {
        this.isProcessing = true;
        
        try {
            // In a real implementation, this would:
            // 1. Use oblivious transfer to get encrypted inputs
            // 2. Evaluate the garbled circuit
            // 3. Decrypt the result
            
            const inputs = this.circuitInputs.map(input => input.value ? 1 : 0);
            const result = this.evaluateCircuit(inputs);
            
            this.result = `Circuit result: ${result}`;
            
        } catch (error) {
            this.result = 'Error evaluating circuit: ' + error.message;
        } finally {
            this.isProcessing = false;
        }
    }

    // Simple circuit evaluation for demonstration
    evaluateCircuit(inputBits) {
        // AND gate: a AND b
        const ab = inputBits[0] && inputBits[1];
        // XOR gate: ab XOR c
        const result = ab !== inputBits[2];
        
        return result ? 1 : 0;
    }

    // Handle input change
    handleInputChange(event) {
        const inputName = event.target.name;
        const inputValue = event.target.checked;
        
        this.circuitInputs = this.circuitInputs.map(input => {
            if (input.name === inputName) {
                return { ...input, value: inputValue };
            }
            return input;
        });
    }

    // Reset circuit
    resetCircuit() {
        this.circuitInputs = this.circuitInputs.map(input => ({
            ...input,
            value: false
        }));
        this.garbledCircuit = [];
        this.result = '';
    }
}
```

```html
<!-- yaoGarbledCircuit.html -->
<template>
    <div class="container">
        <h2>Yao's Garbled Circuit Protocol</h2>
        
        <div class="input-section">
            <h3>Circuit Inputs</h3>
            <lightning-input 
                type="checkbox" 
                label="Input A" 
                name="a" 
                checked={circuitInputs[0].value}
                onchange={handleInputChange}>
            </lightning-input>
            
            <lightning-input 
                type="checkbox" 
                label="Input B" 
                name="b" 
                checked={circuitInputs[1].value}
                onchange={handleInputChange}>
            </lightning-input>
            
            <lightning-input 
                type="checkbox" 
                label="Input C" 
                name="c" 
                checked={circuitInputs[2].value}
                onchange={handleInputChange}>
            </lightning-input>
        </div>

        <div class="controls">
            <lightning-button 
                label="Generate Garbled Circuit" 
                variant="brand"
                onclick={generateGarbledCircuit}
                disabled={isProcessing}>
            </lightning-button>
            
            <lightning-button 
                label="Evaluate Circuit" 
                variant="success"
                onclick={evaluateGarbledCircuit}
                disabled={isProcessing || garbledCircuit.length === 0}>
            </lightning-button>
            
            <lightning-button 
                label="Reset" 
                variant="neutral"
                onclick={resetCircuit}>
            </lightning-button>
        </div>

        <div class="result-section">
            <h3>Result</h3>
            <p>{result}</p>
            
            <template if:true={garbledCircuit.length > 0}>
                <h4>Generated Garbled Circuit</h4>
                <lightning-datatable
                    data={garbledCircuit}
                    columns={columns}
                    key-field="gateId">
                </lightning-datatable>
            </template>
        </div>

        <div class="explanation">
            <h3>How Yao's Protocol Works</h3>
            <ul>
                <li>1. Generate garbled tables for each gate</li>
                <li>2. Encrypt input values using random keys</li>
                <li>3. Send encrypted inputs to evaluator</li>
                <li>4. Evaluate garbled circuit without revealing inputs</li>
                <li>5. Reveal only the final output</li>
            </ul>
        </div>
    </div>
</template>
```

```css
/* yaoGarbledCircuit.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.input-section {
    background-color: #f5f5f5;
    padding: 15px;
    border-radius: 8px;
    margin-bottom: 20px;
}

.controls {
    margin: 20px 0;
    display: flex;
    gap: 10px;
    flex-wrap: wrap;
}

.result-section {
    margin-top: 20px;
    padding: 15px;
    background-color: #e8f4f8;
    border-radius: 8px;
}

.explanation {
    margin-top: 30px;
    padding: 15px;
    background-color: #fff8e1;
    border-radius: 8px;
}

.explanation ul {
    margin: 10px 0;
    padding-left: 20px;
}

.explanation li {
    margin: 5px 0;
}
```

## Key Features of this Implementation

1. **Circuit Configuration**: Defines the logic gates and inputs
2. **Garbled Table Generation**: Creates encrypted truth tables for each gate
3. **Input Handling**: Manages boolean inputs through checkboxes
4. **Circuit Evaluation**: Demonstrates how the protocol would work
5. **Visual Interface**: Provides a user-friendly LWC interface
6. **Error Handling**: Includes proper error management

## Important Notes

This is a simplified educational implementation. A production version would require:
- Proper cryptographic libraries
- Secure key generation and management
- Oblivious transfer protocols
- Actual garbled circuit evaluation
- Secure communication channels

The protocol demonstrates the core concept of Yao's garbled circuit protocol for secure two-party computation.

