# Viterbi Algorithm in Lightning Web Component

Here's a complete example of implementing the Viterbi algorithm in a Lightning Web Component:

```javascript
// viterbiExample.js
import { LightningElement } from 'lwc';

export default class ViterbiExample extends LightningElement {
    // Example: Hidden Markov Model for weather prediction
    // States: Sunny, Rainy, Cloudy
    // Observations: Walk, Shop, Clean
    
    states = ['Sunny', 'Rainy', 'Cloudy'];
    observations = ['Walk', 'Shop', 'Clean'];
    
    // Transition probabilities
    transitionProb = {
        'Sunny': {'Sunny': 0.5, 'Rainy': 0.4, 'Cloudy': 0.1},
        'Rainy': {'Sunny': 0.3, 'Rainy': 0.4, 'Cloudy': 0.3},
        'Cloudy': {'Sunny': 0.2, 'Rainy': 0.3, 'Cloudy': 0.5}
    };
    
    // Emission probabilities
    emissionProb = {
        'Sunny': {'Walk': 0.6, 'Shop': 0.3, 'Clean': 0.1},
        'Rainy': {'Walk': 0.1, 'Shop': 0.6, 'Clean': 0.3},
        'Cloudy': {'Walk': 0.3, 'Shop': 0.3, 'Clean': 0.4}
    };
    
    // Initial probabilities
    initialProb = {'Sunny': 0.6, 'Rainy': 0.3, 'Cloudy': 0.1};
    
    // Input observations
    inputObservations = ['Walk', 'Shop', 'Clean'];
    result = '';
    path = [];
    
    handleRunViterbi() {
        const result = this.viterbi(this.inputObservations);
        this.result = `Most likely weather sequence: ${result.path.join(' -> ')}`;
        this.path = result.path;
    }
    
    viterbi(observations) {
        const V = [{}]; // Viterbi table
        const path = {};
        
        // Initialize base cases (t == 0)
        for (let i = 0; i < this.states.length; i++) {
            const state = this.states[i];
            V[0][state] = this.initialProb[state] * this.emissionProb[state][observations[0]];
            path[state] = [state];
        }
        
        // Run Viterbi for t > 0
        for (let t = 1; t < observations.length; t++) {
            V[t] = {};
            const currentObservation = observations[t];
            
            for (let j = 0; j < this.states.length; j++) {
                const currentState = this.states[j];
                let maxProb = 0;
                let maxState = '';
                
                // Find the maximum probability from all previous states
                for (let i = 0; i < this.states.length; i++) {
                    const prevState = this.states[i];
                    const prob = V[t-1][prevState] * this.transitionProb[prevState][currentState];
                    
                    if (prob > maxProb) {
                        maxProb = prob;
                        maxState = prevState;
                    }
                }
                
                V[t][currentState] = maxProb * this.emissionProb[currentState][currentObservation];
                path[currentState] = path[maxState].concat(currentState);
            }
        }
        
        // Find the most likely final state
        let maxProb = 0;
        let finalState = '';
        
        for (let i = 0; i < this.states.length; i++) {
            const state = this.states[i];
            if (V[observations.length - 1][state] > maxProb) {
                maxProb = V[observations.length - 1][state];
                finalState = state;
            }
        }
        
        return {
            probability: maxProb,
            path: path[finalState]
        };
    }
    
    handleInputChange(event) {
        const value = event.target.value;
        this.inputObservations = value.split(',').map(obs => obs.trim());
    }
}
```

```html
<!-- viterbiExample.html -->
<template>
    <div class="container">
        <h2>Viterbi Algorithm Example</h2>
        <p>This example demonstrates the Viterbi algorithm for finding the most likely sequence of hidden states.</p>
        
        <div class="input-section">
            <lightning-input 
                label="Enter observations (comma separated):" 
                value={inputObservations.join(', ')}
                onchange={handleInputChange}>
            </lightning-input>
            <lightning-button 
                label="Run Viterbi Algorithm" 
                onclick={handleRunViterbi}
                variant="brand">
            </lightning-button>
        </div>
        
        <div class="result-section">
            <lightning-card title="Result">
                <div class="slds-p-around_medium">
                    <p>{result}</p>
                    <p class="slds-text-title">Path:</p>
                    <ul class="slds-list_dotted">
                        <template for:each={path} for:item="state">
                            <li key={state}>{state}</li>
                        </template>
                    </ul>
                </div>
            </lightning-card>
        </div>
        
        <div class="model-info">
            <lightning-card title="Model Information">
                <div class="slds-p-around_medium">
                    <p><strong>States:</strong> {states.join(', ')}</p>
                    <p><strong>Observations:</strong> {observations.join(', ')}</p>
                </div>
            </lightning-card>
        </div>
    </div>
</template>
```

```css
/* viterbiExample.css */
.container {
    max-width: 800px;
    margin: 0 auto;
    padding: 1rem;
}

.input-section {
    margin-bottom: 1rem;
}

.result-section {
    margin-bottom: 1rem;
}

.model-info {
    margin-top: 1rem;
}

.slds-list_dotted {
    padding-left: 1rem;
}
```

## How the Viterbi Algorithm Works in This Example

1. **Model Setup**: 
   - States: Sunny, Rainy, Cloudy (weather conditions)
   - Observations: Walk, Shop, Clean (activities)
   - Transition probabilities between weather states
   - Emission probabilities for activities given weather

2. **Algorithm Steps**:
   - Initialize probabilities for first observation
   - For each subsequent observation, calculate the probability of reaching each state
   - Keep track of the most probable path to each state
   - Backtrack to find the complete most likely sequence

3. **Key Components**:
   - `viterbi()`: Main algorithm implementation
   - `V` table: Stores maximum probabilities at each time step
   - `path` object: Tracks the most likely path to each state
   - Dynamic input handling for different observation sequences

This implementation shows how the Viterbi algorithm can be used for Hidden Markov Model problems, specifically for weather prediction based on daily activities.

