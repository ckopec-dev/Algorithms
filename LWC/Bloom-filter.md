# Bloom Filter Implementation in Lightning Web Component

```javascript
// bloomFilter.js
import { LightningElement } from 'lwc';

export default class BloomFilter extends LightningElement {
    // Bloom filter parameters
    filterSize = 1000; // Size of bit array
    hashCount = 3; // Number of hash functions
    bitArray = new Array(this.filterSize).fill(false);
    items = new Set(); // Store actual items for verification
    
    // Initialize the Bloom filter
    initialize() {
        this.bitArray = new Array(this.filterSize).fill(false);
        this.items.clear();
    }
    
    // Hash function using DJB2 algorithm
    djb2Hash(str, seed = 5381) {
        let hash = seed;
        for (let i = 0; i < str.length; i++) {
            hash = ((hash << 5) + hash) + str.charCodeAt(i);
            hash = hash & hash; // Convert to 32-bit integer
        }
        return Math.abs(hash) % this.filterSize;
    }
    
    // Generate multiple hash values for a string
    getHashes(str) {
        const hashes = [];
        for (let i = 0; i < this.hashCount; i++) {
            const hash = this.djb2Hash(str + i);
            hashes.push(hash);
        }
        return hashes;
    }
    
    // Add an item to the Bloom filter
    add(item) {
        const hashes = this.getHashes(item);
        hashes.forEach(hash => {
            this.bitArray[hash] = true;
        });
        this.items.add(item);
    }
    
    // Check if an item might exist in the Bloom filter
    mightContain(item) {
        const hashes = this.getHashes(item);
        for (let i = 0; i < hashes.length; i++) {
            if (!this.bitArray[hashes[i]]) {
                return false; // Definitely not present
            }
        }
        return true; // Might be present (false positive possible)
    }
    
    // Check if an item is definitely present (using actual set)
    isPresent(item) {
        return this.items.has(item);
    }
    
    // Get filter statistics
    getStats() {
        const setCount = this.items.size;
        const bitCount = this.bitArray.filter(bit => bit).length;
        const utilization = (bitCount / this.filterSize * 100).toFixed(2);
        
        return {
            totalItems: setCount,
            bitCount: bitCount,
            utilization: utilization + '%',
            falsePositiveRate: this.calculateFalsePositiveRate()
        };
    }
    
    // Calculate approximate false positive rate
    calculateFalsePositiveRate() {
        // Formula: (1 - e^(-kn/m))^k
        // where k = hashCount, n = items, m = filterSize
        const k = this.hashCount;
        const n = this.items.size;
        const m = this.filterSize;
        
        if (n === 0) return '0.00%';
        
        const rate = Math.pow(1 - Math.exp(-k * n / m), k);
        return (rate * 100).toFixed(2) + '%';
    }
    
    // Example usage methods
    handleAddItem() {
        const input = this.template.querySelector('input');
        if (input && input.value.trim()) {
            this.add(input.value.trim());
            input.value = '';
            this.dispatchEvent(new CustomEvent('itemadded'));
        }
    }
    
    handleCheckItem() {
        const input = this.template.querySelector('input');
        if (input && input.value.trim()) {
            const item = input.value.trim();
            const result = this.mightContain(item);
            const isPresent = this.isPresent(item);
            
            const event = new CustomEvent('checkresult', {
                detail: {
                    item: item,
                    mightExist: result,
                    definitelyExists: isPresent
                }
            });
            this.dispatchEvent(event);
        }
    }
}
```

```html
<!-- bloomFilter.html -->
<template>
    <div class="bloom-filter-container">
        <h2>Bloom Filter Demo</h2>
        
        <div class="input-section">
            <input type="text" placeholder="Enter item to add..." />
            <lightning-button label="Add Item" onclick={handleAddItem}></lightning-button>
            <lightning-button label="Check Item" onclick={handleCheckItem}></lightning-button>
        </div>
        
        <div class="stats-section">
            <h3>Filter Statistics</h3>
            <p>Total Items: {stats.totalItems}</p>
            <p>Bits Set: {stats.bitCount}</p>
            <p>Utilization: {stats.utilization}</p>
            <p>False Positive Rate: {stats.falsePositiveRate}</p>
        </div>
        
        <div class="result-section">
            <h3>Check Result</h3>
            <template if:true={checkResult}>
                <p>Item: {checkResult.item}</p>
                <p>Might Exist: {checkResult.mightExist}</p>
                <p>Definitely Exists: {checkResult.definitelyExists}</p>
            </template>
        </div>
        
        <div class="visualization">
            <h3>Bit Array Visualization</h3>
            <div class="bit-array">
                <template for:each={bitArray} for:item="bit">
                    <span key={bit.index} class="bit" 
                          style={bit.style}>
                        {bit.value}
                    </span>
                </template>
            </div>
        </div>
    </div>
</template>
```

```css
/* bloomFilter.css */
.bloom-filter-container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.input-section {
    margin-bottom: 20px;
    display: flex;
    gap: 10px;
    align-items: center;
}

.input-section input {
    flex: 1;
    padding: 8px;
    border: 1px solid #ccc;
    border-radius: 4px;
}

.stats-section {
    background-color: #f5f5f5;
    padding: 15px;
    border-radius: 4px;
    margin-bottom: 20px;
}

.stats-section p {
    margin: 5px 0;
}

.result-section {
    background-color: #e8f5e8;
    padding: 15px;
    border-radius: 4px;
    margin-bottom: 20px;
}

.bit-array {
    display: flex;
    flex-wrap: wrap;
    gap: 2px;
    margin-top: 10px;
}

.bit {
    width: 20px;
    height: 20px;
    display: inline-block;
    text-align: center;
    line-height: 20px;
    font-size: 10px;
    border: 1px solid #ccc;
    border-radius: 3px;
}

.bit[style*="background-color: rgb(0, 123, 255)"] {
    background-color: #007bff;
    color: white;
}

.bit[style*="background-color: rgb(220, 53, 69)"] {
    background-color: #dc3545;
    color: white;
}
```

## Usage Example

```javascript
// In your parent component
import { LightningElement } from 'lwc';

export default class ParentComponent extends LightningElement {
    bloomFilterRef;
    
    handleItemAdded() {
        console.log('Item added to Bloom filter');
    }
    
    handleCheckResult(event) {
        const result = event.detail;
        console.log(`Checking item: ${result.item}`);
        console.log(`Might exist: ${result.mightExist}`);
        console.log(`Definitely exists: ${result.definitelyExists}`);
    }
    
    connectedCallback() {
        // Initialize Bloom filter
        this.bloomFilterRef = this.template.querySelector('c-bloom-filter');
        this.bloomFilterRef.initialize();
        
        // Add some sample items
        this.bloomFilterRef.add('apple');
        this.bloomFilterRef.add('banana');
        this.bloomFilterRef.add('cherry');
    }
}
```

This implementation demonstrates a complete Bloom filter in Lightning Web Components with:

- **Hash functions**: Uses DJB2 algorithm for multiple hash generation
- **Bit array management**: Efficient storage and retrieval
- **False positive handling**: Shows the trade-off of Bloom filters
- **Real-time statistics**: Displays utilization and false positive rate
- **Visualization**: Shows the internal bit array state
- **Event-driven architecture**: Communicates with parent components through custom events

The Bloom filter is particularly useful for:
- Database indexing
- Cache optimization
- Network applications
- Duplicate detection
- Memory-efficient data structures

