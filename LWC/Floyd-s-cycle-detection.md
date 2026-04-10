# Floyd's Cycle Detection Algorithm in Lightning Web Component

Here's an example implementation of Floyd's Cycle Detection (Tortoise and Hare) algorithm in a Lightning Web Component:

```javascript
import { LightningElement } from 'lwc';

export default class CycleDetection extends LightningElement {
    nodeList = [];
    hasCycle = false;
    cycleStart = null;
    message = '';

    connectedCallback() {
        // Create a sample linked list with a cycle for demonstration
        this.createSampleList();
        this.detectCycle();
    }

    createSampleList() {
        // Create nodes
        this.nodeList = [
            { id: 1, value: 1, next: 2 },
            { id: 2, value: 2, next: 3 },
            { id: 3, value: 3, next: 4 },
            { id: 4, value: 4, next: 5 },
            { id: 5, value: 5, next: 2 } // This creates a cycle back to node 2
        ];
    }

    detectCycle() {
        // Floyd's Cycle Detection Algorithm
        if (this.nodeList.length < 2) {
            this.message = 'List too short to have a cycle';
            return;
        }

        // Initialize slow (tortoise) and fast (hare) pointers
        let slow = 0;  // Start at first node
        let fast = 0;  // Start at first node

        // Phase 1: Detect if cycle exists
        let hasCycle = false;
        let cycleStartIndex = -1;

        do {
            // Move slow pointer one step
            slow = this.nodeList[slow].next - 1; // Convert to 0-based index
            
            // Move fast pointer two steps
            fast = this.nodeList[fast].next - 1;
            fast = this.nodeList[fast].next - 1;

            // Check if pointers meet
            if (slow === fast) {
                hasCycle = true;
                cycleStartIndex = slow;
                break;
            }

        } while (slow !== fast && slow < this.nodeList.length && fast < this.nodeList.length);

        this.hasCycle = hasCycle;

        if (hasCycle) {
            // Phase 2: Find the start of the cycle
            let startPointer = 0;
            let cyclePointer = cycleStartIndex;

            while (startPointer !== cyclePointer) {
                startPointer = this.nodeList[startPointer].next - 1;
                cyclePointer = this.nodeList[cyclePointer].next - 1;
            }

            this.cycleStart = startPointer + 1; // Convert back to 1-based
            this.message = `Cycle detected! Start of cycle is node ${this.cycleStart}`;
        } else {
            this.message = 'No cycle detected in the linked list';
        }
    }

    // Alternative method to demonstrate the algorithm with a simpler approach
    detectCycleSimple() {
        const nodes = this.nodeList;
        
        if (nodes.length < 2) {
            return { hasCycle: false, message: 'Too few nodes' };
        }

        let slow = 0;
        let fast = 0;

        // Phase 1: Detect cycle
        while (fast < nodes.length && nodes[fast].next !== undefined) {
            slow = nodes[slow].next - 1;
            fast = nodes[fast].next - 1;
            if (nodes[fast].next !== undefined) {
                fast = nodes[fast].next - 1;
            }

            if (slow === fast) {
                return { hasCycle: true, message: 'Cycle detected!' };
            }
        }

        return { hasCycle: false, message: 'No cycle found' };
    }

    // Method to reset and re-run detection
    handleReset() {
        this.hasCycle = false;
        this.cycleStart = null;
        this.message = '';
        this.detectCycle();
    }

    get cycleClass() {
        return this.hasCycle ? 'cycle-found' : 'no-cycle';
    }
}
```

```html
<!-- cycle-detection.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2>Floyd's Cycle Detection Algorithm</h2>
        
        <div class="slds-text-body_small">
            <p>Algorithm detects cycles in linked lists using two pointers moving at different speeds.</p>
        </div>

        <div class="slds-m-top_medium">
            <lightning-card title="Detection Result">
                <div class="slds-text-body_regular">
                    <p class={cycleClass}>{message}</p>
                    
                    <template if:true={hasCycle}>
                        <p>Start of cycle: Node {cycleStart}</p>
                    </template>
                </div>
                <footer class="slds-box slds-theme_default slds-m-top_medium">
                    <lightning-button 
                        label="Reset Detection" 
                        onclick={handleReset}
                        variant="brand">
                    </lightning-button>
                </footer>
            </lightning-card>
        </div>

        <div class="slds-m-top_medium">
            <lightning-card title="Linked List Structure">
                <div class="slds-grid slds-gutters slds-wrap">
                    <template for:each={nodeList} for:item="node">
                        <div class="slds-col slds-size_1-of-5 slds-p-around_small">
                            <div class="slds-box slds-theme_default slds-text-align_center">
                                <p>Node {node.id}</p>
                                <p>Value: {node.value}</p>
                                <p>Next: {node.next}</p>
                            </div>
                        </div>
                    </template>
                </div>
            </lightning-card>
        </div>

        <div class="slds-m-top_medium">
            <lightning-card title="Algorithm Steps">
                <ul class="slds-list_dotted">
                    <li>Initialize two pointers (slow and fast) at the head</li>
                    <li>Move slow pointer one step, fast pointer two steps</li>
                    <li>If pointers meet, cycle exists</li>
                    <li>Reset one pointer to head, move both one step until they meet</li>
                    <li>Meeting point is start of cycle</li>
                </ul>
            </lightning-card>
        </div>
    </div>
</template>
```

```css
/* cycle-detection.css */
.cycle-found {
    color: #b32d2d;
    font-weight: bold;
}

.no-cycle {
    color: #179876;
    font-weight: bold;
}
```

## Key Features of this Implementation:

1. **Floyd's Algorithm**: Uses the classic two-pointer approach (tortoise and hare)
2. **Cycle Detection**: Detects if a cycle exists in the linked list
3. **Cycle Start Identification**: Finds the starting node of the cycle
4. **Visual Feedback**: Shows results in a user-friendly interface
5. **Reset Functionality**: Allows re-running the detection
6. **Educational Component**: Includes algorithm explanation and steps

## How it Works:

1. **Phase 1**: Two pointers move through the list - slow (1 step) and fast (2 steps)
2. **Phase 2**: If they meet, a cycle exists; find the start of the cycle
3. **Time Complexity**: O(n)
4. **Space Complexity**: O(1)

This implementation demonstrates the classic Floyd's cycle detection algorithm in a practical Lightning Web Component context.

