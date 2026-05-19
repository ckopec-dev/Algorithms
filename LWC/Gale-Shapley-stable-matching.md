```javascript
// Lightning Web Component for Gale-Shapley Stable Matching Algorithm
import { LightningElement } from 'lwc';

export default class GaleShapleyMatcher extends LightningElement {
    // Input data for the algorithm
    applicants = [
        { id: 'a1', name: 'Alice' },
        { id: 'a2', name: 'Bob' },
        { id: 'a3', name: 'Charlie' }
    ];

    employers = [
        { id: 'e1', name: 'Company A' },
        { id: 'e2', name: 'Company B' },
        { id: 'e3', name: 'Company C' }
    ];

    // Preference lists (ranked from 1 to n)
    applicantPreferences = {
        a1: ['e1', 'e2', 'e3'],
        a2: ['e2', 'e1', 'e3'],
        a3: ['e3', 'e2', 'e1']
    };

    employerPreferences = {
        e1: ['a2', 'a1', 'a3'],
        e2: ['a1', 'a3', 'a2'],
        e3: ['a3', 'a1', 'a2']
    };

    // State variables
    matchedPairs = [];
    unmatchedApplicants = [];
    result = '';

    connectedCallback() {
        this.performGaleShapley();
    }

    performGaleShapley() {
        // Initialize unmatched applicants
        this.unmatchedApplicants = [...this.applicants.map(a => a.id)];
        const engaged = {};
        const proposalHistory = {};

        // Initialize proposal history for each applicant
        this.applicants.forEach(applicant => {
            proposalHistory[applicant.id] = 0;
        });

        // Continue until all applicants are matched
        while (this.unmatchedApplicants.length > 0) {
            const currentApplicant = this.unmatchedApplicants[0];
            const currentApplicantPref = this.applicantPreferences[currentApplicant];
            const nextEmployerIndex = proposalHistory[currentApplicant];
            
            if (nextEmployerIndex >= currentApplicantPref.length) {
                // Applicant has proposed to all employers
                this.unmatchedApplicants.shift();
                continue;
            }

            const nextEmployer = currentApplicantPref[nextEmployerIndex];
            proposalHistory[currentApplicant]++;

            // Check if employer is already matched
            if (engaged[nextEmployer] === undefined) {
                // Employer is free, make the match
                engaged[nextEmployer] = currentApplicant;
                this.unmatchedApplicants.shift();
                this.result += `${currentApplicant} -> ${nextEmployer}\n`;
            } else {
                // Employer is matched, check preferences
                const currentPartner = engaged[nextEmployer];
                const employerPref = this.employerPreferences[nextEmployer];
                
                const currentPartnerRank = employerPref.indexOf(currentPartner);
                const currentApplicantRank = employerPref.indexOf(currentApplicant);
                
                if (currentApplicantRank < currentPartnerRank) {
                    // New applicant is preferred, break old engagement
                    this.unmatchedApplicants.push(currentPartner);
                    engaged[nextEmployer] = currentApplicant;
                    this.unmatchedApplicants.shift();
                    this.result += `${currentApplicant} -> ${nextEmployer}\n`;
                }
                // If current partner is preferred, do nothing
            }
        }

        // Format the final matching
        this.matchedPairs = Object.entries(engaged).map(([employerId, applicantId]) => ({
            employer: this.employers.find(e => e.id === employerId)?.name,
            applicant: this.applicants.find(a => a.id === applicantId)?.name
        }));

        this.result = `Stable Matching Result:\n${this.matchedPairs.map(pair => 
            `${pair.applicant} -> ${pair.employer}`).join('\n')}`;
    }

    // Method to run the algorithm manually
    handleRunMatching() {
        this.performGaleShapley();
    }

    // Method to reset the matching
    handleReset() {
        this.matchedPairs = [];
        this.result = '';
        this.unmatchedApplicants = [...this.applicants.map(a => a.id)];
    }

    get matchingResult() {
        return this.result;
    }

    get pairs() {
        return this.matchedPairs;
    }
}
```

```html
<!-- galeShapleyMatcher.html -->
<template>
    <div class="slds-card">
        <div class="slds-card__header slds-grid">
            <header class="slds-card__header-title slds-truncate" title="Gale-Shapley Algorithm">
                <h2 class="slds-text-heading_small">Gale-Shapley Stable Matching</h2>
            </header>
        </div>
        <div class="slds-card__body">
            <div class="slds-grid slds-gutters">
                <div class="slds-col slds-size_1-of-2">
                    <h3>Applicants</h3>
                    <ul class="slds-list_vertical slds-has-dividers_top-space">
                        <template for:each={applicants} for:item="applicant">
                            <li key={applicant.id} class="slds-list__item">
                                {applicant.name}
                            </li>
                        </template>
                    </ul>
                </div>
                <div class="slds-col slds-size_1-of-2">
                    <h3>Employers</h3>
                    <ul class="slds-list_vertical slds-has-dividers_top-space">
                        <template for:each={employers} for:item="employer">
                            <li key={employer.id} class="slds-list__item">
                                {employer.name}
                            </li>
                        </template>
                    </ul>
                </div>
            </div>

            <div class="slds-m-top_medium">
                <lightning-button 
                    label="Run Matching" 
                    variant="brand" 
                    onclick={handleRunMatching}>
                </lightning-button>
                <lightning-button 
                    label="Reset" 
                    variant="neutral" 
                    onclick={handleReset}
                    class="slds-m-left_small">
                </lightning-button>
            </div>

            <div class="slds-m-top_medium">
                <h3>Matching Result</h3>
                <lightning-textarea 
                    value={matchingResult} 
                    label="Stable Matching"
                    readonly>
                </lightning-textarea>
            </div>

            <div class="slds-m-top_medium">
                <h3>Final Pairs</h3>
                <template for:each={pairs} for:item="pair">
                    <div key={pair.employer} class="slds-p-around_small slds-border_bottom">
                        <p>{pair.applicant} → {pair.employer}</p>
                    </div>
                </template>
            </div>
        </div>
    </div>
</template>
```

```css
/* galeShapleyMatcher.css */
.slds-card__header-title {
    flex: 1 1 auto;
}

.slds-list_vertical {
    margin: 0;
    padding: 0;
    list-style: none;
}

.slds-list__item {
    padding: 0.5rem;
    border-bottom: 1px solid #e5e5e5;
}
```

This LWC component demonstrates the Gale-Shapley stable matching algorithm with:

1. **Data Structure**: Defines applicants and employers with their preference lists
2. **Algorithm Implementation**: 
   - Uses a proposal-based approach
   - Handles unmatched applicants and engagements
   - Implements preference checking logic
3. **UI Components**:
   - Displays applicant and employer lists
   - Shows the matching process
   - Provides run and reset functionality
4. **Stable Matching Logic**:
   - Ensures no unstable pairs exist
   - Maintains the algorithm's properties
   - Shows step-by-step matching process

The algorithm guarantees a stable matching where no applicant and employer would prefer each other over their current matches.

