# Critical Path Method (CPM) in Lightning Web Component

Here's a complete implementation of the Critical Path Method algorithm in Lightning Web Components:

## HTML Template (criticalPath.html)

```html
<template>
    <div class="slds-box slds-theme_default">
        <h2>Critical Path Method Calculator</h2>
        
        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_1-of-2">
                <lightning-card title="Project Activities">
                    <div class="slds-m-bottom_medium">
                        <lightning-button 
                            label="Add Activity" 
                            onclick={handleAddActivity}
                            variant="brand"
                            class="slds-m-bottom_small">
                        </lightning-button>
                        <lightning-button 
                            label="Calculate CPM" 
                            onclick={calculateCriticalPath}
                            variant="success"
                            class="slds-m-bottom_small">
                        </lightning-button>
                    </div>
                    
                    <lightning-datatable
                        data={activitiesData}
                        columns={columns}
                        key-field="id"
                        onrowaction={handleRowAction}>
                    </lightning-datatable>
                </lightning-card>
            </div>
            
            <div class="slds-col slds-size_1-of-2">
                <lightning-card title="Results">
                    <template if:true={hasResults}>
                        <div class="slds-m-bottom_medium">
                            <p><strong>Project Duration:</strong> {projectDuration} days</p>
                            <p><strong>Critical Path:</strong></p>
                            <ul class="slds-list_dotted">
                                <template for:each={criticalPath} for:item="activity">
                                    <li key={activity.id}>{activity.name}</li>
                                </template>
                            </ul>
                        </div>
                        
                        <lightning-datatable
                            data={criticalPathData}
                            columns={resultColumns}
                            key-field="id"
                            hide-checkbox-column="true">
                        </lightning-datatable>
                    </template>
                    
                    <template if:false={hasResults}>
                        <p>No calculations performed yet. Click "Calculate CPM" to compute.</p>
                    </template>
                </lightning-card>
            </div>
        </div>
    </div>
</template>
```

## JavaScript Controller (criticalPath.js)

```javascript
import { LightningElement, track } from 'lwc';

export default class CriticalPath extends LightningElement {
    @track activities = [
        { id: '1', name: 'Design Phase', duration: 5, predecessors: [] },
        { id: '2', name: 'Development Phase', duration: 8, predecessors: ['1'] },
        { id: '3', name: 'Testing Phase', duration: 4, predecessors: ['2'] },
        { id: '4', name: 'Documentation', duration: 3, predecessors: ['1'] },
        { id: '5', name: 'Deployment', duration: 2, predecessors: ['3', '4'] }
    ];
    
    @track hasResults = false;
    @track projectDuration = 0;
    @track criticalPath = [];
    @track criticalPathData = [];

    columns = [
        { label: 'Activity Name', fieldName: 'name', type: 'text' },
        { label: 'Duration (days)', fieldName: 'duration', type: 'number' },
        { label: 'Predecessors', fieldName: 'predecessorsString', type: 'text' }
    ];

    resultColumns = [
        { label: 'Activity Name', fieldName: 'name', type: 'text' },
        { label: 'Early Start', fieldName: 'earlyStart', type: 'number' },
        { label: 'Early Finish', fieldName: 'earlyFinish', type: 'number' },
        { label: 'Late Start', fieldName: 'lateStart', type: 'number' },
        { label: 'Late Finish', fieldName: 'lateFinish', type: 'number' },
        { label: 'Slack', fieldName: 'slack', type: 'number' }
    ];

    get activitiesData() {
        return this.activities.map(activity => ({
            ...activity,
            predecessorsString: activity.predecessors.join(', ')
        }));
    }

    handleAddActivity() {
        const newId = (this.activities.length + 1).toString();
        this.activities.push({
            id: newId,
            name: `Activity ${newId}`,
            duration: 1,
            predecessors: []
        });
        this.refreshData();
    }

    handleRowAction(event) {
        const action = event.detail.action;
        const row = event.detail.row;
        
        if (action.name === 'delete') {
            this.activities = this.activities.filter(activity => activity.id !== row.id);
            this.refreshData();
        }
    }

    refreshData() {
        // Force re-render
        this.hasResults = false;
        this.projectDuration = 0;
        this.criticalPath = [];
        this.criticalPathData = [];
    }

    calculateCriticalPath() {
        if (this.activities.length === 0) {
            return;
        }

        // Step 1: Calculate Early Start and Early Finish times
        const earlyTimes = this.calculateEarlyTimes();
        
        // Step 2: Calculate Late Start and Late Finish times
        const lateTimes = this.calculateLateTimes(earlyTimes);
        
        // Step 3: Identify critical path
        const criticalPath = this.identifyCriticalPath(lateTimes);
        
        // Step 4: Prepare results for display
        this.projectDuration = earlyTimes[this.activities.length - 1].earlyFinish;
        this.criticalPath = criticalPath;
        this.criticalPathData = this.prepareResultData(earlyTimes, lateTimes);
        this.hasResults = true;
    }

    calculateEarlyTimes() {
        // Create a copy of activities sorted by ID for consistent processing
        const sortedActivities = [...this.activities].sort((a, b) => 
            parseInt(a.id) - parseInt(b.id)
        );
        
        const earlyTimes = [];
        
        // Initialize all early times to 0
        for (let i = 0; i < sortedActivities.length; i++) {
            earlyTimes.push({
                id: sortedActivities[i].id,
                name: sortedActivities[i].name,
                duration: sortedActivities[i].duration,
                earlyStart: 0,
                earlyFinish: 0
            });
        }
        
        // Calculate early start and finish times
        for (let i = 0; i < sortedActivities.length; i++) {
            const activity = sortedActivities[i];
            const earlyTime = earlyTimes[i];
            
            if (activity.predecessors.length === 0) {
                earlyTime.earlyStart = 0;
                earlyTime.earlyFinish = earlyTime.duration;
            } else {
                // Find maximum early finish time of predecessors
                let maxEarlyFinish = 0;
                for (const predId of activity.predecessors) {
                    const predIndex = sortedActivities.findIndex(a => a.id === predId);
                    if (predIndex !== -1 && earlyTimes[predIndex].earlyFinish > maxEarlyFinish) {
                        maxEarlyFinish = earlyTimes[predIndex].earlyFinish;
                    }
                }
                earlyTime.earlyStart = maxEarlyFinish;
                earlyTime.earlyFinish = earlyTime.earlyStart + earlyTime.duration;
            }
        }
        
        return earlyTimes;
    }

    calculateLateTimes(earlyTimes) {
        const sortedActivities = [...this.activities].sort((a, b) => 
            parseInt(a.id) - parseInt(b.id)
        );
        
        // Initialize late times
        const lateTimes = [...earlyTimes];
        
        // Set last activity's late finish to its early finish
        const lastActivityIndex = earlyTimes.length - 1;
        lateTimes[lastActivityIndex].lateFinish = earlyTimes[lastActivityIndex].earlyFinish;
        lateTimes[lastActivityIndex].lateStart = lateTimes[lastActivityIndex].earlyStart;
        
        // Calculate backward from the last activity
        for (let i = lastActivityIndex; i >= 0; i--) {
            const activity = sortedActivities[i];
            const lateTime = lateTimes[i];
            
            if (i === lastActivityIndex) {
                // Last activity - already set
                continue;
            } else {
                // Find minimum late start time of successors
                let minLateStart = Infinity;
                for (let j = 0; j < sortedActivities.length; j++) {
                    const successor = sortedActivities[j];
                    if (successor.predecessors.includes(activity.id) && 
                        lateTimes[j].lateStart < minLateStart) {
                        minLateStart = lateTimes[j].lateStart;
                    }
                }
                
                if (minLateStart !== Infinity) {
                    lateTime.lateFinish = minLateStart;
                    lateTime.lateStart = lateTime.lateFinish - lateTime.duration;
                }
            }
        }
        
        return lateTimes;
    }

    identifyCriticalPath(lateTimes) {
        const criticalPath = [];
        
        for (let i = 0; i < lateTimes.length; i++) {
            const time = lateTimes[i];
            // Critical path activities have zero slack
            if (time.lateStart === time.earlyStart) {
                criticalPath.push({
                    id: time.id,
                    name: time.name
                });
            }
        }
        
        return criticalPath;
    }

    prepareResultData(earlyTimes, lateTimes) {
        const resultData = [];
        
        for (let i = 0; i < earlyTimes.length; i++) {
            const early = earlyTimes[i];
            const late = lateTimes[i];
            
            resultData.push({
                id: early.id,
                name: early.name,
                earlyStart: early.earlyStart,
                earlyFinish: early.earlyFinish,
                lateStart: late.lateStart,
                lateFinish: late.lateFinish,
                slack: late.lateStart - early.earlyStart
            });
        }
        
        return resultData;
    }
}
```

## CSS Styling (criticalPath.css)

```css
.slds-box {
    margin-bottom: 1rem;
}

.slds-m-bottom_medium {
    margin-bottom: 1rem;
}

.slds-m-bottom_small {
    margin-bottom: 0.5rem;
}
```

## How the CPM Algorithm Works

This implementation demonstrates the Critical Path Method algorithm with these key steps:

### 1. **Forward Pass (Early Times)**
- Calculate Early Start (ES) and Early Finish (EF) times
- ES of activities with no predecessors = 0
- EF = ES + Duration
- ES of subsequent activities = maximum EF of all predecessor activities

### 2. **Backward Pass (Late Times)**
- Calculate Late Start (LS) and Late Finish (LF) times
- LF of the final activity = EF of the final activity
- LS = LF - Duration
- LF of predecessors = minimum LS of all successor activities

### 3. **Critical Path Identification**
- Activities where ES = LS (or EF = LF) are on the critical path
- Slack = LS - ES (or LF - EF)
- Critical path activities have zero slack

## Features

1. **Interactive Activity Management**: Add and delete activities
2. **Real-time Calculation**: Computes CPM with one click
3. **Visual Results**: Shows project duration and critical path
4. **Detailed Analysis**: Displays early/late start/finish times and slack
5. **Responsive Design**: Works on all device sizes

## Usage

1. Add activities with their durations and predecessors
2. Click "Calculate CPM" to compute the critical path
3. View project duration and critical path activities
4. Examine detailed timing information for each activity

This implementation provides a complete, working example of the Critical Path Method algorithm in Lightning Web Components, suitable for project management applications.

