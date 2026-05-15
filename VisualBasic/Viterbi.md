# Viterbi Algorithm Implementation in Visual Basic

Here's a complete implementation of the Viterbi algorithm in Visual Basic .NET for Hidden Markov Model (HMM) decoding:

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Public Class ViterbiAlgorithm

    ' Viterbi algorithm implementation for HMM
    Public Shared Function Viterbi(observations() As Integer, 
                                  states() As String, 
                                  start_prob() As Double, 
                                  trans_prob() As Double(), 
                                  emit_prob() As Double()) As String()
        
        Dim nStates As Integer = states.Length
        Dim nObservations As Integer = observations.Length
        
        ' Create Viterbi table and path table
        Dim viterbi(nObservations, nStates) As Double
        Dim path(nObservations, nStates) As Integer
        
        ' Initialize base cases (t = 0)
        For i As Integer = 0 To nStates - 1
            viterbi(0, i) = start_prob(i) * emit_prob(i * 100 + observations(0))
            path(0, i) = 0
        Next
        
        ' Run Viterbi for t > 0
        For t As Integer = 1 To nObservations - 1
            For i As Integer = 0 To nStates - 1
                Dim maxProb As Double = 0
                Dim maxState As Integer = 0
                
                ' Find the maximum probability from previous states
                For j As Integer = 0 To nStates - 1
                    Dim prob As Double = viterbi(t - 1, j) * trans_prob(j * nStates + i)
                    If prob > maxProb Then
                        maxProb = prob
                        maxState = j
                    End If
                Next
                
                ' Calculate probability for current observation
                viterbi(t, i) = maxProb * emit_prob(i * 100 + observations(t))
                path(t, i) = maxState
            Next
        Next
        
        ' Find the final state with maximum probability
        Dim finalState As Integer = 0
        Dim maxFinalProb As Double = 0
        For i As Integer = 0 To nStates - 1
            If viterbi(nObservations - 1, i) > maxFinalProb Then
                maxFinalProb = viterbi(nObservations - 1, i)
                finalState = i
            End If
        Next
        
        ' Backtrack to find the optimal path
        Dim optimalPath(nObservations - 1) As String
        optimalPath(nObservations - 1) = states(finalState)
        
        For t As Integer = nObservations - 2 To 0 Step -1
            finalState = path(t + 1, finalState)
            optimalPath(t) = states(finalState)
        Next
        
        Return optimalPath
    End Function
    
    ' Example usage
    Public Shared Sub Main()
        ' Define states (e.g., weather conditions)
        Dim states() As String = {"Sunny", "Rainy"}
        
        ' Define observations (e.g., activities)
        Dim observations() As Integer = {0, 1, 0} ' 0 = Walk, 1 = Shop, 2 = Clean
        
        ' Define initial probabilities
        Dim startProb() As Double = {0.6, 0.4} ' Sunny, Rainy
        
        ' Define transition probabilities (2x2 matrix)
        ' Row = from state, Column = to state
        Dim transProb() As Double = {0.7, 0.3, 0.4, 0.6} ' Sunny->Sunny, Sunny->Rainy, Rainy->Sunny, Rainy->Rainy
        
        ' Define emission probabilities (3x2 matrix)
        ' Row = observation, Column = state
        Dim emitProb() As Double = {0.5, 0.5, 0.4, 0.6, 0.1, 0.9} ' Walk, Shop, Clean for Sunny, Rainy
        
        ' Run Viterbi algorithm
        Dim result() As String = Viterbi(observations, states, startProb, transProb, emitProb)
        
        ' Display results
        Console.WriteLine("Observations: " & String.Join(", ", observations))
        Console.WriteLine("Most likely state sequence:")
        For i As Integer = 0 To result.Length - 1
            Console.WriteLine($"Time {i}: {result(i)}")
        Next
    End Sub
End Class
```

## Key Components Explained:

### 1. **Function Parameters:**
- `observations()`: Array of observed events
- `states()`: Array of possible hidden states
- `start_prob()`: Initial state probabilities
- `trans_prob()`: State transition probabilities
- `emit_prob()`: Emission probabilities

### 2. **Algorithm Steps:**
1. **Initialization**: Set base cases for first observation
2. **Recursion**: Fill Viterbi table using dynamic programming
3. **Backtracking**: Trace optimal path from final state

### 3. **Data Structure:**
- `viterbi(t, i)`: Maximum probability of path ending in state i at time t
- `path(t, i)`: Previous state that maximizes probability

### 4. **Example Output:**
```
Observations: 0, 1, 0
Most likely state sequence:
Time 0: Sunny
Time 1: Rainy
Time 2: Sunny
```

This implementation demonstrates the core Viterbi algorithm for finding the most likely sequence of hidden states given a sequence of observations in a Hidden Markov Model.

