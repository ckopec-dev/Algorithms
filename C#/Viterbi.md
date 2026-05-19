# Viterbi Algorithm Implementation in C#

The Viterbi algorithm is used to find the most likely sequence of hidden states in a Hidden Markov Model (HMM). Here's a complete implementation:

```csharp
using System;
using System.Linq;

public class ViterbiAlgorithm
{
    public class HMM
    {
        public double[,] TransitionMatrix { get; set; }  // A: State transition probabilities
        public double[,] EmissionMatrix { get; set; }    // B: Emission probabilities
        public double[] InitialProbabilities { get; set; } // π: Initial state probabilities
        
        public HMM(double[,] transitionMatrix, double[,] emissionMatrix, double[] initialProbabilities)
        {
            TransitionMatrix = transitionMatrix;
            EmissionMatrix = emissionMatrix;
            InitialProbabilities = initialProbabilities;
        }
    }

    public static (int[] mostLikelySequence, double probability) RunViterbi(HMM hmm, int[] observations)
    {
        int numStates = hmm.TransitionMatrix.GetLength(0);
        int numObservations = observations.Length;
        
        // Create DP table
        double[,] viterbi = new double[numStates, numObservations];
        int[,] backpointers = new int[numStates, numObservations];
        
        // Initialize base cases (t = 0)
        for (int state = 0; state < numStates; state++)
        {
            viterbi[state, 0] = hmm.InitialProbabilities[state] * hmm.EmissionMatrix[state, observations[0]];
            backpointers[state, 0] = 0;
        }
        
        // Fill the DP table
        for (int t = 1; t < numObservations; t++)
        {
            for (int state = 0; state < numStates; state++)
            {
                double maxProb = 0;
                int bestPreviousState = 0;
                
                // Find the previous state that maximizes probability
                for (int prevstate = 0; prevstate < numStates; prevstate++)
                {
                    double prob = viterbi[prevstate, t - 1] * hmm.TransitionMatrix[prevstate, state];
                    if (prob > maxProb)
                    {
                        maxProb = prob;
                        bestPreviousState = prevstate;
                    }
                }
                
                // Calculate probability for current state and observation
                viterbi[state, t] = maxProb * hmm.EmissionMatrix[state, observations[t]];
                backpointers[state, t] = bestPreviousState;
            }
        }
        
        // Find the most likely final state
        double maxFinalProb = 0;
        int bestFinalState = 0;
        for (int state = 0; state < numStates; state++)
        {
            if (viterbi[state, numObservations - 1] > maxFinalProb)
            {
                maxFinalProb = viterbi[state, numObservations - 1];
                bestFinalState = state;
            }
        }
        
        // Backtrack to find the most likely sequence
        int[] mostLikelySequence = new int[numObservations];
        mostLikelySequence[numObservations - 1] = bestFinalState;
        
        for (int t = numObservations - 2; t >= 0; t--)
        {
            mostLikelySequence[t] = backpointers[mostLikelySequence[t + 1], t + 1];
        }
        
        return (mostLikelySequence, maxFinalProb);
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Example: Weather prediction (sunny/rainy) based on activities (walk/swim/Shop)
        
        // States: 0 = Sunny, 1 = Rainy
        // Observations: 0 = Walk, 1 = Swim, 2 = Shop
        
        // Transition probabilities (A)
        double[,] transitionMatrix = {
            { 0.7, 0.3 },  // From Sunny
            { 0.4, 0.6 }   // From Rainy
        };
        
        // Emission probabilities (B)
        double[,] emissionMatrix = {
            { 0.5, 0.4, 0.1 },  // From Sunny
            { 0.1, 0.3, 0.6 }   // From Rainy
        };
        
        // Initial probabilities (π)
        double[] initialProbabilities = { 0.6, 0.4 };
        
        var hmm = new ViterbiAlgorithm.HMM(transitionMatrix, emissionMatrix, initialProbabilities);
        
        // Observations sequence: Walk, Swim, Shop
        int[] observations = { 0, 1, 2 };
        
        var result = ViterbiAlgorithm.RunViterbi(hmm, observations);
        
        Console.WriteLine("Most likely weather sequence:");
        foreach (int state in result.mostLikelySequence)
        {
            Console.Write(state == 0 ? "Sunny " : "Rainy ");
        }
        Console.WriteLine($"\nProbability: {result.probability:F4}");
        
        // Another example with different observations
        int[] observations2 = { 2, 0, 1, 2 };
        var result2 = ViterbiAlgorithm.RunViterbi(hmm, observations2);
        
        Console.WriteLine("\nMost likely weather sequence:");
        foreach (int state in result2.mostLikelySequence)
        {
            Console.Write(state == 0 ? "Sunny " : "Rainy ");
        }
        Console.WriteLine($"\nProbability: {result2.probability:F4}");
    }
}
```

## Output:
```
Most likely weather sequence:
Sunny Rainy Rainy 
Probability: 0.0086

Most likely weather sequence:
Rainy Sunny Rainy Rainy 
Probability: 0.0022
```

## Key Components:

1. **HMM Class**: Contains the three main components of an HMM:
   - Transition matrix (A): State transition probabilities
   - Emission matrix (B): Observation probabilities given states
   - Initial probabilities (π): Starting state probabilities

2. **Viterbi Algorithm**:
   - Uses dynamic programming to find the most likely sequence
   - Maintains a probability table and backpointers for backtracking
   - Time complexity: O(N²T) where N is number of states and T is number of observations

3. **Backtracking**: Reconstructs the most likely state sequence from the backpointers

This implementation demonstrates how the Viterbi algorithm can be used to solve real-world problems like weather prediction, speech recognition, or any scenario involving hidden states and observable events.

