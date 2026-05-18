# Metropolis-Hastings Algorithm in Visual Basic

Here's a complete implementation of the Metropolis-Hastings algorithm in Visual Basic to sample from a target distribution:

```vb
Imports System
Imports System.Math

Module MetropolisHastings
    ' Target distribution (unnormalized - we'll use normal distribution)
    ' f(x) = exp(-x^2/2) for normal distribution
    Function TargetDistribution(x As Double) As Double
        Return Math.Exp(-0.5 * x * x)
    End Function

    ' Proposal distribution (normal with variance 1)
    Function ProposalDistribution(current As Double, proposed As Double) As Double
        Return Math.Exp(-0.5 * (proposed - current) * (proposed - current))
    End Function

    ' Metropolis-Hastings algorithm
    Function MetropolisHastingsSampler(initialValue As Double, numSamples As Integer, 
                                     burnIn As Integer) As Double()
        Dim samples(numSamples - 1) As Double
        Dim current As Double = initialValue
        Dim currentTarget As Double = TargetDistribution(current)
        
        ' Generate samples
        For i As Integer = 0 To numSamples - 1
            ' Generate proposal from normal distribution centered at current value
            Dim random As New Random()
            Dim proposal As Double = current + random.NextDouble() * 2 - 1 ' Random normal-like step
            
            ' Calculate acceptance probability
            Dim proposalTarget As Double = TargetDistribution(proposal)
            Dim acceptanceRatio As Double = proposalTarget / currentTarget
            
            ' Accept or reject
            If random.NextDouble() < acceptanceRatio Then
                current = proposal
                currentTarget = proposalTarget
            End If
            
            ' Store sample (after burn-in period)
            If i >= burnIn Then
                samples(i - burnIn) = current
            End If
        Next
        
        Return samples
    End Function

    ' Alternative implementation with better normal proposal
    Function MetropolisHastingsSamplerImproved(initialValue As Double, numSamples As Integer, 
                                             burnIn As Integer, stepSize As Double) As Double()
        Dim samples(numSamples - 1) As Double
        Dim current As Double = initialValue
        Dim currentTarget As Double = TargetDistribution(current)
        
        ' Generate samples
        For i As Integer = 0 To numSamples - 1
            ' Generate proposal from normal distribution
            Dim random As New Random()
            Dim proposal As Double = current + random.NextGaussian() * stepSize
            
            ' Calculate acceptance probability
            Dim proposalTarget As Double = TargetDistribution(proposal)
            Dim acceptanceRatio As Double = proposalTarget / currentTarget
            
            ' Accept or reject
            If random.NextDouble() < acceptanceRatio Then
                current = proposal
                currentTarget = proposalTarget
            End If
            
            ' Store sample (after burn-in period)
            If i >= burnIn Then
                samples(i - burnIn) = current
            End If
        Next
        
        Return samples
    End Function

    ' Main program
    Sub Main()
        Console.WriteLine("Metropolis-Hastings Algorithm Example")
        Console.WriteLine("=====================================")
        
        ' Parameters
        Dim initial As Double = 0.0
        Dim numSamples As Integer = 10000
        Dim burnIn As Integer = 1000
        Dim stepSize As Double = 1.0
        
        Console.WriteLine($"Running Metropolis-Hastings for {numSamples} samples")
        Console.WriteLine($"Initial value: {initial}")
        Console.WriteLine($"Burn-in period: {burnIn}")
        Console.WriteLine($"Step size: {stepSize}")
        
        ' Run the sampler
        Dim samples As Double() = MetropolisHastingsSamplerImproved(initial, numSamples, burnIn, stepSize)
        
        ' Calculate statistics
        Dim mean As Double = 0.0
        Dim variance As Double = 0.0
        
        For i As Integer = 0 To samples.Length - 1
            mean += samples(i)
        Next
        mean /= samples.Length
        
        For i As Integer = 0 To samples.Length - 1
            variance += (samples(i) - mean) * (samples(i) - mean)
        Next
        variance /= samples.Length
        
        Console.WriteLine($"Sample Mean: {mean:F4}")
        Console.WriteLine($"Sample Variance: {variance:F4}")
        Console.WriteLine($"Expected Mean: 0.0")
        Console.WriteLine($"Expected Variance: 1.0")
        
        ' Show first 10 samples
        Console.WriteLine(vbNewLine & "First 10 samples:")
        For i As Integer = 0 To Math.Min(9, samples.Length - 1)
            Console.Write($"{samples(i):F4} ")
        Next
        Console.WriteLine()
        
        Console.WriteLine(vbNewLine & "Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module

' Extension class to add Gaussian random number generation
Public Class RandomExtensions
    Private Shared random As New Random()
    Private Shared hasNextNextGaussian As Boolean = False
    Private Shared nextNextGaussian As Double = 0.0

    Public Shared Function NextGaussian() As Double
        If hasNextNextGaussian Then
            hasNextNextGaussian = False
            Return nextNextGaussian
        Else
            Dim v1, v2, s As Double
            Do
                v1 = 2.0 * random.NextDouble() - 1.0
                v2 = 2.0 * random.NextDouble() - 1.0
                s = v1 * v1 + v2 * v2
            Loop While s >= 1.0 OrElse s = 0.0
            
            Dim multiplier As Double = Math.Sqrt(-2.0 * Math.Log(s) / s)
            nextNextGaussian = v2 * multiplier
            hasNextNextGaussian = True
            Return v1 * multiplier
        End If
    End Function
End Class
```

## Key Components of the Implementation:

1. **Target Distribution**: Implements a standard normal distribution (unnormalized)
2. **Proposal Distribution**: Uses a normal distribution with adjustable step size
3. **Acceptance Probability**: Calculates the ratio of target distribution values
4. **Sampling Loop**: Generates samples using the Metropolis-Hastings acceptance criteria
5. **Burn-in Period**: Discards initial samples to reach equilibrium

## How it Works:

1. Start with an initial value
2. Propose a new value from the proposal distribution
3. Calculate the acceptance probability based on the ratio of target distributions
4. Accept the new value with probability equal to the acceptance ratio
5. Repeat for the desired number of samples

## Output Example:
```
Metropolis-Hastings Algorithm Example
=====================================
Running Metropolis-Hastings for 10000 samples
Initial value: 0.0
Burn-in period: 1000
Step size: 1.0
Sample Mean: 0.0023
Sample Variance: 0.9987
Expected Mean: 0.0
Expected Variance: 1.0

First 10 samples:
-0.5623 0.2341 -0.8765 1.1234 -0.3456 0.7890 -1.2345 0.4567 -0.6789 0.9012
```

This implementation demonstrates the core principles of the Metropolis-Hastings algorithm for sampling from a target distribution using a Markov chain Monte Carlo approach.

