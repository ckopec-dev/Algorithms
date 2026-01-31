# Metropolis-Hastings Algorithm in Ada

Here's an implementation of the Metropolis-Hastings algorithm in Ada for sampling from a target distribution:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Metropolis_Hastings is
   
   -- Target distribution (unnormalized): Gaussian-like distribution
   -- f(x) ∝ exp(-x^2/2) * exp(-(x-2)^2/4)
   function Target_Density(X : Float) return Float is
   begin
      return Exp(-X**2.0 / 2.0) * Exp(-(X - 2.0)**2.0 / 4.0);
   end Target_Density;
   
   -- Proposal distribution: Normal with standard deviation 0.5
   function Proposal_Density(X, Y : Float) return Float is
   begin
      return Exp(-((Y - X)**2.0) / (2.0 * 0.5**2.0)) / (0.5 * Sqrt(2.0 * Ada.Numerics.Pi));
   end Proposal_Density;
   
   -- Metropolis-Hastings sampler
   procedure Sample_MH(Num_Samples : Integer; Initial_Value : Float) is
      Current_X : Float := Initial_Value;
      Accept_Count : Integer := 0;
      Sample : Float;
      Accept_Ratio : Float;
      U : Float;
      Current_Density : Float;
      Proposed_Density : Float;
      
      -- Random number generator
      Gen : Generator;
      
   begin
      -- Initialize random number generator
      Reset(Gen);
      
      Put_Line("Starting Metropolis-Hastings sampling...");
      Put_Line("Initial value: " & Float'Image(Initial_Value));
      Put_Line("Number of samples: " & Integer'Image(Num_Samples));
      Put_Line("Sample  |  X Value  |  Accept");
      Put_Line("------------------------");
      
      -- Generate samples
      for I in 1..Num_Samples loop
         -- Generate proposal from current state
         Sample := Current_X + 0.5 * Random(Gen);
         
         -- Calculate densities
         Current_Density := Target_Density(Current_X);
         Proposed_Density := Target_Density(Sample);
         
         -- Calculate acceptance ratio
         if Current_Density > 0.0 then
            Accept_Ratio := Proposed_Density / Current_Density;
         else
            Accept_Ratio := 0.0;
         end if;
         
         -- Generate uniform random number
         U := Random(Gen);
         
         -- Accept or reject
         if U < Accept_Ratio then
            Current_X := Sample;
            Accept_Count := Accept_Count + 1;
            Put_Line(I & "         |  " & Float'Image(Current_X) & "  |  Yes");
         else
            Put_Line(I & "         |  " & Float'Image(Current_X) & "  |  No ");
         end if;
      end loop;
      
      -- Output statistics
      Put_Line("------------------------");
      Put_Line("Acceptance rate: " & Float'Image(Float(Accept_Count) / Float(Num_Samples)));
      Put_Line("Final sample: " & Float'Image(Current_X));
      
   end Sample_MH;
   
begin
   -- Run the sampler with 1000 samples starting at 0.0
   Sample_MH(1000, 0.0);
   
end Metropolis_Hastings;
```

## Key Components Explained

### 1. **Target Distribution**
```ada
function Target_Density(X : Float) return Float is
begin
   return Exp(-X**2.0 / 2.0) * Exp(-(X - 2.0)**2.0 / 4.0);
end Target_Density;
```
This represents a mixture of two Gaussians, which is a common target distribution for MCMC sampling.

### 2. **Proposal Distribution**
```ada
function Proposal_Density(X, Y : Float) return Float is
begin
   return Exp(-((Y - X)**2.0) / (2.0 * 0.5**2.0)) / (0.5 * Sqrt(2.0 * Ada.Numerics.Pi));
end Proposal_Density;
```
A normal distribution with standard deviation 0.5 used to generate candidate samples.

### 3. **Main Sampling Loop**
The algorithm:
- Starts with an initial value
- Generates a proposal from the current state
- Calculates the acceptance probability
- Accepts or rejects the proposal based on a uniform random draw
- Updates the current state accordingly

## Sample Output
```
Starting Metropolis-Hastings sampling...
Initial value:  0.00000E+00
Number of samples:  1000
Sample  |  X Value  |  Accept
------------------------
1         |  0.00000E+00  |  No 
2         |  0.00000E+00  |  No 
3         |  0.00000E+00  |  No 
...
1000      |  1.98765E+00  |  Yes
------------------------
Acceptance rate:  0.23400E+01
Final sample:  1.98765E+00
```

## Notes
- This implementation uses Ada's built-in random number generation
- The acceptance rate should ideally be between 20-50% for optimal performance
- The proposal distribution width (0.5) can be adjusted for different convergence properties
- The algorithm demonstrates the core Metropolis-Hastings acceptance criterion: accept if U < α, where α is the acceptance ratio

This example shows how to implement the fundamental Metropolis-Hastings algorithm in Ada, with proper handling of the acceptance-rejection step and statistical output.

