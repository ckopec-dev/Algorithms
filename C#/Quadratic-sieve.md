# Quadratic Sieve Algorithm Implementation in C#

```csharp
using System;
using System.Collections.Generic;
using System.Numerics;

public class QuadraticSieve
{
    private static readonly BigInteger Two = new BigInteger(2);
    
    public static BigInteger Factorize(BigInteger n)
    {
        if (n <= 1) return n;
        
        // Simple check for small primes
        if (IsPrime(n)) return n;
        
        Console.WriteLine($"Factoring {n}");
        
        // Use quadratic sieve algorithm
        var factors = new List<BigInteger>();
        var queue = new Queue<BigInteger>();
        queue.Enqueue(n);
        
        while (queue.Count > 0)
        {
            BigInteger current = queue.Dequeue();
            
            if (IsPrime(current))
            {
                factors.Add(current);
                continue;
            }
            
            // Try to find a factor
            BigInteger factor = FindFactor(current);
            
            if (factor == current)
            {
                // If we couldn't find a factor, try with a different approach
                factors.Add(current);
            }
            else
            {
                queue.Enqueue(factor);
                queue.Enqueue(current / factor);
            }
        }
        
        return factors.Count > 0 ? factors[0] : n;
    }
    
    private static BigInteger FindFactor(BigInteger n)
    {
        // This is a simplified version - in practice, this would be much more complex
        // The full quadratic sieve implementation requires:
        // 1. Finding smooth numbers
        // 2. Building a matrix of relations
        // 3. Solving the linear algebra problem
        
        // For demonstration, we'll use trial division for small factors
        for (BigInteger i = 2; i * i <= n; i++)
        {
            if (n % i == 0)
                return i;
        }
        
        return n;
    }
    
    private static bool IsPrime(BigInteger n)
    {
        if (n <= 1) return false;
        if (n <= 3) return true;
        if (n % 2 == 0 || n % 3 == 0) return false;
        
        for (BigInteger i = 5; i * i <= n; i += 6)
        {
            if (n % i == 0 || n % (i + 2) == 0)
                return false;
        }
        
        return true;
    }
    
    // More complete implementation of the core algorithm components
    public static void DemonstrateQuadraticSieve()
    {
        Console.WriteLine("=== Quadratic Sieve Algorithm Demo ===");
        
        // Example factorization
        BigInteger[] testNumbers = { 
            1340780792994259709957402499820584612747936582059239337772356144372176403007354697680187429816690342769003185818648605085375382152639060206340748092252318217444326170502102571410335347192702352573442452430151715328127312487110236300453710015144435013919522642625203550325130370819254302425762452275432521243430702060267322532284442029102304205617540313220392204532300148034511420490535510653750414404007351315004214380673430525705012432313226537098083725433512100835410150410021000437193924360160047091320415153451211553201015261815114418523301139104384357002417634222200293818002080728733543025420214140413628316881133137443031051200849571373653104023817012616350095451083105225835210034846023481512186407560041901001030020845144521250422216328800460403145551454005882517133062406113190016629418402404310941840930062884915468641445400340366430400434637246212422121122305331120440240000000000000000000000000000000