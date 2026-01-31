```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Sieve_Of_Eratosthenes is
   type Boolean_Array is array (Integer range <>) of Boolean;
   
   procedure Print_Primes(Primes : in Boolean_Array; Limit : in Integer) is
   begin
      Put("Prime numbers up to ");
      Put(Limit);
      Put_Line(":");
      
      for I in Primes'First .. Primes'Last loop
         if Primes(I) then
            Put(I);
            Put(" ");
         end if;
      end loop;
      New_Line;
   end Print_Primes;
   
   procedure Sieve(Primes : in out Boolean_Array; Limit : in Integer) is
      I, J : Integer;
   begin
      -- Initialize all numbers as prime (True)
      for I in Primes'First .. Primes'Last loop
         Primes(I) := True;
      end loop;
      
      -- 0 and 1 are not prime
      Primes(0) := False;
      Primes(1) := False;
      
      -- Sieve of Eratosthenes algorithm
      I := 2;
      while I * I <= Limit loop
         if Primes(I) then
            -- Mark all multiples of I as not prime
            J := I * I;
            while J <= Limit loop
               Primes(J) := False;
               J := J + I;
            end loop;
         end if;
         I := I + 1;
      end loop;
   end Sieve;
   
   Limit : constant Integer := 30;
   Primes : Boolean_Array(0 .. Limit);
   
begin
   Sieve(Primes, Limit);
   Print_Primes(Primes, Limit);
end Sieve_Of_Eratosthenes;
```

This Ada implementation of the Sieve of Eratosthenes algorithm:

1. **Defines a Boolean array** to track prime numbers (True = prime, False = not prime)
2. **Initializes all numbers** as potentially prime (True)
3. **Marks 0 and 1** as not prime
4. **Applies the sieve algorithm** by:
   - Starting with 2 (first prime)
   - Marking all multiples of each prime as not prime
   - Continuing until √n
5. **Prints the results** showing all prime numbers up to the specified limit

The program will output: `Prime numbers up to 30: 2 3 5 7 11 13 17 19 23 29`

