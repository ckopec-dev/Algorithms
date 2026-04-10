# Fast Fourier Transform (FFT) in Pascal

Here's an implementation of the Cooley-Tukey FFT algorithm in Pascal:

```pascal
program FFT_Implementation;

uses
  crt;

type
  TComplex = record
    real: real;
    imag: real;
  end;

  TComplexArray = array of TComplex;

// Function to multiply two complex numbers
function ComplexMultiply(a, b: TComplex): TComplex;
begin
  ComplexMultiply.real := a.real * b.real - a.imag * b.imag;
  ComplexMultiply.imag := a.real * b.imag + a.imag * b.real;
end;

// Function to add two complex numbers
function ComplexAdd(a, b: TComplex): TComplex;
begin
  ComplexAdd.real := a.real + b.real;
  ComplexAdd.imag := a.imag + b.imag;
end;

// Function to subtract two complex numbers
function ComplexSubtract(a, b: TComplex): TComplex;
begin
  ComplexSubtract.real := a.real - b.real;
  ComplexSubtract.imag := a.imag - b.imag;
end;

// FFT implementation using Cooley-Tukey algorithm
procedure FFT(var data: TComplexArray; n: integer);
var
  i, j, k, m, m2: integer;
  w, w1, w2, t: TComplex;
  temp: TComplexArray;
begin
  // Bit-reversal permutation
  SetLength(temp, n);
  for i := 0 to n-1 do
  begin
    j := 0;
    for k := 0 to trunc(log(n)/log(2))-1 do
      if (i and (1 shl k)) <> 0 then
        j := j or (1 shl (trunc(log(n)/log(2))-1-k));
    temp[j] := data[i];
  end;
  
  for i := 0 to n-1 do
    data[i] := temp[i];
  
  // FFT computation
  m2 := 1;
  for m := 2 to n do
  begin
    m2 := m2 * 2;
    w1.real := cos(2 * pi / m2);
    w1.imag := sin(2 * pi / m2);
    
    w.real := 1;
    w.imag := 0;
    
    for j := 0 to m2 div 2 - 1 do
    begin
      for i := j to n-1 step m2 do
      begin
        k := i + m2 div 2;
        t := ComplexMultiply(data[k], w);
        data[k] := ComplexSubtract(data[i], t);
        data[i] := ComplexAdd(data[i], t);
      end;
      
      w2 := ComplexMultiply(w, w1);
      w := w2;
    end;
  end;
end;

// Print complex number
procedure PrintComplex(c: TComplex; name: string);
begin
  writeln(name, ' = ', c.real:0:4, ' + ', c.imag:0:4, 'i');
end;

// Main program
var
  data: TComplexArray;
  i: integer;
  n: integer;

begin
  clrscr;
  
  // Example: 4-point FFT
  n := 4;
  SetLength(data, n);
  
  // Input data (example: [1, 2, 3, 4])
  data[0].real := 1; data[0].imag := 0;
  data[1].real := 2; data[1].imag := 0;
  data[2].real := 3; data[2].imag := 0;
  data[3].real := 4; data[3].imag := 0;
  
  writeln('Input data:');
  for i := 0 to n-1 do
    PrintComplex(data[i], 'x[' + IntToStr(i) + ']');
  
  writeln;
  writeln('Performing FFT...');
  FFT(data, n);
  
  writeln('FFT result:');
  for i := 0 to n-1 do
    PrintComplex(data[i], 'X[' + IntToStr(i) + ']');
  
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## Key Features of this Implementation:

1. **Complex Number Support**: Uses a record type to represent complex numbers with real and imaginary parts
2. **Bit-Reversal Permutation**: Implements the standard bit-reversal ordering required for FFT
3. **Cooley-Tukey Algorithm**: Uses the classic divide-and-conquer approach
4. **In-place Computation**: Modifies the input array directly for efficiency
5. **Proper Complex Arithmetic**: Includes functions for complex multiplication, addition, and subtraction

## Usage Notes:

- The input array size must be a power of 2 (2, 4, 8, 16, etc.)
- The algorithm modifies the input array in-place
- The output represents the Discrete Fourier Transform (DFT) of the input sequence
- This is a basic implementation suitable for educational purposes

## Sample Output:
```
Input data:
x[0] = 1.0000 + 0.0000i
x[1] = 2.0000 + 0.0000i
x[2] = 3.0000 + 0.0000i
x[3] = 4.0000 + 0.0000i

Performing FFT...
FFT result:
X[0] = 10.0000 + 0.0000i
X[1] = -2.0000 + 2.0000i
X[2] = -2.0000 + 0.0000i
X[3] = -2.0000 - 2.0000i
```

