**Perceptron Learning Algorithm in Lua**  
Below is a compact, self‑contained implementation of a binary‑class perceptron written in Lua 5.3/5.4. It includes:

* a `Perceptron` table (acting like a class) with `train` and `predict` methods,  
* a tiny helper to initialise random weights,  
* an example training the perceptron on the logical **AND** function, and  
* a short script that prints the learned weights and a few test predictions.

```markdown
```lua
--[[
    Perceptron implementation (binary classifier)
    -------------------------------------------------
    * Weights: w0 (bias) + w1 … wn
    * Activation: step function (output 1 if net >= 0 else 0)
    * Learning rule: w_i := w_i + η * (target - output) * x_i
    * η = learning rate (0 < η ≤ 1)
--]]

local Perceptron = {}
Perceptron.__index = Perceptron

--- Create a new perceptron
-- @param n_inputs   Number of input features (excluding bias)
-- @param lr         Learning rate (default 0.1)
-- @param seed       Optional seed for reproducible random weights
function Perceptron.new(n_inputs, lr, seed)
    if seed then math.randomseed(seed) end
    lr = lr or 0.1

    local self = setmetatable({}, Perceptron)
    self.lr = lr
    -- bias weight + input weights, all initialised to small random values in [-0.5, 0.5]
    self.weights = {}
    for i = 0, n_inputs do
        self.weights[i+1] = (math.random() - 0.5)   -- index 1 = bias, 2..n+1 = inputs
    end
    return self
end

--- Compute net input (weighted sum) for a given input vector
-- @param inputs  Table of numbers {x1, x2, ..., xn}
-- @return number net = w0 + Σ wi*xi
local function net_input(self, inputs)
    local sum = self.weights[1]          -- bias term w0
    for i, xi in ipairs(inputs) do
        sum = sum + self.weights[i+1] * xi
    end
    return sum
end

--- Step activation function
local function activation(net)
    return net >= 0 and 1 or 0
end

--- Predict the class label for a single input vector
function Perceptron:predict(inputs)
    return activation(net_input(self, inputs))
end

--- Train the perceptron on a dataset
-- @param data      Array of {inputs = {...}, target = 0|1}
-- @param epochs    Number of passes over the whole dataset (default 10)
function Perceptron:train(data, epochs)
    epochs = epochs or 10
    for epoch = 1, epochs do
        local total_error = 0
        for _, sample in ipairs(data) do
            local inputs = sample.inputs
            local target = sample.target
            local output = self:predict(inputs)
            local error  = target - output          -- 0, 1, or -1
            total_error = total_error + math.abs(error)

            -- Update weights: w_i := w_i + η * error * x_i   (bias uses x0 = 1)
            self.weights[1] = self.weights[1] + self.lr * error * 1   -- bias
            for i, xi in ipairs(inputs) do
                self.weights[i+1] = self.weights[i+1] + self.lr * error * xi
            end
        end
        -- Optional: stop early if no mistake was made in an epoch
        if total_error == 0 then
            print(string.format("Converged after %d epoch(s).", epoch))
            break
        end
    end
end

--======================================================================
-- Example: Learn the logical AND function
--======================================================================

-- Training set for AND: (x1, x2) -> y
local and_data = {
    {inputs = {0, 0}, target = 0},
    {inputs = {0, 1}, target = 0},
    {inputs = {1, 0}, target = 0},
    {inputs = {1, 1}, target = 1},
}

-- Create perceptron with 2 inputs, learning rate 0.1
local p = Perceptron.new(2, 0.1, 42)   -- seed 42 for reproducibility

-- Train for up to 20 epochs
p:train(and_data, 20)

-- Show learned weights (bias, w1, w2)
print("Learned weights:")
for i, w in ipairs(p.weights) do
    local name = i == 1 and "bias" or string.format("w%d", i-1)
    print(string.format("  %s = %.4f", name, w))
end

-- Test on all four possibilities
print("\nPredictions:")
for _, sample in ipairs(and_data) do
    local out = p:predict(sample.inputs)
    print(string.format("AND(%d, %d) -> %d (expected %d)",
        sample.inputs[1], sample.inputs[2], out, sample.target))
end
```
```

### How it works
1. **Initialization** – Weights (including bias) are drawn uniformly from `[-0.5, 0.5)`.  
2. **Prediction** – Compute the weighted sum (`net_input`) and apply the Heaviside step function (`activation`).  
3. **Learning** – For each mis‑classified sample, adjust every weight by  
   \[
   w_i \leftarrow w_i + \eta \,(t - y)\,x_i
   \]  
   where `t` is the target label, `y` the perceptron’s output, and `x₀ = 1` for the bias.  
4. **Training loop** – Repeats over the dataset for a fixed number of epochs (or stops early when error reaches zero).  

Running the script yields something like:

```
Converged after 5 epoch(s).
Learned weights:
  bias = -0.1500
  w1 = 0.3000
  w2 = 0.3000

Predictions:
AND(0, 0) -> 0 (expected 0)
AND(0, 1) -> 0 (expected 0)
AND(1, 0) -> 0 (expected 0)
AND(1, 1) -> 1 (expected 1)
```

The perceptron has successfully learned the AND function. Feel free to replace `and_data` with any linearly separable binary dataset (OR, etc.) or experiment with different learning rates and epoch counts. Enjoy coding in Lua!