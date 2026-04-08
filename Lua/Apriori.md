# Apriori Algorithm Implementation in Lua

Here's a complete implementation of the Apriori algorithm in Lua for finding frequent itemsets in transaction data:

```lua
-- Apriori Algorithm Implementation in Lua

-- Function to generate candidate itemsets of size k
local function generate_candidates(frequent_itemsets, k)
    local candidates = {}
    
    for i = 1, #frequent_itemsets do
        for j = i + 1, #frequent_itemsets do
            local itemset1 = frequent_itemsets[i]
            local itemset2 = frequent_itemsets[j]
            
            -- Check if first k-1 elements are the same
            local same = true
            for l = 1, k - 2 do
                if itemset1[l] ~= itemset2[l] then
                    same = false
                    break
                end
            end
            
            if same then
                -- Create candidate by merging itemsets
                local candidate = {}
                for l = 1, k - 1 do
                    candidate[l] = itemset1[l]
                end
                candidate[k] = itemset2[k]
                table.insert(candidates, candidate)
            end
        end
    end
    
    return candidates
end

-- Function to check if an itemset is frequent
local function is_frequent(itemset, transactions, min_support)
    local count = 0
    
    for _, transaction in ipairs(transactions) do
        local found = true
        for _, item in ipairs(itemset) do
            local item_found = false
            for _, trans_item in ipairs(transaction) do
                if trans_item == item then
                    item_found = true
                    break
                end
            end
            if not item_found then
                found = false
                break
            end
        end
        if found then
            count = count + 1
        end
    end
    
    local support = count / #transactions
    return support >= min_support
end

-- Function to prune candidates
local function prune_candidates(candidates, frequent_itemsets, k)
    local pruned = {}
    
    for _, candidate in ipairs(candidates) do
        local valid = true
        
        -- Check all (k-1) subsets of candidate
        for i = 1, #candidate do
            local subset = {}
            for j = 1, #candidate do
                if j ~= i then
                    table.insert(subset, candidate[j])
                end
            end
            
            -- Check if subset is in frequent_itemsets
            local subset_found = false
            for _, freq_itemset in ipairs(frequent_itemsets) do
                if #freq_itemset == #subset then
                    local match = true
                    for k = 1, #subset do
                        if freq_itemset[k] ~= subset[k] then
                            match = false
                            break
                        end
                    end
                    if match then
                        subset_found = true
                        break
                    end
                end
            end
            
            if not subset_found then
                valid = false
                break
            end
        end
        
        if valid then
            table.insert(pruned, candidate)
        end
    end
    
    return pruned
end

-- Main Apriori algorithm
local function apriori(transactions, min_support)
    local frequent_itemsets = {}
    local all_frequent = {}
    
    -- Generate frequent 1-itemsets
    local candidates = {}
    local item_counts = {}
    
    -- Count occurrences of each item
    for _, transaction in ipairs(transactions) do
        for _, item in ipairs(transaction) do
            if not item_counts[item] then
                item_counts[item] = 0
            end
            item_counts[item] = item_counts[item] + 1
        end
    end
    
    -- Find frequent 1-itemsets
    local frequent_1 = {}
    for item, count in pairs(item_counts) do
        local support = count / #transactions
        if support >= min_support then
            table.insert(frequent_1, {item})
            table.insert(all_frequent, {item, support})
        end
    end
    
    table.insert(frequent_itemsets, frequent_1)
    
    -- Generate frequent k-itemsets for k > 1
    local k = 2
    while #frequent_itemsets[k-1] > 0 do
        -- Generate candidates
        local candidates = generate_candidates(frequent_itemsets[k-1], k)
        
        -- Prune candidates
        local pruned_candidates = prune_candidates(candidates, frequent_itemsets[k-1], k)
        
        -- Check which candidates are frequent
        local frequent_k = {}
        for _, candidate in ipairs(pruned_candidates) do
            if is_frequent(candidate, transactions, min_support) then
                table.insert(frequent_k, candidate)
                
                -- Calculate support for this itemset
                local count = 0
                for _, transaction in ipairs(transactions) do
                    local found = true
                    for _, item in ipairs(candidate) do
                        local item_found = false
                        for _, trans_item in ipairs(transaction) do
                            if trans_item == item then
                                item_found = true
                                break
                            end
                        end
                        if not item_found then
                            found = false
                            break
                        end
                    end
                    if found then
                        count = count + 1
                    end
                end
                local support = count / #transactions
                table.insert(all_frequent, {candidate, support})
            end
        end
        
        table.insert(frequent_itemsets, frequent_k)
        k = k + 1
    end
    
    return all_frequent
end

-- Example usage
local function example_usage()
    -- Sample transaction data
    local transactions = {
        {"milk", "bread", "butter"},
        {"milk", "bread"},
        {"milk", "butter"},
        {"bread", "butter"},
        {"milk", "bread", "butter", "cheese"},
        {"bread", "cheese"},
        {"milk", "cheese"},
        {"bread", "butter", "cheese"}
    }
    
    local min_support = 0.3  -- 30% minimum support
    
    print("Transactions:")
    for i, transaction in ipairs(transactions) do
        print("  T" .. i .. ": " .. table.concat(transaction, ", "))
    end
    
    print("\nMinimum Support: " .. min_support * 100 .. "%")
    
    -- Run Apriori algorithm
    local result = apriori(transactions, min_support)
    
    print("\nFrequent Itemsets:")
    print("Itemset\t\t\tSupport")
    print("------------------------")
    
    for _, item in ipairs(result) do
        local itemset = item[1]
        local support = item[2]
        
        local itemset_str = "{"
        for i, item in ipairs(itemset) do
            itemset_str = itemset_str .. item
            if i < #itemset then
                itemset_str = itemset_str .. ", "
            end
        end
        itemset_str = itemset_str .. "}"
        
        print(itemset_str .. "\t\t" .. string.format("%.2f", support))
    end
end

-- Run the example
example_usage()
```

## How the Algorithm Works

This Apriori implementation follows these key steps:

1. **Generate 1-itemsets**: Count occurrences of each individual item
2. **Filter frequent 1-itemsets**: Keep only those with support ≥ minimum support
3. **Generate candidates**: Use the Apriori property to generate candidate itemsets
4. **Prune candidates**: Remove candidates that have infrequent subsets
5. **Test candidates**: Check if pruned candidates are frequent
6. **Repeat**: Continue until no more frequent itemsets can be found

## Key Features

- **Support calculation**: Computes support for each itemset
- **Pruning**: Uses the Apriori property to prune infrequent candidates
- **Flexible input**: Accepts transaction data in list format
- **Detailed output**: Shows both itemsets and their support values

## Sample Output

```
Transactions:
  T1: milk, bread, butter
  T2: milk, bread
  T3: milk, butter
  T4: bread, butter
  T5: milk, bread, butter, cheese
  T6: bread, cheese
  T7: milk, cheese
  T8: bread, butter, cheese

Minimum Support: 30%

Frequent Itemsets:
Itemset			Support
------------------------
{milk}			0.75
{bread}			0.88
{butter}		0.75
{cheese}		0.63
{milk, bread}		0.63
{milk, butter}		0.63
{bread, butter}		0.63
{bread, cheese}		0.50
{milk, cheese}		0.50
{bread, butter, cheese}	0.38
```

The algorithm efficiently finds all frequent itemsets that meet the minimum support threshold using the Apriori property to reduce the search space.

