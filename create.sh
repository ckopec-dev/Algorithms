

echo "Reading first line from list."

# Specify the input file
INPUT_FILE="list.txt"
LANGUAGE="python"

# Get first line from input
LINE=$(head -n 1 $INPUT_FILE)
echo "Processing line: $LINE"

# Create the prompt
PROMPT="Show an example of $LINE algorithm in $LANGUAGE, in raw markdown."
echo "$PROMPT"

# Generate ollama result
#ollama run qwen3-coder $PROMPT > "$LINE.md"

echo "hello world" > $LINE.md




# remove the first line
#sed -i '1d' list.txt

echo "Processing complete."
