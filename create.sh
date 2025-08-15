#!/bin/bash

echo "Reading first line from list."

# Specify the input file
INPUT_FILE="list.txt"
LANGUAGE="python"
FOLDER="Python"

# Get first line from input
LINE=$(head -n 1 $INPUT_FILE)
echo "Processing line: $LINE"

# Create the prompt
PROMPT="Show an example of $LINE algorithm in $LANGUAGE, in raw markdown."
echo "Prompt: $PROMPT"

# Generate ollama result
OUTPUT_PATH="./$FOLDER/$LINE.md"
echo "Output path: $OUTPUT_PATH"
echo "Using ollama to generate output."
ollama run qwen3-coder $PROMPT > $OUTPUT_PATH





# remove the first line
#sed -i '1d' list.txt

echo "Processing complete."
