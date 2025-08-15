#!/bin/bash

echo "Processing list."

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

# Authenticate to github
gh auth login --hostname github.com --with-token < ../github_token.txt

# Create a new branch
git checkout -b "$FOLDER-$LINE"

# Add updated items
git add .

# Make commit
git commit -m "Added $OUTPUT_PATH"

# Push the commit
git push --set-upstream origin $FOLDER-$LINE

# Creating the pull request
gh pr create --title "$FOLDER_$LINE ready for review" --body "Generating automatically from create.sh."

# remove the first line
#sed -i '1d' list.txt

echo "Processing complete."
