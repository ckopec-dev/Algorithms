#!/bin/bash

echo "Processing list."

# Change to working directory
cd /home/ckopec/code/Algorithms

# Specify the input file
INPUT_FILE="algos_$1.txt"
LANGUAGE=$1
FOLDER=$1

# Get first line from input
LINE=$(head -n 1 $INPUT_FILE)
CLEAN_LINE=${LINE// /_}
echo "SCRIPT: Processing line: $LINE"

# Create the prompt
PROMPT="Show an example of $LINE algorithm in the $LANGUAGE programming language, in raw markdown."
echo "SCRIPT: Prompt: $PROMPT"

# Generate ollama result
OUTPUT_PATH="./$FOLDER/$CLEAN_LINE.md"
echo "SCRIPT: Using ollama to generate output."
ollama run qwen3-coder $PROMPT > $OUTPUT_PATH

# Authenticate to github
echo "SCRIPT: Authenticating to Github."
gh auth login --hostname github.com --with-token < ../github_token.txt

# Get fresh pull
echo "SCRIPT: Pulling latest version."
git pull

# Create a new branch
echo "SCRIPT: Creating a new branch."
git checkout -b "$FOLDER-$CLEAN_LINE"

# Add updated items
echo "SCRIPT: Adding files to staging."
git add .

# Make commit
echo "SCRIPT: Making commit."
git commit -m "Added $OUTPUT_PATH"

# Push the commit
echo "SCRIPT: Pushing commit."
git push --set-upstream origin $FOLDER-$CLEAN_LINE

# Creating the pull request
echo "SCRIPT: Creating pull request."
gh pr create --title "$FOLDER_$CLEAN_LINE ready for review" --body "Generating automatically from create.sh."

# Switch back to main branch
echo "SCRIPT: Switching back to main."
git switch main

# Remove the first line
echo "SCRIPT: Pruning list."
sed -i '1d' $INPUT_FILE

echo "SCRIPT: Processing complete."
