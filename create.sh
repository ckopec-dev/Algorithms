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
echo "Authenticating to Github."
gh auth login --hostname github.com --with-token < ../github_token.txt

# Get fresh pull
echo "Pulling latest version."
git pull

# Create a new branch
echo "Creating a new branch."
git checkout -b "$FOLDER-$LINE"

# Add updated items
echo "Adding files to staging."
git add .

# Make commit
echo "Making commit."
git commit -m "Added $OUTPUT_PATH"

# Push the commit
echo "Pushing commit."
git push --set-upstream origin $FOLDER-$LINE

# Creating the pull request
echo "Creating pull request."
gh pr create --title "$FOLDER_$LINE ready for review" --body "Generating automatically from create.sh."

# Switch back to main branch
echo "Switching back to main."
git switch main

# Remove the first line
echo "Pruning list."
sed -i '1d' $INPUT_FILE

echo "Processing complete."
