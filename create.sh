#!/bin/bash

# Check if exactly two arguments are provided
if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <LANGUAGE_NAME>"
  exit 1 # Exit with a non-zero status to indicate an error
fi

echo "SCRIPT: Entering create."

LANGUAGE_NAME=$1
echo "SCRIPT: Setting variable LANGUAGE_NAME: $LANGUAGE_NAME."
INPUT_FILE="$LANGUAGE_NAME/problems.txt"
echo "SCRIPT: Setting variable INPUT_FILE: $INPUT_FILE."
WORKING_DIR="/mnt/SATA08/intranet/Jobs/Algorithms"
echo "SCRIPT: Setting variable WORKING_DIR: $WORKING_DIR."

echo "SCRIPT: Changing working directory to $WORKING_DIR."
cd $WORKING_DIR

ALGO_NAME=$(head -n 1 $INPUT_FILE)
echo "SCRIPT: Setting variable ALGO_NAME: $ALGO_NAME."
OUTPUT_FILE="$LANGUAGE_NAME/${ALGO_NAME// /_}.md" # Replaces spaces with underscores
echo "SCRIPT: Setting variable OUTPUT_FILE: $OUTPUT_FILE."

PROMPT="Show an example of $ALGO_NAME algorithm in $LANGUAGE_NAME programming language. Use markdown format."
echo "SCRIPT: Setting variable PROMPT: $PROMPT"

echo "SCRIPT: Using ollama to generate output."
ollama run qwen3-coder $PROMPT > $OUTPUT_FILE
echo "SCRIPT: Created $OUTPUT_FILE."

echo "SCRIPT: Authenticating to Github."
gh auth login --hostname github.com --with-token < ../github_token.txt

echo "SCRIPT: Pulling latest version."
git pull

$BRANCH_NAME=${LANGUAGE_NAME// /_}-${ALGO_NAME// /_}
echo "SCRIPT: Setting variable BRANCH_NAME: $BRANCH_NAME."
echo "SCRIPT: Creating a new branch named $BRANCH_NAME."
git checkout -b "$BRANCH_NAME"

echo "SCRIPT: Adding files to staging."
git add .

echo "SCRIPT: Making commit."
git commit -m "Added $OUTPUT_FILE"

# Push the commit
echo "SCRIPT: Pushing commit."
git push --set-upstream origin $BRANCH_NAME

# Creating the pull request
echo "SCRIPT: Creating pull request."
gh pr create --title "$BRANCH_NAME ready for review" --body "Generated automatically from create.sh."

# Switch back to main branch
echo "SCRIPT: Switching back to main."
git switch main

# Remove the first line
echo "SCRIPT: Pruning list."
sed -i '1d' $INPUT_FILE

echo "SCRIPT: Create complete."