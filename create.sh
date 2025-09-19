#!/bin/bash

# Check if exactly two arguments are provided
if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <LANGUAGE_NAME>"
  exit 1 # Exit with a non-zero status to indicate an error
fi

LANGUAGE_NAME=$1
echo "SCRIPT: Setting variable LANGUAGE_NAME: $LANGUAGE_NAME."
INPUT_FILE="$LANGUAGE_NAME/problems.txt"
echo "SCRIPT: Setting variable INPUT_FILE: $INPUT_FILE."
WORKING_DIR="/mnt/SATA08/intranet/Jobs/Algorithms"
echo "SCRIPT: Setting variable WORKING_DIR: $WORKING_DIR."

