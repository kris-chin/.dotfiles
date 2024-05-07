#!/bin/bash

# Check if a filename was provided
if [ -z "$1" ]; then
  echo "Usage: $0 filename.txt"
  exit 1
fi

# Check if the file exists
if [ ! -f "$1" ]; then
  echo "Error: File '$1' not found."
  exit 1
fi

# Read the file line by line
while IFS= read -r package; do
  # Skip empty lines and lines that start with #
  if [[ -z "$package" || "$package" =~ ^# ]]; then
    continue
  fi
  
  echo "Installing package: $package"
  
  # Try to install the package
  sudo apt-get install -y "$package"
  
  # Check if the installation was successful
  if [ $? -ne 0 ]; then
    echo "Error installing package: $package"
  else
    echo "Successfully installed: $package"
  fi

done < "$1"

echo "Script completed."
