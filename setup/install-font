#!/bin/bash

#NOTE: this script WILL Need to be ran in sudo since we write to the /tmp/ directory
#TODO: There are possible workarounds to this, like creating a folder within /tmp and doing work there.

# Get the original user's home directory
USER_HOME=$(getent passwd "$SUDO_USER" | cut -d: -f6)

# Check if the URL argument is provided
if [ -z "$1" ]; then
    echo "Usage: $0 <url_to_zipped_font>"
    exit 1
fi

FONT_URL=$1
ZIP_FILE="/tmp/font.zip"
TEMP_UNZIP_DIR="/tmp/font_unzip"
FONT_DIR="$USER_HOME/.local/share/fonts/"

# Download the zipped font file to a temporary location
curl -o "$ZIP_FILE" -L "$FONT_URL"

# Create the font directory if it doesn't exist
#Also create the temporary unzip directory if it doesnt exist
mkdir -p "$FONT_DIR"
mkdir -p "$TEMP_UNZIP_DIR"

# Unpack all files into the temporary unzip directory
unzip -o -u "$ZIP_FILE" -d "$TEMP_UNZIP_DIR" 

#Move the unpacked files to the font directory
mv -v "$TEMP_UNZIP_DIR"/* "$FONT_DIR"

# Refresh the font cache
fc-cache -f -v

# Remove the downloaded zip file
rm "$ZIP_FILE"
rm -r "$TEMP_UNZIP_DIR"

echo "Restart anything that has access to fonts to see changes."
