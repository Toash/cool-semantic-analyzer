#!/bin/bash

# Directory to store parsed output
OUTPUT_DIR="parsed_outputs"

# Create the output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"

# Find all .cl files and process them
find . -type f -name "*.cl" | while read -r file; do
    # Extract filename without path
    filename=$(basename "$file")

    # Define output file path
    output_file="$OUTPUT_DIR/$filename"

    # Run cool --parse and explicitly set the output path
    cool --parse "$file" --out "$output_file"

    echo "Parsed $file -> $output_file"
done

echo "All .cl files have been parsed and stored in $OUTPUT_DIR"
