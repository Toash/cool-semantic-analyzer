#!/bin/bash

# Directories and output files
INPUT_DIR_AST="wrong_cool/parsed_outputs"
INPUT_DIR_CL="wrong_cool"
OUTPUT_FILE_AST="./output.log"
OUTPUT_FILE_CLASS_MAP="./cool_output.log"

# Ensure the directories exist
if [ ! -d "$INPUT_DIR_AST" ]; then
    echo "Error: Directory $INPUT_DIR_AST does not exist."
    exit 1
fi

if [ ! -d "$INPUT_DIR_CL" ]; then
    echo "Error: Directory $INPUT_DIR_CL does not exist."
    exit 1
fi

# Clear the output files
> "$OUTPUT_FILE_AST"
> "$OUTPUT_FILE_CLASS_MAP"

echo "Processing .cl-ast files with ./a.out..."
# Process .cl-ast files with ./a.out
find "$INPUT_DIR_AST" -type f -name "*.cl-ast" | while read -r file; do
    echo "Processing $file..." | tee -a "$OUTPUT_FILE_AST"
    ./a.out "$file" >> "$OUTPUT_FILE_AST" 2>&1
    echo "---------------------------------" >> "$OUTPUT_FILE_AST"
done

echo "Processing .cl files with cool --type..."
# Process .cl files with cool --class-map
find "$INPUT_DIR_CL" -type f -name "*.cl" | while read -r file; do
    echo "Processing $file..." | tee -a "$OUTPUT_FILE_CLASS_MAP"
    cool --type "$file" >> "$OUTPUT_FILE_CLASS_MAP" 2>&1
    echo "---------------------------------" >> "$OUTPUT_FILE_CLASS_MAP"
done

echo "Processing complete."
echo "Output of ./a.out saved in $OUTPUT_FILE_AST"
echo "Output of cool --class-map saved in $OUTPUT_FILE_CLASS_MAP"