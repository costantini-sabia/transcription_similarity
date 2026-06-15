# function that cleans a .cha file by removing metadata, speaker labels, empty lines, and commas, then saves the result as a .txt file.
clean_cha_file <- function(input_path, output_folder) {
  # Read the .cha file
  lines <- readLines(input_path, warn = FALSE)
  
  # 1. Remove header lines beginning with "@"
  lines <- lines[!grepl("^@", lines)]
  
  # 2. Remove "*PAR:" speaker tiers
  lines <- gsub("^\\*PAR:\\s*", "", lines)
  
  # 3. Remove @End if it remains
  lines <- lines[!grepl("^@End", lines)]
  
  # 4. Remove empty/whitespace-only lines
  lines <- lines[nzchar(trimws(lines))]
  
  # Collapse into one continuous text string
  cleaned_text <- paste(lines, collapse = " ")
  
  # 5. Remove commas
  cleaned_text <- gsub(",", "", cleaned_text)
  
  # Determine output filename
  file_name <- basename(input_path)
  file_name <- sub("\\.cha$", ".txt", file_name)
  output_path <- file.path(output_folder, file_name)
  
  # Save as .txt
  writeLines(cleaned_text, output_path)
  
  message("✔ Cleaned text saved to: ", output_path)
  return(cleaned_text)
}