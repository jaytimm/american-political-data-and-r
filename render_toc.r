render_toc <- function(file_path) {
  
  # Read the Rmd file
  lines <- readLines(file_path)
  
  # Extract headers (lines starting with ##, ###, etc., but not in code blocks)
  # Must have space after # and not be inside ``` code blocks
  in_code_block <- FALSE
  header_lines <- integer(0)
  
  for (i in 1:length(lines)) {
    line <- lines[i]
    # Check for code block delimiters
    if (grepl("^```", line)) {
      in_code_block <- !in_code_block
      next
    }
    # Only look for headers outside code blocks
    if (!in_code_block && grepl("^#{1,6}\\s+.+", line)) {
      header_lines <- c(header_lines, i)
    }
  }
  
  if (length(header_lines) == 0) {
    cat("No headers found.\n")
    return(invisible(NULL))
  }
  
  # Extract header text and levels
  headers <- data.frame(
    line_num = header_lines,
    text = lines[header_lines]
  )
  
  # Parse header level and text
  headers$level <- nchar(sub("^(#{1,6}).*", "\\1", headers$text))
  headers$text <- sub("^#{1,6}\\s+", "", headers$text)
  # Remove trailing spaces
  headers$text <- trimws(headers$text)
  
  # Create anchor links (convert text to GitHub-flavored markdown anchor format)
  # GitHub format: lowercase, replace spaces with hyphens, remove special chars except hyphens
  create_anchor <- function(text) {
    # Convert to lowercase
    text <- tolower(text)
    # Replace spaces with hyphens
    text <- gsub("\\s+", "-", trimws(text))
    # Remove special characters except hyphens and alphanumerics
    text <- gsub("[^a-z0-9-]", "", text)
    # Collapse multiple consecutive hyphens
    text <- gsub("-+", "-", text)
    # Remove leading/trailing hyphens
    text <- gsub("^-|-$", "", text)
    return(text)
  }
  
  headers$anchor <- create_anchor(headers$text)
  
  # Generate TOC with proper indentation
  toc_lines <- character()
  
  for (i in 1:nrow(headers)) {
    level <- headers$level[i]
    text <- headers$text[i]
    anchor <- headers$anchor[i]
    
    # Indent based on level (level 2 = ## gets no indent, level 3 = ### gets 2 spaces, etc.)
    indent <- ifelse(level > 2, paste(rep("  ", level - 2), collapse = ""), "")
    
    # Create markdown link
    toc_line <- paste0(indent, "- [", text, "](#", anchor, ")")
    toc_lines <- c(toc_lines, toc_line)
  }
  
  # Output the TOC (this will be rendered inline in the Rmd)
  cat(paste(toc_lines, collapse = "\n"))
  cat("\n\n")
  
  return(invisible(toc_lines))
}
