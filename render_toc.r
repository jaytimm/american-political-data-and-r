render_toc <- function(file_path) {
  
  # Read the Rmd file
  lines <- readLines(file_path)
  
  # Extract headers (lines starting with ##, ###, etc.)
  header_lines <- grep("^#{1,6}\\s+", lines, value = FALSE)
  
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
  
  # Remove code block headers and special cases
  headers <- headers[!grepl("^```", headers$text), ]
  
  # Create anchor links (convert text to GitHub-flavored markdown anchor format)
  # GitHub format: lowercase, replace spaces and special chars with hyphens
  create_anchor <- function(text) {
    text <- tolower(text)
    text <- gsub("[^a-z0-9\\s-]", "", text)
    text <- gsub("\\s+", "-", trimws(text))
    text <- gsub("-+", "-", text)
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
