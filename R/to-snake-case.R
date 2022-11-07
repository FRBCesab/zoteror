#' Convert lowerCamelCase to lower_snake_case
#' 
#' @noRd

to_snake_case <- function(x) {
  
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  
  if (!is.character(x)) {
    stop("Argument 'x' must be a character", call. = FALSE)
  }
  
  x <- gsub("ID$", "Id", x)
  x <- gsub("^DOI$", "Doi", x)
  x <- gsub("^URL$", "Url", x)
  x <- gsub("^ISSN$", "Issn", x)
  x <- gsub("^ISBN$", "Isbn", x)
  
  x <- strsplit(x, "")
  
  x <- unlist(lapply(x, function(x) {
    pos <- which(x %in% LETTERS)
    if (length(pos) > 0) {
      x[pos] <- paste0("_", tolower(x[pos]))
    }
    paste0(x, collapse = "")
  }))
  
  gsub("^_", "", x)
}
