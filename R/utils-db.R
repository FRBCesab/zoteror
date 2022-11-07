#' Check if the zotero.sqlite file exists
#' 
#' @noRd

check_db_path <- function(path) {
  
  if (missing(path)) {
    stop("Argument 'path' is required", call. = FALSE)
  }
  
  if (!is.character(path) || length(path) != 1) {
    stop("Argument 'path' must be a character of length 1 (path to Zotero ", 
         "directory)", call. = FALSE)
  }
  
  if (!dir.exists(path)) {
    stop("The folder '", path, "' does not exist", call. = FALSE)
  }
  
  filename <- file.path(path, "zotero.sqlite")
  
  if (!file.exists(filename)) {
    stop("The database '", filename, "' cannot be found in '", path, "'", 
         call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check if an object is class SQLiteConnection
#' 
#' @noRd

check_connection <- function(conn) {
  
  if (!inherits(conn, "SQLiteConnection")) {
    stop("Argument 'conn' must be of class 'SQLiteConnection'", 
         call. = FALSE)
  }
  
  invisible(NULL)
}



#' Connect to a Zotero local database
#'
#' @noRd

db_connect <- function(path) {
  
  check_db_path(path)
  
  RSQLite::dbConnect(drv = RSQLite::SQLite(), file.path(path, "zotero.sqlite"))
}



#' Generic function to select all rows from an SQL table
#' 
#' @noRd

select_from <- function(conn, relation) {
  
  check_connection(conn)
  
  if (missing(relation)) {
    stop("Argument 'relation' is required", 
         call. = FALSE)
  }
  
  if (!is.character(relation) || length(relation) != 1) {
    stop("Argument 'relation' must be a character of length (SQL table name)", 
         call. = FALSE)
  }
  
  
  DBI::dbGetQuery(conn, paste0("SELECT * FROM ", relation))
}
