#' Retrieve data from Zotero database
#'
#' @description 
#' Retrieves references metadata from Zotero local database (SQLite).
#'   
#' @param path a `character` of length 1. The folder containing the Zotero 
#'   database named `zotero.sqlite`.
#'
#' @return A `data.frame` with references in row and the following variables:
#'   - `item_id`: the identifier of the publication in the Zotero database;
#'   - `library_type`: the type of the library. Can be `user` (My library)
#'     or `group` (Group libraries);
#'   - `group`: the name of the group library;
#'   - `collection`: the name of the library collection;
#'   - `category`: the category of the publication (`article`, `book`, 
#'     `book_section`, `report`, etc.);
#'   - `year`: the year of the publication;
#'   - `title`: the title of the publication;
#'   - `author`: the authors of the publication;
#'   - `journal`: the journal of the publication (only for `article`);
#'   - `book_title`: the title of the book (only for `book_section`);
#'   - `editor`: the editors of the book (only for `book` and `book_section`);
#'   - `abstract`: the abstract of the publication;
#'   - `volume`: the volume of the publication (only for `article`);
#'   - `issue`: the issue of the publication (only for `article`);
#'   - `pages`: the pages of the publication;
#'   - `publisher`: the publisher name;
#'   - `place`: the location of the publisher;
#'   - `institution`: the institution name; 
#'   - `doi`: the DOI (Digital Object Identifier) of the publication;
#'   - `url`: the URL of the publication;
#'   - `note`: the annotations associated to the publication (only the `PUBID` 
#'     is retrieved).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' references <- get_zotero_data()
#' }

get_zotero_data <- function(path = "~/zotero") {
  
  ## Connect to database ----
  
  conn <- db_connect(path)
  
  
  ## Check for empty database ----
  
  if (nrow(get_items_data_values(conn)) == 0) {
    stop("The Zotero database is empty", call. = FALSE)
  }
  
  
  ## Get libraries, groups & collections names ----
  
  libraries_id   <- get_libraries_id(conn)
  groups_id      <- get_groups_id(conn)
  collections_id <- get_collections_id(conn)
  
  zotero <- merge(libraries_id, groups_id, by = "library_id", all = TRUE)
  zotero <- merge(zotero, collections_id, by = "library_id", all = TRUE)
  
  
  ## Add user library ----
  
  zotero$"group_name" <- ifelse(is.na(zotero$"group_name"), 
                                "Main", 
                                zotero$"group_name")
  
  
  ## Get items data ----
  
  items_types       <- get_items_types(conn)
  items_types_id    <- get_items_types_id(conn)
  
  items_types <- merge(items_types, items_types_id, by = "item_type_id", 
                       all = FALSE)
  
  fields_id         <- get_fields_id(conn)
  items_data_id     <- get_items_data_id(conn)
  items_data_values <- get_items_data_values(conn)
  
  items <- merge(items_data_id, fields_id, by = "field_id", all = FALSE)
  items <- merge(items, items_data_values, by = "value_id", all = FALSE)
  items <- items[ , c("item_id", "field_name", "value")]
  
  items <- tidyr::pivot_wider(items, names_from = "field_name",
                              values_from = "value")
  
  items <- as.data.frame(items)
  
  collections_items_id <- get_collections_items_id(conn)
  
  items <- merge(items, collections_items_id, by = "item_id", all = FALSE)
  items <- merge(items, zotero, by = "collection_id", all = FALSE)
  
  items <- merge(items, items_types, by = "item_id", all = FALSE)
  
  
  ## Get notes ----
  
  items_notes <- get_items_notes(conn)
  items_notes <- items_notes[grep("^pubid", items_notes$"note"), ]
  
  items <- merge(items, items_notes, by = "item_id", all.x = TRUE, 
                 all.y = FALSE)
  
  
  ## Get creators ----
  
  items_creators <- creators_by_item(conn)
  
  items <- merge(items, items_creators, by = "item_id", all = FALSE)
  
  
  ## Disconnect from database ----
  
  DBI::dbDisconnect(conn)
  
  
  ## Rename and select columns ----
  
  colnames(items) <- gsub("DOI", "doi", colnames(items))
  colnames(items) <- gsub("ISSN", "issn", colnames(items))
  colnames(items) <- gsub("ISBN", "isbn", colnames(items))
  colnames(items) <- gsub("publicationTitle", "journal", colnames(items))
  colnames(items) <- gsub("abstractNote", "abstract", colnames(items))
  colnames(items) <- gsub("type_name", "category", colnames(items))
  colnames(items) <- gsub("group_name", "group", colnames(items))
  colnames(items) <- gsub("collection_name", "collection", colnames(items))
  
  colnames(items) <- to_snake_case(colnames(items))
  
  items$"year" <- unlist(lapply(strsplit(items$date, " "), function(x) x[2]))
  
  col_order <- c("item_id", "library_type", "group", "collection", "category", 
                 "year", "title", "author", "journal", "book_title", "editor", 
                 "abstract", "volume", "issue", "pages", "publisher", "place", 
                 "institution", "doi", "url", "note")
  
  items <- items[ , col_order]
  
  
  ## Order references ----
  
  items <- items[with(items, order(library_type, group, collection, 
                                   year, author)), ]
  
  rownames(items) <- NULL
  
  items
}
