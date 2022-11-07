#' Retrieve and format Libraries ID & Names
#' 
#' @noRd

get_libraries_id <- function(conn) {
  
  results <- select_from(conn, "libraries")
  
  results <- results[ , c("libraryID", "type")]
  colnames(results) <- to_snake_case(colnames(results))
  colnames(results)[2] <- "library_type"
  
  results
}



#' Retrieve and format Groups ID & Names
#' 
#' @noRd

get_groups_id <- function(conn) {
  
  results <- select_from(conn, "groups")
  
  results <- results[ , c("libraryID", "groupID", "name")]
  colnames(results) <- to_snake_case(colnames(results))
  colnames(results)[3] <- "group_name"
  
  results
}



#' Retrieve and format Collections ID & Names
#' 
#' @noRd

get_collections_id <- function(conn) {
  
  results <- select_from(conn, "collections")
  
  results <- results[ , c("libraryID", "collectionID", "collectionName")]
  colnames(results) <- to_snake_case(colnames(results))
  
  results
}



#' Retrieve and format links bw Collections & Items ID
#' 
#' @noRd

get_collections_items_id <- function(conn) {
  
  results <- select_from(conn, "collectionItems")
  
  results <- results[ , c("collectionID", "itemID")]
  colnames(results) <- to_snake_case(colnames(results))
  
  results
}



#' Retrieve and format Items Fields ID & Names
#' 
#' @noRd

get_fields_id <- function(conn) {
  
  results <- select_from(conn, "fields")
  
  results <- results[ , c("fieldID", "fieldName")]
  colnames(results) <- to_snake_case(colnames(results))
  
  results
}



#' Retrieve and format Items Types ID & Names
#' 
#' @noRd

get_items_types_id <- function(conn) {
  
  results <- select_from(conn, "itemTypes")
  
  results <- results[ , c("itemTypeID", "typeName")]
  colnames(results) <- to_snake_case(colnames(results))
  
  results
}



#' Retrieve and format links bw Items Types & Items ID
#' 
#' @noRd

get_items_types <- function(conn) {
  
  results <- select_from(conn, "items")
  
  results <- results[ , c("itemID", "itemTypeID")]
  colnames(results) <- to_snake_case(colnames(results))
  
  results
}



#' Retrieve and format Items Data ID
#' 
#' @noRd

get_items_data_id <- function(conn) {
  
  results <- select_from(conn, "itemData")
  
  results <- results[ , c("itemID", "fieldID", "valueID")]
  colnames(results) <- to_snake_case(colnames(results))
  
  results
}



#' Retrieve and format Items Data Values
#' 
#' @noRd

get_items_data_values <- function(conn) {
  
  results <- select_from(conn, "itemDataValues")
  
  results <- results[ , c("valueID", "value")]
  colnames(results) <- to_snake_case(colnames(results))
  
  results
}



#' Retrieve and format Items Notes
#' 
#' @noRd

get_items_notes <- function(conn) {
  
  results <- select_from(conn, "itemNotes")
  
  results <- results[ , c("parentItemID", "title")]
  colnames(results) <- to_snake_case(colnames(results))
  colnames(results) <- c("item_id", "note")
  
  results
}



#' Retrieve and format links bw Items ID and Creators ID
#' 
#' @noRd

get_items_creators <- function(conn) {
  
  results <- select_from(conn, "itemCreators")
  
  results <- results[ , c("itemID", "creatorID", "creatorTypeID", "orderIndex")]
  colnames(results) <- to_snake_case(colnames(results))
  
  results
}



#' Retrieve and format Creators Names
#' 
#' @noRd

get_creators_id <- function(conn) {
  
  results <- select_from(conn, "creators")
  
  results <- results[ , c("creatorID", "firstName", "lastName")]
  colnames(results) <- to_snake_case(colnames(results))
  
  results$"full_name" <- paste(results$"last_name", results$"first_name", 
                               sep = ", ")
  results
}



#' Retrieve and format Creators Types
#' 
#' @noRd

get_creators_types <- function(conn) {
  
  results <- select_from(conn, "creatorTypes")
  
  results <- results[ , c("creatorTypeID", "creatorType")]
  colnames(results) <- to_snake_case(colnames(results))
  
  results
}



#' Retrieve and format links bw Items ID and Creators Names & Types
#' 
#' @noRd

creators_by_item <- function(conn) {
  
  creators_id       <- get_creators_id(conn)
  items_id_creators <- get_items_creators(conn)
  creators_types    <- get_creators_types(conn)
  
  items_creators <- merge(items_id_creators, creators_id, by = "creator_id", 
                          all = TRUE)
  
  items_creators_types <- merge(items_creators, creators_types, 
                                by = "creator_type_id", all = FALSE)
  
  items_creators_types <- items_creators_types[ , c("item_id", "full_name", 
                                                    "creator_type", 
                                                    "order_index")]
  
  items_id <- sort(unique(items_creators_types$"item_id"))
  
  creators_types <- unique(items_creators_types$"creator_type")
  
  creators <- lapply(items_id, function(i) {
    
    dat <- items_creators_types[items_creators_types$"item_id" == i, ]
    
    creators <- lapply(creators_types, function(type) {
      
      tab <- dat[dat$"creator_type" == type, ]
      tab <- tab[order(as.numeric(tab$"order_index"), decreasing = FALSE), ]
      paste(tab$"full_name", collapse = " ; ")
    })
    
    creators <- unlist(creators)
    creators <- ifelse(creators == "", NA, creators)
    creators <- as.data.frame(matrix(creators, nrow = 1))
    colnames(creators) <- creators_types
    data.frame("item_id" = i, creators)
  })
  
  do.call(rbind.data.frame, creators)
}
