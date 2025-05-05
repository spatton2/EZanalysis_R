#' @title EZimport
#' @description Import large datasets and assign datatypes
#' @param data csv or excel file with data
#' @param name name of your EZimported file (character)
#' @return EZimported file
#' @export

EZimport <- function(data, name){
  if(grepl('xl', excel_format(data), fixed = TRUE) == TRUE){
    df <- read_excel(data)
  }
  else {
    df <- read_csv2(data)
  }

  original_colnames <- names(df)

  datatypes <- setNames(character(length(original_colnames)), original_colnames)

  for (colname in original_colnames) {
    if (!colname %in% names(df)) next

    cat("\nColumn:", colname, "\n")
    print(head(df[[colname]]))
    choice <- menu(c("numeric", "character", "factor", "NA (remove column)"),
                   graphics = FALSE,  # safer for most environments
                   title = paste("Which datatype is column", colname, "?"))

    if (choice == 0) {
      warning(paste("No selection made for column:", colname, "- Skipping..."))
      datatypes[colname] <- NA
    } else {
      datatypes[colname] <- c("numeric", "character", "factor", "remove")[choice]
    }
  }

  for (colname in names(datatypes)) {
    if (!colname %in% names(df)) next
    dtype <- datatypes[colname]

    if (is.na(dtype)) next
    if (dtype == "numeric") {
      df[[colname]] <- as.numeric(df[[colname]])
    } else if (dtype == "character") {
      df[[colname]] <- as.character(df[[colname]])
    } else if (dtype == "factor") {
      df[[colname]] <- as.factor(df[[colname]])
    } else if (dtype == "remove") {
      df[[colname]] <- NULL
    }
  }


  assign(name, df, envir = .GlobalEnv)
  message(paste("Successfully imported", name))
  return(invisible(df))
}
