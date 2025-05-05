#' @title EZrename
#' @description Menu interface for renaming columns in a large dataframe or tibble
#' @param df your input dataframe
#' @return df your dataframe with new column names
#' @export

EZrename <- function(df){
  original_colnames <- names(df)
  new_colnames <- original_colnames  # Start with the same names

  for (i in seq_along(original_colnames)) {
    colname <- original_colnames[i]
    cat("\nCurrent column:", colname, "\n")

    rename_choice <- readline(prompt = paste("Do you want to rename column '",
                                             colname, "'? (y/n): ", sep = ""))

    if (tolower(rename_choice) == "y") {
      new_name <- readline(prompt = "Enter new column name: ")
      new_colnames[i] <- new_name
    }
  }

  names(df) <- new_colnames
  message("Column renaming complete.")

  return(df)
}
