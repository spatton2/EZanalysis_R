#' @title EZsummary
#' @description Generates a clean, readable summary table from a glm() model which can be exported as CSV
#' @param model a model object (glm, aov, or manova)
#' @param round_digits Number of digits to round numerical output to (numerical)
#' @param column_menu Logical. If \code{TRUE}, displays a menu for user to manually choose summary stats. If \code{FALSE}, EZsummary will automatically display all stats
#' @param styled Logical. If \code{TRUE}, returns a styled gt table; if \code{FALSE}, r will return a tibble that is more piratical for future use
#' @return either a styled table or a tibble. Also gives the users the chance to export it
#' @export

EZsummary <- function(model, column_menu = TRUE, round_digits = 4, styled = TRUE) {

  model_class <- class(model)[1]
  if (model_class == "aov") {
    tidy_out <- tidy(model) %>%
      mutate(
        `q-value` = p.adjust(p.value, method = "fdr"),
        Significance = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01  ~ "**",
          p.value < 0.05  ~ "*",
          TRUE ~ "")
      ) %>%
      select(term, df, `F` = statistic, `p-value` = p.value, `q-value`, Significance)
  }else if (model_class == "manova") {
    tidy_out <- summary(model, test = "Pillai")$stats %>%
      as.data.frame() %>%
      rownames_to_column("Term") %>%
      rename(
        `Df` = Df,
        `Pillai` = Pillai,
        `F-value` = approx.F,
        `p-value` = Pr..approx.F.
      ) %>%
      mutate(
        `q-value` = p.adjust(`p-value`, method = "fdr"),
        Significance = case_when(
          `p-value` < 0.001 ~ "***",
          `p-value` < 0.01  ~ "**",
          `p-value` < 0.05  ~ "*",
          TRUE ~ ""
        )
      )
  } else if (model_class == "glm") {
    tidy_out <- broom::tidy(model) %>%
      dplyr::mutate(
        `q-value` = p.adjust(p.value, method = "fdr"),
        df = model$df.residual,
        Significance = dplyr::case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01  ~ "**",
          p.value < 0.05  ~ "*",
          TRUE ~ ""
        ),
        Effect = dplyr::case_when(
          estimate > 0 ~ "Positive",
          estimate < 0 ~ "Negative",
          TRUE ~ "Neutral"
        )
      ) %>%
      dplyr::mutate(across(where(is.numeric), ~ round(., round_digits))) %>%
      dplyr::select(
        Term = term, Estimate = estimate, `Std. Error` = std.error,
        `p-value` = p.value, `q-value`, df, Significance, Effect
      )

    # Optional menu to choose which columns to include
    if (column_menu) {
      col_choices <- c("Estimate", "Std. Error", "p-value", "q-value", "df", "Significance", "Effect")
      selected <- c()
      repeat {
        choice <- menu(c(col_choices, "DONE"), graphics = TRUE,
                       title = "Select columns to include in your table. Click DONE when finished.")
        if (choice > length(col_choices)) break
        selected <- c(selected, col_choices[choice])
        col_choices <- col_choices[-choice]
      }
      tidy_out <- tidy_out %>% dplyr::select(Term, dplyr::all_of(selected))
    }
  } else {
    stop("Unsupported model type. Input must be a GLM, AOV, or MANOVA model.")
  }
  # Export to CSV
  export_choice <- readline(prompt = "Would you like to export this summary as a CSV file? (y/n): ")

  if (tolower(export_choice) == "y") {
    file_name <- readline(prompt = "Enter file name (e.g., model_summary.csv): ")
    dir_path <- readline(prompt = "Enter full directory path (leave blank for working directory): ")

    if (dir_path == "") {
      out_path <- file_name
    } else {
      out_path <- file.path(dir_path, file_name)
    }

    write.csv(tidy_out, file = out_path, row.names = FALSE)
    message(paste("Summary table exported to:", normalizePath(out_path)))
  }

  # Styling (optional)
  available_cols <- colnames(tidy_out)

  if (styled) {
    gt_output <- tidy_out %>%
      gt() %>%
      tab_header(title = "GLM Summary Table")

    if ("Term" %in% available_cols) gt_output <- gt_output %>% cols_label(Term = "Variable")
    if ("Estimate" %in% available_cols) gt_output <- gt_output %>% cols_label(Estimate = "Coefficient")
    if ("Std. Error" %in% available_cols) gt_output <- gt_output %>% cols_label(`Std. Error` = "Std. Error")
    if ("p-value" %in% available_cols) {
      gt_output <- gt_output %>%
        cols_label(`p-value` = "p-Value") %>%
        data_color(
          columns = c(`p-value`),
          fn = scales::col_numeric(c("darkgreen", "white", "red"), domain = c(0, 1))
        )
    }

    message("EZsummary complete. use $data to view and rename dataframe, and $styled_table to view your table.")

    if ("df" %in% available_cols) gt_output <- gt_output %>% cols_label(df = "DF")

    return(list(data = tidy_out, styled_table = gt_output))
  } else {
    return(tidy_out)
  }
}
