#' @title EZanova
#' @description Menu user-interfaced analysis of variance model creation
#' @param data dataset (tibble or dataframe) to create model from
#' @param normal_detect does the function give a warning when data is not normally distributed? (Logical)
#' @return aov object
#' @export

EZanova <- function(data, normal_detect = TRUE) {

  dep_index <- menu(choices = colnames(data), graphics = TRUE,
                    title = "Select your dependent variable (y-axis):")
  dependent_var <- colnames(data)[dep_index]

  add_more <- readline(prompt = "Do you want to add more dependent variables for MANOVA? (y/n): ")
  dep_vars <- dependent_var
  while (tolower(add_more) == "y") {
    remaining <- setdiff(colnames(data), dep_vars)
    next_choice <- menu(choices = c(remaining, "DONE"), graphics = TRUE,
                        title = "Select additional dependent variables or DONE.")
    if (next_choice > length(remaining)) break
    dep_vars <- c(dep_vars, remaining[next_choice])
  }

  if (any(sapply(data[dep_vars], is.factor))) {
    stop("Dependent variables must be numeric for ANOVA or MANOVA.")
  }

  indep_choices <- setdiff(colnames(data), dep_vars)
  selected <- c()
  repeat {
    choice <- menu(c(indep_choices, "DONE"), graphics = TRUE,
                   title = "Select independent variables (x-axis). Click DONE when finished.")
    if (choice > length(indep_choices)) break
    selected <- c(selected, indep_choices[choice])
    indep_choices <- indep_choices[-choice]
  }

  if (length(dep_vars) > 1) {
    formula_text <- paste0("cbind(", paste(dep_vars, collapse = ", "), ") ~ ", paste(selected, collapse = " + "))
    model_formula <- as.formula(formula_text)
    model <- manova(model_formula, data = data)
    message("MANOVA model fitted.")
  } else {
    model_formula <- as.formula(paste(dep_vars, "~", paste(selected, collapse = " + ")))
    model <- aov(model_formula, data = data)
    message("ANOVA or ANCOVA model fitted.")
  }

  if (normal_detect && length(dep_vars) == 1) {
    res <- residuals(model)
    sw <- shapiro.test(res)
    if (sw$p.value < 0.05) {
      message("Warning: Residuals are not normally distributed. ANOVA assumptions may be violated.")
      print(sw)
    }
  }

  if (length(dep_vars) == 1 && all(sapply(data[selected], is.factor))) {
    tukey <- TukeyHSD(model)
    message("Tukey post-hoc test results:")
    print(tukey)
  } else if (length(dep_vars) == 1) {
    message("Skipping Tukey HSD: not all predictors are factors.")
  }

  return(model)
}
