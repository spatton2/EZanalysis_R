#' @title EZglm
#' @description Menu user-interfaced linear model creation
#' @param data dataset (tibble or dataframe) to create model from
#' @param type_detect does the function automatically assign the Gaussian (linear) or Binomial (logistic) curve? (Logical)
#' @param normal_detect does the function give a warning when data is not normally distributed? (Logical)
#' @param standardize_data should the data be standardized for adjusted p and beta values? (Logical)
#' @return glm object
#' @export

EZglm <- function(data, type_detect = TRUE, normal_detect = TRUE, standardize_data = FALSE){

  if (standardize_data) {
    data <- data %>% mutate(across(where(is.numeric), scale))
  }

  dep_index <- menu(choices = colnames(data), graphics = TRUE,
                    title = "Which variable is your dependent variable (y-axis)?")
  dependent_var <- colnames(data)[dep_index]

  indep_choices <- setdiff(colnames(data), dependent_var)
  selected <- c()
  repeat {
    choice <- menu(c(indep_choices, "DONE"), graphics = TRUE,
                   title = "Select independent variables (x-axis). Click DONE when finished.")
    if (choice > length(indep_choices)) break
    selected <- c(selected, indep_choices[choice])
    indep_choices <- indep_choices[-choice]
  }

  # formula_text <- paste(dependent_var, "~", paste(selected, collapse = " + "))
  # model_formula <- as.formula(formula_text)

  if (type_detect) {
    if (length(unique(data[[dependent_var]])) == 2) {
      model <- glm(as.formula(paste(dependent_var, "~", paste(selected, collapse = " + "))), family = binomial(), data = data)
    } else {
      model <- glm(as.formula(paste(dependent_var, "~", paste(selected, collapse = " + "))), family = gaussian(), data = data)
    }
  } else {
    model <- glm(as.formula(paste(dependent_var, "~", paste(selected, collapse = " + "))), family = gaussian(), data = data)
  }

  print(model)

  if (normal_detect){
    res <- residuals.glm(model)
    sw <- shapiro.test(res)
    if(sw$p.value < 0.05){
      message("Warning: Residuals are not normal, linear test may not be significant.")
      print(sw)
    }
  }

  if (length(data) < (length(selected)*10)){
    message("Warning, small sample size. test may not be significant.")
  }
  return(model)
}
