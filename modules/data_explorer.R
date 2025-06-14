source("modules/package_manager.R")
pm <- PackageManager$new(config$packages)
pm$load_packages(update = TRUE)

get_label <- function(data, var_name) {
  # Get the original data with labels
  data_with_labels <- read_sav("data/interim/5.data_clean.sav")
  
  # Get label and scale information
  label <- attr(data_with_labels[[var_name]], "label")
  scale <- attr(data_with_labels[[var_name]], "labels")
  
  # Return both label and scale
  return(list(
    variable = var_name,
    label = label,
    scale = scale
  ))
}