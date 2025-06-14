source("modules/package_manager.R")
pm <- PackageManager$new(config$packages)
pm$load_packages(update = TRUE)

save_data <- function(df, path, fname, extension) {
  # Combine path and filename
  base_path <- file.path('./data', path)
  full_path <- file.path(base_path, paste0(fname, ".", extension))
  
  # Create directory if it doesn't exist
  dir.create(base_path, showWarnings = FALSE, recursive = TRUE)
  
  # Write the file based on extension
  if (extension == "csv") {
    # For CSV, convert haven_labelled to regular data types
    df_csv <- df %>% mutate(across(where(is.character), as.character)) %>%
                    mutate(across(where(function(x) inherits(x, "haven_labelled")), as.numeric))
    write.csv(df_csv, full_path, row.names = FALSE)
  } else if (extension == "sav") {
    # For SAV, preserve haven_labelled format
    write_sav(df, full_path)
  } else {
    stop("Unsupported file extension: ", extension)
  }
  
  print(paste("Data saved to", full_path))
}

label_headers <- function(df) {
  for(i in 1:ncol(df)) {
    df[[i]] <- labelled(
      df[[i]], 
      label = df[1,i]
    )
  }
  df <- as_tibble(df[3:nrow(df),])
  df
}

simplify_timer_data <- function(df) {
  timer_start <- 40   # A
  timer_end <- 167    # FK
  q_idx <- 17         # Q
  timer_cols <- timer_start:timer_end

  timer_matrix <- t(apply(df[, timer_cols], 1, function(row) {
    vals <- row[!is.na(row) & row != ""]
    length(vals) <- 4
    vals
  }))
  colnames(timer_matrix) <- c("first_click", "last_click", "page_submit", "click_count")

  # Debug: Print timer matrix dimensions
  print(paste("Timer matrix dimensions:", nrow(timer_matrix), "x", ncol(timer_matrix)))

  df <- df[, -timer_cols]
  left <- df[, 1:q_idx, drop=FALSE]
  right <- df[, (q_idx+1):ncol(df), drop=FALSE]
  
  # Debug: Print dimensions of parts
  print(paste("Left part dimensions:", nrow(left), "x", ncol(left)))
  print(paste("Right part dimensions:", nrow(right), "x", ncol(right)))

  result <- cbind(left, timer_matrix, right)
  
  # Debug: Print final dimensions
  print(paste("Final dimensions:", nrow(result), "x", ncol(result)))
  
  as.data.frame(result)
}

filter_valid_participants <- function(df) {
  df %>%
    mutate(respondent_id = as.character(respondent_id),
           finished = finished %>% as.character() %>% as.numeric(),
           consent_form = consent_form %>% as.character() %>% as.numeric(),
           citizenship = citizenship %>% as.character() %>% as.numeric(),
           duration__in_seconds_ = duration__in_seconds_ %>% zap_labels() %>% as.character() %>% as.numeric()) %>% 
    filter(
      finished == 1,
      consent_form == 1,
      citizenship == 1,
      !grepl("test", respondent_id)
    ) %>%
    group_by(respondent_id) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    as.data.frame()
}

filter_outliers <- function(df, thresholds) {
  df %>%
    mutate(logDuration = log(duration__in_seconds_)) %>%
    filter(logDuration >= thresholds["lower"] & 
           logDuration <= thresholds["upper"]) %>%
    `row.names<-`(NULL)
}

check_randomization <- function(df, joint = FALSE) {
  if (joint) {
    cat("\nJoint chi-square test for all combinations:\n")
    tbl <- table(paste(df$occupation, df$topic, df$model, df$label, sep = " / "))
    print(tbl)
    counts <- as.vector(tbl)
    print(chisq.test(counts))
  } else {
    for (col in c("occupation", "topic", "model", "label")) {
      cat("\n", col, ":\n")
      tbl <- table(df[[col]])
      print(tbl)
      print(chisq.test(tbl))
    }
  }
  invisible(NULL)
}