source("modules/package_manager.R")
pm <- PackageManager$new(config$packages)
pm$load_packages(update = TRUE)

write_csv <- function(df, path, fname) {
  # Combine path and filename
  base_path <- file.path('./data', path)
  full_path <- file.path(base_path, fname)
  
  # Create directory if it doesn't exist
  dir.create(base_path, showWarnings = FALSE, recursive = TRUE)
  
  # Write the CSV file
  write.csv(df, full_path, row.names = FALSE)
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
  colnames(timer_matrix) <- c("First.Click", "Last.Click", "Page.Submit", "Click.Count")

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
    mutate(RESPONDENT_ID = as.character(RESPONDENT_ID),
           Finished = Finished %>% as.character() %>% as.numeric(),
           Consent.Form = Consent.Form %>% as.character() %>% as.numeric(),
           Citizenship = Citizenship %>% as.character() %>% as.numeric(),
           Duration..in.seconds. = Duration..in.seconds. %>% zap_labels() %>% as.character() %>% as.numeric()) %>%
    filter(
      Finished == 1,
      Consent.Form == 1,
      Citizenship == 1,
      !grepl("test", RESPONDENT_ID)
    ) %>%
    group_by(RESPONDENT_ID) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    as.data.frame()
}

filter_outliers <- function(df, thresholds) {
  df %>%
    mutate(logDuration = log(Duration..in.seconds.)) %>%
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