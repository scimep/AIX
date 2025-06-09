#
#
#
#
#
#
#
#
#
# Load config and package manager
source("modules/config.R")
source("modules/package_manager.R")
source("modules/data_cleaner.R")
#
#
#
# Create package manager instance and update/load packages
pm <- PackageManager$new(config$packages)
pm$load_packages(update = TRUE)
#
#
#
# Load data
dataRaw <- read.csv("data/data_raw.csv") %>%
            as.data.frame()

#
#
#
#
#
#
# Clean headers
dataHeaders <- label_headers(dataRaw)

# Preview data
head(dataHeaders)
str(dataHeaders)
#
#
#
#
# Simplify timer data
dataTimers <- simplify_timer_data(dataHeaders)

head(dataTimers)
str(dataTimers)
#
#
#
#
# Filter valid participants
dataValid <- filter_valid_participants(dataTimers)

# Preview data
head(dataValid)
str(dataValid)
dim(dataValid)
#
#
#
#
#
# Filter outliers in response time
dataOutliers <- filter_outliers(dataValid)

# Preview data
head(dataOutliers)
str(dataOutliers)
dim(dataOutliers)
#
#
#
#
#
