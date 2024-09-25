# Loading data
cat("Loading data from file...\n")

# Step 1: Set working directory to the folder containing the .csv files
setwd(data_dir)


# Step 2: List all CSV files in the directory
csv_files <- list.files(pattern = "*.csv", full.names = TRUE)

# Step 3: Read all CSV files into a list of data frames
library(data.table)
data_list <- lapply(csv_files, fread)

# Saving data object into a file in rdas folder
# saveRDS(data, file = "rdas/data.rda") 



cat("Data loaded successfully!\n")



