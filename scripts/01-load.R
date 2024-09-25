# Loading data
cat("Loading data from file...\n")

# Step 1: Set working directory to the folder containing the .csv files
setwd(data_dir)


# Step 2: List all CSV files in the directory
csv_files <- list.files(pattern = "*.csv", full.names = TRUE)

# Step 3: Read all CSV files into a list of data frames
data_list <- lapply(csv_files, fread)



cat("Data loaded successfully!\n")



