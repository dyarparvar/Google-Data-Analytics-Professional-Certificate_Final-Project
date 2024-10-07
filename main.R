# Set up the environment
cat("Setting up the environment...\n")

# 1. Load necessary packages (you can modify based on your needs)
cat("Loading required packages...\n")
packages <- c("data.table", "skimr", "tidyverse", "lubridate", "dplyr", "ggplot2", "readr", "hms", "RColorBrewer")  # Add packages you need
lapply(packages, require, character.only = TRUE)

# 2. Set project directories
project_dir <- setwd(here::here()) # set the directory to the project's directory using the here package, which is great for managing file paths relative to your project
# Or specify the project directory manually
scripts_dir <- file.path(project_dir, "scripts")
scripts_dir <- file.path(project_dir, "scripts")
data_dir <- file.path(project_dir, "data")
output_dir <- file.path(project_dir, "outputs")

cat("Project directory set to: ", project_dir, "\n")

# 3. Source and run individual scripts
cat("Running script 1: Data loading...\n")
source(file.path(scripts_dir, "01-load.R"))  

cat("Running script 2: Data preprocessing...\n")
source(file.path(scripts_dir, "02-tidy.R")) 

cat("Running script 3: Data analysis...\n")
source(file.path(scripts_dir, "03-analysis.R")) 

# 4. Print a completion message
cat("All scripts executed successfully!\n")

