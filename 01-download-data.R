getwd()
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dest_file <- "data/raw-data.csv" # the path is relative to the current project folder which is "creation date-project"
download.file(url, destfile = dest_file)