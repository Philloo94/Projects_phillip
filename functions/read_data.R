
# function to read files

read_data <- function(filepath, sep = ",") {
  if (file.exists(filepath)) {
    return(readr::read_csv(filepath, col_types = cols()))
  } else {
    return(NULL)
  }
}
