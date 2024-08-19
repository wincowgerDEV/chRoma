library(jsonlite)

my_data <- jsonlite::fromJSON('R/data.json')

df <- my_data$data
