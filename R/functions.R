packages <- c("dplyr", 
              "tidyr", 
              "httr", 
              "jsonlite", 
              "sf",
              "lubridate", 
              "dbscan", 
              "stringr", 
              "purrr")

functions <- list.files(path = "R/")
functions <- functions[functions != "functions.R"]

# Install and require external packages

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

lapply(packages, require, character.only = TRUE)

# Import custom functions

for(i in 1:length(functions)){
    source(paste0("R/", functions[i]))
}

# Clean the rubbish

rm(packages, functions, i)

load("data/data.Rdata")
