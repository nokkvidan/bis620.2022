## code to prepare `DATASET` dataset goes here

diabetes <- read.csv("diabetes_binary.csv")
usethis::use_data(diabetes, overwrite = TRUE)
