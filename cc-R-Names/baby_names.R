# Load packages into environment.
library(readr)
library(dplyr)
library(ggplot2)

# Read a data file into memory
# Note: use the read_csv() function, not read.csv()
names2010 <- read_csv("./data/2010")

# Insepcting the data
# View the data as a table in the file pane
View(names2010)

# View the data in the console
names2010

