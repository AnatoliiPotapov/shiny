# libaray(devtools)
# install_github("RDML", "kablag")

library(RDML)
library(chipPCR)
library(shiny)

source("./readDT.R")

data <- read_DT("./Data/резус_и_пол__Нерабочий набор.r48",48)
CPP(1:60, data$optic$FAM_2[,1], amptest = TRUE)