rm(list = ls())

#.............................................................................
# 0 Macros
#.............................................................................


wd <- "C:/Users/wrgar/OneDrive - UW/02Work/AHAPiCol/AHAModelCol/"

wd_c <- paste0(wd,"Code/")
#wd_d <- paste0("C:/Users/wrgar/OneDrive - UW/02Work/AHAPiCol/Data/")
wd_dinpu <- paste0("C:/Users/wrgar/OneDrive - UW/02Work/AHAPiCol/Data/")
wd_dproc <- paste0(wd,"/Data/")
wd_r <- paste0(wd,"Resu/")

#libraries
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(doParallel)
library(readxl)

setwd(wd_c)

# load ad-hoc functions
source("01Functions.R")

# Policy scenarios

POLICY <- c("Business as Usual","Progress","Aspirational")
Tstart <- 2024
Tend   <- 2050
TIME   <- 2024:2050

#.............................................................................
# 1 Data processing
#.............................................................................

source("10Data.R")

#.............................................................................
# 2 Parameter estimation
#.............................................................................

source("20Estima.R")

#.............................................................................
# 3 Model
#.............................................................................

source("30Model.R")

#.............................................................................
# 4 Outputs
#.............................................................................

source("40Resu.R")


