
#temp

# Case fatality
nqx_bg <- 0.004
cf_ihd <- 0.001
cf_hhd <- 0.0005
cf_istr <- 0.00025
cf_hstr <- 0.00025

# Infection rates

ir_ihd  <- 0.0005
ir_hhd  <- 0.0005
ir_istr <- 0.0005
ir_hstr <- 0.0005


###.................
### 1. Demographic-----
###.................

pop0 <- readRDS(file=paste0(paste0(wd_proc,"PopProjectionCol2050.rds")))
pop0 <- pop0[year==Tstart,]

###.................
## 1.1 Fertility-----
###.................

birth <- copy(pop0)
birth[,births:=0]
birth[,T:=0]
birth[,c('population'):=NULL]

###.................
## 1.2 Death-----
###.................

death <- copy(pop0)
death[,T:=0]
death[,deaths_bg:=population*nqx_bg]
death[,deaths_ihd:=population*cf_ihd]
death[,deaths_hhd:=population*cf_hhd]
death[,deaths_istr:=population*cf_istr]
death[,deaths_hstr:=population*cf_hstr]
death[,deaths_all:=deaths_bg+deaths_ihd+deaths_hhd+deaths_istr+deaths_hstr]

death[,c('population'):=NULL]

###.................
# 2. Epidemiologic-----
###.................

###.................
## 2.1 prevalence-----
###.................

sick <- copy(pop0)
sick[,T:=0]
sick[,sicks_ihd:=population*ir_ihd]
sick[,sicks_hhd:=population*ir_hhd]
sick[,sicks_istr:=population*ir_istr]
sick[,sicks_hstr:=population*ir_hstr]
sick[,sicks_all:=sicks_ihd+sicks_hhd+sicks_istr+sicks_hstr]

sick[,c('population'):=NULL]

###.................
## 2.2 incidence-----
###.................

#IRc is the transition from well to sick (incidence) from that cause

infect <- copy(pop0)
infect[,T:=0]
infect[,infects_ihd:=population*ir_ihd]
infect[,infects_hhd:=population*ir_hhd]
infect[,infects_istr:=population*ir_istr]
infect[,infects_hstr:=population*ir_hstr]
infect[,infects_all:=infects_ihd+infects_hhd+infects_istr+infects_hstr]

infect[,c('population'):=NULL]

###.................
# 3. Economic-----
###.................

###.................
## 3.1 Costs-----
###.................