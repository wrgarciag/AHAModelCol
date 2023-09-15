##..............................................................................
## 3.1 Initial state ----
##..............................................................................

#temp

nqx_bg <- 0.004
nqx_ihd <- 0.001
nqx_hhd <- 0.0005
nqx_istr <- 0.00025
nqx_hstr <- 0.00025

###.................
### 1. Demographic-----
###.................

pop0 <- readRDS(file=paste0(paste0(wd_proc,"PopProjectionCol2050.rds")))
pop0 <- pop0[ANO==Tstart,]

###.................
## 1.1 Fertility-----
###.................

births <- copy(pop0)
births[,births:=0]

###.................
## 1.2 Death-----
###.................

death[,timey:=0]
death[,deaths_bg:=population*nqx_bg]
death[,deaths_ihd:=population*nqx_ihd]
death[,deaths_hhd:=population*nqx_hhd]
death[,deaths_istr:=population*nqx_istr]
death[,deaths_hstr:=population*nqx_hstr]
death[,deaths_all:=deaths_bg+deaths_ihd+deaths_hhd+deaths_istr+deaths_hstr]



###.................
# 2. Epidemiologic-----
###.................

###.................
## 2.1 prevalence-----
###.................

###.................
## 2.2 incidence-----
###.................

#IRc is the transition from well to sick (incidence) from that cause

#CFc is the cause-specific transition probability 
#from sick to dead from cause c 

#BGmx,t is the cause-specific transition from sick to dead from any non-c cause 

###.................
# 3. Economic-----
###.................

###.................
## 3.1 Costs-----
###.................


##..............................................................................
# 3.2 Simulation ----
##..............................................................................

for (tt in TIME) {
  
  dt <- copy(pop0)
  
  for(pp in POLICY){
    
  }
  
}