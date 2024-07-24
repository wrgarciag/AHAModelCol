#################################################################################################
#################################################################################################
###########################################
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd("../data_preprocessing")
setwd(paste0("C:/Users/wrgar/OneDrive - UW/02Work/AHAPiCol/AHAModelCol/Data/"))
#################################################################################################

#libraries
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(openxlsx)
library(writexl)
library(readxl)
library(EnvStats)
library(fitdistrplus)

wd <- "C:/Users/wrgar/OneDrive - UW/02Work/AHAPiCol/AHAModelCol/"

wd_c <- paste0(wd,"Code/")
#wd_d <- paste0("C:/Users/wrgar/OneDrive - UW/02Work/AHAPiCol/Data/")
wd_dinpu <- paste0("C:/Users/wrgar/OneDrive - UW/02Work/AHAPiCol/Data/")
wd_dproc <- paste0(wd,"Data/")
wd_r <- paste0(wd,"Resu/")

wd_heor <- "C:/Users/wrgar/OneDrive - UW/01Study/HEOR530/"

# Parameters

scenarios <- c("bau","prog","asp")

# results
dt <- as.data.table(read_excel(paste0(wd_heor,"HEOR530_ProjectFinal_WilliamGarcia.xlsx"), sheet = "Results",range = "A4:BN17028"))

parameters <- read_excel(paste0(wd_heor,"HEOR530_ProjectFinal_WilliamGarcia.xlsx"), sheet = "BCA",range = "B1:D27")

#b1 - d26
base_year <-	parameters$Value[parameters$Parameter=="base year"]
last_year	<- parameters$Value[parameters$Parameter=="last year"]
n <-	last_year - base_year

delta	<- parameters$Value[parameters$Parameter=="delta"]

Annualization <- parameters$Value[parameters$Parameter=="Annualization factor"] 

# Benefit Parameters
GNIGrowth <-	parameters$Value[parameters$Parameter=="GNI Growth"]
VSL <- parameters$Value[parameters$Parameter=="VSL $USD"] 
COP_USD <- parameters$Value[parameters$Parameter=="COP/USD Rate"] 
PPP<- parameters$Value[parameters$Parameter=="PPP Country/USD"]

# Cost parameters
COI <- parameters$Value[parameters$Parameter=="COI Base"] 
Cost_growth	<- parameters$Value[parameters$Parameter=="Cost growth"]
COI_CVD	<- parameters$Value[parameters$Parameter=="COI CVD"] 
Cost_Management <- parameters$Value[parameters$Parameter=="Cost of Management Events"] 
Cost_event<- parameters$Value[parameters$Parameter=="Cost of event (average)"]
Cost_Diagnosing	<- parameters$Value[parameters$Parameter=="Cost of Diagnosing"] 
Cost_treatment	<- parameters$Value[parameters$Parameter=="Cost of treatment"] 
Cost_control	 <- parameters$Value[parameters$Parameter=="Cost of control"] 
Cost_intervention	 <- parameters$Value[parameters$Parameter=="Cost of intervention"] 

# Intervention parameters

Hbp_prev	<- parameters$Value[parameters$Parameter=="Hypertension prevalence"]
Diagnosed_Base	<- parameters$Value[parameters$Parameter=="Diagnosed Base"]
Treated_base	<- parameters$Value[parameters$Parameter=="Treated base"]
Controled_base	<- parameters$Value[parameters$Parameter=="Controled base"]
Coverage_base <- parameters$Value[parameters$Parameter=="Coverage base"]
covg_bau <- parameters$Value[parameters$Parameter=="BAU Coverage growth"]
covg_prog <- parameters$Value[parameters$Parameter=="Progress Coverage growth"]
covg_asp <-	parameters$Value[parameters$Parameter=="Aspirational Coverage growth"]

dt_out <- c()

dt[,dead_base:=dead_bau]

for(jj in 1:1000) {
  
  cat(paste0("simulation ",jj), sep = "\n")
  
  set.seed(123+jj)
  delta <- 	rtri(n = 1, min = 0.01, max = 0.05, mode = 0.03)
  VSL <- runif(1, 1434096,1949000)
  
  desired_mean <- 342
  shape <- 5
  rate <- shape/desired_mean
  
  Cost_intervention	 <- rgamma(n = 1, shape = shape, rate = rate)
  
  dt_resu <- c()
  
  for(ii in seq_along(scenarios)){
    
    covg_scenario <- c(covg_bau,covg_prog,covg_asp)
    covg <- covg_scenario[ii]
    
    dt_s <- dt[,c("age","cause","sex","year","dead_base",colnames(dt)[grepl(paste0("_",scenarios[ii]), names(dt))]),with = FALSE]
    colnames(dt_s) <- gsub(paste0("_",scenarios[ii]),"",colnames(dt_s))
    
    dt_s[,vsl:=(dead_base-dead)*VSL*(1+GNIGrowth)^(year-base_year)]
    dt_s[,discount:=1/(1+delta)^(year-base_year)]
    dt_s[,vpnb:=vsl*discount]
    
    dt_s[,coi:=sick*COI_CVD*(1+Cost_growth)^(year-base_year)]
    dt_s[,vpncoi:=discount*coi]
    #dt_s[,coverage:=Coverage_base*(1+covg)^(year-base_year)]
    dt_s[,cost:=Hbp_prev*pop*coverage*Cost_intervention*(1+Cost_growth)^(year-base_year)]
    dt_s[,vpncost:=discount*cost]
    dt_s[,cost_sys:=well*COI*(1+Cost_growth)^(year-base_year)]
    dt_s[,vpncost_syst:=cost_sys*discount]
    dt_s[,vpnc:=vpncost+vpncost_syst]
    dt_s[,netpv:=vpnb-vpnc]
    
    dt_r <- dt_s[,list(CVD_Deaths=sum(dead),
                       AllDeaths=sum(all.mx),
                       PrevalentCases=sum(sick),
                       Exposure=sum(pop),
                       Benefit_NPV=sum(vpnb),
                       Intervention_Cost_NPV=sum(vpncost),	
                       Well_System_Cost_NPV=sum(vpncost_syst),	
                       Budget_COI_NPV=sum(vpncoi),
                       escenario=scenarios[ii]
    ),by=list()]
    
    dt_resu <- rbind(dt_resu,dt_r)
  }
  
  dt_resu[,System_Cost_NPV:=Well_System_Cost_NPV+Budget_COI_NPV]
  
  ben_prog <- as.numeric(dt_resu[escenario=="prog","Benefit_NPV",with=F] - dt_resu[escenario=="bau","Benefit_NPV",with=F])
  
  cost_sys <- as.numeric(dt_resu[escenario=="prog","System_Cost_NPV",with=F] - dt_resu[escenario=="bau","System_Cost_NPV",with=F])
  cost_int <- as.numeric(dt_resu[escenario=="prog","Intervention_Cost_NPV",with=F] - dt_resu[escenario=="bau","Intervention_Cost_NPV",with=F])
  cost_prog <- cost_int + cost_sys
    
  bcr_prog <- ben_prog/cost_prog
  bcr_prog
  
  ben_asp <- as.numeric(dt_resu[escenario=="asp","Benefit_NPV",with=F] - dt_resu[escenario=="bau","Benefit_NPV",with=F])
  
  cost_sys <- as.numeric(dt_resu[escenario=="asp","System_Cost_NPV",with=F] - dt_resu[escenario=="bau","System_Cost_NPV",with=F])
  cost_int <- as.numeric(dt_resu[escenario=="asp","Intervention_Cost_NPV",with=F] - dt_resu[escenario=="bau","Intervention_Cost_NPV",with=F])
  cost_asp <- cost_int + cost_sys
  bcr_asp <- ben_asp/cost_asp

  # bcr_prog_mc[jj] <- bcr_prog  
  # bcr_asp_mc[jj]  <- bcr_asp
  
  # Scenario
  dt_resu[,bcr_prog:=bcr_prog]
  dt_resu[,bcr_asp:=bcr_asp]
  dt_resu[,simulation:=jj]
  dt_resu[,delta:=delta]
  dt_resu[,vsl:=VSL]
  dt_resu[,Cost_intervention:=Cost_intervention]
  
  # scenario*MC
  dt_out <- rbind(dt_out,dt_resu)
  
  cat(paste("delta",round(delta,3)), sep = "\n")
  cat(paste("bcr prog",round(bcr_prog,3)), sep = "\n")
  cat(paste("bcr asp",round(bcr_asp,3)), sep = "\n")
}

bcr_mc <- unique(dt_out[,c("simulation","delta","vsl","Cost_intervention","bcr_prog","bcr_asp"),with=F])

saveRDS(bcr_mc,file = paste0(wd_r,"BCRMonteCarlo.rds"))

bcr_mc <- readRDS(file = paste0(wd_r,"BCRMonteCarlo.rds"))

summary(bcr_mc$bcr_prog)
summary(bcr_mc$bcr_asp)

p90<- quantile(bcr_mc$bcr_prog,probs = 0.90)
p90
p90<- quantile(bcr_mc$bcr_asp,probs = 0.90)
p90

p99<- quantile(bcr_mc$bcr_prog,probs = 0.99)
p01<- quantile(bcr_mc$bcr_prog,probs = 0.001) 

bcr_mc <- bcr_mc[bcr_prog>p01 & bcr_prog<p99,]

# hist(bcr_mc$bcr_prog)
# summary(bcr_mc$bcr_prog)

plotdist(bcr_mc$bcr_prog, histo = TRUE, demp = TRUE)
plotdist(bcr_mc$bcr_asp, histo = TRUE, demp = TRUE)



# plot(hist(bcr_prog_mc))
# plot(hist(bcr_asp_mc),add=T, col = 2)

