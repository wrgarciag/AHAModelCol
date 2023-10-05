
##William----

# Define a function for cubic spline interpolation
cubic_spline_interpolation <- function(x, y, xout) {
  # Perform cubic spline interpolation
  spline_result <- spline(x, y, xout = xout, method = "natural")
  
  # Return the interpolated values
  return(spline_result$y)
}


interpolate_nmx_i <- function(nmx, ii, jj, ll) {
  
  if(nrow(nmx)==0){
   int_val <- c() 
  }else{
    nmx_i <- nmx[cause == ii & region1 == jj & sex == ll & gage_5 >= 20, ]
    
    min_x <- min(nmx_i$gage_5, na.rm = TRUE)
    max_x <- max(nmx_i$gage_5, na.rm = TRUE)
    x <- nmx_i$gage_5
    y <- nmx_i$MortalityRate
    xout <- seq(min_x, max_x, by = 1)
    
    int_val <- cubic_spline_interpolation(x, y, xout)
    
    int_val <- as.data.table(cbind(jj, ll, ii, xout, int_val))
    setnames(int_val, old = c("jj", "ll", "ii", "xout", "int_val"), new = c('region1', 'sex', 'cause', 'gage_5', 'MortalityRate'))
    
  }
  return(int_val)
}


categorize_age <- function(age,option="5year"){
  
  if(option=="5year"){
    gedad <- cut(age,breaks = c(-Inf,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,Inf),
                 labels = c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95),
                 right = FALSE)
    
  }else if(option=="upc"){
    
    gedad <- cut(age,breaks = c(-Inf,1,5,15,19,45,50,55,60,65,70,75,Inf),
                     labels = c("01.0","02.1-4","03.5-14","04.15-18","05.19-44","06.45-49","07.50-54",
                                "08.55-59","09.60-64","10.65-69","11.70-74","12.75+"),
                     right = FALSE)
  }else{
    
    warning("Invalid age categorizing option")
    return(NULL)
  }
  
}

# Define a function to calculate the life table
calculate_life_table <- function(mortality_data) {
  # Create an empty life table data.table
  life_table <- c()
  
  # Initialize variables
  n <- nrow(mortality_data)
  qx <- numeric(n)
  lx <- numeric(n)
  Lx <- numeric(n)
  Tx <- numeric(n)
  dx <- numeric(n)
  LxTx <- numeric(n)
  Tpx <- numeric(n)
  
  # Calculate life table values
  for (i in 1:n) {
    if (i == 1) {
      lx[i] <- 1000  # Initial population (e.g., 1000 at age 0)
    } else {
      lx[i] <- lx[i - 1] - dx[i - 1]
    }
    
    qx[i] <- mortality_data[i, MortalityRate]
    dx[i] <- lx[i] * qx[i]
    
    if (i == 1) {
      Lx[i] <- lx[i] / 2
    } else {
      Lx[i] <- lx[i - 1] + (lx[i] + lx[i - 1]) / 2
    }
    
    Tx[i] <- sum(Lx)
    LxTx[i] <- Lx[i] * Tx[i]
    Tpx[i] <- LxTx[i] / Lx[1]
  }
  
  # Populate the life table data.table
  life_table$Age <- mortality_data$Age
  life_table$qx <- qx
  life_table$lx <- lx
  life_table$Lx <- Lx
  life_table$Tx <- Tx
  life_table$dx <- dx
  life_table$LxTx <- LxTx
  life_table$Tpx <- Tpx
  
  life_table <- as.data.table(life_table)
  return(life_table)
}




### Sarah----

get.bp.prob<-function(DT, salteff, saltmet, saltyear1, saltyear2, rx, drugaroc){
  
  if(rx==1 & drugaroc =="baseline"){
    DT[,covinc:=aroc2]
    #DT[,target_year:=ifelse(reach_base>2022, reach_base, 2022)]
  }
  
  if(rx==1 & drugaroc=="p75"){
    DT[,covinc:=p_change2]
    #DT[,target_year:=ifelse(refwsalt>2022, refwsalt, 2022)]
  }
  
  if(rx==1 & drugaroc=="p975"){
    DT[,covinc:=a_change2]
    #DT[,target_year:=ifelse(aspwsalt>2022, aspwsalt, 2022)]
  }
  
  if(rx==1 & drugaroc=="ideal"){
    DT[,covinc:=ideal]
    #DT[,target_year:=2030]
  }
  
  else{}
  
  
  #make salt variable represent salt gap
  if(saltmet=="percent"){
    DT[,salt_target:=salt*(1-salteff)]
    DT[salt_target<5.04, salt_target:=5.04]
    DT[salt<5.04, salt:=0]
    DT[salt>0,salt:=salt-salt_target]
    DT[salt<0, salt:=0]
  }
  
  if(saltmet=="target"){
    DT[,salt:=salt-salteff]
    DT[salt<0, salt:=0]
  }
  
  if(saltmet=="app"){
    DT[,salt:=salteff]
  }
  
  else{}
  
  if(salteff!=0){
    DT[Year>=saltyear1 & Year<=saltyear2, Mean:=Mean-(((1.12*raisedBP)+((1-raisedBP)*0.58))*salt*(Year-saltyear1+1)/(saltyear2-saltyear1+1))]
    DT[Year>saltyear2, Mean:=Mean-(((1.12*raisedBP)+((1-raisedBP)*0.58))*salt)]
  }
  
  else{}
  
  DT[bp_cat=="<120", prob:=pnorm(120,Mean,stdev)]
  DT[bp_cat=="120-129", prob:=pnorm(130,Mean,stdev)-pnorm(120,Mean,stdev)]
  DT[bp_cat=="130-139", prob:=pnorm(140,Mean,stdev)-pnorm(130,Mean,stdev)]
  DT[bp_cat=="140-149", prob:=pnorm(150,Mean,stdev)-pnorm(140,Mean,stdev)]
  DT[bp_cat=="150-159", prob:=pnorm(160,Mean,stdev)-pnorm(150,Mean,stdev)]
  DT[bp_cat=="160-169", prob:=pnorm(170,Mean,stdev)-pnorm(160,Mean,stdev)]
  DT[bp_cat=="170-179", prob:=pnorm(180,Mean,stdev)-pnorm(170,Mean,stdev)]
  DT[bp_cat=="180+", prob:=1-pnorm(180,Mean,stdev)]
  
  if(rx==1){
    DT[,shift:=prob*(covinc)]
    DT[bp_cat=="<120" | bp_cat=="120-129" | bp_cat=="130-139", shift:=0]
    DT[, add130:=sum(shift*diabetes), by=.(age, sex, Year)]
    DT[, add140:=sum(shift*(1-diabetes)), by=.(age, sex, Year)]
    DT[,prob:=prob-shift]
    DT[bp_cat=="120-129", prob:=prob+add130]
    DT[bp_cat=="130-139", prob:=prob+add140]
  }
  
  else{}
  
  DT[,c("age", "sex", "Year", "bp_cat" ,"prob", "location")]
  
}


#################################################################################################
# As a function
#################################################################################################
project.all <- function(Country, saltmet, salteff, saltyear2, drugcov){
  #################################################################################################
  base_rates<-b_rates[location==Country]#[, -c("year")]
  #################################################################################################
  #################################################################################################
  #intervention scenarios
  DT<-unique(data.in[location==Country][,Year:=2017][,-c("Lower95", "Upper95")])
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  
  bp_prob_salt<-get.bp.prob(DT.in, salteff, saltmet, 2023, saltyear2, 1, "baseline")
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_drug<-get.bp.prob(DT.in, 0, "app", 2023, saltyear2, 1, drugcov)
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_both<-get.bp.prob(DT.in, salteff, saltmet, 2023, saltyear2, 1, drugcov)
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_base<-get.bp.prob(DT.in, 0, "app", 2023, saltyear2, 0, "baseline")
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_bau<-get.bp.prob(DT.in, 0, "app", 2023, saltyear2, 1, "baseline")
  
  bp_prob_salt[,intervention:="Salt reduction"]
  bp_prob_drug[,intervention:="Antihypertensive therapy"]
  bp_prob_both[,intervention:="Both"]
  bp_prob_bau[,intervention:="b.a.u"]
  
  setnames(bp_prob_base, "prob", "prob_0")
  
  bp_probs<-bind_rows(bp_prob_both, bp_prob_drug, bp_prob_salt, bp_prob_bau)
  bp_probs<-merge(bp_probs, bp_prob_base, by=c("age","sex", "bp_cat", "Year", "location")) #change to "Year"
  
  #duplicating data to be age-specific
  bp_probs[, age:=as.numeric(substr(age, 1,2))]
  bp_probs<-bp_probs[rep(seq_len(nrow(bp_probs)), each=5)]
  bp_probs[,age2:=rep(1:5, nrow(bp_probs)/5)][,age:=age+age2-1]
  
  over90<-bp_probs[age==89]
  
  over90<-over90[rep(seq_len(nrow(over90)), each=6)]
  over90[,age2:=rep(1:6, nrow(over90)/6)][,age:=age+age2]
  
  #bind  
  bp_probs<-rbindlist(list(bp_probs, over90))
  
  ##add RRis##
  addRR<-function(RR, bp){
    if(bp=="<120"){1}
    else if (bp=="120-129"){1/RR}
    else if (bp=="130-139"){1/RR^2}
    else if (bp=="140-149"){1/RR^3}
    else if (bp=="150-159"){1/RR^4}
    else if (bp=="160-169"){1/RR^5}
    else if (bp=="170-179"){1/RR^6}
    else {1/RR^7}
  }
  
  bp_probs[, RRi_IHD:=sapply(bp_cat, addRR, RR=0.83)]
  bp_probs[, RRi_HHD:=sapply(bp_cat, addRR, RR=0.72)]
  bp_probs[, RRi_stroke:=sapply(bp_cat, addRR, RR=0.73)]
  
  
  ##add alphas##
  alphas<-bp_probs[,.(ihd=sum(prob_0*RRi_IHD), istroke=sum(prob_0*RRi_stroke), 
                      hstroke=sum(prob_0*RRi_stroke), hhd=sum(prob_0*RRi_HHD)), 
                   by=.(age, sex, location, intervention, Year)] #change to "Year"
  
  alphas<-melt(alphas, id.vars=c("age", "sex", "location", "intervention", "Year"), measure.vars=c(), variable.name = "cause",
               value.name="alpha")#change to "Year"
  
  
  rris<-bp_probs[,list(age, sex, Year, location, intervention, bp_cat, prob, RRi_IHD, RRi_HHD, RRi_stroke)]#change to "Year"
  rris[,hstroke:=RRi_stroke]
  
  setnames(rris, c("RRi_IHD", "RRi_HHD", "RRi_stroke"), c("ihd", "hhd","istroke"))
  rris<-melt(rris, id.vars=c("age", "sex", "location", "intervention", "bp_cat", "prob", "Year"), measure.vars=c(), variable.name = "cause",
             value.name="RRi")#change to "Year"
  
  bp_probs<-merge(rris, alphas, by=c("age", "sex", "location", "intervention","cause", "Year"))#change to "Year"
  setnames(bp_probs, "Year", "year")
  
  ####adding baseline_rates
  intervention_rates<-merge(bp_probs, base_rates, by=c("age", "sex", "location", "cause", "year"))
  
  #calculating yi*pi
  intervention_rates[, yixpi:=(RRi*IR/alpha)*prob]
  intervention_rates[, IR:=sum(yixpi), by=.(age, sex, location, intervention, cause, CF, 
                                            BG.mx, BG.mx.all, PREVt0, DIS.mx.t0, Nx, year, ALL.mx)]#change to "Year"
  
  intervention_rates<-unique(intervention_rates[,-c("prob", "bp_cat", "yixpi", "RRi", "alpha")])
  
  ##add CF effects##
  #this is ugly code
  
  intervention_rates<-as.data.table(left_join(intervention_rates, inc%>%rename(year=Year), by=c("location","year")))
  
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="ihd",     CF:=CF*(1-0.24*aroc)]
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="istroke", CF:=CF*(1-0.36*aroc)]
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="hstroke", CF:=CF*(1-0.76*aroc)]
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="hhd",     CF:=CF*(1-0.20*aroc)]
  
  if(drugcov=="p75"){
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="ihd",     CF:=CF*(1-0.24*p_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="istroke", CF:=CF*(1-0.36*p_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hstroke", CF:=CF*(1-0.76*p_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hhd",     CF:=CF*(1-0.20*p_change)]
  }
  
  if(drugcov=="p975"){
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="ihd",     CF:=CF*(1-0.24*a_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="istroke", CF:=CF*(1-0.36*a_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hstroke", CF:=CF*(1-0.76*a_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hhd",     CF:=CF*(1-0.20*a_change)]
  }
  
  if(drugcov=="ideal"){
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="ihd",     CF:=CF*(1-0.24*ideal)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="istroke", CF:=CF*(1-0.36*ideal)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hstroke", CF:=CF*(1-0.76*ideal)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hhd",     CF:=CF*(1-0.20*ideal)]
  }
  
  ##########################################################################################
  
  ## calculate initial states for the incoming year 2000 and all years for age 20 population
  intervention_rates[year==2017 | age==20, sick:=Nx*PREVt0]
  intervention_rates[year==2017 | age==20, dead:=Nx*DIS.mx.t0]
  intervention_rates[year==2017 | age==20, well:=Nx*(1-(PREVt0+ALL.mx))]
  
  
  #base_rates<-base_rates[location %in% countrylist]
  intervention_rates[age==20 | year==2017, pop:=Nx]
  intervention_rates[age==20 | year==2017, all.mx:=Nx*ALL.mx]
  
  intervention_rates[CF>0.99, CF:=0.99]
  intervention_rates[IR>0.99, IR:=0.99]
  
  #STATE TRANSITIONS#
  for(i in 1:41){
    
    b2<-intervention_rates[year<=2017+i & year>=2017+i-1]
    b2[,age2:=age+1]
    
    #newcases
    b2[, newcases2:=shift(well)*IR, by=.(sex, location, cause, age, intervention)]
    
    #sick
    b2[, sick2:=shift(sick)*(1-(CF+BG.mx+covid.mx)) + shift(well)*IR, by=.(sex, location, cause, age, intervention)]
    b2[sick2<0, sick2:=0]
    
    #dead
    b2[, dead2:=shift(sick)*CF, by=.(sex, location, cause, age, intervention)]
    b2[dead2<0, dead2:=0]
    
    #pop
    b2[,pop2:=shift(pop)-shift(all.mx), by=.(sex, location, cause, age, intervention)]
    b2[pop2<0, pop2:=0] #prevent negatives
    
    #all dead envelope
    b2[,all.mx2:=sum(dead2), by=.(sex, location, year, age, intervention)]
    b2[,all.mx2:=all.mx2+(pop2*BG.mx.all)+(pop2*covid.mx)] #UPDATE w/ covid data
    b2[all.mx2<0, all.mx:=0]
    
    #well
    b2[, well2:=pop2-all.mx2-sick2]
    b2[well2<0, well2:=0] #prevent negatives
    
    #re-combined into original data.table
    b2<-b2[year==2017+i & age2<96, c("age2", "newcases2", "sick2", "dead2", "well2", "pop2", 
                                     "all.mx2", "sex", "location", "cause", "intervention")]
    setnames(b2, "age2", "age")
    intervention_rates[year==2017+i & age>20, newcases:=b2[, newcases2]]
    intervention_rates[year==2017+i & age>20, sick:=b2[,sick2]]
    intervention_rates[year==2017+i & age>20, dead:=b2[,dead2]]
    intervention_rates[year==2017+i & age>20, well:=b2[,well2]]
    intervention_rates[year==2017+i & age>20, pop:=b2[,pop2]]
    intervention_rates[year==2017+i & age>20, all.mx:=b2[,all.mx2]]
    
  }
  
  out.df<-intervention_rates[, c("age", "cause", "sex", "year", "well", "sick", "newcases",
                                 "dead", "pop", "all.mx", "intervention", "location")]
  
  return(out.df)
  
  
}#as a fxn