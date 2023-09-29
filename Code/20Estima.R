
###.................
# 1. Demographic-----
###.................

###.................
## 1.1 Base population-----
###.................


###.................
## 1.1 nfx-----
###.................

###.................
## 1.2 nqx-----
###.................

Nx <- as.data.table(readRDS(file = paste0(wd_dinpu,"PopCounts1519.rds")))
Nx <-  Nx[,list(Nx=sum(population)),by=list(region1,sex,gage_5)]

nmx <- readRDS(file = paste0(wd_dinpu,"DeathCounts1519.rds"))
nmx <- nmx[,list(Mx=sum(deaths)),by=list(region1,sex,gage_5,cause)]  
nmx_bg <- merge(Nx,nmx_bg,all.x = T)
causes  <- unique(nmx$cause)
regions <- unique(nmx$region1)
sex     <- unique(nmx$sex)

life_tables <- c()

for (ii in causes) {
  cat(ii,sep = "\n")
  for (jj in regions) {
    cat(jj,sep = "\n")
    for (ll in sex) {
      cat(ll,sep = "\n")
      nmx_bg <- nmx[cause==ii,]
      nmx_bg <- merge(Nx,nmx_bg,all.x = T)
      nmx_bg[,MortalityRate:=Mx/Nx]
      
      nmx_bg <- nmx_bg[region1==jj & sex==ll,]
      nmx_bg <- nmx_bg[order(nmx_bg$gage_5),]
      lt <- as.data.table(life.table(nmx_bg$MortalityRate))
      lt[,gage_5:=x]
      lt[,cause:=ii]
      lt[,region1:=jj]
      lt[,sex:=ll]
      life_tables <- rbind(life_tables,lt)
      
    }
  }
}




#CFc is the cause-specific transition probability 
#from sick to dead from cause c 
#BGmx,t is the cause-specific transition from sick to dead from any non-c cause 









###.................
## 1.3 Leslie-----
###.................


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

###.................
# 3. Economic-----
###.................

###.................
## 3.1 Costs-----
###.................
