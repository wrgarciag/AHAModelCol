
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

# Include the entire contry
Nx_c <-  Nx[,list(Nx=sum(population)),by=list(sex,gage_5)]
Nx_c[,region1:="00"]

Nx   <-  Nx[,list(Nx=sum(population)),by=list(region1,sex,gage_5)]
Nx <- rbind(Nx_c,Nx)
  
nmx <- readRDS(file = paste0(wd_dinpu,"DeathCounts1519.rds"))

# Include the entire contry
nmx_c <- nmx[,list(Mx=sum(deaths)),by=list(sex,gage_5,cause)]  
nmx_c[,region1:="00"]

nmx <- nmx[,list(Mx=sum(deaths)),by=list(region1,sex,gage_5,cause)]  

nmx <- rbind(nmx_c,nmx)
nmx <- merge(Nx,nmx,by=c('region1','sex','gage_5'),all.x = T,all.y = T)

nmx[,nmx:=Mx/Nx]
nmx[,MortalityRate:=Mx/Nx]

nmx <- nmx[!is.na(cause),]
nmx <- nmx[!sex=="N",]
# Spline smoothing of mortality rates
causes  <- unique(nmx$cause)
regions <- unique(nmx$region1)
sex     <- unique(nmx$sex)

int_mortality <- c()
  
for (ii in causes) {
  cat(ii,sep = "\n")
  for (jj in regions) {
    cat(jj,sep = "\n")
    for (ll in sex) {
      cat(ll,sep = "\n")
      
      # nmx_i <- nmx[cause==ii & region1==jj & sex==ll & gage_5>=20,]
      # 
      # # plot(nmx_i$gage_5, nmx_i$MortalityRate, type = "b", pch = 19, 
      # #      col = "red", xlab = "x", ylab = "y")
      # 
      # min_x <- min(nmx_i$gage_5,na.rm = T)
      # max_x <- max(nmx_i$gage_5,na.rm = T)
      # x <- nmx_i$gage_5
      # y <- nmx_i$MortalityRate
      # xout <- seq(min_x, max_x, by = 1)
      # 
      # int_val <- cubic_spline_interpolation(x, y, xout)
      # 
      # int_val <- as.data.table(cbind(jj,ll,ii,xout,int_val))
      # 
      # # plot(int_val$xout, int_val$int_val, type = "b", pch = 19, 
      # #      col = "red", xlab = "x", ylab = "y")
      # 
      # setnames(int_val,old = c("jj","ll","ii","xout","int_val"),new = c('region1','sex','cause','gage_5','MortalityRate'))
      # 
      # int_val[,MortalityRate:=as.numeric(MortalityRate)]
      
      # tryCatch({
      #   int_val <- interpolate_nmx_i(nmx[cause==ii & region1==jj & sex==ll & gage_5>=20,], ii, jj, ll)
      # }, error = function(e) {
      #   # Handle the error here (e.g., print a message)
      #   cat("Error in iteration", ii,jj,ll, ":", conditionMessage(e), "\n")
      #   int_val <- NA  # Assign a placeholder value for failed iterations
      #   next
      # })
      
      int_val <- try(interpolate_nmx_i(nmx[cause==ii & region1==jj & sex==ll & gage_5>=20,], ii, jj, ll))
      
      int_mortality <- rbind(int_mortality,int_val)
      
    }
  }
}



# # Life table
# 
# life_tables <- c()
# 
# for (ii in causes) {
#   cat(ii,sep = "\n")
#   for (jj in regions) {
#     cat(jj,sep = "\n")
#     for (ll in sex) {
#       cat(ll,sep = "\n")
#       nmx_bg <- nmx[cause==ii,]
#       nmx_bg <- merge(Nx,nmx_bg,all.x = T)
#       nmx_bg[,MortalityRate:=Mx/Nx]
#       
#       nmx_bg <- nmx_bg[region1==jj & sex==ll,]
#       nmx_bg <- nmx_bg[order(nmx_bg$gage_5),]
#       lt <- as.data.table(life.table(nmx_bg$MortalityRate))
#       lt[,gage_5:=x]
#       lt[,cause:=ii]
#       lt[,region1:=jj]
#       lt[,sex:=ll]
#       life_tables <- rbind(life_tables,lt)
#       
#     }
#   }
# }




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
