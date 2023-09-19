###.................
# 1. Demographic-----
###.................

###.................
## 1.1 DANE Population-----
###.................

dane_pop <- as.data.table(read_excel(paste0(wd_inpu,"DCD-area-sexo-edad-proyepoblacion-dep-2020-2050-ActPostCOVID-19.xlsx"), sheet = "PPODeptos",range = "A12:KU2091"))
dane_pop <- dane_pop[AREA=="Total",]

dane_pop <- melt(dane_pop,id.vars = c("DP","DPNOM","ANO","AREA"),value.name = "population")

dane_pop[,age:=as.numeric(sub('.*_', '', as.character(variable)))]
dane_pop[,sex:=sub('_.*', '', as.character(variable))]

dane_pop <- dane_pop[!(sex=="Total"),]

dane_pop[,sex:=ifelse(sex=="Hombres","Male","Female")]

dane_pop[,c('variable','DPNOM','AREA'):=NULL]

setnames(dane_pop,old = c("DP","ANO"),new = c("region1","year"))

saveRDS(dane_pop,file = paste0(wd_proc,"PopProjectionCol2050.rds"))

#Cleaning

rm(dane_pop)

###.................
## 1.2 nfx-----
###.................

###.................
## 1.2 nqx-----
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

###.................
# 3. Economic-----
###.................

###.................
## 3.1 Costs-----
###.................





