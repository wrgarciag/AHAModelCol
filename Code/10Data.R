###.................
# 1. Demographic-----
###.................

###.................
## 1.1 DANE Population-----
###.................

dane_pop <- as.data.table(read_excel(paste0(wd_dinpu,"DCD-area-sexo-edad-proyepoblacion-dep-2020-2050-ActPostCOVID-19.xlsx"), sheet = "PPODeptos",range = "A12:KU2091"))
dane_pop <- dane_pop[AREA=="Total",]

dane_pop <- melt(dane_pop,id.vars = c("DP","DPNOM","ANO","AREA"),value.name = "population")

dane_pop[,age:=as.numeric(sub('.*_', '', as.character(variable)))]
dane_pop[,sex:=sub('_.*', '', as.character(variable))]

dane_pop <- dane_pop[!(sex=="Total"),]

dane_pop[,sex:=ifelse(sex=="Hombres","Male","Female")]

dane_pop[,c('variable','DPNOM','AREA'):=NULL]

setnames(dane_pop,old = c("DP","ANO"),new = c("region1","year"))

saveRDS(dane_pop,file = paste0(wd_dproc,"PopProjectionCol2050.rds"))

#Cleaning

rm(dane_pop)

# Population counts for mortality rates

dane_pop <- as.data.table(read_excel(paste0(wd_dinpu,"DCD-area-sexo-edad-proypoblacion-dep-2005-2019.xlsx"), sheet = "Departamental_2005-2019",range = "A12:GX1497"))
dane_pop <- dane_pop[AREA=="Total",]

dane_pop <- melt(dane_pop,id.vars = c("DP","DPNOM","ANO","AREA"),value.name = "population")

dane_pop[,age:=as.numeric(sub('.*_', '', as.character(variable)))]
dane_pop[,sex:=sub('_.*', '', as.character(variable))]

dane_pop <- dane_pop[!(sex=="Total"),]

dane_pop[,sex:=ifelse(sex=="Hombres","Male","Female")]

dane_pop[,c('variable','DPNOM','AREA'):=NULL]

setnames(dane_pop,old = c("DP","ANO"),new = c("region1","year"))

dane_pop <- dane_pop[year>= 2015 & year<= 2019,]
dane_pop[,gage_5:=as.numeric(as.character(categorize_age(age,option = "5year")))]

saveRDS(dane_pop,file = paste0(wd_dinpu,"PopCounts1519.rds"))

#Cleaning
rm(dane_pop)


## 1.1 Un worldPopulation

wpp_pop <- as.data.table(read_excel(paste0(wd_dinpu,"WPP2022_POP_F01_2_POPULATION_SINGLE_AGE_MALE.xlsx"), sheet = "Medium variant",range = "A17:DH22536"))
wpp_pop <- wpp_pop[region=="Colombia",]
wpp_pop[,c("Index","Variant","Notes","Location_code","ISO3_Alpha_code","ISO2_Alpha_code","SDMX_code**","Type","Parent_code"):=NULL]
wpp_pop <- melt(wpp_pop,id.vars = c("region","Year"),value.name = "population")
wpp_pop[,age:=as.numeric(str_extract(variable, "([0-9]+)"))]
wpp_pop[,sex:="Male"]
wpp_pop[,c('variable'):=NULL]
setnames(wpp_pop,old = c("Year"),new = c("year"))

wpp_pop_m <- copy(wpp_pop)

wpp_pop <- as.data.table(read_excel(paste0(wd_dinpu,"WPP2022_POP_F01_3_POPULATION_SINGLE_AGE_FEMALE.xlsx"), sheet = "Medium variant",range = "A17:DH22536"))
wpp_pop <- wpp_pop[region=="Colombia",]
wpp_pop[,c("Index","Variant","Notes","Location_code","ISO3_Alpha_code","ISO2_Alpha_code","SDMX_code**","Type","Parent_code"):=NULL]
wpp_pop <- melt(wpp_pop,id.vars = c("region","Year"),value.name = "population")
wpp_pop[,age:=as.numeric(str_extract(variable, "([0-9]+)"))]
wpp_pop[,sex:="Female"]
wpp_pop[,c('variable'):=NULL]
setnames(wpp_pop,old = c("Year"),new = c("year"))

wpp_pop <- rbind(wpp_pop,wpp_pop_m)
wpp_pop[,population:=as.numeric(population)*1000]

saveRDS(wpp_pop,file = paste0(wd_dproc,"PopWppCol2050.rds"))


# Population counts and Pyramids

dane_pop <- readRDS(file = paste0(wd_dproc,"PopProjectionCol2050.rds"))
dane_pop[,gage_5:=as.numeric(as.character(categorize_age(age,option = "5year")))]
dane_pop <- dane_pop[,list(population=sum(population)),by=list(year,sex,gage_5)]
dane_pop[,source:="DANE"]

wpp_pop <- readRDS(file = paste0(wd_dproc,"PopWppCol2050.rds"))
wpp_pop[,gage_5:=as.numeric(as.character(categorize_age(age,option = "5year")))]
wpp_pop <- wpp_pop[,list(population=sum(as.numeric(population))),by=list(year,sex,gage_5)]
wpp_pop[,source:="WPP"]

model_pop <- rbind(dane_pop,wpp_pop)

model_pop <- model_pop[year %in% c(2024,2030,2050),]


# Line plot

model_pop_t <- model_pop[,list(population=sum(population)),by=list(year,sex,source)] 

p <- ggplot(model_pop_t, aes(x=year, y=round(population/1000,0), group=interaction(source,sex),color=sex,shape=source))
p <- p +  geom_point(size=3) + geom_line()
p <- p + labs(title="Forecasted population by source",x ="Year", y = "Population (Thousands)")
p

ggsave(filename = paste0(wd_r,"population_source_2050.png"),plot = last_plot(),width = 15, height = 10, units = "cm")

###.................
## 1.2 nfx-----
###.................

###.................
## 1.2 nqx-----
###.................

# Mortality counts 2015-2019

years <- 2015:2019

#Group causes
DxToMerge <- as.data.table(read_excel(paste0(wd_dinpu,"ICD10Codes.xlsx")))

dnmx <- c()
for (ii in years){
  cat(ii, sep = "\n")
  
  nmx <- fread(paste0(wd_dinpu,'nofetal',ii,'.csv'))
  
  nmx <- nmx[,c('COD_DPTO',"SEXO","GRU_ED1","C_BAS1"),with=FALSE]
  
  nmx[,sex:=ifelse(SEXO==1,'Male','Female')]
  nmx[!SEXO %in% 1:2,sex:='N']
  
  nmx[,C_BAS:=substr(C_BAS1,1,3)]
  
  nmx <- merge(nmx,DxToMerge,by.x=c('C_BAS'),by.y=c('Codes'),all.x = T)
  
  nmx[is.na(CVD),CVD:="BG"]
 
  nmx$gage<-NA
  nmx$gage[which(nmx$GRU_ED1<=6)]<-0
  nmx$gage[which(nmx$GRU_ED1==7 | nmx$GRU_ED1==8)]<-1
  nmx$gage[which(nmx$GRU_ED1==9)]<-5
  nmx$gage[which(nmx$GRU_ED1==10)]<-10
  nmx$gage[which(nmx$GRU_ED1==11)]<-15
  nmx$gage[which(nmx$GRU_ED1==12)]<-20
  nmx$gage[which(nmx$GRU_ED1==13)]<-25
  nmx$gage[which(nmx$GRU_ED1==14)]<-30
  nmx$gage[which(nmx$GRU_ED1==15)]<-35
  nmx$gage[which(nmx$GRU_ED1==16)]<-40
  nmx$gage[which(nmx$GRU_ED1==17)]<-45
  nmx$gage[which(nmx$GRU_ED1==18)]<-50
  nmx$gage[which(nmx$GRU_ED1==19)]<-55
  nmx$gage[which(nmx$GRU_ED1==20)]<-60
  nmx$gage[which(nmx$GRU_ED1==21)]<-65
  nmx$gage[which(nmx$GRU_ED1==22)]<-70
  nmx$gage[which(nmx$GRU_ED1==23)]<-75
  nmx$gage[which(nmx$GRU_ED1>=24)]<-80
  nmx$gage[which(nmx$GRU_ED1==29)]<-NA
  
  nmx <- nmx[,list(deaths=.N),by=list(COD_DPTO,sex,gage,CVD)]
  table(nmx$CVD)
  
  nmx[,year:=ii]
  setnames(nmx,c("COD_DPTO","CVD","gage"),c("region1","cause","gage_5"))
  
  nmx[,region1:=str_pad(region1, 2, pad = "0")]
  
  dnmx <- rbind(dnmx,nmx)
}

saveRDS(dnmx,file = paste0(wd_dinpu,"DeathCounts1519.rds"))

#Cleaning
rm(dnmx,nmx)


###.................
# 2. Epidemiologic-----
###.................

###.................
## 2.1 prevalence-----
###.................

prev_ihd <- as.data.table(read_excel(paste0(wd_inpu,"2EAcemiENT_IHD_Resu.xlsx"), sheet = "Costos_EdadSexo"))

# cf_ihd <- 0.001
# cf_hhd <- 0.0005
# cf_istr <- 0.00025
# cf_hstr <- 0.00025

###.................
## 2.2 incidence-----
###.................

###.................
# 3. Economic-----
###.................

###.................
## 3.1 Costs-----
###.................

cost_ihd  <- as.data.table(read_excel(paste0(wd_inpu,"2EAcemiENT_IHD_Resu.xlsx"), sheet = "Costos_EdadSexo"))
cost_ihd[,cause:="ihd"]
cost_hstr <- as.data.table(read_excel(paste0(wd_inpu,"2EAcemiENT_HSTR_Resu.xlsx"), sheet = "Costos_EdadSexo"))
cost_hstr[,cause:="hstroke"]
cost_istr <- as.data.table(read_excel(paste0(wd_inpu,"2EAcemiENT_ISTR_Resu.xlsx"), sheet = "Costos_EdadSexo"))
cost_istr[,cause:="istroke"]

cost_sys <- fread(paste0(wd_inpu,"2EAcemiENT_IHD_CostoSistemaUpc.csv"))
setnames(cost_sys,old=c("Pacientes","Valor","CostoPromedio"),new=c("N","Total","Promedio"))
cost_sys[,anio:=2019]
cost_sys[,cause:="all"]
cost_sys[,Sexo:="M"]

cost_sys.f <- copy(cost_sys)
cost_sys[,Sexo:="F"]
cost_sys <- rbind(cost_sys.f,cost_sys)
cost_iy_combined <- do.call(rbind, list(cost_ihd, cost_hstr, cost_istr))

cost_iy_combined <- rbind(cost_iy_combined,cost_sys,fill=T)

saveRDS(cost_iy_combined,file = paste0(wd_proc,"CostCombined2019.rds"))

#Plot

cost_iy_combined <- readRDS(file = paste0(wd_proc,"CostCombined2019.rds"))

exr <- 3277

cost_iy_combined[,PromedioUSD:=Promedio/exr]

p <- ggplot(data=cost_iy_combined[Sexo=='F'], aes(x=gedad_upc, y=PromedioUSD, fill=cause)) +
  geom_bar(stat="identity", position=position_dodge())
p <- p +labs(title="",
             x ="Age", y = "Cost (US Dollars No PPP adjusted)")
p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p

ggsave(filename = paste0(wd_r,"cost.iy.2019.female.png"),plot = last_plot(),width = 15, height = 10, units = "cm")

p <- ggplot(data=cost_iy_combined[Sexo=='M'], aes(x=gedad_upc, y=PromedioUSD, fill=cause)) +
  geom_bar(stat="identity", position=position_dodge())
p <- p +labs(title="",
             x ="Age", y = "Cost (US Dollars No PPP adjusted)")
p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p

ggsave(filename = paste0(wd_r,"cost.iy.2019.male.png"),plot = last_plot(),width = 15, height = 10, units = "cm")
