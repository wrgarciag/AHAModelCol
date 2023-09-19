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
