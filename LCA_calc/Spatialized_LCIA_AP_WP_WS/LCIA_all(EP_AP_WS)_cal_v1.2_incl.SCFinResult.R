library(sf)
library(stringr)

Eutro_mid <- st_read("ImpactWorld+/EutroFW_Mid/EutroFW_Midpoint_native.shp")    #0.5 * 0.5 degree spatial resolution
#summary(Eutro_mid)   #mean:PHOSPHATE 0.601, PHOSPHORUS 1.839
acid_Terr <- st_read("ImpactWorld+/AcidTerr_Mid/AcidTerr_Midpoint_native.shp")    #2 * 2.5 degree spatial resolution
#summary(acid_Terr)

#readin farm location
farm <- read.csv("Data/farm_loc.csv", header = T)
farm_long <- farm_lat <- list()
farm_loc <- list()
for (i in 1:nrow(farm)) {
  farm_loc[[i]] <- c(long=farm$Long[i], lat=farm$Lat[i])
}


###################EUTROFW########freshwater EU, it's P-limited, no CF for N-related chemicals, only coal burning cause the prob.#####################################
#speed-up, subset the global CF, to only the study region cellID
eutro_list <- c(seq(26109,26113),seq(25826,25831), seq(25540,25545), seq(25253,25258), seq(24966,24971))
Eutro_mid_sub <- Eutro_mid[eutro_list,]

#find the spatial Eutro_midCF for the specific farm location
find_EutroFW_midCF <- function(farm_loc) {
  for (i in 1:nrow(Eutro_mid_sub)) {
    if(farm_loc$long >= st_bbox(Eutro_mid_sub[i,]$geometry)[1] & farm_loc$long <= st_bbox(Eutro_mid_sub[i,]$geometry)[3]  
       & farm_loc$lat >= st_bbox(Eutro_mid_sub[i,]$geometry)[2] & farm_loc$lat <= st_bbox(Eutro_mid_sub[i,]$geometry)[4]){
      index = i    #index = i, not "i" as in global dataset
      #print(paste("cell_ID =", index))   #check if correct cell_ID for the farm
    }
  }
  EutroFW_midCF = list("BOD5, Biological Oxygen Demand"=Eutro_mid_sub[index,]$BOD, "COD, Chemical Oxygen Demand"=Eutro_mid_sub[index,]$COD, "Phosphate"=Eutro_mid_sub[index,]$PHOSPHATE, 
                       "Phosphoric acid"=Eutro_mid_sub[index,]$PHOSACID, "Phosphorus"=Eutro_mid_sub[index,]$PHOSPHORUS, "Phosphorus pentoxide"=Eutro_mid_sub[index,]$PHOSPENTOX)
  return(EutroFW_midCF)
}

#define upstream EUFW CF [kg PO4 P-lim eq], from "EutroFW_CF_original.xls"
EutroFW_CF_upstream = data.frame("BOD5, Biological Oxygen Demand" = 0.006992069, "COD, Chemical Oxygen Demand" = 0.006992069, "Phosphate" = 0.317821317,
                                 "Phosphoric acid" = 0.308286678,"Phosphorus" = 0.972533231,"Phosphorus pentoxide" = 0.425880565)
colnames(EutroFW_CF_upstream) = c("BOD5, Biological Oxygen Demand", "COD, Chemical Oxygen Demand", "Phosphate","Phosphoric acid","Phosphorus","Phosphorus pentoxide")


#calculation, upstream CF = EutroFW_CF_upstream, onsite using CF from equation = find_EutroFW_midCF()
cal_EutroFW_upstream <- function(EutroFW_upstreamCF,farm_lci) {
  EutroFWupelement1 <- EutroFWupelement2 <- EutroFWupelement3 <- EutroFWupelement4 <- EutroFWupelement5 <-EutroFWupelement6 <- list()
  for (i in 1:6) {
    assign(paste("EutroFWupelement", i, sep = ""), subset(farm_lci, farm_lci$name == names(EutroFW_upstreamCF)[i]))
  }
  EutroFW_i_up = EutroFW_upstream = 0
  for (i in 1:6) {
    EutroFW_i_up = sum(as.numeric(str_sub(get(paste("EutroFWupelement", i, sep = ""))$value, 2, -2)) * EutroFW_upstreamCF[[i]])
    EutroFW_upstream = EutroFW_upstream + EutroFW_i_up
  }
  return(EutroFW_upstream)
}

cal_EutroFW_onsite <- function(EutroFW_midCF, farm_lci) {
 EutroFWelement1 <- EutroFWelement2 <- EutroFWelement3 <- EutroFWelement4 <- EutroFWelement5 <-EutroFWelement6 <- list()
 for (i in 1:length(EutroFW_midCF)) {
   assign(paste("EutroFWelement", i, sep = ""), subset(farm_lci, farm_lci$name == names(EutroFW_midCF)[i]))
 }
 EutroFW_i = EutroFW_total = 0
 for (i in 1:length(EutroFW_midCF)) {
  #EutroFW_i = sum(as.numeric(str_extract(get(paste("EutroFWelement", i, sep = ""))$value, "\\-*\\d+\\.*\\d*")) * EutroFW_midCF[[i]])
  EutroFW_i = sum(as.numeric(str_sub(get(paste("EutroFWelement", i, sep = ""))$value, 2, -2)) * EutroFW_midCF[[i]])
  EutroFW_total = EutroFW_total + EutroFW_i
 }
 return(EutroFW_total)
}
######################################### END OF EUFW #################################################################



######################################### TerrAcid #################################################################
#speed-up, subset the global CF, to only the study region cellID  
acid_terr_list1 <- c(3000,3001, 3144, 3145)
AcidTerr_sub <- acid_Terr[acid_terr_list1,]

#find the spatial acidTerr_midCF for the specific farm location
find_acidTerr_midCF <- function(farm_loc) {
  for (i in 1:nrow(AcidTerr_sub)) {
    if(farm_loc$long >= st_bbox(AcidTerr_sub$geometry[i])[1] & farm_loc$long <= st_bbox(AcidTerr_sub$geometry[i])[3]  
       & farm_loc$lat >= st_bbox(AcidTerr_sub$geometry[i])[2] & farm_loc$lat <= st_bbox(AcidTerr_sub$geometry[i])[4]){
      index = i  #AcidTerr_sub[i,]$IDCELL
    }
  }
  acidTerr_midCF = list("Ammonia"=AcidTerr_sub[index,]$NH3, "Nitrogen oxides"=AcidTerr_sub[index,]$NOX, "Sulfur dioxide"=AcidTerr_sub[index,]$SO2)
  return(acidTerr_midCF)
}

#define upstream CF [kg SO2 eq], from "AcidTerr_CF_original.xls"
AcidTerr_CF_upstream = data.frame("Ammonia" = 0.003702277, "Nitrogen oxides" = 0.00098195, "Sulfur dioxide" = 0.002185744)
colnames(AcidTerr_CF_upstream) = c("Ammonia", "Nitrogen oxides", "Sulfur dioxide")

#calculation, upstream CF - AcidTerr_CF_upstream, onsite using CF from above equation = find_acidTerr_midCF()
cal_AcidTerr_upstream <- function(AcidTerr_upstreamCF,farm_lci) {
  acidupstream1 <- acidupstream2 <- acidupstream3 <- list()
  for (i in 1:3) {
    assign(paste("acidupstream", i, sep = ""), subset(farm_lci, farm_lci$name == names(AcidTerr_upstreamCF)[i]))
  }
  acidTerr_i_up = acidTerr_upstream = 0
  for (i in 1:3) {
    acidTerr_i_up = sum(as.numeric(str_sub(get(paste("acidupstream", i, sep = ""))$value, 2, -2)) * AcidTerr_upstreamCF[[i]])
    acidTerr_upstream = acidTerr_upstream + acidTerr_i_up
  }
  return(acidTerr_upstream)
}


cal_AcidTerr_onsite <- function(acidterr_midCF, farm_lci) {
  acidelement1 <- acidelement2 <- acidelement3 <- list()
  for (i in 1:length(acidterr_midCF)) {
    assign(paste("acidelement", i, sep = ""), subset(farm_lci, farm_lci$name == names(acidterr_midCF)[i]))
  }
  acidTerr_i = acidTerr_total_onsite = 0
  for (i in 1:length(acidterr_midCF)) {
    acidTerr_i = sum(as.numeric(str_sub(get(paste("acidelement", i, sep = ""))$value, 2, -2)) * acidterr_midCF[[i]])
    acidTerr_total_onsite = acidTerr_total_onsite + acidTerr_i
  }
  return(acidTerr_total_onsite)
}
######################################### END OF Acid #################################################################




######################################## Water Scarc ###############################################################
#calculation, upstream CF = 42.95353087m3 (see original .csv), onsite using CF from study region 
cal_waterScarc_upstream <- function(farm_lci) {
  farm_lci_water_up = subset(farm_lci, farm_lci$name == "Water, unspecified natural origin" | farm_lci$name == "Water, lake" | 
                               farm_lci$name == "Water, river" | farm_lci$name == "Water, well, in ground" |
                               farm_lci$name == "Water, cooling, unspecified natural origin" | farm_lci$name == "Water, turbine use, unspecified natural origin")
  waterscarc_upstream = sum(42.95353087 * as.numeric(str_sub(farm_lci_water_up$value, 2, -2)))
  return(waterscarc_upstream)
}

cal_waterScarc_onsite <- function(AGRIWaterScarc_midCF, farm_lci) {
  farm_lci_water = subset(farm_lci, farm_lci$name == "Water, unspecified natural origin" | farm_lci$name == "Water, lake" | 
                            farm_lci$name == "Water, river" | farm_lci$name == "Water, well, in ground")
  waterscarc_onsite = sum(as.numeric(str_sub(farm_lci_water$value, 2, -2)) * AGRIWaterScarc_midCF)  #m3 world-eq
  return(waterscarc_onsite)
}
######################################### END OF Water #################################################################




########################################## final calculattion for farm_x in forloop ############################################################
setwd("LCA_calc/ALLfarmLCI_and_GWPfossil_LCIA/LCI_fromBW2")
upstreamEU <- onsitedieselEU <- onsitecoalEU <-onsiteanimalEU <- upstreamAcid <- onsitedieselAcid <- onsitecoalAcid <- onsiteanimalAcid <- upstreamWater<-onsiteWater <- list()
for (farmno in 1:45){
 if(file.exists(paste("farm", farmno,"_upstreamprocess.csv", sep=""))){ 
  farm_lci_upstream = read.csv(paste("farm", farmno,"_upstreamprocess.csv", sep=""), header = T)
  farm_lci_onsite_diesel = read.csv(paste("farm", farmno,"_onsiteDiesel.csv", sep=""), header = T)   
  farm_lci_onsite_coal = read.csv(paste("farm", farmno,"_onsiteCoal.csv", sep=""), header = T)
  farm_lci_onsite_animalemi = read.csv(paste("farm", farmno,"_onsiteAnimalemi.csv", sep=""), header = T)
  farm_lci_onsite_resource = read.csv(paste("farm", farmno,"_onsiteResource.csv", sep=""), header = T)

  upstreamEU[farmno] = cal_EutroFW_upstream(EutroFW_CF_upstream, farm_lci_upstream)
  #onsite EU  farm1: Phosphate =  0.3749699, Phosphorus = 1.147408
  eufw = find_EutroFW_midCF(as.list(farm_loc[[farmno]]))  #first find out spaital CF for farm location, used as input for cal_EutroFW_onsite()
  onsitedieselEU[farmno] = cal_EutroFW_onsite(eufw, farm_lci_onsite_diesel)
  onsitecoalEU[farmno] =  cal_EutroFW_onsite(eufw, farm_lci_onsite_coal)
  onsiteanimalEU[farmno] =  cal_EutroFW_onsite(eufw, farm_lci_onsite_animalemi)
  EUFWdf = cbind(upstreamEU,onsitedieselEU,onsitecoalEU,onsiteanimalEU, eufw)
  
  #calculation Acid
  #upstream
  upstreamAcid[farmno] = cal_AcidTerr_upstream(AcidTerr_CF_upstream,farm_lci_upstream)     
  #onsite
  acidTerr_mid_CF_onsite = find_acidTerr_midCF(as.list(farm_loc[[farmno]]))
  onsitedieselAcid[farmno] = cal_AcidTerr_onsite(acidTerr_mid_CF_onsite, farm_lci_onsite_diesel)
  onsitecoalAcid[farmno] = cal_AcidTerr_onsite(acidTerr_mid_CF_onsite, farm_lci_onsite_coal)  
  onsiteanimalAcid[farmno] = cal_AcidTerr_onsite(acidTerr_mid_CF_onsite, farm_lci_onsite_animalemi)
  Aciddf = cbind(upstreamAcid,onsitedieselAcid,onsitecoalAcid,onsiteanimalAcid, acidTerr_mid_CF_onsite)
  
  #calculation Water
  #upstream
  upstreamWater[farmno] = cal_waterScarc_upstream(farm_lci_upstream)         #[m3 world-eq / m3 consumed]
  #onsite
  #BAS34S_ID 25628, BASIN0_ID	22944, default 2.240000 for study region
  onsiteWater[farmno] = cal_waterScarc_onsite(2.24000000000, farm_lci_onsite_resource)
  Wtdf= cbind(upstreamWater,onsiteWater)
  
  }
}

finalall = cbind(EUFWdf,Aciddf,Wtdf)
write.csv(finalall, "result.csv")