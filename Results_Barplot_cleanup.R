library(lattice)
library(sp)
library(ggplot2)
library(ggmap) 
library(tidyverse)
library(viridis)
library(extrafont)
GWP = read.csv("Data/LCA_results/LCA_results_GWP_v4.csv", header = T)
lcia = read.csv("Data/LCA_results/LCA_results_ALL_v4.csv", header = T)
acid = read.csv("Data/LCA_results/LCA_results_Acid_v4.csv", header = T)

#######################################################  GHG #################################################################
# Create gwp_by RMB dataset
gwprmbdata <- data.frame(
  #individual=lcia$X,
  individual=lcia$New_name,
  upstream=lcia$GWPperRMB_upstream,
  onsite=lcia$GWPperRMB_onsite
)
gwprmbdata <- gwprmbdata[order(gwprmbdata$individual,decreasing = F),] %>% gather(key = "process", value="value", -1) 

nObsType <- nlevels(as.factor(gwprmbdata$process))
gwprmbdata$id <- c(seq(1, nrow(gwprmbdata)/nObsType), seq(1, nrow(gwprmbdata)/nObsType))

label_data <- gwprmbdata %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

ggplot(gwprmbdata) +      
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=process), stat="identity", alpha=0.5) +
  scale_fill_manual(values=c("mediumturquoise", "violetred1")) + 
  #scale_fill_viridis(discrete=TRUE, option="cividis") +
  theme_minimal() +
  ylim(-0.5,max(label_data$tot, na.rm=T)+0.5) +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
     
  ) +
  coord_polar() +
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+0.05, label=individual, hjust=hjust), colour="black", fontface="bold",alpha=0.8,size=3, angle= label_data$angle, inherit.aes = F) 







####################################################### Acid #################################################################
#acid_by RMB
acidrmb <- data.frame(
  individual=acid$New_name,
  upstream=acid$upstreamAcid_RMB,
  onsite=acid$onsite_total_RMB
)
acidrmb <- acidrmb[order(acidrmb$individual,decreasing = F),] %>% gather(key = "process", value="value", -1) 

nObsType <- nlevels(as.factor(acidrmb$process))
acidrmb$id <- c(seq(1, nrow(acidrmb)/nObsType), seq(1, nrow(acidrmb)/nObsType))

label_data <- acidrmb %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

ggplot(acidrmb) +      
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=process), stat="identity", alpha=0.5) +
  scale_fill_manual(values=c("mediumturquoise", "violetred1")) + 
  theme_minimal() +
  ylim(-0.000006,max(label_data$tot, na.rm=T)+0.000001) +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
    #plot.margin = unit(rep(-1,20), "cm") 
  ) +
  coord_polar() +
  geom_text(data=label_data, aes(x=id, y=tot+0.000001, label=individual, hjust=hjust), colour="black", fontface="bold",alpha=0.8,size=3, angle= label_data$angle, inherit.aes = F) 




####################################################### water  #################################################################
wtrmbdata <- data.frame(
  individual=lcia$New_name,
  upstream=lcia$WTperRMB_upstream,
  onsite=lcia$WTperRMB_onsite
)
wtrmbdata <- wtrmbdata[order(wtrmbdata$individual,decreasing = F),] %>% gather(key = "process", value="value", -1) 
nObsType <- nlevels(as.factor(wtrmbdata$process))
wtrmbdata$id <- c(seq(1, nrow(wtrmbdata)/nObsType), seq(1, nrow(wtrmbdata)/nObsType))

label_data <- wtrmbdata %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

ggplot(wtrmbdata) +      
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=process), stat="identity", alpha=0.5) +
  scale_fill_manual(values=c("mediumturquoise", "violetred1")) + 
  theme_minimal() +
  ylim(-20,max(label_data$tot, na.rm=T)+5) +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
    #plot.margin = unit(rep(-1,20), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+3, label=individual, hjust=hjust), colour="black", fontface="bold",alpha=0.8,size=3, angle= label_data$angle, inherit.aes = F) 





#######################################################  Eutro #################################################################
# Create eu_bylv dataset, replace NA onsite EU by 0
lcia$EUperRMB_onsite[is.na(lcia$EUperRMB_onsite)] <- 0
eurmbdata <- data.frame(
  individual=lcia$New_name,
  upstream=lcia$EUperRMB_upstream,
  onsite=lcia$EUperRMB_onsite
)
eurmbdata <- eurmbdata[order(eurmbdata$individual,decreasing = F),]  %>% gather(key = "process", value="value", -1) 
nObsType <- nlevels(as.factor(eurmbdata$process))
eurmbdata$id <- c(seq(1, nrow(eurmbdata)/nObsType), seq(1, nrow(eurmbdata)/nObsType))

# Get the name and the y position of each label
label_data <- eurmbdata %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

ggplot(eurmbdata) +      
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=process), stat="identity", alpha=0.5) +
  scale_fill_manual(values=c("mediumturquoise", "violetred1")) + 
  theme_minimal() +
  ylim(-0.0003,max(label_data$tot, na.rm=T)+0.0001) +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
    #plot.margin = unit(rep(-1,20), "cm") 
  ) +
  coord_polar() +
  #ggtitle("Freshwater eutrophication potential (EP)") +
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+0.00005, label=individual, hjust=hjust), colour="black", fontface="bold",alpha=0.8,size=3, angle= label_data$angle, inherit.aes = F) 



########################## boxplot GWP and acid  ###############################################################
gwp_boxplot_rmb <- cbind(lcia$GWPperRMB_total,lcia$GWPperRMB_upstream, lcia$GWPperRMB_onsite)
colnames(gwp_boxplot_rmb) <- c("Total", "Upstream", "Onsite")
gwp_boxplot_onsite_rmb <- cbind(GWP$onsitedieselGWP_RMB, GWP$onsitecoalGWP_RMB, GWP$onsiteanimalGWP_RMB)
acid_boxplot_rmb <- cbind(acid$all_total_RMB,acid$upstreamAcid_RMB, acid$onsite_total_RMB)
colnames(acid_boxplot_rmb) <- c("Total", "Upstream", "Onsite")
acid_boxplot_onsite_rmb <- cbind(acid$onsitedieselAcid_RMB, acid$onsitecoalAcid_RMB, acid$onsiteanimalAcid_RMB)
colnames(acid_boxplot_onsite_rmb) <- colnames(gwp_boxplot_onsite_rmb) <-c("Diesel", "Coal", "Animal")
wt_boxplot_total_lv <- cbind(lcia$WTperLV_total, lcia$WTperLV_upstream, lcia$WTperLV_onsite)
wt_boxplot_total_rmb <- cbind(lcia$WTperRMB_total, lcia$WTperRMB_upstream, lcia$WTperRMB_onsite)
eu_boxplot_total_lv <- cbind(lcia$EUperLV_total, lcia$EUperLV_upstream, lcia$EUperLV_onsite)
eu_boxplot_total_rmb <- cbind(lcia$EUperRMB_total, lcia$EUperRMB_upstream, lcia$EUperRMB_onsite)
colnames(wt_boxplot_total_lv) <- colnames(wt_boxplot_total_rmb) <- colnames(eu_boxplot_total_lv) <- colnames(eu_boxplot_total_rmb) <- c("Total", "Upstream", "Onsite")
fossil_boxplot_lvandrmb <- cbind(lcia$MJperLV,lcia$MJperRMB)
colnames(fossil_boxplot_lvandrmb) <- c("Total_by LV", "Total_by RMB")


dev.off()
par(mfrow = c(2,3),bty='n', mai = c(0.6, 0.7, 0.2, 0.1),family = "serif", cex.lab=1.2, cex.axis=1.4, cex.main = 1.5)
boxplot(gwp_boxplot_rmb, ylab= "GWP (kg CO2 eq.)", main = "a. GWP by FU (per RMB income)")
boxplot(acid_boxplot_rmb, ylab= "AP (kg SO2 eq.)", main = "b. AP by FU (per RMB income)")
boxplot(eu_boxplot_total_rmb, ylab= "EP (kg PO4 P-lim eq.)", main = "c. EP by FU (per RMB income)")
boxplot(gwp_boxplot_onsite_rmb, ylab= "GWP (kg CO2 eq.)", main = "a.1. Onsite GWP breakdown")
boxplot(acid_boxplot_onsite_rmb,ylab= "AP (kg SO2 eq.)", main = "b.1. Onsite AP breakdown")
boxplot(wt_boxplot_total_rmb, ylab= "WS (m3 world-eq./m3 consumed)", main = "d. WS by FU (per RMB income)")