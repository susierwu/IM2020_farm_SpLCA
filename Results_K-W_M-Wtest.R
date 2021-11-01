library(ggpubr)
library(dplyr)
#setwd("D:/LCA_agri+spatial/SpLCA_paper/Code_published")
GWP = read.csv("Data/LCA_results/LCA_results_GWP_v4.csv", header = T)
lcia = read.csv("Data/LCA_results/LCA_results_ALL_v4.csv", header = T)
acid = read.csv("Data/LCA_results/LCA_results_Acid_v4.csv", header = T)

############################# define function to add spatial groups to LCA data  #############################
add_3Sp_group <- function(df, dfraw) {
for (i in 1:nrow(df)) {
  df$Sp_group[i] <- if (startsWith(dfraw$New_name[i], 'E')) {
    "East"
  } else if (startsWith(dfraw$New_name[i], 'S')) {
    "South"
  } else {
    "West"
  }
}
return(df) 
}

add_2Sp_group <- function(df, dfraw) {
  for (i in 1:nrow(df)) {
    df$Sp_group[i] <- if (startsWith(dfraw$New_name[i], 'E')) {
      "East"
    } else {
      "SouthWest"
    }
  }
  return(df)
}
############################# End of define function to add spatial groups to LCA data  #############################


#############################  Mann-Whitney U test on two Sp clusters (EAST vs. West&South farms) ##########################################
# to test high vs. a low emission cluster for farms located close to Hailar (NorthEast farms) compared to those located further south/west of the city (South/West farms) 
lcia_MW <- lcia
lcia_MW <- add_2Sp_group(lcia_MW, lcia)
#######################################################  Mann-Whitney U - on - GWP ######################################################
wilcox.test(GWPperRMB_total ~ Sp_group,  data = lcia_MW, exact = FALSE)         #p-value = 0.01968
wilcox.test(GWPperRMB_upstream ~ Sp_group, data = lcia_MW, exact = FALSE)       #p-value = 0.03353
wilcox.test(GWPperRMB_onsite ~ Sp_group, data = lcia_MW, exact = FALSE)         #p-value = 0.0185
#######################################################  Mann-Whitney U - on - EU ######################################################
wilcox.test(EUperRMB_total ~ Sp_group, data = lcia_MW,  exact = FALSE)        #p-value = 0.04684
wilcox.test(EUperRMB_upstream ~ Sp_group, data = lcia_MW,  exact = FALSE)     #p-value = 0.04684
wilcox.test(EUperRMB_onsite ~ Sp_group, data = lcia_MW,  exact = FALSE)       #p-value = 0.4389
#######################################################  Mann-Whitney U - on - Water ######################################################
wilcox.test(WTperRMB_total ~ Sp_group, data = lcia_MW,  exact = FALSE)     #p-value = 0.1209  
wilcox.test(WTperRMB_onsite ~ Sp_group, data = lcia_MW,  exact = FALSE)    #p-value = 0.04944
wilcox.test(WTperRMB_upstream ~ Sp_group, data = lcia_MW,  exact = FALSE)  #p-value = 0.2022
#######################################################  Mann-Whitney U - on - MJ ######################################################
wilcox.test(MJperRMB ~ Sp_group, data = lcia_MW,  exact = FALSE)               #p-value = 0.02988
#######################################################  Mann-Whitney U - on - AP ######################################################
acid_MW <- acid
acid_MW <- add_2Sp_group(acid_MW, acid)
wilcox.test(all_total_RMB ~ Sp_group, data = acid_MW,  exact = FALSE)          #p-value =  4.095e-05
wilcox.test(upstreamAcid_RMB ~ Sp_group, data = acid_MW,  exact = FALSE) #p-value = 0.1052
wilcox.test(onsite_total_RMB ~ Sp_group, data = acid_MW,  exact = FALSE) #p-value = 9.699e-06 (mainly from onsiteanimalAcid see below)
#wilcox.test(onsiteanimalAcid_RMB ~ Sp_group, data = acid_MW,  exact = FALSE)   #p-value = 3.446e-06
#wilcox.test(onsitedieselAcid_RMB ~ Sp_group, data = acid_MW,  exact = FALSE)   #p-value = 0.01968
############################# End of Mann-Whitney U test on two Sp clusters (EAST vs. West&South farms) ##########################################


############################### Kruskal-Wallis test on three clusters (EAST/WEST/SOUTH farms) ###########################################
lcia_KW <- lcia 
lcia_KW <- add_3Sp_group(lcia_KW , lcia)
#######################################################  Kruskal-Wallis - on - GWP/EP/WS/MJ ######################################################
kruskal.test(GWPperRMB_total ~ Sp_group, data = lcia_KW)     #p-value = 0.06377
kruskal.test(EUperRMB_total ~ Sp_group, data = lcia_KW)      #p-value = 0.1071
kruskal.test(WTperRMB_total ~ Sp_group, data = lcia_KW)      #p-value = 0.2444
kruskal.test(MJperRMB ~ Sp_group, data = lcia_KW)            #p-value = 0.03931

#######################################################  Kruskal-Wallis - on - AP ######################################################
acid_KW <- acid
acid_KW <- add_3Sp_group(acid_KW , acid)
kruskal.test(all_total_RMB ~ Sp_group, data = acid_KW)           #p-value = 0.0002115
############################### End of Kruskal-Wallis test on three clusters (EAST/WEST/SOUTH farms) ###########################################







############################### Boxplot with p-value labeled ###########################################
KW_pvalue <- function(yvalue, group, dt){
  p <- kruskal.test(yvalue ~ group, data = dt)$p.value
  sci_p <- formatC(p, format = "e", digits = 2)
  str_p = as.character(sci_p)
  return(str_p)
}

MW_pvalue <- function(yvalue, group, dt){
  p <- wilcox.test(yvalue ~ group, data = dt, exact = FALSE)$p.value
  sci_p <- formatC(p, format = "e", digits = 2)
  str_p = as.character(sci_p)
  return(str_p)
}


KW_bplot <- function(yvalue, group, dt, y_lb, main_lb){
  bp <- boxplot(yvalue ~ group, data=dt, ylab= y_lb, xlab="", main = main_lb)
  
  if (as.numeric(KW_pvalue(yvalue, group, dt)) < 0.01) {
    color = "red"
  } else if (as.numeric(KW_pvalue(yvalue, group, dt)) < 0.05) {
    color = "blue"
  } else {
    color = "gray48"
  }
  
  text(x=2.1, cex=1.3, y=max(yvalue)*0.97, labels= paste("K-W: p-value =", KW_pvalue(yvalue, group, dt)), col= color)
  
  return(bp)
}



MW_bplot <- function(yvalue, group, dt, y_lb, main_lb){
  bp <- boxplot(yvalue ~ group, data=dt, ylab= y_lb, xlab="Farm location", cex.lab=1.6, main = main_lb)
  if (as.numeric(KW_pvalue(yvalue, group, dt)) < 0.01) {
    color = "red"
  } else if (as.numeric(KW_pvalue(yvalue, group, dt)) < 0.05) {
    color = "blue"
  } else {
    color = "gray48"
  }
  text(x=1.7, cex=1.3, y=max(yvalue)*0.97, labels= paste("M-W: p-value =", MW_pvalue(yvalue, group, dt)), col= color)
  return(bp)
}



dev.off()
par(mfrow = c(2,5),bty='n', mai = c(0.6, 0.7, 0.4, 0.1),family = "serif", cex.lab=1.3, cex.axis=1.2, cex.main = 1.3)
KW_bplot(lcia_KW$GWPperRMB_total, lcia_KW$Sp_group, lcia_KW, expression(paste("GWP (kg CO"[2], " eq.)")), "GWP per FU")
KW_bplot(acid_KW$all_total_RMB, acid_KW$Sp_group, acid_KW, expression(paste("AP (kg SO"[2], " eq.)")), "AP per FU")
legend(3.3, 0.000002, legend = c("p-value<0.01", "p-value<0.05","p-value \u2265 0.05"), cex=1.4,bty = "n", text.col = c("red","blue","gray"),  xpd="NA")  
KW_bplot(lcia_KW$EUperRMB_total, lcia_KW$Sp_group, lcia_KW, expression(paste("EP (kg PO"[4], " P-lim eq.)")), "EP per FU")
KW_bplot(lcia_KW$WTperRMB_total, lcia_KW$Sp_group, lcia_KW,  expression(paste("WS (m"^"3", " world-eq./m"^"3"," consumed)")), "WS per FU")
KW_bplot(lcia_KW$MJperRMB, lcia_KW$Sp_group, lcia_KW,  "Fossil (MJ eq.)", "Fossil per FU")

MW_bplot(lcia_MW$GWPperRMB_total, lcia_MW$Sp_group, lcia_MW,  "", "")
MW_bplot(acid_MW$all_total_RMB, acid_MW$Sp_group, acid_MW,  "", "")
MW_bplot(lcia_MW$EUperRMB_total, lcia_MW$Sp_group, lcia_MW,  "", "")
MW_bplot(lcia_MW$WTperRMB_total, lcia_MW$Sp_group, lcia_MW,  "", "")
MW_bplot(lcia_MW$MJperRMB, lcia_MW$Sp_group, lcia_MW,  "", "")


# plot(1,1, main=expression('title'^2))  #superscript
# plot(1,1, main=expression('title'[2])) #subscript