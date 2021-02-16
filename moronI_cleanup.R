library(lattice)
library(sp)
library(ggplot2)
library(ggmap) 
library(spdep)
library(gstat)
library(ncf)

GWP = read.csv("Data/LCA_results/LCA_results_GWP_v4.csv", header = T)
acid = read.csv("Data/LCA_results/LCA_results_Acid_v4.csv", header = T)
lcia = read.csv("Data/LCA_results/LCA_results_ALL_v4.csv", header = T)
fm_lci = read.csv("Data/LCA_results/Farm_rawlci_v4.csv", header = T)

######################################### GWP ################################################################
coordinates(GWP) <- c("x", "y") 
coords <- coordinates(GWP)
gwp.nb <- tri2nb(coords, row.names=NULL)
#moran.test(GWP$upstreamGWP_RMB, nb2listw(gwp.nb, style="B"))
moran.test(GWP$upstreamGWP_RMB, nb2listw(gwp.nb))   #p-value = 0.004445
moran.test(GWP$onsite_total_RMB, nb2listw(gwp.nb))  #p-value = 0.05413
moran.test(GWP$all_total_RMB, nb2listw(gwp.nb))     #p-value = 0.02455

#onsite breakdown, only weak spatial autocorrelation for coal 
moran.test(GWP$onsitedieselGWP_RMB, nb2listw(gwp.nb))
moran.test(GWP$onsitecoalGWP_RMB, nb2listw(gwp.nb)) #p-value = 0.01668
moran.test(GWP$onsiteanimalGWP_RMB, nb2listw(gwp.nb))

######################################### acid ################################################################
coordinates(acid) <- c("x", "y") 
coords <- coordinates(acid)
acid.nb <- tri2nb(coords, row.names=NULL)
moran.test(acid$upstreamAcid_RMB, nb2listw(acid.nb))   #p-value = 0.005892
moran.test(acid$onsite_total_RMB, nb2listw(acid.nb))   #p-value = 3.294e-09
moran.test(acid$all_total_RMB, nb2listw(acid.nb))      #p-value = 2.394e-06

#onsite breakdown, diesel no spatial at all, strongest spatial pattern from animal emissions, due to SCF (as inventory no spatial)
moran.test(acid$onsitedieselAcid_RMB, nb2listw(acid.nb))  
moran.test(acid$onsitecoalAcid_RMB, nb2listw(acid.nb))   #p-value = 0.005984
moran.test(acid$onsiteanimalAcid_RMB, nb2listw(acid.nb)) #p-value = 4.263e-11  

######################################### Eutro ################################################################
eutro <- lcia[, c(1,3,4,5,6,21,22,23)]
coordinates(eutro) <- c("x", "y") 
coords <- coordinates(eutro)
eutro.nb <- tri2nb(coords, row.names=NULL)
moran.test(eutro$EUperRMB_upstream, nb2listw(eutro.nb))                    #p-value = 0.006082
moran.test(eutro$EUperRMB_onsite, na.action=na.pass, nb2listw(eutro.nb))   #p-value = 0.006302, na.pass: zero is substituted for NA 
moran.test(eutro$EUperRMB_onsite, na.action=na.omit, nb2listw(eutro.nb))   #p-value = 0.0009248, na.omit: remove NA 
moran.test(eutro$EUperRMB_total, nb2listw(eutro.nb))                       #p-value = 0.006082

######################################### WS ################################################################
ws <- lcia[, c(1,3,4,5,6,30,31,32)]
coordinates(ws) <- c("x", "y") 
coords <- coordinates(ws)
ws.nb <- tri2nb(coords, row.names=NULL)
moran.test(ws$WTperRMB_upstream, nb2listw(ws.nb))                 #p-value = 0.008721
moran.test(ws$WTperRMB_onsite, nb2listw(ws.nb))                   #p-value = 0.03616
moran.test(ws$WTperRMB_total, nb2listw(ws.nb))                    #p-value = 0.01112

######################################### MJ ################################################################
mj <-  lcia[,c(1,3,4,5,6,18,19)]
coordinates(mj) <- c("x", "y") 
coords <- coordinates(mj)
mj.nb <- tri2nb(coords, row.names=NULL)

moran.test(mj$MJperRMB, nb2listw(mj.nb))        #p-value = 0.002564
moran.test(mj$MJperLV, nb2listw(mj.nb))         #p-value = 0.00969



######################################### other ################################################################
ammonia_RMB <- fm_lci$Ammonia/(lcia$Income * 10000)
methane_RMB <- fm_lci$Methane/(lcia$Income * 10000)
diesel_RMB <- fm_lci$diesel_consump/(lcia$Income * 10000)
coal_RMB <- fm_lci$coal_consump/(lcia$Income * 10000)
elec_RMB <- fm_lci$elec_consump/(lcia$Income * 10000)
hay_prod_RMB <- fm_lci$hay_prod_ton*1000/(lcia$Income * 10000)
hay_purch_RMB <- fm_lci$feed1_purch_ton*1000/(lcia$Income * 10000)
fm_lci.nb <- tri2nb(coords, row.names=NULL)


#can not reject the null hypothesis that there is zero spatial autocorrelation present in the diesel consumption and animal emissions 
moran.test(ammonia_RMB, nb2listw(fm_lci.nb))    
moran.test(methane_RMB, nb2listw(fm_lci.nb))   
moran.test(diesel_RMB, nb2listw(fm_lci.nb))   
moran.test(coal_RMB, nb2listw(fm_lci.nb))       #p-value = 0.02955
moran.test(elec_RMB, nb2listw(fm_lci.nb))       #p-value = 0.02471
moran.test(hay_prod_RMB, nb2listw(fm_lci.nb))   #p-value = 0.04744  
moran.test(hay_purch_RMB, nb2listw(fm_lci.nb))  #p-value = 0.02753  
