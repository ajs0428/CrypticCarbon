library(terra)
library(sf)
library(rgdal)
library(leaflet)
library(tidyterra)
library(tidyverse)
library(tidyr)
library(tmap)
library(ggplot2)
library(basemaps)
library(maptiles)
library(parallel)
library(spatialEco)
library(landscapemetrics)

setwd("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/")
#setwd("/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/")
wd <- "/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/"
rev_path <- "/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/"

### hoh_poly ###
hoh_poly <- vect("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_POLYGON_7_11_2022/HOH_POLYGON_711.gpkg")

####Import WIP ####
hoh_WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")
plot(hoh_WIP)

#### Import MNDWI ####
hoh_MNDWI <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_MNDWI_SUM_R.tif")

#### Mask WIP with MNDWI ####
hoh_WIP_mask <- mask(hoh_WIP, (hoh_MNDWI>-0.30),  maskvalues = 1, updatevalue = NA)

########################################################################################################################
#### SOC map masking #### 
########################################################################################################################

#### Masking Function ####
mask_func <- function(MNDWI, CARBON, fileName){
    MNDWI_mask <- MNDWI > -0.30
    CARBON_0mask <- CARBON <0
    
    CARBON_0 <- mask(CARBON, CARBON_0mask, maskvalues = 1, updatevalue =0)
    CARBON_MNDWI <- mask(CARBON_0, MNDWI_mask, maskvalues = 1, updatevalue = NA)
    return(CARBON_MNDWI)
}


########################################################################################################################
####Can START HERE For latest masked carbon output ####
########################################################################################################################
## import carbon stock map ##
hoh_WIP_mask <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/NonCarbon/Hoh_WIP_MNDWI_masked.tif")
SOC_1M <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/CrypticCarbon_1M_SOC_MNDWI_masked.tif")
SOC_30CM <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/CrypticCarbon_30CM_SOC_MNDWI_masked.tif")

#histograms
quick_hist = function(values_vec, breaks=50) {
    res = hist(values_vec, plot=FALSE, breaks=breaks)
    
    dat = data.frame(xmin=head(res$breaks, -1L),
                     xmax=tail(res$breaks, -1L),
                     ymin=0.0,
                     ymax=res$counts)
    
    ggplot(dat, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
        geom_rect(size=0.5, colour="black", fill="grey80")
}
WIP_hist_df  <- (values(hoh_WIP_mask, na.rm = T))
WIP_hist_graph <- quick_hist(values_vec = WIP_hist) +
    scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale())) +
    theme(legend.position = 'right', 
          legend.key.size = unit(1.7, "cm"),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey80"),
          axis.ticks = element_blank(),
          text = element_text(size = 22))

#### Forest Cover ####

GEE_HOHTCCproj <- rast("AGB/HOH/GEE_hohTCC.tif_proj.tif")
plot(GEE_HOHTCCproj>= 50)
hist(GEE_HOHTCCproj)

for_mask <- function(SOC, forlay){
    SOC_rspl <- terra::resample(SOC, forlay)
    forested <- terra::mask(SOC_rspl, (forlay >=50), maskvalues = 0, updatevalue = NA,
                     filename = paste0("NWCA Data/",substr(deparse(substitute(forlay)), 1,3),deparse(substitute(SOC)), '.tif'), overwrite = T )
    return(forested)
    }

#### Uhran Harmonized Soil Carbon Stocks ####
#conus_deep_mean <- rast("NWCA Data/CONUS_Stock_Nov_20/CONUS_Deep_Stock_Mean.tif")
conus_full_mean <- rast("NWCA Data/CONUS_Stock_Nov_20/CONUS_Full_Stock_Mean.tif")
hoh_poly <- vect("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_POLYGON_7_11_2022/HOH_POLYGON_711.gpkg")

hoh_uhran <- rast("NWCA Data/hoh_harmonized_uhran.tif")/100
hoh_uhran_max <- rast("NWCA Data/hoh_harmonized_uhran_max.tif")/100
hoh_uhran_min <- rast("NWCA Data/hoh_harmonized_uhran_min.tif")/100
hoh_uhran_stdev <- hoh_uhran - hoh_uhran_min

for_hoh_uhran_mean <- rast("NWCA Data/GEEhoh_uhran.tif")#for_mask(hoh_uhran, GEE_HOHTCCproj)
for_hoh_uhran_max <- rast("NWCA Data/GEEhoh_uhran_max.tif")#for_mask(hoh_uhran_max, GEE_HOHTCCproj)
for_hoh_uhran_min<- rast("NWCA Data/GEEhoh_uhran_min.tif")#for_mask(hoh_uhran_min, GEE_HOHTCCproj)
for_hoh_uhran_stdev <- for_hoh_uhran_mean - for_hoh_uhran_min
#shallow Uhran
hoh_uhran_shalmean <- rast("NWCA Data/hoh_harmonized_uhran_shal_mean.tif")/100
hoh_uhran_shalmin <- rast("NWCA Data/hoh_harmonized_uhran_shal_min.tif")/100
hoh_uhran_shalmax <- rast("NWCA Data/hoh_harmonized_uhran_shal_max.tif")/100
hoh_uhran_shal_stdev <- hoh_uhran_shalmean - hoh_uhran_shalmin

for_hoh_uhran_shalmean <- rast("NWCA Data/GEEhoh_uhran_shalmean.tif")#for_mask(hoh_uhran_shalmean, GEE_HOHTCCproj)
for_hoh_uhran_shalmax <- rast("NWCA Data/GEEhoh_uhran_shalmax.tif")#for_mask(hoh_uhran_shalmax, GEE_HOHTCCproj)
for_hoh_uhran_shalmin <- rast("NWCA Data/GEEhoh_uhran_shalmin.tif")#for_mask(hoh_uhran_shalmin, GEE_HOHTCCproj)
for_hoh_uhran_shal_stdev <- for_hoh_uhran_shalmean - for_hoh_uhran_shalmin

writeRaster(hoh_uhran_shal_stdev, filename = paste0(rev_path, "NWCA_derived_FOR_30M_STDEV.tif"), overwrite = T)

#### Uhran/NWCA replacement in SoilGrids ####
hoh_uhran_rspl <- resample(hoh_uhran, hoh_sg)
hoh_uhran_sg <- terra::ifel(is.na(hoh_uhran_rspl), hoh_sg, hoh_uhran_rspl, filename = "SOIL CARBON/CrypticCarbonMaps/hoh_uhran_with_soilgrids.tif")

hoh_uhran_shalrspl <- resample(hoh_uhran_shalmean, hoh_sg)
hoh_uhran_sg_shal <- terra::ifel(is.na(hoh_uhran_shalrspl), hoh_sg, hoh_uhran_shalrspl, filename = "SOIL CARBON/CrypticCarbonMaps/hoh_uhran_shal_with_soilgrids.tif")
plot(hoh_uhran_sg_shal)


########################################################################################################################
#### Carbon masking statistics ####
########################################################################################################################

########################################################################################################################
#### The actual Carbon Function ####
########################################################################################################################
C_func <- function(carbon_masked, WIP, forestCover, path){
        #area 
        area_test <- WIP > -999 #placeholder for cell size all cells = 1
        x <- xres(carbon_masked) #hoh = 4
        y <- yres(carbon_masked) # hoh = 4
        resol <- (x*y)/10000 # Hoh = 0.0016
 
        
        total_area <- sum(values(cellSize(area_test, unit = "ha", mask = TRUE)), na.rm = T)#Total Area in 'ha' THIS NEEDS TO BE CONSISTENT
        C_mean <- mean(values(carbon_masked), na.rm = T) #mean value of all values of Mg/ha cells
        carbon_cell <- carbon_masked*cellSize(area_test, unit = "ha", mask = TRUE) # carbon in Mg per cell which is then added up 
        #There will be small numbers here because of Mg/ha
        TotalC_sum <- sum(values(carbon_cell), na.rm =T) #Total carbon in entire area

        
        #The WIP probability section masks
        WIP_upl_mask <- WIP < 0.5 #The wetlands and betweeen go to 0, uplands go to 1
        WIP_wet_mask <- WIP >= 0.5 #The wetlands go to 1, upland and between areas go to 0
        WIP_between <- (WIP<0.5 & WIP>0.25) #The area less than 0.5 and greater than 0.1 goes to 1
        
        
        #making maps of the upland, wetland, and between areas
        C_mask0_uplMg <- mask(carbon_masked, WIP_upl_mask, maskvalues = 0, updatevalue = NA, 
                              filename = paste0(path, ((substitute(carbon_masked))),"UPL_Mg", '.tif'),
                              overwrite = T)
        C_mask0_wetMg <- mask(carbon_masked, WIP_wet_mask, maskvalues = 0, updatevalue = NA, 
                              filename = paste0(path, ((substitute(carbon_masked))),"WET_Mg", '.tif'),
                              overwrite = T)
        C_mask0_betweenMg <- mask(carbon_masked, WIP_between, maskvalues = 0, updatevalue = NA, 
                                  filename = paste0(path, ((substitute(carbon_masked))),"BET_Mg", '.tif'),
                                  overwrite = T)
        
        #Taking the carbon amt in each cell (Mg) and masking by the WIP sections
        C_mask0_upl_cell <- mask(carbon_cell, WIP_upl_mask, maskvalues = 0, updatevalue = NA) # all wetlands masked out and upland carbon values are left
        C_mask0_wet_cell <- mask(carbon_cell, WIP_wet_mask, maskvalues = 0, updatevalue = NA) # all uplands are masked out and wetland carbon values are left
        C_mask0_between_cell <- mask(carbon_cell, WIP_between, maskvalues = 0, updatevalue = NA) #maybe should change to 1?
        
        #Summing up the total of the carbon amt in each cell from the WIP section masks
        C_upl <- global(C_mask0_upl_cell, fun ="sum", na.rm = T)[[1]]
        C_wet <- global(C_mask0_wet_cell, fun ="sum", na.rm = T)[[1]] 
        C_bet <- global(C_mask0_between_cell, fun ="sum", na.rm = T)[[1]]
        
        #number of cells in each
        # C_upl_count<- freq((WIP_upl_mask == 1), value = 1)[,3] #- min(cells(hoh_C_mask0_upl)) # This is the # of cells in UPL areas: 
        # C_wet_count <- freq((WIP_wet_mask == 1), value = 1)[,3]# - min(cells(hoh_C_mask0_wet)) # This is the sum of all Carbon in UPL areas:  
        # C_bet_count <- freq((WIP_between ==1), value = 1)[,3] #MASK VALUES =1
        
        
        upl_area <- sum(values(cellSize(C_mask0_upl_cell >-999, unit = "ha", mask = TRUE)), na.rm = T)#(C_upl_count*16)/10000 #62088.9 #<- expanse(hoh_C_mask0_upl, unit = "ha") # 62088.9ha
        wet_area <- sum(values(cellSize(C_mask0_wet_cell >-999, unit = "ha", mask = TRUE)), na.rm = T) #7417.701#<- expanse(hoh_C_mask0_wet, unit = "ha") #7417.701 ha
        bet_area <- sum(values(cellSize(C_mask0_between_cell >-999, unit = "ha", mask = TRUE)), na.rm = T)
        
        
        ## The forested wetland masking NEEDS WORK## 
        #Forest cover defined by the Cowardin/NWI is 30% for dominant overstory vegetation 
        forested <- forestCover >=50 #the forested areas are 1, nonforest are 0
        #wipmask <- mask(WIP, WIP_upl_mask, maskvalues= 1, updatevalue = NA)
        #fwmask <- mask(WIP_wet_mask, forested, maskvalues = 0, updatevalue = NA)
        #WIP_wet_fnf <- mask(WIP, forested, maskvalues = 0, updatevalue = NA) # The upland areas are 0 and are updated to NA giving WIP 
        C_mask0_wet_forestedMg <- mask(C_mask0_wetMg, forested, maskvalues = 0, updatevalue = NA,
                                       filename = paste0(path, (deparse(substitute(carbon_masked))),("FORWET"), '.tif'),
                                       overwrite = T)
        C_mask0_forestedWet_cell <- mask(C_mask0_wet_cell, forested, maskvalues = 0, updatevalue = NA)
        C_forW <- global(C_mask0_forestedWet_cell, fun ="sum", na.rm = T)[[1]]
        #C_forW_count <- freq((fwmask >0), value = 1)[,3]
        forW_area <- sum(values(cellSize(C_mask0_forestedWet_cell > -999, unit = "ha", mask = TRUE)), na.rm = T)
        
       
        #amount in uplands
        upl_amt <- C_upl
        #average amount in uplands?
        avg_upl <- mean(values(C_mask0_uplMg), na.rm = T)#(C_upl*(1/0.0016))/C_upl_count
        #amount in wetlands
        wet_amt <- C_wet 
        #average amount in wetlands
        avg_wet<- mean(values(C_mask0_wetMg, na.rm = T))#(C_wet*(1/0.0016))/C_wet_count 
        #amount in between
        bet_amt <- C_bet
        #average amount in between
        avg_bet <-  mean(values(C_mask0_betweenMg, na.rm = T))#(C_bet*(1/0.0016))/C_bet_count 
        #amount in forested wetlands
        forW_amt <- C_forW
        #average amount in forested wetlands
        avg_forW <- mean(values(C_mask0_wet_forestedMg, na.rm = T))#(C_forW*(1/0.0016))/C_forW_count
        forW_percC <- (C_forW)/TotalC_sum
        forW_percA <- forW_area/total_area
        
        
        #C_sum <- TotalC_sum/total_area
        
        wet_percC <- (C_wet)/TotalC_sum #C_wet_area[[1]]/C_area #This is the wet area carbon proportion of total carbon
        upl_percC <- C_upl/TotalC_sum # This is the upl area carbon proportion of the total carbon
        bet_percC <- (C_bet)/TotalC_sum # This is the between area carbon proportion of the total carbon
        
        wet_percA <- wet_area/total_area #This is the wetland area proportion of total
        upl_percA <- upl_area/total_area #This is the upland area proportion of total
        bet_percA <- bet_area/total_area #This is the upland area proportion of total
        
        
        cat("upland area =", upl_area, 
            "\nwet_area =", wet_area, 
            "\nforested wetland =", forW_area,
            "\nbetween area = ", bet_area, 
            "\nTotal Area =", total_area,
            "\nsum of parts = ", (upl_area + wet_area),
            "\namount in uplands Mg, " , upl_amt,
            "\namount in uplandTg= ",  upl_amt/1e6, 
            "\naverage amount in uplands Mg/ha= ", avg_upl,
            "\namount in wetlands Mg = ", wet_amt,
            "\n amount in wetlands Tg= ", wet_amt/1e6, 
            "\naverage amount in wetlands Mg/ha= ", avg_wet,
            "\namount in between Mg = ", bet_amt,
            "\namount in between Tg= ", bet_amt/1e6, 
            "\naverage amount in between Mg/ha= ", avg_bet,
            "\nThe total soil Carbon in Mg = ", TotalC_sum[[1]],
            "\nThe total soil Carbon in Tg= ",TotalC_sum[[1]]/1e6, 
            "\nThe overall average soil Carbon Mg/ha is= ", C_mean,
            "\nwetland proportion of total carbon= ", wet_percC[[1]],
            "\nwetland proportion of land area= ", wet_percA[[1]],
            "\nupland proportion of total carbon= ", upl_percC[[1]],
            "\nupland proportion of land area= ", upl_percA[[1]], 
            "\nbetween land proportion of total carbon= ", bet_percC[[1]],
            "\nbetween land proportion of land area= ", bet_percA[[1]],
            "\namount in forested wetlands Mg = ", forW_amt,
            "\namount in forested wetlands Tg= ", forW_amt/1e6,
            "\naverage amount in forested wetlands Mg/ha= ", avg_forW,
            "\nforested wetland proportion of total carbon= ", forW_percC[[1]],
            "\nforested wetland proportion of land area= ", forW_percA[[1]]
        )

    }



C_func(CC_1M_mask0, hoh_WIP_mask, GEE_HOHTCCproj, rev_path)
C_func(CC_30CM_mask0, hoh_WIP_mask, GEE_HOHTCCproj, rev_path)

#soilgrids
C_func(hoh_sg, hoh_WIP_mask, GEE_HOHTCCproj)
C_func(hoh_sg_uncmask, hoh_WIP_mask, GEE_HOHTCCproj)

#30cm
C_func(hoh_C_30cm, hoh_WIP_mask, GEE_HOHTCCproj)

####################################################################################################################################
#### Uncertainty masking statistics ####
####################################################################################################################################
#CC_5_95 <- rast("bootstrapped/intervals.tif")
CC_1M_95diff <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/CrypticCarbon_1M_SOC_95difference.tif")
CC_1M_97pt5 <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/CrypticCarbon_1M_SOC_97pt5_percentile.tif")
CC_1M_2pt5 <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/CrypticCarbon_1M_SOC_2pt5__percentile.tif")
CC_1M_sd <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/CrypticCarbon_1M_SOC_stdev.tif")

CC_30CM_95diff <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/CrypticCarbon_30CM_SOC_95difference.tif")
CC_30CM_97pt5 <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/CrypticCarbon_30CM_SOC_97pt5_percentile.tif")
CC_30CM_2pt5 <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/CrypticCarbon_30CM_SOC_2pt5_percentile.tif")
CC_30CM_sd <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/CrypticCarbon_30CM_SOC_stdev.tif")


C_func(CC_1M_95diff, hoh_WIP_mask, GEE_HOHTCCproj, rev_path)
C_func(CC_1M_97pt5, hoh_WIP_mask, GEE_HOHTCCproj, rev_path)
C_func(CC_1M_5th, hoh_WIP_mask, GEE_HOHTCCproj, rev_path)
C_func(CC_sd, hoh_WIP_mask, GEE_HOHTCCproj, rev_path)

C_func(CC_30CM_95diff, hoh_WIP_mask, GEE_HOHTCCproj, rev_path)
C_func(CC_30CM_97pt5, hoh_WIP_mask, GEE_HOHTCCproj, rev_path)
C_func(CC_30CM_2pt5, hoh_WIP_mask, GEE_HOHTCCproj, rev_path)
C_func(CC_30CM_sd, hoh_WIP_mask, GEE_HOHTCCproj, rev_path)

####This is WIP UPLAND Carbon  ####

upl_mask <- function(SOCmap, WIPmap, path){
    upl_only <- terra::mask(SOCmap, (WIPmap< 0.5), maskvalues = 0, updatevalue = NA, 
                            filename = paste0(path,gsub("_mask0", "", deparse(substitute(SOCmap))),"_UPL", ".tif"), overwrite = T)
    return(upl_only)
}
#1m WIP wetlands and uncertainty
CC_1M_UPL <- upl_mask(CC_1M_mask0, hoh_WIP_mask, rev_path)
CC_1M_UPL_97pt5 <- upl_mask(CC_1M_97pt5, hoh_WIP_mask, rev_path)
CC_1M_UPL_2pt5 <- upl_mask(CC_1M_2pt5, hoh_WIP_mask, rev_path)
CC_1M_UPL_95diff <- upl_mask(CC_1M_95diff, hoh_WIP_mask, rev_path)
CC_1M_UPL_sd <- upl_mask(CC_1M_sd, hoh_WIP_mask, rev_path)
#30cm WIP wetlands and uncertainty
CC_30CM_UPL <- upl_mask(CC_30CM_mask0, hoh_WIP_mask, rev_path)
CC_30CM_UPL_97pt5 <- upl_mask(CC_30CM_97pt5, hoh_WIP_mask, rev_path)
CC_30CM_UPL_2pt5 <- upl_mask(CC_30CM_2pt5, hoh_WIP_mask, rev_path)
CC_30CM_UPL_95diff <- upl_mask(CC_30CM_95diff, hoh_WIP_mask, rev_path)
CC_30CM_UPL_sd <- upl_mask(CC_30CM_sd, hoh_WIP_mask, rev_path)

####This is WIP wetland Carbon  ####

wet_mask <- function(SOCmap, WIPmap, path){
    wet_only <- terra::mask(SOCmap, (WIPmap>= 0.5), maskvalues = 0, updatevalue = NA, 
                            filename = paste0(path,gsub("_mask0", "", deparse(substitute(SOCmap))),"_WET", ".tif"), overwrite = T)
    return(wet_only)
}

#1m WIP wetlands and uncertainty
CC_1M_WET <- wet_mask(CC_1M_mask0, hoh_WIP_mask, rev_path)
CC_1M_WET_97pt5 <- wet_mask(CC_1M_97pt5, hoh_WIP_mask, rev_path)
CC_1M_WET_2pt5 <- wet_mask(CC_1M_2pt5, hoh_WIP_mask, rev_path)
CC_1M_WET_95diff <- wet_mask(CC_1M_95diff, hoh_WIP_mask, rev_path)
CC_1M_WET_sd <- wet_mask(CC_1M_sd, hoh_WIP_mask, rev_path)
#30cm WIP wetlands and uncertainty
CC_30CM_WET <- wet_mask(CC_30CM_mask0, hoh_WIP_mask, rev_path)
CC_30CM_WET_97pt5 <- wet_mask(CC_30CM_97pt5, hoh_WIP_mask, rev_path)
CC_30CM_WET_2pt5 <- wet_mask(CC_30CM_2pt5, hoh_WIP_mask, rev_path)
CC_30CM_WET_95diff <- wet_mask(CC_30CM_95diff, hoh_WIP_mask, rev_path)
CC_30CM_WET_sd <- wet_mask(CC_30CM_sd, hoh_WIP_mask, rev_path)

####Forested WIP Wetland SOC Map ####
# CC_1M_mask0_wet_forestedMg <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/SOC_1M_mask0forestedMg.tif")
# plot(CC_1M_mask0_wet_forestedMg, main = "Forested WIP Wetland SOC Map 1M")
# CC_30CM_mask0_wet_forestedMg <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/SOC_30CM_mask0forestedMg.tif")
# plot(CC_30CM_mask0_wet_forestedMg, main = "Forested WIP Wetland SOC Map 30CM")
# 
# #This is FORESTED WIP wetland Carbon uncertainty STDEV
# CC_1M_FOR_WET_stdev <- terra::mask(CC_1M_WET_stdev, GEE_HOHTCCproj>=50, maskvalues = 0, updatevalue = NA,  filename = "SOIL CARBON/CrypticCarbonMaps_Revised/CrypticCarbon_FOR_WET_1M_SOC_stdev.tif", overwrite = TRUE)
# #This is FORESTED WIP wetland Carbon 97.5% Percentile
# CC_1M_FOR_WET_97pt5 <- terra::mask(CC_1M_WET_97pt5, GEE_HOHTCCproj>=50, maskvalues = 0, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps_Revised/CrypticCarbon_FOR_WET_1M_SOC_97pt5th_percentile.tif", overwrite = T)
# plot(CC_1M_FOR_WET_97pt5)
# #This is FORESTED WIP wetland Carbon 5% Percentile
# CC_1M_FOR_WET_5th <- terra::mask(CC_1M_WET_5th, GEE_HOHTCCproj>=50, maskvalues = 0, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps_Revised/CrypticCarbon_FOR_WET_1M_SOC_5th_percentile.tif", overwrite = T)
# plot(CC_1M_FOR_WET_5th)

for_wet_mask <- function(SOCmap, FORmap, path){
    for_wet_only <- terra::mask(SOCmap, (FORmap >= 50), maskvalues = 0, updatevalue = NA, 
                            filename = paste0(path,substitute(SOCmap),"_FORWET", ".tif"), overwrite = T)
    return(for_wet_only)
}

CC_1M_FORWET <- for_wet_mask(CC_1M_WET, GEE_HOHTCCproj, rev_path)
CC_1M_FORWET_97pt5 <- for_wet_mask(CC_1M_WET_97pt5, GEE_HOHTCCproj, rev_path)
CC_1M_FORWET_2pt5 <- for_wet_mask(CC_1M_WET_2pt5, GEE_HOHTCCproj, rev_path)
CC_1M_FORWET_95diff <- for_wet_mask(CC_1M_WET_95diff, GEE_HOHTCCproj, rev_path)
CC_1M_FORWET_sd <- for_wet_mask(CC_1M_WET_sd, GEE_HOHTCCproj, rev_path)
 
CC_30CM_FORWET <- for_wet_mask(CC_30CM_WET, GEE_HOHTCCproj, rev_path)
CC_30CM_FORWET_97pt5 <- for_wet_mask(CC_30CM_WET_97pt5, GEE_HOHTCCproj, rev_path)
CC_30CM_FORWET_2pt5 <- for_wet_mask(CC_30CM_WET_2pt5, GEE_HOHTCCproj, rev_path)
CC_30CM_FORWET_95diff <- for_wet_mask(CC_30CM_WET_95diff, GEE_HOHTCCproj, rev_path)
CC_30CM_FORWET_sd <- for_wet_mask(CC_30CM_WET_sd, GEE_HOHTCCproj, rev_path)


####This is WIP wetland MINUS Uhran wetland Carbon  ####
uhran_mask <- rast("NWCA Data/uhran_mask.tif")
plot(uhran_mask, col = "black")


uhran_mask_func <- function(uhran_mask, SOC, forested_SOC, path){
    CC_WET_NoUhran <- terra::mask(SOC, uhran_mask, maskvalues = 1, updatevalue = NA, 
                                     filename = paste0(path,substitute(SOC),"_noUhran", ".tif"), overwrite = T)
    CC_WET_NoUhran_for <- terra::mask(forested_SOC, uhran_mask, maskvalues = 1, updatevalue = NA,
                                         filename =paste0(path,substitute(forested_SOC),"_noUhran", ".tif"), overwrite = T)

}

uhran_mask_func(uhran_mask, CC_1M_WET, CC_1M_FORWET, rev_path)
uhran_mask_func(uhran_mask, CC_1M_WET_97pt5, CC_1M_FORWET_97pt5, rev_path)
uhran_mask_func(uhran_mask, CC_1M_WET_2pt5, CC_1M_FORWET_2pt5, rev_path)
uhran_mask_func(uhran_mask, CC_1M_WET_95diff, CC_1M_FORWET_95diff, rev_path)
uhran_mask_func(uhran_mask, CC_1M_WET_sd, CC_1M_FORWET_sd, rev_path)

uhran_mask_func(uhran_mask, CC_30CM_WET, CC_30CM_FORWET, rev_path)
uhran_mask_func(uhran_mask, CC_30CM_WET_97pt5, CC_30CM_FORWET_97pt5, rev_path)
uhran_mask_func(uhran_mask, CC_30CM_WET_2pt5, CC_30CM_FORWET_2pt5, rev_path)
uhran_mask_func(uhran_mask, CC_30CM_WET_95diff, CC_30CM_FORWET_95diff, rev_path)
uhran_mask_func(uhran_mask, CC_30CM_WET_sd, CC_30CM_FORWET_sd, rev_path)

# #1m WIP Wetalnd SOC minus Uhran
# CC_1M_WET_NoUhran <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/CC_1M_WET_noUhran.tif")
# plot(CC_1M_WET_NoUhran, main = "1M No Uhran")
# #1m Forested WIP SOC minus Uhran
# CC_1M_WET_NoUhran_for <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/CC_1M_mask0_wet_forestedMg_noUhran.tif")
# plot(CC_1M_WET_NoUhran_for, main = "1m Forested WIP SOC minus Uhran")
# #30cm WIP Wetland SOC minus Uhran
# CC_30CM_WET_NoUhran <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/CC_30CM_WET_noUhran.tif")
# plot(CC_30CM_WET_NoUhran, main = "30CM No Uhran")
# #30m Forested WIP SOC minus Uhran
# CC_30M_WET_NoUhran_for <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/CC_30CM_mask0_wet_forestedMg_noUhran.tif")
# plot(CC_1M_WET_NoUhran_for, main = "30cm Forested WIP SOC minus Uhran")
# 
# #1m STDEV WIP Wetalnd SOC  minus Uhran
# CC_1M_WET_NoUhran_sd
# plot(CC_1M_WET_NoUhran_sd, main = "1M CC SD, no uhran")
# #1m STDEV forested WIP Wetland SOC minus Uhran
# CC_1M_WET_NoUhran_for_sd 
# plot(CC_1M_WET_NoUhran_for_sd, main = "1M Forested CC SD, no uhran")

#1m WIP Wetland SOC minus Uhran
CC_1M_WET_97pt5_noUhran <- rast(paste0(rev_path, substitute(CC_1M_WET_97pt5_noUhran), ".tif"))
CC_1M_FORWET_97pt5_noUhran <- rast(paste0(rev_path, substitute(CC_1M_FORWET_97pt5_noUhran), ".tif"))
CC_1M_WET_2pt5_noUhran <- rast(paste0(rev_path, substitute(CC_1M_WET_2pt5_noUhran), ".tif"))
CC_1M_FORWET_2pt5_noUhran <- rast(paste0(rev_path, substitute(CC_1M_FORWET_2pt5_noUhran), ".tif"))
CC_1M_WET_95diff_noUhran <- rast(paste0(rev_path, substitute(CC_1M_WET_95diff_noUhran), ".tif"))
CC_1M_FORWET_95diff_noUhran <- rast(paste0(rev_path, substitute(CC_1M_FORWET_95diff_noUhran), ".tif"))
CC_1M_WET_sd_noUhran <- rast(paste0(rev_path, substitute(CC_1M_WET_sd_noUhran), ".tif"))
CC_1M_FORWET_sd_noUhran <- rast(paste0(rev_path, substitute(CC_1M_FORWET_sd_noUhran), ".tif"))

#30cm WIP Wetland SOC minus Uhran
CC_30CM_WET_97pt5_noUhran <- rast(paste0(rev_path, substitute(CC_30CM_WET_97pt5_noUhran), ".tif"))
CC_30CM_FORWET_97pt5_noUhran <- rast(paste0(rev_path, substitute(CC_30CM_FORWET_97pt5_noUhran), ".tif"))
CC_30CM_WET_2pt5_noUhran <- rast(paste0(rev_path, substitute(CC_30CM_WET_2pt5_noUhran), ".tif"))
CC_30CM_FORWET_2pt5_noUhran <- rast(paste0(rev_path, substitute(CC_30CM_FORWET_2pt5_noUhran), ".tif"))
CC_30CM_WET_95diff_noUhran <- rast(paste0(rev_path, substitute(CC_30CM_WET_95diff_noUhran), ".tif"))
CC_30CM_FORWET_95diff_noUhran <- rast(paste0(rev_path, substitute(CC_30CM_FORWET_95diff_noUhran), ".tif"))
CC_30CM_WET_sd_noUhran <- rast(paste0(rev_path, substitute(CC_30CM_WET_sd_noUhran), ".tif"))
CC_30CM_FORWET_sd_noUhran <- rast(paste0(rev_path, substitute(CC_30CM_FORWET_sd_noUhran), ".tif"))

for_hoh_uhran_shalmean <- rast("NWCA Data/GEEhoh_uhran_shalmean.tif")


#### Take Lithology/Geology and mask Riverine and Non-Riverine wetlands for SOC ####
LITHOL <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/NonCarbon/Hoh_Surfical_Geology_MNDWI_masked.tif")

RIV_PAL_mask_func <- function(LITHOL, WETSOC, path){
    CC_WET_RIV <- terra::mask(WETSOC, (LITHOL== "alluvium_marine_water"), maskvalues = 0, updatevalue = NA, 
                                 filename = paste0(path,substitute(WETSOC),"_Riverine", ".tif"), overwrite = T)
    CC_WET_PAL <- terra::mask(WETSOC, (LITHOL== "alluvium_marine_water"), maskvalues = 1, updatevalue = NA, 
                                 filename = paste0(path,substitute(WETSOC),"_Palustrine", ".tif"), overwrite = T)
    return(list(paste0(path,substitute(WETSOC),"_Riverine", ".tif"), paste0(path,substitute(WETSOC),"_Palustrine", ".tif")))
}

CC_1M_WET_RIVPAL <- RIV_PAL_mask_func(LITHOL, CC_1M_WET, rev_path)
CC_1M_WET_97pt5_RIVPAL <- RIV_PAL_mask_func(LITHOL, CC_1M_WET_97pt5, rev_path)
CC_1M_WET_2pt5_RIVPAL <- RIV_PAL_mask_func(LITHOL, CC_1M_WET_2pt5, rev_path)
CC_1M_WET_95diff_RIVPAL <- RIV_PAL_mask_func(LITHOL, CC_1M_WET_95diff, rev_path)
CC_1M_WET_sd_RIVPAL <- RIV_PAL_mask_func(LITHOL, CC_1M_WET_sd, rev_path)

CC_30CM_WET_RIVPAL <- RIV_PAL_mask_func(LITHOL, CC_30CM_WET, rev_path)
CC_30CM_WET_97pt5_RIVPAL <- RIV_PAL_mask_func(LITHOL, CC_30CM_WET_97pt5, rev_path)
CC_30CM_WET_2pt5_RIVPAL <- RIV_PAL_mask_func(LITHOL, CC_30CM_WET_2pt5, rev_path)
CC_30CM_WET_95diff_RIVPAL <- RIV_PAL_mask_func(LITHOL, CC_30CM_WET_95diff, rev_path)
CC_30CM_WET_sd_RIVPAL <- RIV_PAL_mask_func(LITHOL, CC_30CM_WET_sd, rev_path)

#### This is the Riverine Wetland SOC 
CC_1M_RIV_WET <- rast(CC_1M_WET_RIVPAL[[1]])
CC_1M_PAL_WET <- rast(CC_1M_WET_RIVPAL[[2]])
#97.5% CI Riverine and Non Riverine Wetland SOC
CC_30CM_WET_97pt5_RIV <- rast(CC_30CM_WET_97pt5_RIVPAL[[1]])
CC_30CM_WET_97pt5_PAL <- rast(CC_30CM_WET_97pt5_RIVPAL[[2]])
#2.5% Riverine and Non Riverine Wetland SOC
CC_30CM_WET_2pt5_RIV <- CC_30CM_WET_2pt5_RIVPAL[[1]]
CC_30CM_WET_2pt5_PAL <- CC_30CM_WET_2pt5_RIVPAL[[2]]
#30cm 95diff Riverine and Palustrine/Non-riverine
CC_30CM_WET_95diff_RIV <- rast(CC_30CM_WET_95diff_RIVPAL[[1]])
CC_30CM_WET_95diff_PAL <- rast(CC_30CM_WET_95diff_RIVPAL[[2]])
#30cm STDEV Riverine and Palustrine/Non-riverine
CC_30CM_WET_sd_RIV <- rast(CC_30CM_WET_sd_RIVPAL[[1]])
CC_30CM_WET_sd_PAL <- rast(CC_30CM_WET_sd_RIVPAL[[2]])


##### GSOC Map #####################################
GSOC <- rast("SOIL CARBON/OTHER_DATA/GSOC/GSOCmap1.5.0.tif")

GSOC_func <- function(x, WIP){
    hoh_wip_prj <- terra::project(WIP, crs(x))
    GSOC_crop <- crop(x, hoh_wip_prj)
    GSOC_rpj <- terra::project(GSOC_crop, crs(WIP))
    GSOC_rspl <- terra::resample(GSOC_rpj, WIP)
    GSOC_mask <- mask(GSOC_rspl, WIP > -999)
    return(GSOC_mask)
}

GSOC_hoh <- GSOC_func(GSOC, hoh_WIP_mask)
plot(GSOC_hoh)
C_map_simp(GSOC_hoh)


#GSOC uncertainty
GSOCu <- rast("SOIL CARBON/OTHER_DATA/GSOC/GSOCseq_T0_UncertaintyMap030.tif")
GSOCu_hoh <- GSOC_func(GSOCu, hoh_WIP_mask)
plot(GSOCu_hoh)
C_map_simp(GSOCu_hoh)

writeRaster(GSOCu_hoh, "SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/hoh_GSOCuncert.tif", overwrite = T)

#### Soil Grids #### 
sg <- rast("SOIL CARBON/OTHER_DATA/soilgrids250m.tif") 
sg_unc <- rast("SOIL CARBON/OTHER_DATA/soilgrids_uncertainty.tif")
plot(sg)

sg_func <- function(x, WIP){
    sg_rpj <- terra::project(x, crs(WIP))
    sg_crop <- terra::crop(sg_rpj, WIP)
    sg_rsp <-  terra::resample(sg_crop, WIP)
    sg_mask <- terra::mask(sg_rsp, (WIP > -999)) 
    m <- c(2000, 32767, 0)
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    sg_rcl <- terra::classify(sg_mask, rclmat)
    return(sg_rcl)
}

hoh_sg <- sg_func(sg, hoh_WIP_mask)
hoh_sg_unc <- sg_func(sg_unc, hoh_WIP_mask)
plot(hoh_sg)
plot(hoh_sg_unc)
C_map_simp(hoh_sg)
C_map_simp(hoh_sg_unc)
writeRaster(hoh_sg, "SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/hoh_soilgrids.tif", overwrite = T)


########################################################################################################################
#####simple carbon sum calculator ####
########################################################################################################################


C_map_simp<- function(C_map){
    name <- deparse(substitute(C_map))
    gt <- (C_map > -999)
    area_tot <- sum(values(cellSize(gt, unit = "ha", mask = TRUE)), na.rm = T)
    C_mean <- mean(values(C_map), na.rm = T) #mean value of all values of Mg/ha cells
    carbon_cell <- C_map*cellSize(gt, unit = "ha") # carbon in Mg per cell which is then added up 
    #There will be small numbers here because of Mg/ha
    TotalC_sum <- sum(values(carbon_cell), na.rm =T)
    
    # cat("Total area =", area_tot, 
    #     "\nAverage Carbon Stock (Mg/ha) =", C_mean, 
    #     "\ntotal Carbon (Tg) =", TotalC_sum/1e6)
    
    return(data.frame("Name" = name, 
                      "Total_area" = area_tot, 
                      "AverageSOC_Mgha" = C_mean, 
                      "total_Carbon_Tg" = TotalC_sum/1e6,
                      stringsAsFactors = T))
}



CC_1M_WET_noUhran <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/CC_1M_WET_noUhran.tif")
NWCA_1M <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/NWCA_derived_1M_mean.tif") 
CC_30CM_WET_noUhran <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/CC_30CM_WET_noUhran.tif")
NWCA_30CM <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/NWCA_derived_30CM_mean.tif") 
cmb <- terra::merge(CC_1M_WET_noUhran, NWCA_1M)

C_map_simp(NWCA_30CM) 


all_files <- list.files(rev_path, full.names = T, include.dirs = F, recursive = F, all.files = T, no.. = T, pattern = "STDEV.tif")

names_all_files <- all_files |> dplyr::as_tibble() |> 
    mutate(across(everything(), ~gsub( pattern = "/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper//", replacement = "", .)),
           across(everything(), ~gsub( pattern = ".tif", replacement = "", .)),
           across(everything(), ~gsub( pattern = "_", replacement = " ", .)),
           across(everything(), ~gsub( pattern = " WET", replacement = " WETLAND", .)),
           across(everything(), ~gsub( pattern = " UPL", replacement = " UPLAND", .)),
           across(everything(), ~gsub( pattern = " FORWET", replacement = " FOREST WETLAND", .)),
           across(everything(), ~gsub( pattern = " noUhran", replacement = " NoNWCA", .)),
           across(everything(), ~gsub( pattern = "2pt5", replacement = "2.5%", .)),
           across(everything(), ~gsub( pattern = "97pt5", replacement = "97.5%", .)),
           across(everything(), ~gsub( pattern = "sd", replacement = "STDEV", .)))

list_results <- list()
system.time(for(i in 1:length(all_files)){
    temp_rast <- rast(all_files[[i]])
    temp_SOC <- C_map_simp(temp_rast)
    list_results[[i]] <- temp_SOC
})
results <- dplyr::bind_rows(list_results)
results_names <- cbind(names_all_files, results) 

results_table_stdev <- results_names |> dplyr::select(-Name) |> 
    mutate(depth = case_when(grepl(" 1M", value) == TRUE ~ "1M",
                             grepl(" 30CM", value) == TRUE ~ "30CM",
                             .default = NA),
           stat = case_when(grepl("2.5%", value) == TRUE ~ "2.5%",
                            grepl("97.5%", value) == TRUE ~ "97.5%",
                            grepl("STDEV", value, ignore.case = T) == TRUE ~ "STDEV",
                            grepl("min", value) == TRUE ~ "2.5%",
                            grepl("max", value) == TRUE ~ "97.5%",
                            .default = "Average"),
           landtype = case_when(grepl(" UPLAND", value) == TRUE ~ "UPLAND",
                                grepl(" FOREST", value) == TRUE ~ "FORESTED WETLAND",
                                grepl("FOR ", value) == TRUE ~ "FORESTED WETLAND",
                                grepl("Riverine", value, ignore.case = T) == TRUE ~ "Riverine",
                                grepl("Palustrine", value, ignore.case = T) == TRUE ~ "Palustrine",
                                grepl("CrypticCarbon", value) == TRUE ~ "Total Area",
                                .default = "WETLAND"),
           NWCA = case_when(grepl("NoNWCA", value) == TRUE ~ "NWCA Masked",
                             grepl("NWCA der", value) == TRUE ~ "NWCA Model",
                             .default = "WIP Model")
           )

write.csv(results_table, file = "/Users/Anthony/OneDrive - UW/University of Washington/Writing and Drafting/Cryptic Carbon/Submission/Revision/R_all_maps_SOC_table.csv", row.names = F)


