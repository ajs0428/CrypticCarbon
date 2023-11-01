library(terra)
library(sf)
library(tidyverse)
library(car)
library(ggplot2)
library(ggcorrplot)
library(RColorBrewer)

setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/')

######## Horizon data ##########

hoh_hor <- read.csv("SOIL CARBON/ANALYSIS/CrypticCarbonData_revised/CrypticCarbon_horizon.csv")
GEO <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_GEO_100k_reclassified.tif")
WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")
EVI <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_EVI_SUM_R.tif")
MNDWI <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_MNDWI_SUM_R.tif")
NDVI <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_spec_5yr_sea2.tif")["NDVI"] |> terra::project(WIP) |> terra::resample(WIP)


predstack <- c(WIP, EVI, MNDWI, NDVI)

hoh_stocks <- hoh_hor |>  dplyr::group_by(sample_ID) |> 
                            mutate(top = case_when(is.na(depth_cm - lag(depth_cm)) ~ 0,
                           .default = lag(depth_cm)), 
                           bottom = depth_cm, center = abs(top - (top - bottom)/2)) |>
                            mutate(thick100 = case_when(is.na(depth_cm - lag(depth_cm)) ~ bottom,
                                                        bottom > 100 & top < 100 ~ bottom - (bottom -100)-lag(bottom),
                                                        bottom > 100 & top > 100 ~ 0,
                                                        bottom < 100 & bottom - lag(bottom) < 0 ~ bottom,
                                                        bottom < 100 & bottom - lag(bottom) > 0 ~ bottom - lag(bottom),
                                                        bottom == 100 ~ bottom - lag(bottom),
                                                   .default = NA),
                                   SOC_hor_stock100 = ((carbon_perc/100)*BD_g_cm3*thick100*(1-(rock_perc/100))) ) |> 
                            mutate(thick30 = case_when(is.na(depth_cm - lag(depth_cm)) ~ bottom,
                                bottom > 30 & top <= 30 ~ bottom - (bottom -30)-lag(bottom),
                                bottom > 30 & top >= 30 ~ 0,
                                bottom < 30 & bottom - lag(bottom) < 0 ~ bottom,
                                bottom < 30 & bottom - lag(bottom) > 0 ~ bottom - lag(bottom),
                                bottom == 30 ~ bottom - lag(bottom),
                                .default = NA),
                                SOC_hor_stock30 = ((carbon_perc/100)*BD_g_cm3*thick30*(1-(rock_perc/100))) ) |>
                            mutate(thick60 = case_when(is.na(depth_cm - lag(depth_cm)) ~ bottom,
                                bottom > 60 & top <= 60 ~ bottom - (bottom -60)-lag(bottom),
                                bottom > 60 & top >= 60 ~ 0,
                                bottom < 60 & bottom - lag(bottom) < 0 ~ bottom,
                                bottom < 60 & bottom - lag(bottom) > 0 ~ bottom - lag(bottom),
                                bottom == 60 ~ bottom - lag(bottom),
                                .default = NA),
                                SOC_hor_stock60 = ((carbon_perc/100)*BD_g_cm3*thick60*(1-(rock_perc/100))) ) |>
                            mutate(thick120 = case_when(is.na(depth_cm - lag(depth_cm)) ~ bottom,
                               bottom > 120 & top <= 120 ~ bottom - (bottom -120)-lag(bottom),
                               bottom > 120 & top >= 120 ~ 0,
                               bottom < 120 & bottom - lag(bottom) < 0 ~ bottom,
                               bottom < 120 & bottom - lag(bottom) > 0 ~ bottom - lag(bottom),
                               bottom == 120 ~ bottom - lag(bottom),
                               .default = NA),
                                SOC_hor_stock120 = ((carbon_perc/100)*BD_g_cm3*thick120*(1-(rock_perc/100))) ) |>
                            mutate(thickfull = case_when(is.na(depth_cm - lag(depth_cm)) ~ bottom,
                               bottom > 999 & top <= 999 ~ bottom - (bottom -999)-lag(bottom),
                               bottom > 999 & top >= 999 ~ 0,
                               bottom < 999 & bottom - lag(bottom) < 0 ~ bottom,
                               bottom < 999 & bottom - lag(bottom) > 0 ~ bottom - lag(bottom),
                               bottom == 999 ~ bottom - lag(bottom),
                               .default = NA),
                            SOC_hor_stockfull = ((carbon_perc/100)*BD_g_cm3*thickfull*(1-(rock_perc/100))) ) |>
                            dplyr::summarise(SOC_stock_100 = sum(SOC_hor_stock100)*100,
                                             SOC_stock_30 = sum(SOC_hor_stock30)*100,
                                             SOC_stock_60 = sum(SOC_hor_stock60)*100,
                                             SOC_stock_120 = sum(SOC_hor_stock120)*100,
                                             SOC_stock_full = sum(SOC_hor_stockfull)*100,
                             #stock_check = mean(SOC120),
                             #WIP = mean(WIP),
                             maxdepth = max(depth_cm),
                             n = n(),
                             lat = mean(lat),
                             lon = mean(lon)
                             # .by = sample_ID
                             ) |>
    mutate(SOC_stock_0to30 = SOC_stock_30,
           SOC_stock_30to60 = SOC_stock_60 - SOC_stock_30,
           SOC_stock_60to100 = SOC_stock_100 - SOC_stock_60,
           SOC_stock_100to120 = SOC_stock_120 - SOC_stock_100) |> 
            as.data.frame()



hoh_stocks_vect <- hoh_stocks |> 
    terra::vect(geom = c("lon", "lat"), crs = "EPSG:4326", keepgeom = T) |>
    terra::project(crs(WIP)) 

hoh_stocks_ext <-  terra::extract(predstack, hoh_stocks_vect,  method = "bilinear", bind = T, ID = F) 
hoh_stocks_df <- terra::extract(GEO,hoh_stocks_ext,  method = "simple", bind = T, ID = F) |>
    dplyr::rename("LITHOL" = LITHOLOGY,"WIP" = WET) |>
    as.data.frame()
    

write.csv(hoh_stocks_df, "SOIL CARBON/ANALYSIS/CrypticCarbonData_revised/CrypticCarbon_Stocks_Full_Revised.csv")

