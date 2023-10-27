library(terra)
library(sf)
library(tidyverse)
setwd("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/")
#setwd("/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/")

########################################################################################################################################################################################################################
#### Let's get wetlands as objects ####
########################################################################################################################################################################################################################
hoh_WIP_mask <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")

HRW_CC_1M<- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/CrypticCarbon_1M_SOC_MNDWI_masked.tif")
HRW_CC_1M_2pt5<- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/CrypticCarbon_1M_SOC_2pt5__percentile.tif")
HRW_CC_1M_97pt5<- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/CrypticCarbon_1M_SOC_97pt5_percentile.tif")
HRW_CC_1M_stdev<- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/CrypticCarbon_1M_SOC_stdev.tif")


m <- c(0, 0.5, 1,
       0.5, 1, 2)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
# WIP either Wetland or not
hoh_WIP_bin <- classify(hoh_WIP_mask, rclmat, include.lowest = T, right = F, filename = "SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/SizeDistribution/hoh_WIP_binary.tif", overwrite = T)
hoh_WIP_bin <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/SizeDistribution/hoh_WIP_binary.tif")
plot(hoh_WIP_bin)

#### Make binary WIP raster into polygons ####
get_polys <- function(raster){
    vect_polys <- as.polygons(raster)
    sfpolys <- st_as_sf(vect_polys)
    sfcast <- sfpolys |> st_cast("POLYGON") 
    sfcast <- sfcast |>
        mutate(area = as.numeric(st_area(sfcast)),
               area_ha = area*0.0001)
    final_polys <- vect(sfcast)
}
hoh_WIP_bin_sfcastvect <- get_polys(hoh_WIP_bin)
#plot(hoh_WIP_bin_sfcastvect) #takes a while to plot

# binary only wetlands
hoh_WIP_bin_polys_tib <-  hoh_WIP_bin_sfcastvect |> as_tibble() |>
    filter(WET == 2)

#Need to make a cutoff value and find actual quantiles for patch sizes
(hoh_WIP_bin_polys_hist <- hoh_WIP_bin_polys_tib |>
    # Target map unit for NWI is usually 1 acre = 4047m2 but they can map below 
    filter(WET == 2, area >= 64) |>
    dplyr::reframe(quantile = scales::percent(c(0.01, 0.25, 0.5, 0.75, 0.80, 0.99)),
                     areaper = quantile(area, c(0.01, 0.25, 0.5, 0.75, 0.80, 0.99))))
length(hoh_WIP_bin_polys_tib$WET)
hist(hoh_WIP_bin_polys_tib$area, breaks = 100000)

# back to polygons - take out the ones that are less 2x2pix (8x8m = 64m**2)
#probably want this in 
#### Function for size filtering
sizefilter <- function(polygons){
    
    polygons_filtered <- polygons |> tidyterra::filter(if_any(1) == 2,
                               if_any(2) >= 64) #|>#all wetlands above 64 m2, 64m2
    
    writeVector(polygons_filtered, filename = paste0("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/SizeDistribution/", substitute(polygons),
                                                                   sub(".*vect","",substitute(polygons)), "", 
                                                                   '.gpkg'), overwrite = T)
    return(polygons_filtered)
}

hoh_WIP_bin_polys_ab64 <- sizefilter(hoh_WIP_bin_sfcastvect)
hist(hoh_WIP_bin_polys_ab64$area, breaks = 1000)

#### WIP above 64 SOC 1M ####
CC_WIPbin_mask64 <- mask(HRW_CC_1M, hoh_WIP_bin_polys_ab64, touches = F,filename = "SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/SizeDistribution/CrypticCarbon1M_WIP_bin_ab64_mask.tif", overwrite = T)
CC_WIPbin_mask64 <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/SizeDistribution/CrypticCarbon1M_WIP_bin_ab64_mask.tif")
plot(CC_WIPbin_mask64)
CC_WIPbin_mask64_2pt5 <- mask(HRW_CC_1M_2pt5, hoh_WIP_bin_polys_ab64, touches = F,filename = "SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/SizeDistribution/CrypticCarbon1M_WIP_bin_ab64_mask_2pt5.tif", overwrite = T)
CC_WIPbin_mask64_97pt5 <- mask(HRW_CC_1M_97pt5, hoh_WIP_bin_polys_ab64, touches = F,filename = "SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/SizeDistribution/CrypticCarbon1M_WIP_bin_ab64_mask_97pt5.tif", overwrite = T)
CC_WIPbin_mask64_stdev <- mask(HRW_CC_1M_stdev, hoh_WIP_bin_polys_ab64, touches = F,filename = "SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/SizeDistribution/CrypticCarbon1M_WIP_bin_ab64_mask_stdev.tif", overwrite = T)


#define function to calculate SOC without writing to memory
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

#### No filtered WIP ####
CC_zb64_df <- rbind(C_map_simp(CC_WIPbin_mask64), 
      C_map_simp(CC_WIPbin_mask64_2pt5), 
      C_map_simp(CC_WIPbin_mask64_97pt5), 
      C_map_simp(CC_WIPbin_mask64_stdev) )

#### Zonal stats using extract on WIP majority filters ####
CC_stack <- c(HRW_CC_1M, HRW_CC_1M_2pt5, HRW_CC_1M_97pt5, HRW_CC_1M_stdev)
hoh_WIP_bin_polys_ab64_zonext <- CC_stack |>
    terra::extract(hoh_WIP_bin_polys_ab64, fun = "mean",  touches = F, na.rm = T, bind = T) |>
    rename(extract_mean_SOC = "lyr1",
           extract_2pt5_SOC = "2.5%", 
          extract_97pt5_SOC = "97.5%",
          extract_stdev_SOC = "sd") |>
    mutate(extract_stock_SOC = area_ha*extract_mean_SOC,
           extract_2pt5_stock_SOC = area_ha*extract_2pt5_SOC,
           extract_97pt5_stock_SOC = area_ha*extract_97pt5_SOC,
           extract_stdev_stock_SOC = area_ha*extract_stdev_SOC) |>
    writeVector(filename = "SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/SizeDistribution/hoh_WIP_bin_polys_ab64_zonext.gpkg", overwrite = T)
hoh_WIP_bin_polys_ab64_zonext <- vect("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/SizeDistribution/hoh_WIP_bin_polys_ab64_zonext.gpkg")

sum(hoh_WIP_bin_polys_ab64_zonext$extract_97pt5_stock_SOC, na.rm = T)/1000000 
hist(sqrt(hoh_WIP_bin_polys_ab64_zonext$extract_97pt5_stock_SOC), breaks = 1000)
plot(log((hoh_WIP_bin_polys_ab64_zonext$extract_97pt5_stock_SOC)), hoh_WIP_bin_polys_ab64_zonext$area_ha)

############################################################################################################
#### SOC totals by area percentile ####
############################################################################################################

#Largest 5 wetlands
(sort(hoh_WIP_bin_polys_ab64_zonext$extract_stock_SOC, decreasing = T)[1:5])

#cumsum
plot(cumsum(hoh_WIP_bin_polys_ab64_zonext$extract_97pt5_stock_SOC)/1e6)
plot(cumsum(hoh_WIP_bin_polys_ab64_zonext$area_ha))

percentile <- ecdf(hoh_WIP_bin_polys_ab64_zonext$area_ha)
Acre_percentile <- percentile(0.4047) # ha or 1 acre

SOC64 <- hoh_WIP_bin_polys_ab64_zonext |> as_tibble() |>
    mutate(bins = gtools::quantcut(area_ha, q = c(0, 0.05, 0.25, 0.5, 0.75, Acre_percentile, 1)),
           quantiles = case_when(bins == "0.0064"  ~ "0-5%",
                                 bins == "(0.0064,0.008]" ~ "5-25%",
                                 bins == "(0.008,0.0112]" ~ "25-50%",
                                 bins == "(0.0112,0.0256]" ~ "50-75%",
                                 bins == "(0.0256,0.405]" ~ "75-96.4%",
                                 bins == "(0.405,401]"  ~ "96.4- 100%")) |> 
    dplyr::group_by(quantiles) |>
    dplyr::reframe(SOC_stock_sumTg = sum(extract_stock_SOC)/1e6,
                   SOC_stock_meanMg = mean(extract_mean_SOC),
                   SOC_stock_stderrMg = sd(extract_mean_SOC)/sqrt(length((area_ha))),
                   SOC_2pt5_stock_sumTg = sum(extract_2pt5_stock_SOC)/1e6,
                   SOC_2pt5_stock_meanMg = mean(extract_2pt5_SOC),
                   SOC_2pt5_stock_stderrMg = sd(extract_2pt5_SOC)/sqrt(length((area_ha))),
                   SOC_97pt5_stock_sumTg = sum(extract_97pt5_stock_SOC)/1e6,
                   SOC_97pt5_stock_meanMg = mean(extract_97pt5_SOC),
                   SOC_97pt5_stock_stderrMg = sd(extract_97pt5_SOC)/sqrt(length((area_ha))),
                   SOC_stdev_stock_sumTg = sum(extract_stdev_stock_SOC)/1e6,
                   SOC_stdev_stock_meanMg = mean(extract_stdev_SOC),
                   SOC_stdev_stock_stderrMg = sd(extract_stdev_SOC)/sqrt(length((area_ha))),
                   area_count = length((area_ha)),
                   area_sum =  sum(round(area_ha, 4)),
                   area_max = max(round(area_ha, 4)),
                   area_min = min(round(area_ha, 4)),
                   area_mean = mean(area_ha),
                   area_median = median(area_ha),
                   bins = first(bins)) |>
    arrange(match(quantiles, c("0-5%","5-25%","25-50%","50-75%","75-96.4%","96.4- 100%"))) |>
    mutate(cumsumSOC = cumsum(SOC_stock_sumTg),
           cumsum_2pt5_SOC = cumsum(SOC_2pt5_stock_sumTg),
           cumsum_97pt5_SOC = cumsum(SOC_97pt5_stock_sumTg),
           cumsum_stdev_SOC = cumsum(SOC_stdev_stock_sumTg),
           cumsumArea = cumsum(area_sum),
           cumsumCount = cumsum(area_count)) |>
    unite("range", area_min:area_max, remove = F, sep = "-")

sum(SOC64$area_sum) # area total
sum(SOC64$SOC_stock_sumTg) #SOC total
sum(SOC64$area_sum[1:length(SOC64$area_sum)-1])/sum(SOC64["area_sum"]) # 14% of the area is made up of wetlands smaller than 1 acre
sum(SOC64$SOC_stock_sumTg[1:length(SOC64$area_sum)-1])/sum(SOC64$SOC_stock_sumTg) # 13% of the SOC is in these wetlands
SOC64$area_sum[6]/sum(SOC64$area_sum) # 86% of the area is in the biggest wetlands
SOC64$SOC_stock_sumTg[6]/sum(SOC64$SOC_stock_sumTg) # 87% of the SOC is in the biggest wetlands


#### Histogram ####
hoh_WIP_bin_polys_ab64_zonext_tib <- hoh_WIP_bin_polys_ab64_zonext |> as_tibble()

ggplot(SOC64) +
    geom_col(aes(x = (quantiles), y = area_count)) +
    labs(title = "area count") + 
    theme(legend.position = 'right',
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey80"),
          axis.ticks = element_blank(),
          text = element_text(size = 20))
ggplot(SOC64) +
    geom_col(aes(x = (quantiles), y = (SOC_stock_sumTg))) +
    labs(title = "SOC_stock_sumTg") + 
    theme(legend.position = 'right',
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey80"),
          axis.ticks = element_blank(),
          text = element_text(size = 20))



write.csv(SOC64, file = "SOIL CARBON/ANALYSIS/CrypticCarbonData_revised/CrypticCarbon_sizedist_summary.csv")
