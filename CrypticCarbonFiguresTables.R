library(ggplot2)
library(lme4)
library(merTools)
library(lmerTest)
library(tidyverse)
library(terra)
library(tidyterra)
library(ggcorrplot)
library(RColorBrewer)
library(cowplot)
library(kableExtra)
library(openxlsx)


setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/')
#setwd('/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/')
wd = '/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling'
#wd = '/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/' 
figpath <- "/Users/Anthony/OneDrive - UW/University of Washington/Writing and Drafting/Cryptic Carbon/Submission/Revision/"

hoh_dat <- read.csv("SOIL CARBON/ANALYSIS/CrypticCarbonData_revised/CrypticCarbon_Stocks_Full_Revised.csv") |> 
    dplyr::select(-X)

############## Recoding LITHOL to landtype  ##########################################
hoh_dat<- hoh_dat  |> mutate(landtype = case_when(WIP >= 0.5 &LITHOL == "alluvium_marine_water" ~ "Riverine",
                                                  WIP >= 0.5 &LITHOL != "alluvium_marine_water" ~ "Palustrine",
                                                  #WIP < 0.5 &WIP>0.1 &LITHOL != "alluvium_marine_water" ~ "Mesic",
                                                  #WIP < 0.5 & WIP>0.1 &LITHOL == "alluvium_marine_water" ~ "Riparian Mesic",
                                                  WIP <= 0.5 ~ "Upland",
                                                  #WIP <= 0.5 &LITHOL == "alluvium_marine_water" ~ "Riparian Upland",
                                                  TRUE ~ "Upland"),
                             landtype_mes = case_when(WIP >= 0.5 &LITHOL == "alluvium_marine_water" ~ "Riverine",
                                                  WIP >= 0.5 &LITHOL != "alluvium_marine_water" ~ "Palustrine",
                                                  WIP < 0.5 &WIP>0.25 ~ "Mesic",
                                                  WIP < 0.25  ~ "MesicUPL",
                                                  #WIP < 0.5 & WIP>0.25 &LITHOL == "alluvium_marine_water" ~ "Riparian Mesic",
                                                  WIP <= 0.5 ~ "Upland",
                                                  #WIP <= 0.5 &LITHOL == "alluvium_marine_water" ~ "Riparian Upland",
                                                  TRUE ~ "Upland")) |>
    mutate(landtype_wetup = case_when(WIP >= 0.5 ~ "Wetland",
                                      WIP < 0.5 ~ "Upland")) 
write.xlsx(hoh_dat, file = "SOIL CARBON/ANALYSIS/CrypticCarbonData_revised/CrypticCarbon_Stocks_Full_Revised.xlsx")


#### TABLE: Pedon Table ######

pedon_table <- bind_rows(
    {hoh_dat |> 
        dplyr::group_by(landtype) |>
        reframe(n = n(),
                SOC_stock_30_mean = mean(SOC_stock_30),
                SOC_stock_60_mean = mean(SOC_stock_60),
                SOC_stock_100_mean = mean(SOC_stock_100),
                SOC_stock_120_mean = mean(SOC_stock_120),
                SOC_stock_full_mean = mean(SOC_stock_full),
                SOC_stock_30_sd = sd(SOC_stock_30),
                SOC_stock_60_sd = sd(SOC_stock_60),
                SOC_stock_100_sd = sd(SOC_stock_100),
                SOC_stock_120_sd = sd(SOC_stock_120),
                SOC_stock_full_sd = sd(SOC_stock_full),
                sample_depth = mean(maxdepth),
                sample_depth_sd = sd(maxdepth)
            )},
    {hoh_dat |>
            dplyr::group_by(landtype_wetup) |>
            reframe(n = n(),
                    SOC_stock_30_mean = mean(SOC_stock_30[which(landtype_wetup == "Wetland")]),
                    SOC_stock_60_mean = mean(SOC_stock_60[which(landtype_wetup == "Wetland")]),
                    SOC_stock_100_mean = mean(SOC_stock_100[which(landtype_wetup == "Wetland")]),
                    SOC_stock_120_mean = mean(SOC_stock_120[which(landtype_wetup == "Wetland")]),
                    SOC_stock_full_mean = mean(SOC_stock_full[which(landtype_wetup == "Wetland")]),
                    SOC_stock_30_sd = sd(SOC_stock_30[which(landtype_wetup == "Wetland")]),
                    SOC_stock_60_sd = sd(SOC_stock_60[which(landtype_wetup == "Wetland")]),
                    SOC_stock_100_sd = sd(SOC_stock_100[which(landtype_wetup == "Wetland")]),
                    SOC_stock_120_sd = sd(SOC_stock_120[which(landtype_wetup == "Wetland")]),
                    SOC_stock_full_sd = sd(SOC_stock_full[which(landtype_wetup == "Wetland")]),
                    sample_depth = mean(maxdepth),
                    sample_depth_sd = sd(maxdepth)
                    )},
    {hoh_dat |>
            reframe(landtype = "All Landscapes",
                    n = n(),
                    SOC_stock_30_mean = mean(SOC_stock_30),
                    SOC_stock_60_mean = mean(SOC_stock_60),
                    SOC_stock_100_mean = mean(SOC_stock_100),
                    SOC_stock_120_mean = mean(SOC_stock_120),
                    SOC_stock_full_mean = mean(SOC_stock_full),
                    SOC_stock_30_sd = sd(SOC_stock_30),
                    SOC_stock_60_sd = sd(SOC_stock_60),
                    SOC_stock_100_sd = sd(SOC_stock_100),
                    SOC_stock_120_sd = sd(SOC_stock_120),
                    SOC_stock_full_sd = sd(SOC_stock_full),
                    sample_depth = mean(maxdepth),
                    sample_depth_sd = sd(maxdepth)
                    )}
    ) |> 
    mutate(`Landscape Class` = coalesce(landtype, landtype_wetup)) |> 
    dplyr::select(-landtype_wetup, -landtype) |> na.omit() |>
    mutate(across(where(is.numeric), ~ case_when(abs(.) >= 100 ~ signif(., digits = 3),
                                                 abs(.) >=10 & abs(.)<99.9 ~ signif(., digits = 3),
                                                 abs(.) >=1 & abs(.)<10 ~ signif(., digits = 2),
                                                 abs(.) >=0 & abs(.)<1 ~ signif(., digits = 1),
                                                 .default =  signif(., digits = 1)))) |>
    dplyr::relocate(`Landscape Class`) |> dplyr::arrange(match(`Landscape Class`, c( "Wetland", "Riverine", "Palustrine", "Upland", "All Landscapes")))

pedon_table_formatted <- pedon_table |>
    mutate(`30cm SOC Stock` = paste0(SOC_stock_30_mean, " ± ", signif(SOC_stock_30_sd/sqrt(n), 3)),
           `60cm SOC Stock` = paste0(SOC_stock_60_mean, " ± ", signif(SOC_stock_60_sd/sqrt(n), 3)),
           `1m SOC Stock` = paste0(SOC_stock_100_mean, " ± ", signif(SOC_stock_100_sd/sqrt(n), 3)),
           `120cm SOC Stock` = paste0(SOC_stock_120_mean, " ± ", signif(SOC_stock_120_sd/sqrt(n), 3)),
           `Full SOC Stock` = paste0(SOC_stock_full_mean, " ± ", signif(SOC_stock_full_sd/sqrt(n), 3)),
           `Sample Depth` = paste0(signif(sample_depth, 2), " ± ", signif(sample_depth_sd/sqrt(n), 2))) |>
    dplyr::select(`Landscape Class`, `30cm SOC Stock`, `60cm SOC Stock`, `1m SOC Stock`, `120cm SOC Stock`,`Full SOC Stock`, `Sample Depth`, n) #|> 

write.xlsx(pedon_table_formatted, file = paste0(figpath, "Figures_Revised/Tables/CrypticCarbon_Pedon_Table_Formatted.xlsx"))

#### TABLE: Show pedon table in R ####
pedon_table_formatted |> 
    kbl(align = "c", escape = T) |>
    kable_styling(full_width = F) |>
    column_spec(1, bold = T)

#### TABLE: Pedon Percentages ####
pedon_perc <- pedon_table |> 
    mutate(perc_in30 = signif(100*(SOC_stock_30_mean/SOC_stock_full_mean), 2),
           perc_in60 = signif(100*(SOC_stock_60_mean/SOC_stock_full_mean), 2),
           perc_in100 = signif(100*(SOC_stock_100_mean/SOC_stock_full_mean), 2),
           perc_in120 = signif(100*(SOC_stock_120_mean/SOC_stock_full_mean), 2),
           
           perc_in30_60 = signif(perc_in60-perc_in30, 2),
           perc_in60_100 = signif(perc_in100-perc_in60, 2),
           perc_in100_120 = signif(perc_in120-perc_in100, 2)) |>
    dplyr::select(`Landscape Class` , perc_in30, perc_in30_60, perc_in60, perc_in60_100, perc_in100, perc_in120, perc_in100_120)

write.xlsx(pedon_perc, file = paste0(figpath, "Figures_Revised/Tables/CrypticCarbon_Pedon_Percentages_Formatted.xlsx"))

#### TABLE: Show pedon percentages in R
pedon_perc |> 
    kbl(align = "c", escape = T) |>
    kable_styling(full_width = F) |>
    column_spec(1, bold = T)

#### TABLE: Mesic pedons ####
mesic_pedons <- hoh_dat |> 
    dplyr::group_by(landtype_mes) |>
    reframe(n = n(),
            SOC_stock_30_mean = mean(SOC_stock_30),
            SOC_stock_60_mean = mean(SOC_stock_60),
            SOC_stock_100_mean = mean(SOC_stock_100),
            SOC_stock_120_mean = mean(SOC_stock_120),
            SOC_stock_full_mean = mean(SOC_stock_full),
            SOC_stock_30_sd = sd(SOC_stock_30),
            SOC_stock_60_sd = sd(SOC_stock_60),
            SOC_stock_100_sd = sd(SOC_stock_100),
            SOC_stock_120_sd = sd(SOC_stock_120),
            SOC_stock_full_sd = sd(SOC_stock_full),
            sample_depth = mean(maxdepth),
            sample_depth_sd = sd(maxdepth)) |>
    mutate(`30cm SOC Stock` = paste0(signif(SOC_stock_30_mean, 3), " ± ", signif(SOC_stock_30_sd/sqrt(n), 3)),
           `60cm SOC Stock` = paste0(signif(SOC_stock_60_mean, 3), " ± ", signif(SOC_stock_60_sd/sqrt(n), 3)),
           `1m SOC Stock` = paste0(signif(SOC_stock_100_mean, 3), " ± ", signif(SOC_stock_100_sd/sqrt(n), 3)),
           `120cm SOC Stock` = paste0(signif(SOC_stock_120_mean, 3), " ± ", signif(SOC_stock_120_sd/sqrt(n), 3)),
           `Full SOC Stock` = paste0(signif(SOC_stock_full_mean, 3), " ± ", signif(SOC_stock_full_sd/sqrt(n), 3)),
           `Sample Depth` = paste0(signif(sample_depth, 2), " ± ", signif(sample_depth_sd/sqrt(n), 2))) |>
    dplyr::select(`landtype_mes`, `30cm SOC Stock`, `60cm SOC Stock`, `1m SOC Stock`, `120cm SOC Stock`,`Full SOC Stock`, `Sample Depth`, n) 

write.xlsx(mesic_pedons, file = paste0(figpath, "Figures_Revised/Tables/CrypticCarbon_Pedon_Percentages_Formatted.xlsx"))            

#### TABLE: Show mesic pedons in R ####
mesic_pedons |> 
    kbl(align = "c", escape = T) |>
    kable_styling(full_width = F) |>
    column_spec(1, bold = T)

#### GRAPH: pedon histogram ####

pedon_hist <- ggplot(hoh_dat, aes(x = WIP*100)) + 
    geom_histogram(bins = 30, binwidth = 4, color = "black", fill="grey80", boundary = 0) +
    xlim(0, 100) +
    xlab("WIP Wetland Probability %") +
    ylab("Count") +
    theme(legend.position = 'right', 
          legend.key.size = unit(1.7, "cm"),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey80"),
          axis.ticks = element_blank(),
          text = element_text(size = 22))

ggplot2::ggsave(plot = pedon_hist, paste0(figpath, "Figures_Revised/Pedon_WIP_histogram.jpeg"),
                width = 9, height = 7.5, units = "in")


#### GRAPH: histogram of map data ####
hoh_WIP_mask <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/NonCarbon/Hoh_WIP_MNDWI_masked.tif")

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
WIP_hist_graph <- quick_hist(values_vec = WIP_hist_df*100) +
    scale_y_continuous(labels=scales::label_number(scale_cut = scales::cut_short_scale())) +
    xlab("WIP Wetland Probability %") +
    ylab("Count")+ 
    theme(legend.position = 'right', 
          legend.key.size = unit(1.7, "cm"),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey80"),
          axis.ticks = element_blank(),
          text = element_text(size = 22))

ggplot2::ggsave(plot = WIP_hist_graph, paste0(figpath, "Figures_Revised/Map_WIP_histogram.jpeg"),
                width = 9, height = 7.5, units = "in")

############# Model Figures and Graphs ##########################################

##### Models #####
mod <-  lmer(sqrt(SOC_stock_100) ~ WIP + (1|LITHOL), data = hoh_dat)
mod30 <- lmer(sqrt(SOC_stock_30) ~ WIP + (1|LITHOL), data = hoh_dat)


PI <-predictInterval(
    mod,
    which = c("full"),
    level = 0.95,
    n.sims = 1000,
    stat = c("median"),
    type = c("linear.prediction"),
    include.resid.var = TRUE,
    returnSims = TRUE,
    seed = 7
)

PI_trans <- PI |> mutate(fit_t = fit**2,
                         upr_t = upr**2,
                         lwr_t = lwr**2,
                         act = hoh_dat$SOC_stock_100,
                         fit_t_act = (fit_t - act)**2)
#PI_rmse <- sqrt(sum(PI_trans$fit_t - PI_trans$act)**2)

##### GRAPH: Fit of Predicted vs. Actual ####
fitact <- ggplot(hoh_dat, aes(y = (fitted(mod))**2, x = (SOC_stock_100))) +
            geom_point(color='black', position = position_jitter(width = 15, height = 15),  aes(shape= LITHOL,fill = WIP*100),
                           size = 9, stroke = 1, alpha = 0.8) +
            scale_fill_gradientn(colours = brewer.pal(9, "YlGnBu"), name = "WIP %", n.breaks = 5, limits = c(0, 100)) +
            geom_smooth(aes(y = (fitted(mod))**2, x = (SOC_stock_100)), 
                        method = "lm", color = "#fa3e3e", size = 2, linetype = 5, se = F, show.legend = F) +
            scale_shape_manual(name = "Surfical Geology", values = c(21, 22, 23, 24),
                               labels= c("Alluvium", "Glacial Drift", "Clastic", "Till/Outwash")) +
            #geom_ribbon(aes(x = sqrt(SOC_stock_100), ymax = PI$upr, ymin = PI$lwr), alpha = 0.1, color = NA) +
            geom_smooth(aes(x = (SOC_stock_100), y = (PI$lwr)**2), lty = 3, size = 2, method = "lm", se = F, col = "#1664c9", show.legend = F) + 
            geom_smooth(aes(x = (SOC_stock_100), y =( PI$upr)**2), lty = 3, size = 2, method = "lm", se = F, col = "#1664c9", show.legend = F) +
            #ggplot2::scale_size(name = NULL, breaks = NULL, labels = NULL) +
            xlab(expression('Pedon 1m SOC Stock (MgC ha'^-1*')')) + ylab(expression('Predicted 1m SOC Stock (MgC ha'^-1*')')) +
            geom_abline(intercept = 0, slope = 1, linewidth = 2, linetype = "dashed") +
            #labs(colour = "Random Effect") +
            #xlim(0, 32) +
            #ylim(0, 32)  +
            guides(guide_legend(byrow = TRUE)) +
            theme(legend.position = 'right',
                  legend.key.size = unit(1.2, "cm"),
                  legend.key = element_blank(),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "grey80"),
                  axis.ticks = element_blank(),
                  text = element_text(size = 22))

ggplot2::ggsave(plot = fitact, paste0(figpath, "Figures_Revised/Predict_vs_Actual_1M.jpeg"),
                width = 9, height = 7.5, units = "in")


#### GRAPH: 30 cm graph ####
PI30 <- predictInterval(
    mod30,
    which = c("full"),
    level = 0.95,
    n.sims = 1000,
    stat = c("median"),
    type = c("linear.prediction"),
    include.resid.var = TRUE,
    returnSims = TRUE,
    seed = 7
)

PI30_trans <- PI30 |> mutate(fit_t = fit**2,
                         upr_t = upr**2,
                         lwr_t = lwr**2,
                         act = hoh_dat$SOC_stock_100,
                         fit_t_act = (fit_t - act)**2)
#PI_rmse <- sqrt(sum(PI_trans$fit_t - PI_trans$act)**2)

#### GRAPH: Fit of Predicted vs. Actual 30cm ####
fitact30 <- ggplot(hoh_dat, aes(y = (fitted(mod30))**2, x = (SOC_stock_30))) +
            geom_point(color='black', position = position_jitter(width = 15, height = 15),  aes(shape= LITHOL,fill = WIP*100),
                       size = 9, stroke = 1) +
            scale_fill_gradientn(colours = brewer.pal(9, "YlGnBu"), name = "WIP %", n.breaks = 5, limits = c(0, 100)) +
            geom_smooth(aes(y = (fitted(mod30))**2, x = (SOC_stock_30)), method = "lm", color = "#fa3e3e", size = 2, linetype = 5, se = F) +
            scale_shape_manual(name = "Surfical Geology", values = c(21, 22, 23, 24),
                                labels= c("Alluvium", "Glacial Drift", "Clastic", "Till/Outwash")) +
            #geom_ribbon(aes(x = sqrt(SOC_stock_100), ymax = PI$upr, ymin = PI$lwr), alpha = 0.1, color = NA) +
            geom_smooth(aes(x = (SOC_stock_30), y = (PI30$lwr)**2), lty = 3, size = 2, method = "lm", se = F, col = "#1664c9") + 
            geom_smooth(aes(x = (SOC_stock_30), y =( PI30$upr)**2), lty = 3, size = 2, method = "lm", se = F, col = "#1664c9") +
            #ggplot2::scale_size(name = NULL, breaks = NULL, labels = NULL) +
            xlab(expression('Pedon 30 cm SOC Stock (MgC ha'^-1*')')) + ylab(expression('Predicted 30 cm SOC Stock (MgC ha'^-1*')')) +
            geom_abline(intercept = 0, slope = 1, linewidth = 2, linetype = "dashed") +
            #labs(colour = "Random Effect") +
            #xlim(0, 32) +
            #ylim(0, 32)  +
            guides(guide_legend(byrow = TRUE)) +
            theme(legend.position = 'right', 
                  legend.key = element_blank(),
                  legend.key.size = unit(1.2, "cm"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "grey80"),
                  axis.ticks = element_blank(),
                  text = element_text(size = 22))

ggplot2::ggsave(plot = fitact30, paste0(figpath, "Figures_Revised/Predict_vs_Actual_30CM.jpeg"),
                width = 9, height = 7.5, units = "in")



###### TABLE: Table of map values #####
results_table <- read.csv("/Users/Anthony/OneDrive - UW/University of Washington/Writing and Drafting/Cryptic Carbon/Submission/Revision/R_all_maps_SOC_table.csv")


library(janitor)
results_table_summary <- results_table |> 
    mutate(across(where(is.numeric), ~ case_when(abs(.) >= 10000 ~ signif(., digits = 5),
                                                 abs(.) >= 1000 ~ signif(., digits = 5),
                                                 abs(.) >= 100 ~ signif(., digits = 3),
                                                 abs(.) >=10 & abs(.)<100 ~ signif(., digits = 3),
                                                 abs(.) >=1 & abs(.)<10 ~ signif(., digits = 2),
                                                 .default =  signif(., digits = 1)))) |> 
    dplyr::mutate(across(everything(), ~gsub(pattern = " ", replacement= "_", .)),
                  depth_land_nwca = paste0(depth, "_", landtype,"_", NWCA)) |>
    dplyr::group_by(depth_land_nwca) |>
    dplyr::arrange(depth_land_nwca) |>
    #t() |> as.data.frame() #|> row_to_names(row_number = 9)
    reframe(Source = first(NWCA),
            `Landscape Class` = first(landtype),
            `Surface Area` = first(Total_area),
            `Depth` = first(depth),
            `Mean SOC Stock` = paste0(AverageSOC_Mgha[which(stat == "Average")],
                                      " ± ", 
                                      AverageSOC_Mgha[which(stat == "STDEV")], 
                                      " (", 
                                      AverageSOC_Mgha[which(stat == "2.5%")], 
                                      "-", 
                                      AverageSOC_Mgha[which(stat == "97.5%")], 
                                      ")"),
            `Total SOC Stock` = paste0(total_Carbon_Tg[which(stat == "Average")],
                                       " ± ", 
                                       total_Carbon_Tg[which(stat == "STDEV")], 
                                       " (", 
                                       total_Carbon_Tg[which(stat == "2.5%")], 
                                       "-", 
                                       total_Carbon_Tg[which(stat == "97.5%")], 
                                       ")")) |>
    dplyr::select(-depth_land_nwca) |> 
    dplyr::group_by(Depth) |> 
    dplyr::arrange(desc(Source), match(`Landscape Class`, c("WETLAND", "Riverine", "Palustrine", "FORESTED_WETLAND", "UPLAND", "Total_Area")), .by_group = TRUE)

write.xlsx(results_table_summary, file = paste0(figpath, "Figures_Revised/Tables/mapping_results_table_summary.xlsx"))


#### TABLE: Show results table summary in R ####
results_table_summary |> 
    kbl(align = "c", escape = T) |>
    kable_styling(full_width = F) |>
    column_spec(1, bold = T)

#### TABLE: Combining NWCA and WIP ####
results_NWCA_masked <- results_table |>
    filter(landtype == "WETLAND"| landtype == "FORESTED WETLAND" & NWCA == "NWCA Masked" | NWCA == "NWCA Model") |>
    dplyr::mutate(depth_land_nwca = paste0(depth, "_", landtype,"_")) |>
    group_by(depth_land_nwca) |>
    reframe(
            WIP_area = first(Total_area[which(NWCA == "NWCA Masked")]),
            NWCA_area = first(Total_area[which(NWCA == "NWCA Model")]),
            WIP_fraction  = first(Total_area[which(NWCA == "NWCA Masked")])/sum(first(Total_area[which(NWCA == "NWCA Masked")]), first(Total_area[which(NWCA == "NWCA Model")])),
            NWCA_fraction  = first(Total_area[which(NWCA == "NWCA Model")])/sum(first(Total_area[which(NWCA == "NWCA Masked")]), first(Total_area[which(NWCA == "NWCA Model")])),
            WIP_stdev = total_Carbon_Tg[which(stat == "STDEV" & NWCA == "NWCA Masked")],
            NWCA_stdev =  (total_Carbon_Tg[which(stat == "STDEV" & NWCA == "NWCA Model")]),
            Combined_AvgMg = (sum(total_Carbon_Tg[which(stat == "Average" & NWCA == "NWCA Masked")], 
                                  total_Carbon_Tg[which(stat == "Average" & NWCA == "NWCA Model")])/sum(first(Total_area[which(NWCA == "NWCA Masked")]), 
                                                                                                        first(Total_area[which(NWCA == "NWCA Model")])))*1000000,
            Combined_TotTg = sum(total_Carbon_Tg[which(stat == "Average" & NWCA == "NWCA Masked")], 
                                 total_Carbon_Tg[which(stat == "Average" & NWCA == "NWCA Model")]),
            `Mean SOC Stock` = paste0(round(Combined_AvgMg, 4),
                                      " ± ", #STDEV
                                      signif(sum((WIP_stdev/WIP_area)*WIP_fraction*1E6, 
                                           (NWCA_stdev/NWCA_area)*NWCA_fraction*1E6), 4), 
                                      " (", 
                                      signif(Combined_AvgMg - (sum((WIP_stdev/WIP_area)*WIP_fraction*1E6, 
                                                            (NWCA_stdev/NWCA_area)*NWCA_fraction*1E6)), 4),
                                      "-", 
                                      signif(Combined_AvgMg + (sum((WIP_stdev/WIP_area)*WIP_fraction*1E6, 
                                                            (NWCA_stdev/NWCA_area)*NWCA_fraction*1E6)), 4), 
                                      ")"),
            `Total SOC Stock` = paste0(signif(Combined_TotTg, 3),
                                       " ± ", 
                                       signif((sum((WIP_stdev),#*WIP_fraction, 
                                            (NWCA_stdev))), 3),#*NWCA_fraction, 
                                       " (", 
                                       signif((Combined_TotTg - (sum((WIP_stdev),#*WIP_fraction, 
                                                            (NWCA_stdev)))), 3),#*NWCA_fraction, 
                                       "-", 
                                       signif((Combined_TotTg + (sum((WIP_stdev),#*WIP_fraction, 
                                                             (NWCA_stdev)))), 3),#*NWCA_fraction,   
                                       ")"),
            Source = first(NWCA),
            `Landscape Class` = first(landtype),
            `Surface Area` = signif(sum(first(Total_area[which(NWCA == "NWCA Masked")]), first(Total_area[which(NWCA == "NWCA Model")])), 5),
            `Depth` = first(depth)) |>
    dplyr::select(depth_land_nwca, `Surface Area`, `Mean SOC Stock`, `Total SOC Stock`)

write.xlsx(results_NWCA_masked, file = paste0(figpath, "Figures_Revised/Tables/mapping_results_NWCA_masked.xlsx"))

results_NWCA_masked |>
    kbl(align = "c", escape = T) |>
    kable_styling(full_width = F) |>
    column_spec(1, bold = T)

#### TABLE: Percentages #####

Total_SOC_WIP_1M <- results_table[results_table$value == "CrypticCarbon 1M SOC MNDWI masked", "total_Carbon_Tg"]
Total_SOC_WIP_30CM <- results_table[results_table$value == "CrypticCarbon 30CM SOC MNDWI masked", "total_Carbon_Tg"]
WIP_wetland_SOC_1M <- results_table[results_table$value == "CC 1M WETLAND" , "total_Carbon_Tg"]
NWCA_Total_SOC_1M <- results_table[results_table$value == "NWCA derived 1M mean", "total_Carbon_Tg"]
NWCA_Total_Area <- results_table[results_table$value == "NWCA derived 1M mean", "Total_area"]

results_table_perc <-  
    bind_rows(
        {results_table |> dplyr::filter(NWCA != "NWCA Model") |>
            dplyr::group_by(value) |>
            reframe(
                type = "NoNWCA",
                Total_SOCs = as.numeric(total_Carbon_Tg[which(stat == "Average")]),
                Total_Areas = as.numeric(Total_area[which(stat == "Average")]),
                Areaperc_gt_NWCA = 100*((Total_Areas-NWCA_Total_Area)/NWCA_Total_Area),
                SOCperc_in_WIP = 100*(Total_SOCs/Total_SOC_WIP_1M),
                SOCperc_gt_NWCA = 100*((Total_SOCs-NWCA_Total_SOC_1M)/NWCA_Total_SOC_1M),
                SOCperc_in_WIP_Wetand = 100*(Total_SOCs/WIP_wetland_SOC_1M)
            )}, 
        {results_table |> dplyr::filter(NWCA == "NWCA Model") |>
                dplyr::group_by(value) |>
                reframe(
                    type = "NWCAOnly",
                    Total_SOCs = as.numeric(total_Carbon_Tg[which(stat == "Average")]),
                    Total_Areas = as.numeric(Total_area[which(stat == "Average")]),
                    Areaperc_gt_NWCA = 100*((Total_Areas-NWCA_Total_Area)/NWCA_Total_Area),
                    SOCperc_in_WIP = 100*(Total_SOCs/Total_SOC_WIP_1M),
                    SOCperc_gt_NWCA = 100*((Total_SOCs-NWCA_Total_SOC_1M)/NWCA_Total_SOC_1M),
                    SOCperc_in_WIP_Wetand = 100*(Total_SOCs/WIP_wetland_SOC_1M)
                )}
        )
write.xlsx(results_table_perc, file = paste0(figpath, "Figures_Revised/Tables/mapping_results_table_perc.xlsx"))

#### TABLE: Show results table percentages in R ####
results_table_perc|>
    kbl(align = "c", escape = T) |>
    kable_styling(full_width = F) |>
    column_spec(1, bold = T)


#### GRAPH: Cryptic Carbon Polygons and Size class ####
SOC64 <- read.csv("SOIL CARBON/ANALYSIS/CrypticCarbonData_revised/CrypticCarbon_sizedist_summary.csv") |> 
    as_tibble() |> 
    dplyr::select(-X)
hoh_WIP_bin_polys_ab64_zonext <- vect("SOIL CARBON/CrypticCarbonMaps_Revised/FinalForPaper/SizeDistribution/hoh_WIP_bin_polys_ab64_zonext.gpkg")

polys_df <- as_tibble(hoh_WIP_bin_polys_ab64_zonext)

loglog_wetsize <- ggplot(polys_df, aes(x = log(area_ha), y = log(extract_stock_SOC))) +
                    geom_point(aes(colour=extract_mean_SOC)) + 
                    scale_color_continuous(type = "viridis", name = expression('SOC Stock Density (MgC ha'^-1*')')) +
                    ylab(expression('Log Transformed SOC Stock (Tg)')) +
                    xlab('Log Transformed Individual Wetland Extent (ha)') +
                    theme(legend.position = "right", 
                          legend.title = element_text(size=12),
                          legend.text = element_text(size=12),
                          panel.background = element_blank(),
                          panel.grid.major = element_line(colour = "grey80"),
                          axis.ticks = element_blank(),
                          text = element_text(size = 20))

ggplot2::ggsave(plot = loglog_wetsize, paste0(figpath, "Figures_Revised/WetlandSizeClassSOC.jpeg"),
                width = 9, height = 6.5, units = "in")

#### TABLE: Cryptic Carbon Polygons and Size class ####
table64 <- SOC64 |> 
    mutate(across(where(is.numeric), ~ case_when(abs(.) >= 10000 ~ signif(., digits = 6),
                                                 abs(.) >= 100 ~ signif(., digits = 3),
                                                 abs(.) >=10 & abs(.)<100 ~ signif(., digits = 3),
                                                 abs(.) >=1 & abs(.)<10 ~ signif(., digits = 2),
                                                 .default =  signif(., digits = 1)))) |> 
    mutate(`Quantiles` = paste0(quantiles,  " (", area_min, "-", area_max, ")"),
           `SOC Stock (Tg)` = paste0(SOC_stock_sumTg, " ± ",SOC_stdev_stock_sumTg, " (", SOC_2pt5_stock_sumTg, "-", SOC_97pt5_stock_sumTg, ")"),
           `Mean SOC Stock (Mg ha)` = paste0(SOC_stock_meanMg, " ± ",SOC_stdev_stock_meanMg, " (", SOC_2pt5_stock_meanMg, "-", SOC_97pt5_stock_meanMg, ")"),
           `Count` = paste0(area_count),
           `Area (ha)` = paste0(area_sum),
           
           `Cumulative SOC Stock (Tg)` = paste0(cumsumSOC, " ± ",cumsum_stdev_SOC, " (", cumsum_2pt5_SOC, "-", cumsum_97pt5_SOC, ")"),
           `Cumulative Area (ha)` = paste0(cumsumArea),
           `Cumulative Count` = paste0(cumsumCount)) |> 
    dplyr::select(`Quantiles`,`SOC Stock (Tg)`, `Mean SOC Stock (Mg ha)`, `Count`, `Area (ha)`, 
           `Cumulative SOC Stock (Tg)`,`Cumulative Area (ha)`, `Cumulative Count`) 

write.xlsx(table64, file = paste0(figpath, "Figures_Revised/Tables/sizedist_table.xlsx")) 

#### TABLE: Show Size class in R ####
table64 |>
    kbl(align = "c", escape = T) |>
    kable_styling(full_width = F) |>
    column_spec(1, bold = T)

