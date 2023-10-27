library(lme4)
library(lmerTest)
library(terra)
library(sf)
library(tidyverse)
library(car)
library(ggplot2)
library(merTools)
library(glmnet)
library(stats)
library(ggcorrplot)
library(RColorBrewer)
library(cowplot)
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(effects)
library(sjstats) #use for r2 functions
library(boot)
library(splitstackshape)

setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/')
#setwd('/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/')
#wd = '/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling'
#wd = '/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/' 


hoh_dat <- read.csv("SOIL CARBON/ANALYSIS/CrypticCarbonData_revised/CrypticCarbon_Stocks_Full_Revised.csv") |> 
    dplyr::select(-X)



# Quick R^2 function
r.sq <- function(y,y.fitted){
    res <- y-y.fitted
    1-sum(res^2)/sum((y-mean(y))^2)
}

#### model section ####
shapiro.test(sqrt(hoh_dat$SOC_stock_100) ) #normality check 

mod_dat <- hoh_dat[, c("SOC_stock_100", "WIP", "EVI", "NDVI", "MNDWI","LITHOL")]
modlarge <- lmer(sqrt(SOC_stock_100) ~ WIP + EVI+ NDVI+ MNDWI+ (1|LITHOL),  data = mod_dat, na.action = "na.fail", REML = F)
(stepmod <- lmerTest::step(modlarge, 
                         keep = c("WIP"),
                         reduce.random=FALSE))
#investigating prediction maps using the other variables revealed innappropriate SOC predictions in areas
# with variable spectral characteristics (i.e. clearcuts as hotspots for high SOC)
# therefore we elected to utilize just the WIP with surficial geology

mod <-  lmer(sqrt(SOC_stock_100) ~ WIP + (1|LITHOL),  data = hoh_dat)

mod30 <- lmer(sqrt(SOC_stock_30) ~ WIP + (1|LITHOL), data = hoh_dat)


plot(cooks.distance(mod)) # influential points
plot(cooks.distance(mod30))


summary(mod)
anova(mod)
tab_model(mod)
plot(mod)
qqPlot(resid(mod))

rsq::rsq.lmm(mod)
r.sq(sqrt(hoh_dat$SOC_stock_100), fitted(mod)) #actual vs. predicted

#### 30cm summary
summary(mod30)
anova(mod30)
tab_model(mod30)
plot(mod30)
qqPlot(resid(mod30))
rsq::rsq.lmm(mod30)
r.sq(sqrt(hoh_dat$SOC_stock_30), fitted(mod30))

#### Prediction to raster map KEEP HERE####
GEOm <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_GEO_100k_reclassified.tif")
WIPm <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")

rs <- c(WIPm , GEOm)
names(rs) <- c("WIP", "LITHOL")
Pred <- terra::predict(rs, mod30, allow.new.levels = T, type = "response")
Pred_trans <- (Pred**2)
plot((Pred_trans))
writeRaster(Pred_trans, filename = "SOIL CARBON/CrypticCarbonMaps_Revised/CrypticCarbon_30CM_SOC_Revised.tif", overwrite = T  )


#### Model fit ####

set.seed(7)

#This is the confidence intervals for the parameters in the mode
    #re.form = NULL conditions on ALL random effects 
    # The mySumm function gets the model parameters and back transforms them
mySumm <- function(.) { 
    s <- sigma(.)
    vector <- c(beta =(getME(., "beta"))**2, sigma = s**2, sig01 = (unname(s * getME(., "theta")))**2 )
    names(vector)<- c("intercept", "WIP", "sigma", "LITHOL")
    return(vector)
}

non_mod <- lmer(SOC_stock_100 ~ WIP + (1|LITHOL), data = hoh_stocks_df, REML = F)
non_mod30 <- lmer((SOC_stock_30) ~ WIP + (1|LITHOL), data = hoh_dat)

CI_sq <- confint.merMod(mod, nsim = 1000, method = "boot", boot.type = "perc", oldNames = F, re.form = NULL, seed = 7)
CI <- confint.merMod(non_mod, nsim = 1000, method = "boot", boot.type = "perc", oldNames = F, re.form = NULL, seed = 7)
CI_tbl <- cbind(as.data.frame(CI_sq), as.data.frame(CI))
names(CI_tbl) <- c("2.5 % sqrt","97.5 % sqrt","2.5 % sq","97.5 % sq")

CI_30 <- confint.merMod(non_mod30, nsim = 1000, method = "boot", boot.type = "perc", oldNames = F, re.form = NULL, seed = 7)


PI <- predictInterval(
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
PI_rmse <- sqrt(abs(sum(PI_trans$fit_t - PI_trans$act)**2)/36)

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

PI_trans30 <- PI30 |> mutate(fit_t = fit**2,
                         upr_t = upr**2,
                         lwr_t = lwr**2,
                         act = hoh_dat$SOC_stock_30,
                         fit_t_act = (fit_t - act)**2)
PI_rmse30 <- sqrt(abs(sum(PI_trans30$fit_t - PI_trans30$act)**2)/36)

RMSE.merMod(mod, scale = T)*sd(hoh_dat$SOC_stock_100)
RMSE.merMod(mod30, scale = T)*sd(hoh_dat$SOC_stock_30)


ggplot(aes(x=(hoh_dat$SOC_stock_100), y=fit**2, ymin=lwr**2, ymax=upr**2), dat = PI) +
    geom_point() +
    geom_linerange() +
    geom_smooth(aes(y = fitted(mod)**2, x = (hoh_dat$SOC_stock_100)), method = "lm", se = F) + 
    labs(x="SOC MgC ha-1", y="Prediction w/ 95% PI") + theme_bw()


kFoldCV <- function(K = 6, reps = 10, randSeed = 11){
    # Make sure nrow / K is an integer
    stopifnot("Rows not divisible by K" = (nrow(hoh_dat) %% K) == 0)
    set.seed(randSeed)
    kFoldMSE <- 0
    
    for(i in 1:reps){
        rowsLeft <- seq(1, nrow(hoh_dat)) # a sequence of row numbers 1 to 36
        kFoldMSE_temp <- 0 # A zero placeholder for MSE
        for(j in 1:K){
            # Get indices for the fold by sampling the vector of 1-36 with the size of the integer from 'floor'
            rowsTest  <- sample(x        = rowsLeft, #the 1-36 sequence
                                size     = floor(nrow(hoh_dat) / K), #takes single numeric argument (x) and returns vector containing the largest integers not greater than (x) 
                                replace  = FALSE) # don't replace the sample taken from the sequence
            # Remove those already chosen for next loop
            rowsLeft <- rowsLeft[! rowsLeft %in% rowsTest] # which row labels are left 
            
            # Split data
            dataTest  <- hoh_dat[rowsTest, ] # data that is the rows of the fold (smaller)
            dataTrain <- hoh_dat[-rowsTest,] # data that is outside the fold
            
            # Run model on K-1 folds (training data)
            mod_kFold <- lmer(formula = sqrt(SOC_stock_100) ~ WIP +(1|LITHOL),
                              data    = dataTrain,
                              REML    = F)
            # Get predicted values on test dataset, as well as true values
            pred_kFold <- predict(object  = mod_kFold,
                                  newdata = dataTest)
            actual_kFold    <- sqrt(dataTest$SOC_stock_100)
            # Calculate MSE for this fold
            kFoldMSE_temp <- kFoldMSE_temp + sum((pred_kFold - actual_kFold)^2)
        }
        # Add MSE for the given rep
        kFoldMSE <- kFoldMSE + kFoldMSE_temp
    }
    # Return average MSE across the reps
    estMSE <- kFoldMSE / reps
    names(estMSE) <- "Estimated MSE"
    return(estMSE)
}
# Test with K = 6
kFoldCV(K         = 6,
        reps      = 10,
        randSeed  = 7)
# Test with K = N = 36 -  this is LOOCV.
### Note: Tends be lower than with K < N (lower bias, higher variance)
kFoldCV(K         = 36,
        reps      = 10,
        randSeed  = 7)


############# From the pierre roudier github ##############
library(parallel)
library(doParallel)
library(terra)
library(foreach)

#k = 10
# LITHOL <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_GEO_100k_reclassified.tif")
# WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")
# rs <- c(WIP, LITHOL)
# names(rs) <- c("WIP", "LITHOL")
# predlist <- c(WIP)
#val_preds <- matrix(nrow = nrow(hoh_dat), ncol = k)

#________________________________________________________________________________

test <- as_tibble(stratified(hoh_dat, group = "LITHOL", c("alluvium_marine_water" = 5, "glac_drift"= 12, "MioEo"=3, "till_outwash"= 16), replace = TRUE))
tail(test)
#________________________________________________________________________________
            
for(i in 1:98){
        # sq_fun <- function(x) {
        #     z <- x**2
        #     return(z)
        # }
    bootData = as.data.frame(splitstackshape::stratified(hoh_dat, group = "LITHOL", 
                                                         c("alluvium_marine_water" = 5, 
                                                           "glac_drift"= 12, 
                                                           "MioEo"=3, 
                                                           "till_outwash"= 16), replace = TRUE))
    
    # Fit a Hoh SOC model 
    modBoot <- lmer(formula = sqrt(SOC_stock_30) ~ WIP + (1|LITHOL),
                    data    = bootData,
                    REML    = F)
    #modBoot <- glm(SOC_stock_100 ~ WIP + LITHOL, family = Gamma(link = "log"), data = bootData)

    #Predict onto raster
    # pred <- predict(rs, model = modBoot, allow.new.levels = T, cores = 8) 
    # terra::app(x = pred, fun = sq_fun, 
    #                        filename = paste0("bootstrapped/newboots/","boot", format(Sys.time(), "%Y%m%d_%H%M"), '.tif'), 
    #                        overwrite = T)
    pred <- terra::predict(rs, model = modBoot, allow.new.levels = TRUE, type = "response",se.fit = F,
                           filename = paste0("bootstrapped/Revisedboots/Revised_30CM/","boot_mac_30cm", 
                                             format(Sys.time(), "%Y%m%d_%H%M%S"), '.tif')) 
    
    # pred_app <- app(pred, fun = function(r){r**2}, 
    #         filename = paste0("bootstrapped/newboots/depth30cm/","boot30cm", format(Sys.time(), "%Y%m%d_%H%M"), '.tif'), 
    #         overwrite = T)
    #return(pred_app)
    #list()
    #add(predlist) <- pred
    #predstack <- rast(predlist)
    #val_preds[, i] <- predict(modBoot, newdata = rs)
    #predlist <- stack(predlist, predict(rs, modBoot, allow.new.levels = T))
    }

#system.time(terra::predict(rs, mod, allow.new.levels =T, cores = 5))

# Convert the list of results into a stack if needed
# predstack <- do.call(stack, result_list)


filelist_temp <- list.files(("bootstrapped/Revisedboots/Revised_30CM/"), full.names = TRUE)
predlist <- rast(filelist_temp)

#square the rasters to back-transform
sq <- function(x) {
    z <- x**2
    return(z)
}

#apply the back transform 
c_trans <- terra::app(predlist, sq, filename = paste0("bootstrapped/Revisedboots/Revised_30CM/", substitute(predlist),"trans.tif"), overwrite = T)
c_trans <- rast(paste0("bootstrapped/Revisedboots/Revised_30CM/", substitute(predlist),"trans.tif"))

c_interval <- terra::app(c_trans, function(x){
    quantile(x, probs = c(0.025, 0.975), na.rm = T)
}, cores = 3, filename = "SOIL CARBON/CrypticCarbonMaps_Revised/CrypticCarbon_30CM_SOC_interval95.tif", overwrite = T)
c_interval <- rast("SOIL CARBON/CrypticCarbonMaps_Revised/CrypticCarbon_1M_SOC_interval95.tif")
plot(c_interval)
writeRaster(c_interval[[2]], filename = "SOIL CARBON/CrypticCarbonMaps_Revised/CrypticCarbon_30CM_SOC_97pt5_percentile.tif")

c_interval_diff <- c_interval[[2]] - c_interval[[1]]
plot(c_interval_diff)
writeRaster(c_interval_diff, filename = "SOIL CARBON/CrypticCarbonMaps_Revised/CrypticCarbon_30CM_SOC_95difference.tif")

c_sd <- terra::app(c_trans, fun = sd,
                   filename = "SOIL CARBON/CrypticCarbonMaps_Revised/CrypticCarbon_30CM_SOC_stdev.tif", overwrite = T)
plot(c_sd)
