library(ggplot2)
library(readxl)
library(caret)
library(grid)
library(gridExtra)
library(dplyr)
library(tclust)
library(openxlsx)
library(rstudioapi)
library(caTools)
library(car)
library(quantmod)
library(MASS)
library(corrplot)
library(mgcv)
library(performance)
library(see)
library(e1071)

# clears the environment
rm(list=ls())

# set the working directory to the same location of file currently working on
setwd(dirname(getSourceEditorContext()$path))
# Flexible Bathy Depth Interval Parameters
bathy_min <- -7     # Minimum bathy depth to include (change as desired)
bathy_max <- 0       # Maximum bathy depth to include (change as desired)
depth_interval <- 2  # Interval width (meters), e.g. 2 for 0 to -2, -2 to -4, etc.

# Generate dynamic depth intervals for binning and splitting
depth_intervals <- seq(bathy_max, bathy_min, by = -depth_interval)
if (tail(depth_intervals, 1) > bathy_min) {
  depth_intervals <- c(depth_intervals, bathy_min)
}

######################################## import datasets #################################
data_05_04_2021 <- read.csv("Datasets/data_05_04_2021.csv")
data_15_04_2021 <- read.csv("Datasets/data_15_04_2021.csv")
data_17_04_2021 <- read.csv("Datasets/data_17_04_2021.csv")
data_22_04_2021 <- read.csv("Datasets/data_22_04_2021.csv")
# data_30_05_2021 <- read.csv("Datasets/data_30_05_2021.csv")
data_04_06_2021 <- read.csv("Datasets/data_04_06_2021.csv")
# data_21_06_2021 <- read.csv("Datasets/data_21_06_2021.csv")
# data_29_06_2021 <- read.csv("Datasets/data_29_06_2021.csv")
data_16_07_2021 <- read.csv("Datasets/data_16_07_2021.csv")
data_19_07_2021 <- read.csv("Datasets/data_19_07_2021.csv")
data_21_07_2021 <- read.csv("Datasets/data_21_07_2021.csv")
# data_25_08_2021 <- read.csv("Datasets/data_25_08_2021.csv")
data_28_08_2021 <- read.csv("Datasets/data_28_08_2021.csv")
data_14_09_2021 <- read.csv("Datasets/data_14_09_2021.csv")
# Bathy_10m_backscatter <- read.csv("Datasets/Bathy_10m_backscatter.csv")
train_points <- read.csv("train_points.csv")
validation_points <- read.csv("validation_points.csv")

# combine datasets into one list
all_datasets <- list(data_05_04_2021, data_15_04_2021, data_17_04_2021, data_22_04_2021, 
                     data_04_06_2021, data_16_07_2021, data_19_07_2021, 
                     data_21_07_2021, data_28_08_2021, data_14_09_2021)

# dates of cloud-free images (10 images in 2021)
dates <- c('05/04/2021', '15/04/2021', '17/04/2021', '22/04/2021', '04/06/2021', 
           '16/07/2021', '19/07/2021', '21/07/2021', '28/08/2021', '14/09/2021')

# add date column to each dataset
all_datasets <- mapply(cbind, all_datasets, "Date"=dates, SIMPLIFY=F)

# add backscatter info to datasets
for (i in 1:length(dates)) {
  # Use train_points and validation_points to join backscatter info if needed
  # You may need to adjust depending on your join requirements
  all_datasets[[i]] <- inner_join(all_datasets[[i]], train_points[, c("FID","Mean.Total")], by=c('FID_'='FID'))
}

######################################### SPLIT DATASETS #########################################
# Now use the spatially independent splits
# ALL downstream code for modeling, metrics, plots, etc. will use train_points as training and validation_points as validation.
# No more random sampling or splitting needed!

######################################## Lyzyenga regression models on single images ##########################################

R2 <- rep(NA, length(all_datasets))
RMSE <- rep(NA, length(all_datasets))
MAE <- rep(NA, length(all_datasets))
MBE <- rep(NA, length(all_datasets))
AIC_Lyz <- rep(NA, length(all_datasets))
LE95_Lyzenga <- rep(NA, length(all_datasets))
training_datasets <- list()
validation_datasets <- list()

validation_datasets_0_2 <- list()
validation_datasets_2_4 <- list()
validation_datasets_4_6 <- list()
validation_datasets_6_8 <- list()
validation_datasets_8_10 <- list()

R2_split <- NULL
RMSE_split <- NULL
MAE_split <- NULL
n_split <- NULL
coll <- NULL

for (i in 1:length(dates)) {
  # Use train_points and validation_points directly
  training_datasets[[i]] <- train_points
  validation_datasets[[i]] <- validation_points
  
  lyzenga_2021 <- lm(Bathy_MSL~blue+green+red, data = training_datasets[[i]])
  
  validation_datasets[[i]] <- validation_datasets[[i]][, c('Bathy_MSL', 'blue', 'green','red', 'Date')]
  validation_datasets[[i]]$estimated <- predict(lyzenga_2021, newdata = subset(validation_datasets[[i]], select= -c(Bathy_MSL)), interval = 'confidence')[,1]
  LE95_Lyzenga[i] <- quantile(abs(validation_datasets[[i]]$Bathy_MSL - validation_datasets[[i]]$estimated), probs = c(0.95))
  
  R2[i] = R2(validation_datasets[[i]]$estimated, validation_datasets[[i]]$Bathy_MSL)
  RMSE[i] = RMSE(validation_datasets[[i]]$Bathy_MSL, validation_datasets[[i]]$estimated)
  MAE[i] = MAE(validation_datasets[[i]]$Bathy_MSL, validation_datasets[[i]]$estimated)
  MBE[i] = mean(validation_datasets[[i]]$Bathy_MSL - validation_datasets[[i]]$estimated)
  AIC_Lyz[i] = AIC(lyzenga_2021)
  
  validation_datasets_0_2[[i]] <- validation_datasets[[i]][validation_datasets[[i]]$Bathy_MSL >= -2,]
  validation_datasets_2_4[[i]] <- validation_datasets[[i]][validation_datasets[[i]]$Bathy_MSL >= -4 & validation_datasets[[i]]$Bathy_MSL < -2,]
  validation_datasets_4_6[[i]] <- validation_datasets[[i]][validation_datasets[[i]]$Bathy_MSL >= -6 & validation_datasets[[i]]$Bathy_MSL < -4,]
  validation_datasets_6_8[[i]] <- validation_datasets[[i]][validation_datasets[[i]]$Bathy_MSL >= -8 & validation_datasets[[i]]$Bathy_MSL < -6,]
  validation_datasets_8_10[[i]] <- validation_datasets[[i]][validation_datasets[[i]]$Bathy_MSL < -8,]
  
  R2_split <- c(R2_split, c(R2(validation_datasets_0_2[[i]]$estimated, validation_datasets_0_2[[i]]$Bathy_MSL),
                            R2(validation_datasets_2_4[[i]]$estimated, validation_datasets_2_4[[i]]$Bathy_MSL),
                            R2(validation_datasets_4_6[[i]]$estimated, validation_datasets_4_6[[i]]$Bathy_MSL),
                            R2(validation_datasets_6_8[[i]]$estimated, validation_datasets_6_8[[i]]$Bathy_MSL),
                            R2(validation_datasets_8_10[[i]]$estimated, validation_datasets_8_10[[i]]$Bathy_MSL)))
  RMSE_split <- c(RMSE_split, c(RMSE(validation_datasets_0_2[[i]]$estimated, validation_datasets_0_2[[i]]$Bathy_MSL),
                                RMSE(validation_datasets_2_4[[i]]$estimated, validation_datasets_2_4[[i]]$Bathy_MSL),
                                RMSE(validation_datasets_4_6[[i]]$estimated, validation_datasets_4_6[[i]]$Bathy_MSL),
                                RMSE(validation_datasets_6_8[[i]]$estimated, validation_datasets_6_8[[i]]$Bathy_MSL),
                                RMSE(validation_datasets_8_10[[i]]$estimated, validation_datasets_8_10[[i]]$Bathy_MSL)))
  MAE_split <- c(MAE_split, c(MAE(validation_datasets_0_2[[i]]$estimated, validation_datasets_0_2[[i]]$Bathy_MSL),
                              MAE(validation_datasets_2_4[[i]]$estimated, validation_datasets_2_4[[i]]$Bathy_MSL),
                              MAE(validation_datasets_4_6[[i]]$estimated, validation_datasets_4_6[[i]]$Bathy_MSL),
                              MAE(validation_datasets_6_8[[i]]$estimated, validation_datasets_6_8[[i]]$Bathy_MSL),
                              MAE(validation_datasets_8_10[[i]]$estimated, validation_datasets_8_10[[i]]$Bathy_MSL)))
  n_split <- c(n_split, c(nrow(validation_datasets_0_2[[i]]),
                          nrow(validation_datasets_2_4[[i]]),
                          nrow(validation_datasets_4_6[[i]]),
                          nrow(validation_datasets_6_8[[i]]),
                          nrow(validation_datasets_8_10[[i]])))
  
  coll <- c(coll, vif(lyzenga_2021))
  check <- check_collinearity(lyzenga_2021)
  
}

coll_table <- data.frame(c("blue", "green", "red"),coll, rep(dates, each=3))

metrics_10m_lyzenga <- data.frame(dates, R2, RMSE, MAE, MBE, AIC_Lyz, LE95_Lyzenga)
metrics_10m_lyzenga_split <- data.frame(dates=rep(dates, each=5), R2_split, RMSE_split, MAE_split, n_split, 
                                        depth=rep(c("0-2", "2-4", "4-6", "6-8", "8-10"), times=length(all_datasets)))

######################################## Stumpf regression models on single images ##########################################

R2 <- rep(NA, length(all_datasets))
RMSE <- rep(NA, length(all_datasets))
MAE <- rep(NA, length(all_datasets))
MBE <- rep(NA, length(all_datasets))
AIC_Stumpf <- rep(NA, length(all_datasets))
LE95_Stumpf <- rep(NA, length(all_datasets))
training_datasets <- list()
validation_datasets <- list()

for (i in 1:length(dates)) {
  training_datasets[[i]] <- train_points
  validation_datasets[[i]] <- validation_points
  training_datasets[[i]]$BG <- training_datasets[[i]]$blue/training_datasets[[i]]$green
  validation_datasets[[i]]$BG <- validation_datasets[[i]]$blue/validation_datasets[[i]]$green
  stumpf_2021 <- lm(Bathy_MSL~BG, data = training_datasets[[i]])
  validation_2021 <- validation_datasets[[i]][, c('Bathy_MSL','BG')]
  validation_2021$estimated <- predict(stumpf_2021, newdata = subset(validation_2021, select= -c(Bathy_MSL)), interval = 'confidence')[,1]
  LE95_Stumpf[i] <- quantile(abs(validation_2021$Bathy_MSL - validation_2021$estimated), probs = c(0.95))
  
  print(ggplot(validation_2021, aes(x=estimated ,y=Bathy_MSL)) +
          geom_point(size = 1) + geom_abline(slope=1, intercept=0) + xlab('Estimated depth') +
          ylab('Actual depth') + scale_x_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) +
          scale_y_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) + geom_abline(slope=1, intercept=0.5, col="red") +
          geom_abline(slope=1, intercept=-0.5, col="red") +
          geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.24,vjust=2,label=paste("Image", i)), size=6) +
          geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.3,vjust=3.5,label="Stumpf"), size=6))
  
  R2[i] = R2(validation_2021$estimated, validation_2021$Bathy_MSL)
  RMSE[i] = RMSE(validation_2021$estimated, validation_2021$Bathy_MSL)
  MAE[i] = MAE(validation_2021$estimated, validation_2021$Bathy_MSL)
  MBE[i] = mean(validation_2021$estimated-validation_2021$Bathy_MSL)
  AIC_Stumpf[i] = AIC(stumpf_2021)
}

metrics_10m_stumpf <- data.frame(dates, R2, RMSE, MAE, MBE, AIC_Stumpf, LE95_Stumpf)

######################################## GLM regression models on single images ########################################

R2 <- rep(NA, length(all_datasets))
RMSE <- rep(NA, length(all_datasets))
MAE <- rep(NA, length(all_datasets))
MBE <- rep(NA, length(all_datasets))
AIC_inter <- rep(NA, length(all_datasets))
LE95_GLM <- rep(NA, length(all_datasets))
training_datasets_GLM <- list()
validation_datasets_GLM <- list()
pVal <- NULL
corr <- NULL

validation_datasets_0_2 <- list()
validation_datasets_2_4 <- list()
validation_datasets_4_6 <- list()
validation_datasets_6_8 <- list()
validation_datasets_8_10 <- list()

GLM_plots <- list()

R2_split_inter <- NULL
RMSE_split_inter <- NULL
MAE_split_inter <- NULL
n_split_inter <- NULL

options("scipen"=100, "digits"=4)

for (i in 1:length(dates)) {
  training_datasets_GLM[[i]] <- train_points
  validation_datasets_GLM[[i]] <- validation_points
  
  lyzenga_2021 <- lm(Bathy_MSL~(blue+green+red)^2, data = training_datasets_GLM[[i]])
  
  validation_datasets_GLM[[i]] <- validation_datasets_GLM[[i]][, c('Bathy_MSL', 'FID_', 'blue', 'green','red', 'Date')]
  validation_datasets_GLM[[i]]$estimated <- predict(lyzenga_2021, newdata = subset(validation_datasets_GLM[[i]], select= -c(Bathy_MSL, FID_)), interval = 'confidence')[,1]
  LE95_GLM[i] <- quantile(abs(validation_datasets_GLM[[i]]$Bathy_MSL - validation_datasets_GLM[[i]]$estimated), probs = c(0.95))
  
  current_label <- paste("Image", i)
  GLM_plots[[i]] <- (ggplot(validation_datasets_GLM[[i]], aes(x=estimated ,y=Bathy_MSL)) +
                       geom_point(size = 1) + geom_abline(slope=1, intercept=0) + xlab('Estimated depth') +
                       ylab('Actual depth') + scale_x_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) +
                       scale_y_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) + geom_abline(slope=1, intercept=0.5, col="red") +
                       geom_abline(slope=1, intercept=-0.5, col="red") +
                       geom_text(x=-Inf,y=Inf,hjust=-0.25,vjust=2,label=current_label, size=4))
  
  R2[i] = R2(validation_datasets_GLM[[i]]$estimated, validation_datasets_GLM[[i]]$Bathy_MSL)
  RMSE[i] = RMSE(validation_datasets_GLM[[i]]$estimated, validation_datasets_GLM[[i]]$Bathy_MSL)
  MAE[i] = MAE(validation_datasets_GLM[[i]]$estimated, validation_datasets_GLM[[i]]$Bathy_MSL)
  MBE[i] = mean(validation_datasets_GLM[[i]]$estimated-validation_datasets_GLM[[i]]$Bathy_MSL)
  AIC_inter[i] = AIC(lyzenga_2021)
  corr <- c(corr, c(cor(validation_datasets_GLM[[i]]$blue, validation_datasets_GLM[[i]]$green),
                    cor(validation_datasets_GLM[[i]]$blue, validation_datasets_GLM[[i]]$red),
                    cor(validation_datasets_GLM[[i]]$green, validation_datasets_GLM[[i]]$red)))
  pVal <- c(pVal, summary(lyzenga_2021)$coeff[c(5:7),4])
  
  validation_datasets_0_2[[i]] <- validation_datasets_GLM[[i]][validation_datasets_GLM[[i]]$Bathy_MSL >= -2,]
  validation_datasets_2_4[[i]] <- validation_datasets_GLM[[i]][validation_datasets_GLM[[i]]$Bathy_MSL >= -4 & validation_datasets_GLM[[i]]$Bathy_MSL < -2,]
  validation_datasets_4_6[[i]] <- validation_datasets_GLM[[i]][validation_datasets_GLM[[i]]$Bathy_MSL >= -6 & validation_datasets_GLM[[i]]$Bathy_MSL < -4,]
  validation_datasets_6_8[[i]] <- validation_datasets_GLM[[i]][validation_datasets_GLM[[i]]$Bathy_MSL >= -8 & validation_datasets_GLM[[i]]$Bathy_MSL < -6,]
  validation_datasets_8_10[[i]] <- validation_datasets_GLM[[i]][validation_datasets_GLM[[i]]$Bathy_MSL < -8,]
  
  R2_split_inter <- c(R2_split_inter, c(R2(validation_datasets_0_2[[i]]$estimated, validation_datasets_0_2[[i]]$Bathy_MSL),
                                        R2(validation_datasets_2_4[[i]]$estimated, validation_datasets_2_4[[i]]$Bathy_MSL),
                                        R2(validation_datasets_4_6[[i]]$estimated, validation_datasets_4_6[[i]]$Bathy_MSL),
                                        R2(validation_datasets_6_8[[i]]$estimated, validation_datasets_6_8[[i]]$Bathy_MSL),
                                        R2(validation_datasets_8_10[[i]]$estimated, validation_datasets_8_10[[i]]$Bathy_MSL)))
  RMSE_split_inter <- c(RMSE_split_inter, c(RMSE(validation_datasets_0_2[[i]]$estimated, validation_datasets_0_2[[i]]$Bathy_MSL),
                                            RMSE(validation_datasets_2_4[[i]]$estimated, validation_datasets_2_4[[i]]$Bathy_MSL),
                                            RMSE(validation_datasets_4_6[[i]]$estimated, validation_datasets_4_6[[i]]$Bathy_MSL),
                                            RMSE(validation_datasets_6_8[[i]]$estimated, validation_datasets_6_8[[i]]$Bathy_MSL),
                                            RMSE(validation_datasets_8_10[[i]]$estimated, validation_datasets_8_10[[i]]$Bathy_MSL)))
  MAE_split_inter <- c(MAE_split_inter, c(MAE(validation_datasets_0_2[[i]]$estimated, validation_datasets_0_2[[i]]$Bathy_MSL),
                                          MAE(validation_datasets_2_4[[i]]$estimated, validation_datasets_2_4[[i]]$Bathy_MSL),
                                          MAE(validation_datasets_4_6[[i]]$estimated, validation_datasets_4_6[[i]]$Bathy_MSL),
                                          MAE(validation_datasets_6_8[[i]]$estimated, validation_datasets_6_8[[i]]$Bathy_MSL),
                                          MAE(validation_datasets_8_10[[i]]$estimated, validation_datasets_8_10[[i]]$Bathy_MSL)))
  n_split_inter <- c(n_split_inter, c(nrow(validation_datasets_0_2[[i]]),
                                      nrow(validation_datasets_2_4[[i]]),
                                      nrow(validation_datasets_4_6[[i]]),
                                      nrow(validation_datasets_6_8[[i]]),
                                      nrow(validation_datasets_8_10[[i]])))
}

corr_table <- data.frame(c("blue:green", "blue:red", "green:red"),corr, rep(dates, each=3))
pVal_table <- data.frame(c("blue:green", "blue:red", "green:red"), rep(dates, each=3), pVal)

metrics_10m_GLM <- data.frame(dates, R2, RMSE, MBE, MAE, AIC_inter, LE95_GLM)
metrics_10m_GLM_split <- data.frame(dates=rep(dates, each=5), R2_split_inter, RMSE_split_inter, 
                                    MAE_split_inter, n_split_inter,
                                    depth=rep(c("0-2", "2-4", "4-6", "6-8", "8-10"), times=length(all_datasets)))
grid.arrange(grobs = GLM_plots, nrow = 4, ncol = 3, layout_matrix= rbind(c(1:3), c(4:6), c(7:9) , c(NA,10,NA)))
# (continuing seamlessly after previous lines...)

######################################## Multi-image analysis ####################################

all_good_datasets <- all_datasets

# compute the mean and median across images for each pixel
all_mean_cluster <- bind_rows(all_good_datasets) %>%
  group_by(FID_, Bathy_MSL) %>%
  summarise(blue = mean(blue), green = mean(green), red = mean(red))
all_median_cluster <- bind_rows(all_good_datasets) %>%
  group_by(FID_, Bathy_MSL) %>%
  summarise(blue = median(blue), green = median(green), red = median(red))
all_mean_cluster$training <- all_mean_cluster$FID_ %in% train_points$FID_
all_median_cluster$training <- all_median_cluster$FID_ %in% train_points$FID_

training_datasets_mean_comp  <- all_mean_cluster[all_mean_cluster$training==TRUE, ]
validation_datasets_mean_comp <- all_mean_cluster[all_mean_cluster$training==FALSE, ]
training_datasets_median_comp  <- all_median_cluster[all_median_cluster$training==TRUE, ]
validation_datasets_median_comp <- all_median_cluster[all_median_cluster$training==FALSE, ]

# train the model
lyzenga_2021_mean <- lm(Bathy_MSL~(blue+green+red)^2, data = training_datasets_mean_comp)
lyzenga_2021_median <- lm(Bathy_MSL~(blue+green+red)^2, data = training_datasets_median_comp)
# calculate estimated depth on validation datasets
validation_datasets_mean_comp <- validation_datasets_mean_comp[, c('Bathy_MSL', 'FID_', 'blue', 'green','red')]
validation_datasets_median_comp <- validation_datasets_median_comp[, c('Bathy_MSL', 'FID_', 'blue', 'green','red')]

validation_datasets_mean_comp$estimated <- predict(lyzenga_2021_mean, newdata = subset(validation_datasets_mean_comp, select= -c(Bathy_MSL, FID_)), interval = 'confidence')[,1]
validation_datasets_median_comp$estimated <- predict(lyzenga_2021_median, newdata = subset(validation_datasets_median_comp, select= -c(Bathy_MSL, FID_)), interval = 'confidence')[,1]
LE95_mean_comp <- quantile(abs(validation_datasets_mean_comp$Bathy_MSL - validation_datasets_mean_comp$estimated), probs = c(0.95))
LE95_median_comp <- quantile(abs(validation_datasets_median_comp$Bathy_MSL - validation_datasets_median_comp$estimated), probs = c(0.95))

# plot scatterplots estimated vs. actual for both mean and median reducers
plot_rsquared <- paste0("R^2 ==", 0.97)
print(ggplot(validation_datasets_mean_comp, aes(x=estimated ,y=Bathy_MSL)) +
        geom_point(size = 1) + geom_abline(slope=1, intercept=0) + xlab('Estimated depth') +
        ylab('Actual depth') + scale_x_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) +
        scale_y_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) + geom_abline(slope=1, intercept=0.5, col="red") +
        geom_abline(slope=1, intercept=-0.5, col="red")+
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.07,vjust=1.5,label="Multi-image (10 images)"), size=6)+
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.12,vjust=3,label="Mean reducer"), size=6)+
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.17,vjust=3.1,label=plot_rsquared), parse=TRUE,size=6)+
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.10,vjust=6,label="RMSE = 0.45 m"), size=6)+
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.12,vjust=7.5,label="MAE = 0.32 m"), size=6))
# Residual vs Fitted plot with 1m intervals and parallel lines at Y=0.5 and Y=-0.5
validation_datasets_mean_comp$fitted_values <- predict(lyzenga_2021_mean, newdata = validation_datasets_mean_comp)
validation_datasets_mean_comp$residuals <- validation_datasets_mean_comp$Bathy_MSL - validation_datasets_mean_comp$fitted_values

residuals_vs_fitted_plot_mean <- ggplot(data=validation_datasets_mean_comp, aes(x=fitted_values, y=residuals)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  geom_hline(yintercept=0.5, linetype="dashed", color="red") +
  geom_hline(yintercept=-0.5, linetype="dashed", color="red") +
  scale_x_continuous(breaks=seq(floor(min(validation_datasets_mean_comp$fitted_values)), ceiling(max(validation_datasets_mean_comp$fitted_values)), by=1)) +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residuals vs Fitted for Multi-image (10 images) Mean Reducer")

# Save plot as image
ggsave(filename="residuals_vs_fitted_multi_image_mean.png", plot=residuals_vs_fitted_plot_mean)

# Residual vs Fitted plot for GLM best image 6 with 1m intervals and parallel lines at Y=0.5 and Y=-0.5
lyzenga_2021_image6 <- lm(Bathy_MSL ~ (blue + green + red)^2, data = training_datasets_GLM[[6]])
validation_datasets_GLM[[6]]$fitted_values <- predict(lyzenga_2021_image6, newdata = validation_datasets_GLM[[6]])
validation_datasets_GLM[[6]]$residuals <- validation_datasets_GLM[[6]]$Bathy_MSL - validation_datasets_GLM[[6]]$fitted_values

residuals_vs_fitted_plot_glm <- ggplot(data=validation_datasets_GLM[[6]], aes(x=fitted_values, y=residuals)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  geom_hline(yintercept=0.5, linetype="dashed", color="red") +
  geom_hline(yintercept=-0.5, linetype="dashed", color="red") +
  scale_x_continuous(breaks=seq(floor(min(validation_datasets_GLM[[6]]$fitted_values)), ceiling(max(validation_datasets_GLM[[6]]$fitted_values)), by=1)) +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residuals vs Fitted for GLM best image 6")

ggsave(filename="residuals_vs_fitted_glm_best_image_6.png", plot=residuals_vs_fitted_plot_glm)
# QQ plot for GLM best image 6
qq_plot_glm <- ggplot(data=validation_datasets_GLM[[6]], aes(sample=residuals)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for GLM best image 6")

ggsave(filename="qq_plot_glm_best_image_6.png", plot=qq_plot_glm)
plot_rsquared <- paste0("R^2 ==", 0.84)
print(ggplot(validation_datasets_median_comp, aes(x=estimated ,y=Bathy_MSL)) +
        geom_point(size = 1) + geom_abline(slope=1, intercept=0) + xlab('Estimated depth') +
        ylab('Actual depth') + scale_x_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) +
        scale_y_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) + geom_abline(slope=1, intercept=0.5, col="red") +
        geom_abline(slope=1, intercept=-0.5, col="red")+
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.07,vjust=1.5,label="Multi-image (10 images)"), size=6)+
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.1,vjust=3,label="Median reducer"), size=6)+
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.17,vjust=3.1,label=plot_rsquared), parse=TRUE, size=6)+
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.10,vjust=6,label="RMSE = 1.01 m"), size=6)+
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.11,vjust=7.5,label="MAE = 0.81 m"), size=6))
plot_rsquared <- paste0("R^2 ==", 0.91)
print(ggplot(validation_datasets_GLM[[6]], aes(x=estimated ,y=Bathy_MSL)) +
        geom_point(size = 1) + geom_abline(slope=1, intercept=0) + xlab('Estimated depth') +
        ylab('Actual depth') + scale_x_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) +
        scale_y_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) + geom_abline(slope=1, intercept=0.5, col="red") +
        geom_abline(slope=1, intercept=-0.5, col="red")+
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.12,vjust=1.5,label="Single-image"), size=6)+
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.2,vjust=3,label="Image 6"), size=6)+
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.17,vjust=3.1,label=plot_rsquared), parse=TRUE, size=6)+
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.10,vjust=6,label="RMSE = 0.76 m"), size=6)+
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.12,vjust=7.5,label="MAE = 0.57 m"), size=6))
qq_plot_mean <- ggplot(data=validation_datasets_mean_comp, aes(sample=residuals)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for Multi-image (10 images) Mean Reducer")

ggsave(filename="qq_plot_multi_image_mean.png", plot=qq_plot_mean)

mean_reducer_table <- validation_datasets_mean_comp %>%
  mutate(RMSE = RMSE(validation_datasets_mean_comp$estimated, validation_datasets_mean_comp$Bathy_MSL),
         MAE = MAE(validation_datasets_mean_comp$estimated, validation_datasets_mean_comp$Bathy_MSL),
         MBE = mean(validation_datasets_mean_comp$estimated - validation_datasets_mean_comp$Bathy_MSL)) %>%
  select(FID_, Bathy_MSL, estimated, RMSE, MAE, MBE)

write.csv(mean_reducer_table, "mean_reducer_table.csv", row.names = FALSE)

glm_best_image6_table <- validation_datasets_GLM[[6]] %>%
  mutate(RMSE = RMSE(validation_datasets_GLM[[6]]$estimated, validation_datasets_GLM[[6]]$Bathy_MSL),
         MAE = MAE(validation_datasets_GLM[[6]]$estimated, validation_datasets_GLM[[6]]$Bathy_MSL),
         MBE = mean(validation_datasets_GLM[[6]]$estimated - validation_datasets_GLM[[6]]$Bathy_MSL)) %>%
  dplyr::select(FID_, Bathy_MSL, estimated, RMSE, MAE, MBE)

write.csv(glm_best_image6_table, "glm_best_image6_table.csv", row.names = FALSE)
R2_mean = R2(validation_datasets_mean_comp$estimated, validation_datasets_mean_comp$Bathy_MSL)
RMSE_mean = RMSE(validation_datasets_mean_comp$estimated, validation_datasets_mean_comp$Bathy_MSL)
MAE_mean = MAE(validation_datasets_mean_comp$estimated, validation_datasets_mean_comp$Bathy_MSL)
MBE_mean = mean(validation_datasets_mean_comp$estimated - validation_datasets_mean_comp$Bathy_MSL)
R2_median = R2(validation_datasets_median_comp$estimated, validation_datasets_median_comp$Bathy_MSL)
RMSE_median = RMSE(validation_datasets_median_comp$estimated, validation_datasets_median_comp$Bathy_MSL)
MAE_median = MAE(validation_datasets_median_comp$estimated, validation_datasets_median_comp$Bathy_MSL)
MBE_median = mean(validation_datasets_median_comp$estimated - validation_datasets_median_comp$Bathy_MSL)

metrics_comp <- data.frame("all images", R2_mean, RMSE_mean, MAE_mean, MBE_mean, LE95_mean_comp, R2_median, RMSE_median, MAE_median, MBE_median, LE95_median_comp)

## analysis on 0-7 m water depth only for the best combination (=X7)
all_datasets_0_7 <- list(data_05_04_2021, data_15_04_2021, data_17_04_2021, 
                         data_04_06_2021, data_16_07_2021, data_19_07_2021, 
                         data_21_07_2021)

all_good_datasets <- lapply(all_datasets_0_7, function(tbl) {
  tbl[tbl$Bathy_MSL >= -7, ]
})

all_mean_cluster <- bind_rows(all_good_datasets) %>%
  group_by(FID_, Bathy_MSL) %>%
  summarise(blue = mean(blue), green = mean(green), red = mean(red))
all_median_cluster <- bind_rows(all_good_datasets) %>%
  group_by(FID_, Bathy_MSL) %>%
  summarise(blue = median(blue), green = median(green), red = median(red))
all_mean_cluster$training <- all_mean_cluster$FID_ %in% train_points$FID_
all_median_cluster$training <- all_median_cluster$FID_ %in% train_points$FID_

training_datasets_mean_comp  <- all_mean_cluster[all_mean_cluster$training==TRUE, ]
validation_datasets_mean_comp <- all_mean_cluster[all_mean_cluster$training==FALSE, ]
training_datasets_median_comp  <- all_median_cluster[all_median_cluster$training==TRUE, ]
validation_datasets_median_comp <- all_median_cluster[all_median_cluster$training==FALSE, ]

lyzenga_2021_mean <- lm(Bathy_MSL~(blue+green+red)^2, data = training_datasets_mean_comp)
lyzenga_2021_median <- lm(Bathy_MSL~(blue+green+red)^2, data = training_datasets_median_comp)
validation_datasets_mean_comp <- validation_datasets_mean_comp[, c('Bathy_MSL', 'FID_', 'blue', 'green','red')]
validation_datasets_median_comp <- validation_datasets_median_comp[, c('Bathy_MSL', 'FID_', 'blue', 'green','red')]

validation_datasets_mean_comp$estimated <- predict(lyzenga_2021_mean, newdata = subset(validation_datasets_mean_comp, select= -c(Bathy_MSL, FID_)), interval = 'confidence')[,1]
validation_datasets_median_comp$estimated <- predict(lyzenga_2021_median, newdata = subset(validation_datasets_median_comp, select= -c(Bathy_MSL, FID_)), interval = 'confidence')[,1]
LE95_mean_comp <- quantile(abs(validation_datasets_mean_comp$Bathy_MSL - validation_datasets_mean_comp$estimated), probs = c(0.95))
LE95_median_comp <- quantile(abs(validation_datasets_median_comp$Bathy_MSL - validation_datasets_median_comp$estimated), probs = c(0.95))

R2_mean = R2(validation_datasets_mean_comp$estimated, validation_datasets_mean_comp$Bathy_MSL)
RMSE_mean = RMSE(validation_datasets_mean_comp$estimated, validation_datasets_mean_comp$Bathy_MSL)
MAE_mean = MAE(validation_datasets_mean_comp$estimated, validation_datasets_mean_comp$Bathy_MSL)
MBE_mean = mean(validation_datasets_mean_comp$estimated - validation_datasets_mean_comp$Bathy_MSL)
R2_median = R2(validation_datasets_median_comp$estimated, validation_datasets_median_comp$Bathy_MSL)
RMSE_median = RMSE(validation_datasets_median_comp$estimated, validation_datasets_median_comp$Bathy_MSL)
MAE_median = MAE(validation_datasets_median_comp$estimated, validation_datasets_median_comp$Bathy_MSL)
MBE_median = mean(validation_datasets_median_comp$estimated - validation_datasets_median_comp$Bathy_MSL)

metrics_comp_0_7 <- data.frame("all images", R2_mean, RMSE_mean, MAE_mean, MBE_mean, LE95_mean_comp, R2_median, RMSE_median, MAE_median, MBE_median, LE95_median_comp)


######################################## Best image combination algorithm ###################################################

n <- length(all_datasets)

R2_sub <- c(R2_mean, rep(NA, n-1))
RMSE_sub <- c(RMSE_mean, rep(NA, n-1))
MAE_sub <- c(MAE_mean, rep(NA, n-1))
cluster <- mapply(cbind, all_datasets, "Date"=dates, SIMPLIFY=F)
names(cluster) <- dates
combination = data.frame(matrix(nrow = n, ncol = n)) 
colnames(combination) <- paste("combination",c(1:n))
combination[,1] <- c(dates)
LE95_sub <- rep(NA, n)
LE95_sub[1] <- quantile(abs(validation_datasets_mean_comp$Bathy_MSL - validation_datasets_mean_comp$estimated), probs = c(0.95))

for (i in 0:(n-2)) {
  # identify the image with the highest RMSE
  r2 <- which(metrics_10m_GLM[metrics_10m_GLM$dates %in% dates,]$RMSE==sort(metrics_10m_GLM[metrics_10m_GLM$dates %in% dates,]$RMSE, decreasing=FALSE)[n-i])
  gr <- grep(dates[r2], lapply(cluster, head))
  cluster <- cluster[-gr]
  
  # apply the mean to the subset of images to create the pseudo-image
  subset_mean_cluster <- bind_rows(cluster) %>%
    group_by(FID_, Bathy_MSL, Mean.Total) %>%
    summarise(blue = mean(blue), green = mean(green), red = mean(red))
  
  subset_mean_cluster$training <- subset_mean_cluster$FID_ %in% train_points$FID_
  training_datasets_sub  <- subset_mean_cluster[subset_mean_cluster$training==TRUE, ]
  validation_datasets_sub <- subset_mean_cluster[subset_mean_cluster$training==FALSE, ]
  
  lyzenga_2021 <- lm(Bathy_MSL~(blue+green+red)^2, data = training_datasets_sub)
  validation_datasets_sub <- validation_datasets_sub[, c('Bathy_MSL', 'FID_', 'Mean.Total', 'blue', 'green','red')]
  validation_datasets_sub$estimated <- predict(lyzenga_2021, newdata = subset(validation_datasets_sub, select= -c(Bathy_MSL, FID_)), interval = 'confidence')[,1]
  LE95_sub[i+2] <- quantile(abs(validation_datasets_sub$Bathy_MSL - validation_datasets_sub$estimated), probs = c(0.95))
  
  R2_sub[i+2] = R2(validation_datasets_sub$estimated, validation_datasets_sub$Bathy_MSL)
  RMSE_sub[i+2] = RMSE(validation_datasets_sub$estimated, validation_datasets_sub$Bathy_MSL)
  MAE_sub[i+2] = MAE(validation_datasets_sub$estimated, validation_datasets_sub$Bathy_MSL)
  
  combination[,i+2] <- c(names(cluster), rep(NA, n-length(names(cluster))))
}

metrics_comp_sub <- data.frame(combination=paste("combination",c(1:n)), R2_sub, RMSE_sub, MAE_sub, LE95_sub)

best_combo <- metrics_comp_sub$combination[metrics_comp_sub$RMSE_sub==min(metrics_comp_sub$RMSE_sub)]
dates_best_combo <- na.exclude(combination[,best_combo])
cluster <- mapply(cbind, all_datasets, SIMPLIFY=F)
final_images <- cluster[which(dates %in% dates_best_combo)]

transposed <- setNames(as.data.frame(t(metrics_comp_sub[-1])), metrics_comp_sub[,1])
final_table <- rbind(combination, transposed)
# write.xlsx(final_table, 'Best_GLM_combi.xlsx')

######################################## All combo ######################################

all_good_datasets <- all_datasets

test <- list()
for (i in c(1:length(all_good_datasets))) {
  test[[i]] <- combn(unique(all_good_datasets), i)
}

dates_list <- list()
for (i in c(1:length(all_good_datasets))) {
  dates_list[[i]] <- combn(unique(dates), i)
}

lengths <- lapply(dates_list, ncol)
n <- lengths[[which.max(lengths)]]

my_dataframe <- list()
for (i in c(1:length(all_good_datasets))) {
  my_dataframe[[i]] <- as.data.frame(dates_list[[i]])
}

gr <- do.call("merge", c(lapply(list(my_dataframe[[1]], my_dataframe[[2]]), data.frame, row.names=NULL), by = 0, all = TRUE))[-1]
colnames(gr) <- paste("combo", c(1:ncol(gr)))

for (i in 3:length(all_good_datasets)) {
  gr <- do.call("merge", c(lapply(list(gr, my_dataframe[[i]]), data.frame, row.names=NULL), by = 0, all = TRUE))[-1]
  colnames(gr) <- paste("combo", c(1:ncol(gr)))
}

R2_all_combo <- list()
RMSE_all_combo <- list()
MAE_all_combo <- list()
MBE_all_combo <- list()

for (i in 1:length(all_good_datasets)) {
  R2_sub <- list()
  RMSE_sub <- list()
  MAE_sub <- list()
  MBE_sub <- list()
  for (j in 1:ncol(test[[i]])) {
    subset_mean_cluster <- bind_rows(test[[i]][,j]) %>%
      group_by(FID_, Bathy_MSL, Mean.Total) %>%
      summarise(blue = mean(blue), green = mean(green), red = mean(red))
    
    subset_mean_cluster$training <- subset_mean_cluster$FID_ %in% train_points$FID_
    training_datasets_sub  <- subset_mean_cluster[subset_mean_cluster$training==TRUE, ]
    validation_datasets_sub <- subset_mean_cluster[subset_mean_cluster$training==FALSE, ]
    
    lyzenga_2021 <- lm(Bathy_MSL~(blue+green+red)^2, data = training_datasets_sub)
    validation_datasets_sub <- validation_datasets_sub[, c('Bathy_MSL', 'FID_', 'Mean.Total', 'blue', 'green','red')]
    validation_datasets_sub$estimated <- predict(lyzenga_2021, newdata = subset(validation_datasets_sub, select= -c(Bathy_MSL, FID_)), interval = 'confidence')[,1]
    
    R2_sub[[j]] = R2(validation_datasets_sub$estimated, validation_datasets_sub$Bathy_MSL)
    RMSE_sub[[j]] = RMSE(validation_datasets_sub$estimated, validation_datasets_sub$Bathy_MSL)
    MAE_sub[[j]] = MAE(validation_datasets_sub$estimated, validation_datasets_sub$Bathy_MSL)
    MBE_sub[[j]] = mean(validation_datasets_sub$estimated - validation_datasets_sub$Bathy_MSL)
  }
  R2_all_combo <- append(R2_all_combo, R2_sub)
  RMSE_all_combo <- append(RMSE_all_combo, RMSE_sub)
  MAE_all_combo <- append(MAE_all_combo, MAE_sub)
  MBE_all_combo <- append(MBE_all_combo, MBE_sub)
}

R2_all_combo_vector <- unlist(R2_all_combo, use.names=FALSE)
RMSE_all_combo_vector <- unlist(RMSE_all_combo, use.names=FALSE)
MAE_all_combo_vector <- unlist(MAE_all_combo, use.names=FALSE)
MBE_all_combo_vector <- unlist(MBE_all_combo, use.names=FALSE)

gr[nrow(gr) + 1,] = R2_all_combo_vector
gr[nrow(gr) + 1,] = RMSE_all_combo_vector
gr[nrow(gr) + 1,] = MAE_all_combo_vector

rownames(gr) <- c(1:length(all_good_datasets), 'R2', 'RMSE', 'MAE')

gr[which.min(gr['RMSE',])]

top10 <- order(RMSE_all_combo_vector)[1:10]
all_combo_best <- gr[,top10]

write.xlsx(all_combo_best, 'All_combo_best.xlsx')

each_best <- data.frame(matrix(NA, nrow = length(all_good_datasets)+3, ncol = length(all_good_datasets)))
for (i in 1:length(all_good_datasets)) {
  sub <- gr[,colSums(!is.na(gr[1:length(all_good_datasets),]))==i]
  if (class(sub)=="data.frame") {
    each_best[,i] <- sub[which.min(sub['RMSE',])]
  }
  else each_best[,i] <- sub
}

write.xlsx(each_best, 'Each_best.xlsx')

#plot(RMSE_all_combo_vector)

######################################## Backscatter analysis ######################################

dates <- c('15/04/2021', '17/04/2021', '22/04/2021', '30/05/2021', '04/06/2021', 
           '21/06/2021', '29/06/2021', '16/07/2021', '19/07/2021', '21/07/2021', 
           '25/08/2021', '28/08/2021', '14/09/2021')

R2 <- rep(NA, length(all_datasets))
RMSE <- rep(NA, length(all_datasets))
Bias <- rep(NA, length(all_datasets))
MAE <- rep(NA, length(all_datasets))
AIC_backs <- rep(NA, length(all_datasets))
training_datasets <- list()
validation_datasets <- list()
pVal_back <- NULL

validation_datasets_0_2 <- list()
validation_datasets_2_4 <- list()
validation_datasets_4_6 <- list()
validation_datasets_6_8 <- list()
validation_datasets_8_10 <- list()

R2_split_inter <- NULL
RMSE_split_inter <- NULL
MAE_split_inter <- NULL
n_split_inter <- NULL

for (i in 1:length(dates)) {
  training_datasets[[i]]  <- train_points
  validation_datasets[[i]] <- validation_points
  
  lyzenga_2021 <- lm(Bathy_MSL~(blue+green+red)^2+Mean.Total, data = training_datasets[[i]])
  
  validation_datasets[[i]] <- validation_datasets[[i]][, c('Bathy_MSL', 'FID_', 'blue', 'green','red', 'Mean.Total')]
  validation_datasets[[i]]$estimated <- predict(lyzenga_2021, newdata = subset(validation_datasets[[i]], select= -c(Bathy_MSL, FID_)), interval = 'confidence')[,1]
  training_datasets[[i]] <- training_datasets[[i]][, c('Bathy_MSL', 'FID_', 'blue', 'green','red', 'Mean.Total')]
  training_datasets[[i]]$estimated <- predict(lyzenga_2021, newdata = subset(training_datasets[[i]], select= -c(Bathy_MSL, FID_)), interval = 'confidence')[,1]
  
  print(ggplot(validation_datasets[[i]], aes(x=estimated ,y=Bathy_MSL)) +
          geom_point(size = 1) + geom_abline(slope=1, intercept=0) + xlab('Estimated depth') +
          ylab('Actual depth') + ggtitle(dates[i]) + scale_x_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) +
          scale_y_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) + geom_abline(slope=1, intercept=0.5, col="red") +
          geom_abline(slope=1, intercept=-0.5, col="red"))
  
  R2[i] = R2(validation_datasets[[i]]$estimated, validation_datasets[[i]]$Bathy_MSL)
  RMSE[i] = RMSE(validation_datasets[[i]]$estimated, validation_datasets[[i]]$Bathy_MSL)
  Bias[i] = mean(validation_datasets[[i]]$estimated-validation_datasets[[i]]$Bathy_MSL)
  MAE[i] = MAE(validation_datasets[[i]]$estimated, validation_datasets[[i]]$Bathy_MSL)
  
  AIC_backs[i] = AIC(lyzenga_2021)
  pVal_back <- c(pVal_back, summary(lyzenga_2021)$coeff[c(5:8),4])
  
  validation_datasets_0_2[[i]] <- validation_datasets[[i]][validation_datasets[[i]]$Bathy_MSL >= -2,]
  validation_datasets_2_4[[i]] <- validation_datasets[[i]][validation_datasets[[i]]$Bathy_MSL >= -4 & validation_datasets[[i]]$Bathy_MSL < -2,]
  validation_datasets_4_6[[i]] <- validation_datasets[[i]][validation_datasets[[i]]$Bathy_MSL >= -6 & validation_datasets[[i]]$Bathy_MSL < -4,]
  validation_datasets_6_8[[i]] <- validation_datasets[[i]][validation_datasets[[i]]$Bathy_MSL >= -8 & validation_datasets[[i]]$Bathy_MSL < -6,]
  validation_datasets_8_10[[i]] <- validation_datasets[[i]][validation_datasets[[i]]$Bathy_MSL < -8,]
  
  R2_split_inter <- c(R2_split_inter, c(R2(validation_datasets_0_2[[i]]$estimated, validation_datasets_0_2[[i]]$Bathy_MSL),
                                        R2(validation_datasets_2_4[[i]]$estimated, validation_datasets_2_4[[i]]$Bathy_MSL),
                                        R2(validation_datasets_4_6[[i]]$estimated, validation_datasets_4_6[[i]]$Bathy_MSL),
                                        R2(validation_datasets_6_8[[i]]$estimated, validation_datasets_6_8[[i]]$Bathy_MSL),
                                        R2(validation_datasets_8_10[[i]]$estimated, validation_datasets_8_10[[i]]$Bathy_MSL)))
  RMSE_split_inter <- c(RMSE_split_inter, c(RMSE(validation_datasets_0_2[[i]]$estimated, validation_datasets_0_2[[i]]$Bathy_MSL),
                                            RMSE(validation_datasets_2_4[[i]]$estimated, validation_datasets_2_4[[i]]$Bathy_MSL),
                                            RMSE(validation_datasets_4_6[[i]]$estimated, validation_datasets_4_6[[i]]$Bathy_MSL),
                                            RMSE(validation_datasets_6_8[[i]]$estimated, validation_datasets_6_8[[i]]$Bathy_MSL),
                                            RMSE(validation_datasets_8_10[[i]]$estimated, validation_datasets_8_10[[i]]$Bathy_MSL)))
  MAE_split_inter <- c(MAE_split_inter, c(MAE(validation_datasets_0_2[[i]]$estimated, validation_datasets_0_2[[i]]$Bathy_MSL),
                                          MAE(validation_datasets_2_4[[i]]$estimated, validation_datasets_2_4[[i]]$Bathy_MSL),
                                          MAE(validation_datasets_4_6[[i]]$estimated, validation_datasets_4_6[[i]]$Bathy_MSL),
                                          MAE(validation_datasets_6_8[[i]]$estimated, validation_datasets_6_8[[i]]$Bathy_MSL),
                                          MAE(validation_datasets_8_10[[i]]$estimated, validation_datasets_8_10[[i]]$Bathy_MSL)))
  n_split_inter <- c(n_split_inter, c(nrow(validation_datasets_0_2[[i]]),
                                      nrow(validation_datasets_2_4[[i]]),
                                      nrow(validation_datasets_4_6[[i]]),
                                      nrow(validation_datasets_6_8[[i]]),
                                      nrow(validation_datasets_8_10[[i]])))
}
n_split_inter <- c(n_split_inter, c(nrow(validation_datasets_0_2[[i]]),
                                    nrow(validation_datasets_2_4[[i]]),
                                    nrow(validation_datasets_4_6[[i]]),
                                    nrow(validation_datasets_6_8[[i]]),
                                    nrow(validation_datasets_8_10[[i]])))

# table with correlation values and P-values of the bands interaction
pVal_table <- data.frame(c("backscatter", "blue:green", "blue:red", "green:red"), rep(dates, each=4), pVal_back)

# put all metrics in one table called "metrics_10m_lyzenga_interaction"
metrics_10m_backscatter <- data.frame(dates, R2, RMSE, Bias, MAE, AIC_backs)
metrics_10m_backscatter_split <- data.frame(dates=rep(dates, each=5), R2_split_inter, RMSE_split_inter, 
                                            MAE_split_inter, n_split_inter,
                                            depth=rep(c("0-2", "2-4", "4-6", "6-8", "8-10"), times=length(dates)))

######################################## Study of outliers ###################################

# work only with images with R2 > 0.7
validation_datasets_GLM_good <- validation_datasets_GLM

for (i in 1:length(validation_datasets_GLM_good)) {
  res <- validation_datasets_GLM_good[[i]]$Bathy_MSL - validation_datasets_GLM_good[[i]]$estimated
  lower_bound <- median(res) - 3 * mad(res)
  upper_bound <- median(res) + 3 * mad(res)
  outlier_ind <- res < lower_bound | res > upper_bound
  validation_datasets_GLM_good[[i]]$outlier <- outlier_ind
  
  print(ggplot(validation_datasets_GLM_good[[i]], aes(x=estimated ,y=Bathy_MSL, col=outlier)) +
          geom_point(size = 1) + scale_colour_manual(values=c("FALSE"="black","TRUE"="red")) + geom_abline(slope=1, intercept=0) + xlab('Estimated depth') + ylab('Actual depth') +
          ggtitle(paste("Outliers",dates[i])) + scale_x_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) +
          scale_y_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) + geom_abline(slope=1, intercept=0.5, col="red") +
          geom_abline(slope=1, intercept=-0.5, col="red") + theme_bw() + 
          theme(axis.title = element_text(size = 16), legend.title = element_text(size = 12), legend.text = element_text(size = 12)))
}

# put all datasets into one big dataset and name it "total"
total <- bind_rows(validation_datasets_GLM_good)

# filter only point which are considered outliers
outliers <- total[total$outlier==TRUE,]
# in how many images do they appear? count the number of images
count_FID <- outliers %>% group_by(FID_) %>% count 
# how many are error outliers (only one image) and how many are influential outliers (multiple images)?
error_out <- count_FID[count_FID$n==1,]
infl_out <- count_FID[count_FID$n!=1,]
# merge number of times the outlier appears in each image
final_out <- merge(outliers, count_FID, by="FID_")
final_error_out <- merge(outliers, error_out, by="FID_")
final_infl_out <- merge(outliers, infl_out, by="FID_")
final_infl_out_unique <- final_infl_out[!duplicated(final_infl_out$FID_), ]
# number of images in which the same point is classified as outlier
if(length(unique(final_infl_out$n)) > 0) {
  final_infl_out_count <- data.frame(matrix(nrow = max(unique(final_infl_out$n)), ncol = 2)) 
  for (i in unique(final_infl_out$n)) {
    final_infl_out_count[i,1] <- i
    final_infl_out_count[i,2] <-nrow(final_infl_out[final_infl_out$n==i,])/i
  }
}

# shows the percentage of points classified as outliers for each day
g_error <- final_error_out %>%
  group_by(Date) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / nrow(validation_datasets_mean_comp), 3)) %>% 
  arrange(desc(freq))
g_infl <- final_infl_out %>%
  group_by(Date) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / nrow(validation_datasets_mean_comp), 3)) %>% 
  arrange(desc(freq))


validation_datasets_mean_comp <- validation_datasets_mean_comp %>% mutate(error_outlier = ifelse(FID_ %in% final_error_out$FID_, "TRUE", "FALSE"))
validation_datasets_mean_comp <- validation_datasets_mean_comp %>% mutate(infl_outlier = ifelse(FID_ %in% final_infl_out$FID_, "TRUE", "FALSE"))

validation_datasets_median_comp <- validation_datasets_median_comp %>% mutate(error_outlier = ifelse(FID_ %in% final_error_out$FID_, "TRUE", "FALSE"))
validation_datasets_median_comp <- validation_datasets_median_comp %>% mutate(infl_outlier = ifelse(FID_ %in% final_infl_out$FID_, "TRUE", "FALSE"))

# define outliers after image combination (mean)
res <- validation_datasets_mean_comp$Bathy_MSL-validation_datasets_mean_comp$estimated
lower_bound <- median(res) - 3 * mad(res)
upper_bound <- median(res) + 3 * mad(res)
validation_datasets_mean_comp <- validation_datasets_mean_comp %>% mutate(comp_outlier = ifelse(Bathy_MSL-estimated < lower_bound | Bathy_MSL-estimated > upper_bound, "TRUE", "FALSE"))

# create column which differentiates between error and infl outliers (for plot later)
validation_datasets_mean_comp$infl_outlier <- validation_datasets_mean_comp$infl_outlier == "TRUE"
validation_datasets_mean_comp$error_outlier <- validation_datasets_mean_comp$error_outlier == "TRUE"
validation_datasets_mean_comp$type <- ifelse(validation_datasets_mean_comp$infl_outlier, "Multi-image", ifelse(validation_datasets_mean_comp$error_outlier, "Single-image", "no_out"))

# number of outliers within the +-0.5m range and the +-1m range using the mean
sum(validation_datasets_mean_comp$error_outlier==TRUE & abs(validation_datasets_mean_comp$Bathy_MSL-validation_datasets_mean_comp$estimated)<=0.5)
sum(validation_datasets_mean_comp$error_outlier==TRUE & abs(validation_datasets_mean_comp$Bathy_MSL-validation_datasets_mean_comp$estimated)<=1)

# define outliers after image combination (median)
res <- validation_datasets_median_comp$Bathy_MSL-validation_datasets_median_comp$estimated
lower_bound <- median(res) - 3 * mad(res)
upper_bound <- median(res) + 3 * mad(res)
validation_datasets_median_comp <- validation_datasets_median_comp %>% mutate(comp_outlier = ifelse(Bathy_MSL-estimated < lower_bound | Bathy_MSL-estimated > upper_bound, "TRUE", "FALSE"))

# number of outliers within the +-0.5m range and the +-1m range using the median
sum(validation_datasets_median_comp$error_outlier==TRUE & abs(validation_datasets_median_comp$Bathy_MSL-validation_datasets_median_comp$estimated)<=0.5)
sum(validation_datasets_median_comp$error_outlier==TRUE & abs(validation_datasets_median_comp$Bathy_MSL-validation_datasets_median_comp$estimated)<=1)

# number of outliers within the +-0.5m range
above05 <- validation_datasets_mean_comp[validation_datasets_mean_comp$error_outlier==TRUE & abs(validation_datasets_mean_comp$Bathy_MSL-validation_datasets_mean_comp$estimated)>=0.5,]

# error outliers
outl_0_5 <- nrow(validation_datasets_mean_comp[abs(validation_datasets_mean_comp$Bathy_MSL-validation_datasets_mean_comp$estimated)<=0.5 & validation_datasets_mean_comp$error_outlier=="TRUE",])
print(ggplot(validation_datasets_mean_comp, aes(x=estimated ,y=Bathy_MSL, color=type)) +
        geom_point(size = 1) + scale_colour_manual(name="Outlier type", values=c("Multi-image"="blue", "Single-image"="red")) + geom_abline(slope=1, intercept=0) + xlab('Estimated depth') + ylab('Actual depth') +
        scale_x_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) +
        scale_y_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) + 
        geom_abline(slope=1, intercept=0.5, col="red") +
        geom_abline(slope=1, intercept=-0.5, col="red") + theme_bw() +
        labs(color="Outlier type") + theme(legend.position = c(0.85, 0.2)) +
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.1,vjust=1.5,label="Mean reducer"), size=7, col="black")+ 
        theme(axis.title = element_text(size = 16), legend.title = element_text(size = 12), legend.text = element_text(size = 12)))

outl_0_5 <- nrow(validation_datasets_median_comp[abs(validation_datasets_median_comp$Bathy_MSL-validation_datasets_median_comp$estimated)<=0.5 & validation_datasets_median_comp$error_outlier=="TRUE",])
print(ggplot(validation_datasets_median_comp, aes(x=estimated ,y=Bathy_MSL, col=error_outlier)) +
        geom_point(size = 1) + scale_colour_manual(name="error outlier", values=c("TRUE"="red","FALSE"="gray")) + geom_abline(slope=1, intercept=0) + xlab('Estimated depth') + ylab('Actual depth') +
        scale_x_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) +
        scale_y_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) + geom_abline(slope=1, intercept=0.5, col="red") +
        geom_abline(slope=1, intercept=-0.5, col="red") + theme_bw() + theme(legend.position = c(0.85, 0.2)) +
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.1,vjust=1.5,label="Median reducer"), size=7, col="black") + 
        theme(axis.title = element_text(size = 16), legend.title = element_text(size = 12), legend.text = element_text(size = 12)))

# influential outliers plots
outl_0_5 <- nrow(validation_datasets_mean_comp[abs(validation_datasets_mean_comp$Bathy_MSL-validation_datasets_mean_comp$estimated)<=0.5 & validation_datasets_mean_comp$infl_outlier=="TRUE",])
print(ggplot(validation_datasets_mean_comp, aes(x=estimated ,y=Bathy_MSL, col=infl_outlier)) +
        geom_point(size = 1) + scale_colour_manual(name="influential outlier", values=c("TRUE"="blue","FALSE"="gray")) + geom_abline(slope=1, intercept=0) + xlab('Estimated depth') + ylab('Actual depth') +
        scale_x_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) +
        scale_y_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) + geom_abline(slope=1, intercept=0.5, col="red") +
        geom_abline(slope=1, intercept=-0.5, col="red") + theme_bw() + theme(legend.position = c(0.85, 0.2)) +
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.1,vjust=1.5,label="Mean reducer"), size=7, col="black") + 
        theme(axis.title = element_text(size = 16), legend.title = element_text(size = 12), legend.text = element_text(size = 12)))

outl_0_5 <- nrow(validation_datasets_median_comp[abs(validation_datasets_median_comp$Bathy_MSL-validation_datasets_median_comp$estimated)<=0.5 & validation_datasets_median_comp$infl_outlier=="TRUE",])
print(ggplot(validation_datasets_median_comp, aes(x=estimated ,y=Bathy_MSL, col=infl_outlier)) +
        geom_point(size = 1) + scale_colour_manual(name="influential outlier", values=c("TRUE"="blue","FALSE"="gray")) + geom_abline(slope=1, intercept=0) + xlab('Estimated depth') + ylab('Actual depth') +
        scale_x_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) +
        scale_y_continuous(limits = c(-10.5, 0), breaks = c(-10, -8, -6, -4, -2, 0)) + geom_abline(slope=1, intercept=0.5, col="red") +
        geom_abline(slope=1, intercept=-0.5, col="red") + theme_bw()+ theme(legend.position = c(0.85, 0.2)) +
        geom_text(data=NULL,aes(x=-Inf,y=Inf,hjust=-0.1,vjust=1.5,label="Median reducer"), size=7, col="black") + 
        theme(axis.title = element_text(size = 16), legend.title = element_text(size = 12), legend.text = element_text(size = 12)))

