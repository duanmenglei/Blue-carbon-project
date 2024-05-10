### Airborne LiDAR individual tree metrics extraction & RF  biomass models ###

# install and import packages

#install.packages("lidR")
#install.packages("e1071")
#install.packages("randomForest")
#install.packages("RCSF")
#install.packages("ggplot2")

library(lidR)
library(e1071)
library(randomForest)
library(RCSF)
library(ggplot2)
library(caret)
library(nnet)

# preprocessing -----------------------------------------------------------

# read las file
las <- readLAS("D:/UAlberta/PhD/Blue carbon project/forest inventory/las-9plots/kuchi-plot2.las")
print(las)

#lascheck
las_check(las)

# visualize LiDAR data
plot(las,bg="white",axis=TRUE, legend=TRUE)

# denoising - using IVF
las <- classify_noise(las, ivf(5,2))

# Remove outliers using filter_poi()
dlas <- filter_poi(las, Classification != LASNOISE)

# ground classification - CSF
clas <- classify_ground(dlas, algorithm = csf())

# normalize .las
nlas <- normalize_height(clas, knnidw())

#keep everything above zero and below the 99.99th quantile
nlas <- filter_poi(nlas, Z >= 0 & Z < quantile(Z, .9999))

las_check(nlas)

# Individual Trees Segmentation -------------------------------------------

# segment nlas
### Using Li et al.(2012): an “all-in-one” point cloud based method
# set up parameters to segment trees: hmin:0.5m, maximum radius of a crown: 1.5m
algo <- li2012(hmin=0.5, speed_up = 2)    

cnlas <- segment_trees(nlas, algo, attribute = "treeID",uniqueness = "incremental")

# visualization segmented individual trees
plot(cnlas, bg="white", color="treeID", size=4, colorPalette = pastel.colors(200))


# Extract tree metrics ----------------------------------------------------

# derive tree metrics
metrics <- crown_metrics(cnlas,
                         ~list( 
                                Hmean = mean(Z), Hsd = sd(Z),Hmedian = median(Z),
                                Hmax = max(Z), Hskew = skewness(Z), Hkurt = kurtosis(Z),
                                
                                H05TH = quantile(Z,0.05),H10TH = quantile(Z,0.10),
                                H11TH = quantile(Z,0.11),H12TH = quantile(Z,0.12),
                                H13TH = quantile(Z,0.13),H14TH = quantile(Z,0.14),
                                H15TH = quantile(Z,0.15),H16TH = quantile(Z,0.16),
                                H17TH = quantile(Z,0.17),H18TH = quantile(Z,0.18),
                                H19TH = quantile(Z,0.19),H20TH = quantile(Z,0.20),
                                H21TH = quantile(Z,0.21),H22TH = quantile(Z,0.22),
                                H23TH = quantile(Z,0.23),H24TH = quantile(Z,0.24),
                                H25TH = quantile(Z,0.25),H26TH = quantile(Z,0.26),
                                H27TH = quantile(Z,0.27),H28TH = quantile(Z,0.28),
                                H29TH = quantile(Z,0.29),H30TH = quantile(Z,0.30),
                                H31TH = quantile(Z,0.31),H32TH = quantile(Z,0.32),
                                H33TH = quantile(Z,0.33),H34TH = quantile(Z,0.34),
                                H35TH = quantile(Z,0.35),H36TH = quantile(Z,0.36),
                                H37TH = quantile(Z,0.37),H38TH = quantile(Z,0.38),
                                H39TH = quantile(Z,0.39),H40TH = quantile(Z,0.40),
                                H41TH = quantile(Z,0.41),H42TH = quantile(Z,0.42),
                                H43TH = quantile(Z,0.43),H44TH = quantile(Z,0.44),
                                H45TH = quantile(Z,0.45),H46TH = quantile(Z,0.46),
                                H47TH = quantile(Z,0.47),H48TH = quantile(Z,0.48),
                                H49TH = quantile(Z,0.49),H50TH = quantile(Z,0.50),
                                H51TH = quantile(Z,0.51),H52TH = quantile(Z,0.52),
                                H53TH = quantile(Z,0.53),H54TH = quantile(Z,0.54),
                                H55TH = quantile(Z,0.55),H56TH = quantile(Z,0.56),
                                H57TH = quantile(Z,0.57),H58TH = quantile(Z,0.58),
                                H59TH = quantile(Z,0.59),H60TH = quantile(Z,0.60),
                                H61TH = quantile(Z,0.61),H62TH = quantile(Z,0.62),
                                H63TH = quantile(Z,0.63),H64TH = quantile(Z,0.64),
                                H65TH = quantile(Z,0.65),H66TH = quantile(Z,0.66),
                                H67TH = quantile(Z,0.67),H68TH = quantile(Z,0.68),
                                H69TH = quantile(Z,0.69),H70TH = quantile(Z,0.70),
                                H71TH = quantile(Z,0.71),H72TH = quantile(Z,0.72),
                                H73TH = quantile(Z,0.73),H74TH = quantile(Z,0.74),
                                H75TH = quantile(Z,0.75),H76TH = quantile(Z,0.76),
                                H77TH = quantile(Z,0.77),H78TH = quantile(Z,0.78),
                                H79TH = quantile(Z,0.79),H80TH = quantile(Z,0.80),
                                H81TH = quantile(Z,0.81),H82TH = quantile(Z,0.82),
                                H83TH = quantile(Z,0.83),H84TH = quantile(Z,0.84),
                                H85TH = quantile(Z,0.85),H86TH = quantile(Z,0.86),
                                H87TH = quantile(Z,0.87),H88TH = quantile(Z,0.88),
                                H89TH = quantile(Z,0.89),H90TH = quantile(Z,0.90),
                                H91TH = quantile(Z,0.91),H92TH = quantile(Z,0.92),
                                H93TH = quantile(Z,0.93),H94TH = quantile(Z,0.94),
                                H95TH = quantile(Z,0.95),H96TH = quantile(Z,0.96),
                                H97TH = quantile(Z,0.97),H98TH = quantile(Z,0.98),
                                H99TH = quantile(Z,0.99)
                                 
                                #Imean = mean(Intensity), Isd = sd(Intensity),
                                #Imax = max(Intensity),
                                #Iskew = skewness(Intensity), Ikurt = kurtosis(Intensity),
                                #I01TH = quantile(Intensity,0.01),I02TH = quantile(Intensity,0.02),
                                #I03TH = quantile(Intensity,0.03),I04TH = quantile(Intensity,0.04),
                                #I05TH = quantile(Intensity,0.05),I06TH = quantile(Intensity,0.06),
                                #I07TH = quantile(Intensity,0.07),I08TH = quantile(Intensity,0.08),
                                #I09TH = quantile(Intensity,0.09),I10TH = quantile(Intensity,0.10),
                                #I11TH = quantile(Intensity,0.11),I12TH = quantile(Intensity,0.12),
                                #I13TH = quantile(Intensity,0.13),I14TH = quantile(Intensity,0.14),
                                #I15TH = quantile(Intensity,0.15),I16TH = quantile(Intensity,0.16),
                                #I17TH = quantile(Intensity,0.17),I18TH = quantile(Intensity,0.18),
                                #I19TH = quantile(Intensity,0.19),I20TH = quantile(Intensity,0.20),
                                #I21TH = quantile(Intensity,0.21),I22TH = quantile(Intensity,0.22),
                                #I23TH = quantile(Intensity,0.23),I24TH = quantile(Intensity,0.24),
                                #I25TH = quantile(Intensity,0.25),I26TH = quantile(Intensity,0.26),
                                #I27TH = quantile(Intensity,0.27),I28TH = quantile(Intensity,0.28),
                                #I29TH = quantile(Intensity,0.29),I30TH = quantile(Intensity,0.30),
                                #I31TH = quantile(Intensity,0.31),I32TH = quantile(Intensity,0.32),
                                #I33TH = quantile(Intensity,0.33),I34TH = quantile(Intensity,0.34),
                                #I35TH = quantile(Intensity,0.35),I36TH = quantile(Intensity,0.36),
                                #I37TH = quantile(Intensity,0.37),I38TH = quantile(Intensity,0.38),
                                #I39TH = quantile(Intensity,0.39),I40TH = quantile(Intensity,0.40),
                                #I41TH = quantile(Intensity,0.41),I42TH = quantile(Intensity,0.42),
                                #I43TH = quantile(Intensity,0.43),I44TH = quantile(Intensity,0.44),
                                #I45TH = quantile(Intensity,0.45),I46TH = quantile(Intensity,0.46),
                                #I47TH = quantile(Intensity,0.47),I48TH = quantile(Intensity,0.48),
                                #I49TH = quantile(Intensity,0.49),I50TH = quantile(Intensity,0.50),
                                #I51TH = quantile(Intensity,0.51),I52TH = quantile(Intensity,0.52),
                                #I53TH = quantile(Intensity,0.53),I54TH = quantile(Intensity,0.54),
                                #I55TH = quantile(Intensity,0.55),I56TH = quantile(Intensity,0.56),
                                #I57TH = quantile(Intensity,0.57),I58TH = quantile(Intensity,0.58),
                                #I59TH = quantile(Intensity,0.59),I60TH = quantile(Intensity,0.60),
                                #I61TH = quantile(Intensity,0.61),I62TH = quantile(Intensity,0.62),
                                #I63TH = quantile(Intensity,0.63),I64TH = quantile(Intensity,0.64),
                                #I65TH = quantile(Intensity,0.65),I66TH = quantile(Intensity,0.66),
                                #I67TH = quantile(Intensity,0.67),I68TH = quantile(Intensity,0.68),
                                #I69TH = quantile(Intensity,0.69),I70TH = quantile(Intensity,0.70),
                                #I71TH = quantile(Intensity,0.71),I72TH = quantile(Intensity,0.72),
                                #I73TH = quantile(Intensity,0.73),I74TH = quantile(Intensity,0.74),
                                #I75TH = quantile(Intensity,0.75),I76TH = quantile(Intensity,0.76),
                                #I77TH = quantile(Intensity,0.77),I78TH = quantile(Intensity,0.78),
                                #I79TH = quantile(Intensity,0.79),I80TH = quantile(Intensity,0.80),
                                #I81TH = quantile(Intensity,0.81),I82TH = quantile(Intensity,0.82),
                                #I83TH = quantile(Intensity,0.83),I84TH = quantile(Intensity,0.84),
                                #I85TH = quantile(Intensity,0.85),I86TH = quantile(Intensity,0.86),
                                #I87TH = quantile(Intensity,0.87),I88TH = quantile(Intensity,0.88),
                                #I89TH = quantile(Intensity,0.89),I90TH = quantile(Intensity,0.90),
                                #I91TH = quantile(Intensity,0.91),I92TH = quantile(Intensity,0.92),
                                #I93TH = quantile(Intensity,0.93),I94TH = quantile(Intensity,0.94),
                                #I95TH = quantile(Intensity,0.95),I96TH = quantile(Intensity,0.96),
                                #I97TH = quantile(Intensity,0.98),I98TH = quantile(Intensity,0.98),
                                #I99TH = quantile(Intensity,0.99)
                               )
                         ) 

head(metrics)

getwd()

write.csv(metrics, "D:/UAlberta/PhD/Blue carbon project/Model-test/model-.csv/Kuchi2-plot2-metrics.csv", col.names = TRUE)
# Note that here the output metrics.csv needs to be manually matched
# the biomass information from forest inventory according to coordinates


# Develop the RF biomass regression model ---------------------------------

# import data table
metrics <- read.csv("D:/UAlberta/PhD/Blue carbon project/Model-test/model-.csv/model-train-kuchi2.csv")
attach(metrics)


set.seed(222)
ind <- sample(2, nrow(metrics), replace = TRUE, prob = c(0.7, 0.3))
train <- metrics[ind==1,]
test <- metrics[ind==2,]

attach(train)

# train the model
# here, biomass is response variable, and 210 tree metrics are predictor variables
RFmodel <- randomForest(Biomass_ob ~ Hmean+Hsd+Hmedian+Hmax+Hskew+Hkurt
                       +H05TH+H10TH
                       +H11TH+H12TH+H13TH+H14TH+H15TH
                       +H16TH+H17TH+H18TH+H19TH+H20TH
                       +H21TH+H22TH+H23TH+H24TH+H25TH
                       +H26TH+H27TH+H28TH+H29TH+H30TH
                       +H31TH+H32TH+H33TH+H34TH+H35TH
                       +H36TH+H37TH+H38TH+H39TH+H40TH
                       +H41TH+H42TH+H43TH+H44TH+H45TH
                       +H46TH+H47TH+H48TH+H49TH+H50TH
                       +H51TH+H52TH+H53TH+H54TH+H55TH
                       +H56TH+H57TH+H58TH+H59TH+H60TH
                       +H61TH+H62TH+H63TH+H64TH+H65TH
                       +H66TH+H67TH+H68TH+H69TH+H70TH
                       +H71TH+H72TH+H73TH+H74TH+H75TH
                       +H76TH+H77TH+H78TH+H79TH+H80TH
                       +H81TH+H82TH+H83TH+H84TH+H85TH
                       +H86TH+H87TH+H88TH+H89TH+H90TH
                       +H91TH+H92TH+H93TH+H94TH+H95TH
                       +H96TH+H97TH+H98TH+H99TH,
                      
                       #+Imean+Isd+Imax+Iskew+Ikurt
                       #+I01TH+I02TH+I03TH+I04TH+I05TH+I06TH+I07TH+I08TH+I09TH+I10TH
                       #+I11TH+I12TH+I13TH+I14TH+I15TH+I16TH+I17TH+I18TH+I19TH+I20TH
                       #+I21TH+I22TH+I23TH+I24TH+I25TH+I26TH+I27TH+I28TH+I29TH+I30TH
                       #+I31TH+I32TH+I33TH+I34TH+I35TH+I36TH+I37TH+I38TH+I39TH+I40TH
                       #+I41TH+I42TH+I43TH+I44TH+I45TH+I46TH+I47TH+I48TH+I49TH+I50TH
                       #+I51TH+I52TH+I53TH+I54TH+I55TH+I56TH+I57TH+I58TH+I59TH+I60TH
                       #+I61TH+I62TH+I63TH+I64TH+I65TH+I66TH+I67TH+I68TH+I69TH+I70TH
                       #+I71TH+I72TH+I73TH+I74TH+I75TH+I76TH+I77TH+I78TH+I79TH+I80TH
                       #+I81TH+I82TH+I83TH+I84TH+I85TH+I86TH+I87TH+I88TH+I89TH+I90TH
                       #+I91TH+I92TH+I93TH+I94TH+I95TH+I96TH+I97TH+I98TH+I99TH,
 
                       
                       train,
                       ntree=500,
                       importance=T)

# print() to check the result of the trained model
##  % Var explained: goodness of fit (R square)
print(RFmodel)

# import testing dataset
#test <- read.csv("test.csv")

# validate the model:
# i.e., use the trained model to predict testing dataset
RF_predict_biomass <- predict(RFmodel, newdata=test)
#predict_biomass <- predict(RFmodel, newdata=metrics)
#sum(predict_biomass)
# add one column in the testing data table as "precict_biomass"
test$RF_predict_biomass <- RF_predict_biomass

## now, draw a scatter plot (biomass from forest inventory vs biomass from model)
## to compare predict_biomass with observed_biomass in testing dataset ##
ggplot(test, aes(x= Biomass_ob , y= RF_predict_biomass))+
  geom_point(alpha=0.5)+
  geom_abline(slope = 1, linetype="dashed")+
  theme_bw()+
  labs(x="Observed biomass (Kg)", y="RF_Predicted biomass (Kg)",title = "RFmodel")+
  theme(plot.title = element_text(size=18, hjust=0.5),
        axis.text = element_text(size=14))+
  scale_x_continuous(limits = c(0,100),breaks = seq(0,100,25))+
  scale_y_continuous(limits = c(0,100),breaks = seq(0,100,25))+
  theme(axis.title.x = element_text(size=18), axis.title.y=element_text(size=18))

   
# evaluate the model by three evaluation metrics - caret package:
  ## RMSE: Root Mean Squared Error
  ## MAE: Mean Absolute Error
  ## R-square: coefficient of determination
    
RMSE = RMSE(test$Biomass_ob, test$RF_predict_biomass)
RMSE
MAE = MAE(test$Biomass_ob, test$RF_predict_biomass)
MAE
R2 = R2(test$Biomass_ob, test$RF_predict_biomass)
R2
  

# evaluate the variable importance, 
## and according to the result, drop those variables that contribute less
## to improve the computational efficiency and remove redundant information
  
importance(RFmodel)

# Ranking diagram of importance of variables
varImpPlot(RFmodel)          


# SVMmodel ----------------------------------------------------------------

### here we can also try SVM model: ###
SVMmodel <- svm(Biomass_ob ~ Hmean+Hsd+Hmedian+Hmax+Hskew+Hkurt
                        +H05TH+H10TH
                        +H11TH+H12TH+H13TH+H14TH+H15TH
                        +H16TH+H17TH+H18TH+H19TH+H20TH
                        +H21TH+H22TH+H23TH+H24TH+H25TH
                        +H26TH+H27TH+H28TH+H29TH+H30TH
                        +H31TH+H32TH+H33TH+H34TH+H35TH
                        +H36TH+H37TH+H38TH+H39TH+H40TH
                        +H41TH+H42TH+H43TH+H44TH+H45TH
                        +H46TH+H47TH+H48TH+H49TH+H50TH
                        +H51TH+H52TH+H53TH+H54TH+H55TH
                        +H56TH+H57TH+H58TH+H59TH+H60TH
                        +H61TH+H62TH+H63TH+H64TH+H65TH
                        +H66TH+H67TH+H68TH+H69TH+H70TH
                        +H71TH+H72TH+H73TH+H74TH+H75TH
                        +H76TH+H77TH+H78TH+H79TH+H80TH
                        +H81TH+H82TH+H83TH+H84TH+H85TH
                        +H86TH+H87TH+H88TH+H89TH+H90TH
                        +H91TH+H92TH+H93TH+H94TH+H95TH
                        +H96TH+H97TH+H98TH+H99TH,
                     
                        train,
                        
                        kernel="radial")

summary(SVMmodel)


SVM_predict_biomass <- predict(SVMmodel, newdata=test)
test$SVM_predict_biomass <- SVM_predict_biomass

ggplot(test, aes(x= Biomass_ob , y= SVM_predict_biomass))+
  geom_point(alpha=0.5)+
  geom_abline(slope = 1, linetype="dashed")+
  theme_bw()+
  labs(x="Observed biomass (Kg)", y="SVM_Predicted biomass (Kg)", title="SVMmodel")+
  theme(plot.title = element_text(size=18, hjust=0.5),
        axis.text = element_text(size=14))+
  scale_x_continuous(limits = c(0,100),breaks = seq(0,100,25))+
  scale_y_continuous(limits = c(0,100),breaks = seq(0,100,25))+
  theme(axis.title.x = element_text(size=18), axis.title.y=element_text(size=18))

RMSE = RMSE(test$Biomass_ob, test$SVM_predict_biomass)
RMSE
MAE = MAE(test$Biomass_ob, test$SVM_predict_biomass)
MAE
R2 = R2(test$Biomass_ob, test$SVM_predict_biomass)
R2


# Predict unknown area covered with point clouds data ---------------------

# eventually,
# the model is ready to predict biomass in unknown area with available point clouds data
# (no forest inventory in unknown area is needed) #
## derive LiDAR metrics from LiDAR data with unknown biomass,
## and output as .csv file named newdata (like the codes show above) 

# similarly, we need to extract metrics from point clouds data first

# read las file
las <- readLAS("D:/UAlberta/PhD/Blue carbon project/forest inventory/las-9plots/kuchi2.las")
las_check(las)

# visualize LiDAR data
plot(las,bg="white",axis=TRUE, legend=TRUE)

# denoising - using IVF
las <- classify_noise(las, ivf(5,2))

# Remove outliers using filter_poi()
dlas <- filter_poi(las, Classification != LASNOISE)

# ground classification - CSF
clas <- classify_ground(dlas, algorithm = csf())

# normalize .las
nlas <- normalize_height(clas, knnidw())

#keep everything above zero and below the 99.99th quantile
nlas <- filter_poi(nlas, Z >= 0 & Z < quantile(Z, .9999))

las_check(nlas)

# derive tree metrics
metrics <- crown_metrics(cnlas,
                         ~list( 
                           Hmean = mean(Z), Hsd = sd(Z),Hmedian = median(Z),
                           Hmax = max(Z), Hskew = skewness(Z), Hkurt = kurtosis(Z),
                           
                           H05TH = quantile(Z,0.05),H10TH = quantile(Z,0.10),
                           H11TH = quantile(Z,0.11),H12TH = quantile(Z,0.12),
                           H13TH = quantile(Z,0.13),H14TH = quantile(Z,0.14),
                           H15TH = quantile(Z,0.15),H16TH = quantile(Z,0.16),
                           H17TH = quantile(Z,0.17),H18TH = quantile(Z,0.18),
                           H19TH = quantile(Z,0.19),H20TH = quantile(Z,0.20),
                           H21TH = quantile(Z,0.21),H22TH = quantile(Z,0.22),
                           H23TH = quantile(Z,0.23),H24TH = quantile(Z,0.24),
                           H25TH = quantile(Z,0.25),H26TH = quantile(Z,0.26),
                           H27TH = quantile(Z,0.27),H28TH = quantile(Z,0.28),
                           H29TH = quantile(Z,0.29),H30TH = quantile(Z,0.30),
                           H31TH = quantile(Z,0.31),H32TH = quantile(Z,0.32),
                           H33TH = quantile(Z,0.33),H34TH = quantile(Z,0.34),
                           H35TH = quantile(Z,0.35),H36TH = quantile(Z,0.36),
                           H37TH = quantile(Z,0.37),H38TH = quantile(Z,0.38),
                           H39TH = quantile(Z,0.39),H40TH = quantile(Z,0.40),
                           H41TH = quantile(Z,0.41),H42TH = quantile(Z,0.42),
                           H43TH = quantile(Z,0.43),H44TH = quantile(Z,0.44),
                           H45TH = quantile(Z,0.45),H46TH = quantile(Z,0.46),
                           H47TH = quantile(Z,0.47),H48TH = quantile(Z,0.48),
                           H49TH = quantile(Z,0.49),H50TH = quantile(Z,0.50),
                           H51TH = quantile(Z,0.51),H52TH = quantile(Z,0.52),
                           H53TH = quantile(Z,0.53),H54TH = quantile(Z,0.54),
                           H55TH = quantile(Z,0.55),H56TH = quantile(Z,0.56),
                           H57TH = quantile(Z,0.57),H58TH = quantile(Z,0.58),
                           H59TH = quantile(Z,0.59),H60TH = quantile(Z,0.60),
                           H61TH = quantile(Z,0.61),H62TH = quantile(Z,0.62),
                           H63TH = quantile(Z,0.63),H64TH = quantile(Z,0.64),
                           H65TH = quantile(Z,0.65),H66TH = quantile(Z,0.66),
                           H67TH = quantile(Z,0.67),H68TH = quantile(Z,0.68),
                           H69TH = quantile(Z,0.69),H70TH = quantile(Z,0.70),
                           H71TH = quantile(Z,0.71),H72TH = quantile(Z,0.72),
                           H73TH = quantile(Z,0.73),H74TH = quantile(Z,0.74),
                           H75TH = quantile(Z,0.75),H76TH = quantile(Z,0.76),
                           H77TH = quantile(Z,0.77),H78TH = quantile(Z,0.78),
                           H79TH = quantile(Z,0.79),H80TH = quantile(Z,0.80),
                           H81TH = quantile(Z,0.81),H82TH = quantile(Z,0.82),
                           H83TH = quantile(Z,0.83),H84TH = quantile(Z,0.84),
                           H85TH = quantile(Z,0.85),H86TH = quantile(Z,0.86),
                           H87TH = quantile(Z,0.87),H88TH = quantile(Z,0.88),
                           H89TH = quantile(Z,0.89),H90TH = quantile(Z,0.90),
                           H91TH = quantile(Z,0.91),H92TH = quantile(Z,0.92),
                           H93TH = quantile(Z,0.93),H94TH = quantile(Z,0.94),
                           H95TH = quantile(Z,0.95),H96TH = quantile(Z,0.96),
                           H97TH = quantile(Z,0.97),H98TH = quantile(Z,0.98),
                           H99TH = quantile(Z,0.99)
                         )
) 

head(metrics)

write.csv(metrics, "D:/UAlberta/PhD/Blue carbon project/Model-test/model-.csv/Kuchi2-metrics.csv", col.names = TRUE)

 
# import kuchi2-metrics.csv, and name the variable as "newdata"
newdata <- read.csv("D:/UAlberta/PhD/Blue carbon project/Model-test/model-.csv/kuchi2-metrics.csv")

RFpredict_newdata_biomass <- predict(RFmodel, newdata = newdata)
newdata$RFpredict_biomass <- RFpredict_newdata_biomass


# output final newdata.csv with predict_biomass, and name it as "kuchi2-predict"
write.csv(newdata, "D:/UAlberta/PhD/Blue carbon project/Model-test/model-.csv/kuchi2-predict.csv", col.names = TRUE)

