# installing packages
install.packages("ggrepel")
install.packages("knitr")
install.packages("gridExtra")
install.packages("xgboost")

# loading packages
library(naniar)
library(ggplot2)
library(scales)
library(e1071)
library(corrplot)
library(plyr)
library(ggrepel)
library(knitr)
library(gridExtra)
library(randomForest)
library(caret)
library(xgboost)

# Importing data set

train = read.csv("input/train.csv", stringsAsFactors = F)
test = read.csv("input/test.csv", stringsAsFactors = F)

# add SalePrice to test data
test$SalePrice = NA

# add DataFrom feature to our train and test data set
train$DataFrom = 'train'
test$DataFrom = 'test'

# combine the data to our.data
our.data = rbind(train, test)

# Missing Data
NAcol <- which(colSums(is.na(our.data)) > 0)
missing_data = sort(colSums(sapply(our.data[NAcol], is.na)), decreasing = TRUE)

missing_data = as.data.frame(missing_data)
missing_data$variable = row.names(missing_data)
missing_data$perc = round(missing_data$missing_data / nrow(our.data) * 100, digits = 2)

row.names(missing_data) = NULL

# Exploring Variable
# Dependent Variable - SalesPrice 

summary(our.data$SalePrice)

# Numerical Variable
numericVars = which(sapply(our.data, is.numeric))
numericVarsName = names(numericVars)

paste("Total Numeric Variable: ", length(numericVars))

our.data_numVar = our.data[, numericVarsName]

# correlation
cor_numVar = cor(our.data_numVar, use = "pairwise.complete.obs")
cor_sorted = as.matrix(sort(cor_numVar[, 'SalePrice'], decreasing = T))
cor_sorted

# Select only high correlation
corHigh = names(which(apply(cor_sorted, 1, function(x){
  abs(x) > 0.5
})))
corHigh
cor_numVar = cor_numVar[corHigh, corHigh]

corrplot.mixed(cor_numVar, tl.col = "black", tl.pos = "lt")

# Overall Quality


# GrLivingArea - Above grade (ground) living area square feet
# From the graph we find that row number 524 and 1299 have large ground area but low price,
# but may be they have low overall Quality.
# Let's check Sale Price, Overall Quality and Ground living Area for row number 524 and 1299

our.data[c(524, 1299), c("GrLivArea", "OverallQual", "SalePrice")]

# However, we see that this two houses score maximum points on Overall Quality. 
# Therefore, these 524 and 1299 houses are outliers

# IMPUTING MISSING VALUES
# So we have total 35 variables having missing value, out of them one is Survival so 
# we have to do our imputation on remaining 34 variables

# POOLQC
# Assign No Pool to the NAs
our.data$PoolQC[is.na(our.data$PoolQC)] = 'none'

# Label Encoding for POOLQC
qualities = c('none' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
our.data$PoolQC = as.integer(revalue(our.data$PoolQC, qualities))
table(our.data$PoolQC)

#PoolArea 
# Check how many houses has PoolArea > 0 but PoolQC = 0
our.data[our.data$PoolArea > 0 & our.data$PoolQC == 0, c('PoolArea', 'PoolQC', 'SalePrice', 'OverallQual')]

our.data$PoolQC[2421] = 2
our.data$PoolQC[2504] = 3
our.data$PoolQC[2600] = 2

#MiscFeature
# MiscFeature contains 96% missing value

# Assign None to NAs
our.data$MiscFeature[is.na(our.data$MiscFeature)] = 'None'

# convert it into factors
our.data$MiscFeature = as.factor(our.data$MiscFeature)

#
# --------------------------------WORKSPACE SAVE------------------------
#

# Alley has 93.2% of missing data

# Assign None to NA
our.data$Alley[is.na(our.data$Alley)] = 'None'

# change it to factors
our.data$Alley = as.factor(our.data$Alley)
table(our.data$Alley)

# Fence
# Fence contains 80.44% of missing data

# Assign None to NA
our.data$Fence[is.na(our.data$Fence)] = 'None'

# change it to factors
our.data$Fence = as.factor(our.data$Fence)

table(our.data$Fence)

# Fireplaces
# Assign none to NA

our.data$FireplaceQu[is.na(our.data$FireplaceQu)] = 'none'

our.data$FireplaceQu = as.integer(revalue(our.data$FireplaceQu, qualities))

table(our.data$FireplaceQu)
sum(table(our.data$FireplaceQu))

table(our.data$Fireplaces)
sum(table(our.data$Fireplaces))


# LotFrontage - 16.65% of missing data
# LotFrontage is related to Neighbourhood

# Impute the missing value of LotFrontage using mean function
for(i in 1:nrow(our.data)){
  if(is.na(our.data$LotFrontage[i])){
    our.data$LotFrontage[i] = as.integer(mean(our.data$LotFrontage[our.data$Neighborhood == our.data$Neighborhood[i]], 
                                              na.rm = T))
  }
}

# Lot Shape - ordinal values
our.data$LotShape = as.integer(revalue(our.data$LotShape, 
                                       c('IR3' = 0, 'IR2' = 1, 'IR1' = 2, 'Reg' = 3)))

table(our.data$LotShape)

# LotConfig
our.data$LotConfig = as.factor(our.data$LotConfig)

# Garbage Variables
# Altogether there are 7 variables related to garage

# GarageYrBlt
# First of all replace all NAs of GarageYrBlt with the YearBuilt
our.data$GarageYrBlt[is.na(our.data$GarageYrBlt)] = our.data$YearBuilt[is.na(our.data$GarageYrBlt)]

# check unique value of GarageYrBlt
unique(our.data$GarageYrBlt)

# Here we observe that there is one row which having GarageYrBlt is 2207, which seems to be incorrect,
# so we change it with corresponding value of year built

our.data$Id[our.data$GarageYrBlt == 2207] # to find house Id

our.data$GarageYrBlt[our.data$Id == 2593] = our.data$YearBuilt[our.data$Id == 2593]

# We have 3 variables (GarageFinish, GarageQual, GarageCond) each contains 159 missing values
# and one variable (GarageType) contains 157 missing value
# Lets check how many have common missing values

length(which(is.na(our.data$GarageFinish) & is.na(our.data$GarageQual) 
             & is.na(our.data$GarageCond) & is.na(our.data$GarageType)))

# lets find 2 additional NAs
our.data$Id[!is.na(our.data$GarageType) & is.na(our.data$GarageFinish)]

our.data[c(2127, 2577), c('GarageType', 'GarageArea','GarageFinish', 'GarageQual', 'GarageCond' )]

# House 2127 actually have Garage, so we impute them with most common value (mode) for GarageFinish, 
# GarageQual, GarageCond

our.data$GarageFinish[2217] = names(sort(table(our.data$GarageFinish), decreasing = T)[1])
our.data$GarageQual[2217] = names(sort(table(our.data$GarageQual), decreasing = T)[1])
our.data$GarageCond[2217] = names(sort(table(our.data$GarageCond), decreasing = T)[1])

kable(our.data[2217, c('GarageType', 'GarageArea','GarageFinish', 'GarageQual', 'GarageCond' )])

# House 2577
kable(our.data[2577, c('GarageType', 'GarageArea','GarageFinish', 'GarageQual', 'GarageCond', 
                       'GarageCars')])

our.data$GarageType[2577] = 'none'
our.data$GarageArea[2577] = 0
our.data$GarageFinish[2577] = 'none'
our.data$GarageQual[2577] = 'none'
our.data$GarageCond[2577] = 'none'
our.data$GarageCars[2577] = 0

# Assign none to all NAs of 4 garage varaible

our.data$GarageFinish[is.na(our.data$GarageFinish)] = 'none'
our.data$GarageQual[is.na(our.data$GarageQual)] = 'none'
our.data$GarageCond[is.na(our.data$GarageCond)] = 'none'
our.data$GarageType[is.na(our.data$GarageType)] = 'none'

our.data$GarageQual = as.integer(revalue(our.data$GarageQual, qualities))
table(our.data$GarageQual)

our.data$GarageCond = as.integer(revalue(our.data$GarageCond, qualities))
table(our.data$GarageCond)

# GarageFin seems to have ordinal values so we revalue it
our.data$GarageFinish = as.integer(revalue(our.data$GarageFinish, 
                                           c('none' = 0, 'Unf' = 1, 'RFn' = 2, 'Fin' = 3)))
table(our.data$GarageFinish)

# GarageType is not looking as ordinal variable, so convert it to factor
our.data$GarageType = as.factor(our.data$GarageType)

#
# WORKSPACE SAVED
#

# Basement Variables
bsmt_vars = c('BsmtQual', 'BsmtCond', 'BsmtExposure', 
              'BsmtFinType1', 'BsmtFinSF1', 'BsmtFinType2', 
              'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF', 'BsmtFullBath', 'BsmtHalfBath')
# Altogether, there are 12 basement variables
# BsmtCond, BsmtExposure contains 82 missing value each and BsmtQual contains 81 missing values
# Check common missing value


length(which(is.na(our.data$BsmtCond) & is.na(our.data$BsmtExposure) & is.na(our.data$BsmtQual) &
               is.na(our.data$BsmtFinType2) & is.na(our.data$BsmtFinType1)))

# lets find 3 additional NAs
our.data$Id[is.na(our.data$BsmtQual) & !is.na(our.data$BsmtCond) & !is.na(our.data$BsmtExposure)]

# First lets check houses 2218 and 2219
our.data[c(2218, 2219), bsmt_vars]

# Impute most common values to BsmtQual
our.data$BsmtQual[2218] = names(sort(table(our.data$BsmtQual), decreasing = T)[1])
our.data$BsmtQual[2219] = names(sort(table(our.data$BsmtQual), decreasing = T)[1])

# Lets find 3 additional NAs in BsmtCond to BsmtQual
our.data$Id[!is.na(our.data$BsmtQual) & is.na(our.data$BsmtCond)]

# [1] 2041 2186 2525
# Check these above three houses first
our.data[c(2041, 2186, 2525), bsmt_vars]

# Impute most common values to BsmtCond
our.data$BsmtCond[2041] = names(sort(table(our.data$BsmtCond), decreasing = T)[1])
our.data$BsmtCond[2186] = names(sort(table(our.data$BsmtCond), decreasing = T)[1])
our.data$BsmtCond[2525] = names(sort(table(our.data$BsmtCond), decreasing = T)[1])

# Lets find 3 additional NAs in BsmtExposure to BsmtQual
our.data$Id[!is.na(our.data$BsmtQual) & is.na(our.data$BsmtExposure)]

# [1]  949 1488 2349
# Check these above three houses first
our.data[c(949, 1488, 2349), bsmt_vars]

table(our.data$BsmtExposure)

# Impute most common values to BsmtExposure
our.data$BsmtExposure[949] = names(sort(table(our.data$BsmtExposure), decreasing = T)[1])
our.data$BsmtExposure[1488] = names(sort(table(our.data$BsmtExposure), decreasing = T)[1])
our.data$BsmtExposure[2349] = names(sort(table(our.data$BsmtExposure), decreasing = T)[1])

# Find additional 1 NA in BsmtFinType2
our.data$Id[!is.na(our.data$BsmtFinType1) & is.na(our.data$BsmtFinType2)]

our.data[333, bsmt_vars]
# Impute most common values to BsmtFinType2
our.data$BsmtFinType2[333] = names(sort(table(our.data$BsmtFinType2), decreasing = T)[1])

# Assign none to all 79 NAs
our.data$BsmtCond[is.na(our.data$BsmtCond)] = 'none'
our.data$BsmtExposure[is.na(our.data$BsmtExposure)] = 'none'
our.data$BsmtQual[is.na(our.data$BsmtQual)] = 'none'
our.data$BsmtFinType1[is.na(our.data$BsmtFinType1)] = 'none'
our.data$BsmtFinType2[is.na(our.data$BsmtFinType2)] = 'none'

# BsmtFullBath and BsmtHalfBath both have 2 NAs
# Check common
length(which(is.na(our.data$BsmtFullBath) & is.na(our.data$BsmtHalfBath)))

which(is.na(our.data$BsmtFullBath))
our.data[c(2121, 2189), bsmt_vars]      

our.data$BsmtFullBath[c(2121, 2189)] = 0
our.data$BsmtHalfBath[c(2121, 2189)] = 0
our.data$BsmtFinSF1[2121] = 0
our.data$BsmtFinSF2[2121] = 0
our.data$BsmtUnfSF[2121] = 0
our.data$TotalBsmtSF[2121] = 0


# Label Encoding for Basement Variables
# "BsmtQual" ordinal value  
our.data$BsmtQual = as.integer(revalue(our.data$BsmtQual, qualities))
table(our.data$BsmtQual)

# "BsmtCond" ordinal value
our.data$BsmtCond = as.integer(revalue(our.data$BsmtCond, qualities))
table(our.data$BsmtCond)

# "BsmtExposure" ordinal value
our.data$BsmtExposure = as.integer(revalue(our.data$BsmtExposure, 
                                           c('none' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)))
table(our.data$BsmtExposure)

# "BsmtFinType1" ordinal values
our.data$BsmtFinType1 = as.integer(revalue(our.data$BsmtFinType1, 
                                           c('none' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 
                                             'ALQ' = 5, 'GLQ' = 6)))
table(our.data$BsmtFinType1)

# "BsmtFinType2" 
our.data$BsmtFinType2 = as.integer(revalue(our.data$BsmtFinType2, 
                                           c('none' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 
                                             'ALQ' = 5, 'GLQ' = 6)))
table(our.data$BsmtFinType2)

# Masonary Variable
# MasVnrType and MasVnrArea both have missing value
# Check common missing value

length(which(is.na(our.data$MasVnrArea) & is.na(our.data$MasVnrType)))

# lets find the addition NA
our.data$Id[is.na(our.data$MasVnrType) & !is.na(our.data$MasVnrArea)]

# check house 2611
our.data[2611, c('MasVnrArea', 'MasVnrType')]

# Impute the 2nd most common value
our.data$MasVnrType[2611] = names(sort(table(our.data$MasVnrType), decreasing = T)[2])

# Assign none to all 23 remainging NA
our.data$MasVnrArea[is.na(our.data$MasVnrArea)] = 0
our.data$MasVnrType[is.na(our.data$MasVnrType)] = 'None'

# Label Encoding

# MasVnrType
our.data$MasVnrType = as.factor(our.data$MasVnrType)

# MSZoning

# Impute with most number of value
our.data$MSZoning[is.na(our.data$MSZoning)] = names(sort(table(our.data$MSZoning), decreasing = T)[1])
table(our.data$MSZoning)

#
# WORKSPACE SAVED
#

# Kitchen Qualities
# KitchenQual

table(our.data$KitchenQual)
our.data$Id[is.na(our.data$KitchenQual)]

our.data[1556, c('KitchenQual', 'KitchenAbvGr')]

# impute with most common value of KitchenQual
our.data$KitchenQual[1556] = names(sort(table(our.data$KitchenQual), decreasing = T)[1])

# KitchenQual - ordinal variable
our.data$KitchenQual = as.integer(revalue(our.data$KitchenQual, qualities))

# Utilities
table(our.data$Utilities)
our.data$Id[is.na(our.data$Utilities)]

# Impute the most common values of Utilities
our.data$Utilities[is.na(our.data$Utilities)] = names(sort(table(our.data$Utilities), decreasing = T)[1])

# Utilities - ordinal value
our.data$Utilities = as.integer(revalue(our.data$Utilities, c('ELO' = 1, 'NoSeWa' = 2, 
                                                              'NoSewr' = 3, 'AllPub' = 4)))

# Functional
unique(our.data$Functional)
table(our.data$Functional)

# Impute with Typ
our.data$Functional[is.na(our.data$Functional)] = "Typ"

# Functional - ordinal
our.data$Functional = as.integer(revalue(our.data$Functional, 
                                         c('Sal' = 0, 'Sev' = 1, 'Maj2' = 2, 
                                           'Maj1' = 3, 'Mod' = 4, 'Min2' = 5, 
                                           'Min1' = 6, 'Typ' = 7)))

# Exterior1st
table(our.data$Exterior1st)

# Impute with most common values
our.data$Exterior1st[is.na(our.data$Exterior1st)] = names(sort(table(our.data$Exterior1st), decreasing = T)[1])

# Exterior1st - not ordinal
our.data$Exterior1st = as.factor(our.data$Exterior1st)

# Exterior2nd
table(our.data$Exterior2nd)

# Impute with most common values
our.data$Exterior2nd[is.na(our.data$Exterior2nd)] = names(sort(table(our.data$Exterior2nd), decreasing = T)[1])

# Exterior2nd - not ordinal
our.data$Exterior2nd = as.factor(our.data$Exterior2nd)

# ExterQual - ordinal
table(our.data$ExterQual)
our.data$ExterQual = as.integer(revalue(our.data$ExterQual, qualities))

# ExterCond - ordinal
table(our.data$ExterCond)
our.data$ExterCond = as.integer(revalue(our.data$ExterCond, qualities))

# Electrical
table(our.data$Electrical)

# impute with most common value
our.data$Electrical[is.na(our.data$Electrical)] = names(sort(table(our.data$Electrical), decreasing = T)[1])

# Electrical - not ordinal
our.data$Electrical = as.factor(our.data$Electrical)

# Sale Type
table(our.data$SaleType)
which(is.na(our.data$SaleType))

# impute with most common value
our.data$SaleType[is.na(our.data$SaleType)] = names(sort(table(our.data$SaleType), decreasing = T)[1])

# SaleType - not ordinal
our.data$SaleType = as.factor(our.data$SaleType)

# SaleCond - not ordinal
our.data$SaleCondition = as.factor(our.data$SaleCondition)

# CHARACTER COLUMN (CHARCOL)
charcol = names(our.data[, sapply(our.data, is.character)])

paste("Total Character column: ", length(charcol))

# "MSZoning"
# MSZoning - not ordinal

our.data$MSZoning = as.factor(our.data$MSZoning)

# "Street"   
# Street have only two value so convert to integer

our.data$Street = as.integer(revalue(our.data$Street, c('Grvl' = 0, 'Pave' = 1)))

# "LandContour"  
# Not ordinal

our.data$LandContour = as.factor(our.data$LandContour)

# "LandSlope"  
# Ordinal

our.data$LandSlope = as.integer(revalue(our.data$LandSlope, c('Sev' = 0, 'Mod' = 1, 'Gtl' = 2)))

# "Neighborhood" 
# not ordinal

our.data$Neighborhood = as.factor(our.data$Neighborhood)

# "Condition1"   
# Not ordinal

our.data$Condition1 = as.factor(our.data$Condition1)

# "Condition2"  
# not ordinal

our.data$Condition2 = as.factor(our.data$Condition2)

# "BldgType"  
# not ordinal
our.data$BldgType = as.factor(our.data$BldgType)

# "HouseStyle"   
# not ordinal

our.data$HouseStyle = as.factor(our.data$HouseStyle)

# "RoofStyle" 
# not ordinal

our.data$RoofStyle = as.factor(our.data$RoofStyle)

# "RoofMatl"  
# not ordinal

our.data$RoofMatl = as.factor(our.data$RoofMatl)

# "Foundation"
# not ordinal

our.data$Foundation = as.factor(our.data$Foundation)

# "Heating"  
# not ordinal

our.data$Heating = as.factor(our.data$Heating)

# "HeatingQC" 
# ordinal

our.data$HeatingQC = as.integer(revalue(our.data$HeatingQC, qualities))

# "CentralAir"   
# label encoding

our.data$CentralAir = as.integer(revalue(our.data$CentralAir, c('N' = 0, 'Y' = 1)))


# "PavedDrive"  
# label encoding

our.data$PavedDrive = as.integer(revalue(our.data$PavedDrive, c('N' = 0, 'P' = 1, 'Y' = 2)))

#
# WORKSPACE SAVED
# 


# 
# ------------ some numerical to factor
# 

# YrSold

unique(our.data$YrSold)

# factorisation
our.data$YrSold = as.factor(our.data$YrSold)

# MoSold
unique(our.data$MoSold)

# factorisation
our.data$MoSold = as.factor(our.data$MoSold)

#
# WORKSPACE SAVED
# 

# MSSubClass 
# MSSubClass - categorical variable convert it to factors
our.data$MSSubClass = as.factor(our.data$MSSubClass)
table(our.data$MSSubClass)


#
numericVars = which(sapply(our.data, is.numeric))
catVars = which(sapply(our.data, is.factor))
charcol = which(sapply(our.data, is.character))

paste("Total Numeric Variables: " , length(numericVars))
paste("Total Categorical Variables: " , length(catVars))
paste("Total Character Variables: ", length(charcol))
paste("Total variables: ", length(our.data))

# Dropping column "Id" and "DataFrom"

our.data$Id = NULL
our.data$DataFrom = NULL

# Check correlation
numericVars = which(sapply(our.data, is.numeric))
numericVarsName = names(numericVars)

our.data_numVar = our.data[, numericVars]
cor_numVar = cor(our.data_numVar, use = 'pairwise.complete.obs')

# sort on decreasing correlation with sale price
cor_sorted = as.matrix(sort(cor_numVar[, 'SalePrice'], decreasing = T))

corHigh = names(which(apply(cor_sorted, 1, function(x){
  abs(x) > 0.5
})))

cor_numVar = cor_numVar[corHigh, corHigh]
corrplot.mixed(cor_numVar, tl.pos = 'lt', tl.col = 'black')

# Visualising the importance of Variable using quick random forest
quick_rf = randomForest(x = our.data[1:1460, -80], y = our.data$SalePrice[1:1460], ntree = 100, 
                        importance = T)
imp_rf = importance(quick_rf)
imp_df = data.frame(Variable = rownames(imp_rf), MSE = imp_rf[, 1])

imp_df = imp_df[order(imp_df$MSE, decreasing = T), ]

#
# WORKSPACE SAVED
# 

# Bathroom Variables
# There are total 4 Bathroom Variables - BsmtFullBath, BsmtHalfBath, FullBath, HalfBath
# Accomodate them to one Variable - TotalBathroom

our.data$TotalBathroom = (our.data$BsmtFullBath) + (our.data$BsmtHalfBath * 0.5) +
  (our.data$FullBath) + (our.data$HalfBath * 0.5)


# Check correlation b/w 1stFlrSF, 2ndFlrSF, LowQualFinSF and GrLivArea
# I think GrLivArea = 1stFlrSF + 2ndFlrSF + LowQualFinSF

cor(our.data$GrLivArea, our.data$X1stFlrSF + our.data$X2ndFlrSF + our.data$LowQualFinSF) # Highly correlated

# Lets make a copy of our data frame
our.data.mod = our.data

# Dropping features form our.data.mod
# Bathroom Variables
our.data.mod$FullBath = NULL
our.data.mod$BsmtFullBath = NULL
our.data.mod$HalfBath = NULL
our.data.mod$BsmtHalfBath = NULL

# Living area variables
our.data.mod$X1stFlrSF = NULL
our.data.mod$X2ndFlrSF = NULL
our.data.mod$LowQualFinSF = NULL
# our.data.mod$TotalLivArea = NULL


# Creating dummy variables for categorical variables
dummy.model = dummyVars(SalePrice ~ ., data = our.data.mod)

# Create dummy variable using predict
our.data.mod.dummy = predict(dummy.model, newdata = our.data.mod)
our.data.mod.dummy = data.frame(our.data.mod.dummy)

str(our.data.mod.dummy)
which(colnames(our.data.mod.dummy) == 'SalePrice')

sum(is.na(our.data.mod.dummy))

# Preprocess the data
preprocess_range_model = preProcess(our.data.mod.dummy, method = 'range')
our.data.mod.dummy.pp = predict(preprocess_range_model, our.data.mod.dummy)

our.data.mod.dummy.pp$SalePrice = our.data$SalePrice

# # Feature Scaling using Recursive Feature Elimination (rfe)
# ctrl = rfeControl(functions = rfFuncs, 
#                   method = 'repeatedcv', 
#                   repeats = 5, 
#                   verbose = F)
# 
# lmProfile = rfe(x = our.data.mod.dummy.pp[1:1460, -253], 
#                 y = our.data.mod.dummy.pp$SalePrice[1:1460], 
#                 rfeControl = ctrl, 
#                 sizes = 3)
# 
# lmProfile


#
# WORKSPACE SAVED
# 

# Sampling the data
train_data = our.data.mod.dummy.pp[!is.na(our.data.mod.dummy.pp$SalePrice), ]
test_data = our.data.mod.dummy.pp[is.na(our.data.mod.dummy.pp$SalePrice), ]


# Splitting the train_data
indexes = createDataPartition(train_data$SalePrice, 
                              times = 1, 
                              p = 0.7, 
                              list = F)
trd = train_data[indexes, ]
tsd = train_data[-indexes, ]


# ---------------------------LM Model-------------------------------

# fit a linear regressor model on trd data
lm.model = lm(formula = SalePrice ~., 
              data = trd)
summary(lm.model)

our.predict.lm = predict(lm.model, newdata = test_data)

# our.result - lm 
our.result = data.frame(Id = test$Id, SalePrice = our.predict.lm)
write.csv(our.result, file = "output/submission_lm.csv", row.names = F)

# fit a linear regressor model on full training data
lm.model.full = lm(formula = SalePrice ~., 
              data = train_data)
summary(lm.model.full)

our.predict.lm.full = predict(lm.model.full, newdata = test_data)

# our.result - lm Full
our.result = data.frame(Id = test$Id, SalePrice = our.predict.lm.full)
write.csv(our.result, file = "output/submission_lm_full.csv", row.names = F)

# ------------------------GLM Model-------------------------------------
# fit into a model

glm.model.full = glm(formula = SalePrice ~., data = train_data)

summary(glm.model.full)


# Predict tsd values
our.predict.glm.full = predict(glm.model.full, newdata = test_data)

# our.result - glm Full
our.result = data.frame(Id = test$Id, SalePrice = our.predict.glm.full)
write.csv(our.result, file = "output/submission_glm_full.csv", row.names = F)

# fit into a model

glm.model= glm(formula = SalePrice ~., data = trd)

summary(glm.model)


# Predict tsd values
our.predict.glm = predict(glm.model, newdata = test_data)

# our.result - glm Full
our.result = data.frame(Id = test$Id, SalePrice = our.predict.glm)
write.csv(our.result, file = "output/submission_glm.csv", row.names = F)

#
# WORKSPACE SAVED
# 



# ---------------------------- XGBOOST--------------------------------------
xg.model = xgboost(data = as.matrix(train_data[-253]), 
                   label = train_data$SalePrice, 
                   nrounds = 10)
our.predict.xgboost = predict(xg.model, newdata = as.matrix(test_data[-253]))
summary(xg.model)

# our.result - xgboost
our.result = data.frame(Id = test$Id, SalePrice = our.predict.xgboost)
write.csv(our.result, file = "output/submission_xgb.csv", row.names = F)



#
# WORKSPACE SAVED
# 

# ------ XGBoost with CARET --------------------
xgb_trcontrol = trainControl(method = "cv", 
                             number = 5, 
                             allowParallel = F, 
                             verboseIter = F, 
                             returnData = F)

xgbGrid = expand.grid(nrounds = c(50,100),  
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1)

xgb.model.tuned = train(train_data[-253], 
                        train_data$SalePrice, 
                        trControl = xgb_trcontrol, 
                        tuneGrid = xgbGrid, 
                        method = 'xgbTree')
y_pred_xgb_tuned = predict(xgb.model.tuned, test_data)

xgb.model.tuned$bestTune

# our.result - xgboost - tuned
our.result = data.frame(Id = test$Id, SalePrice = y_pred_xgb_tuned)
write.csv(our.result, file = "output/submission_xgb_tuned.csv", row.names = F)