# installing packages
install.packages("ggrepel")

# loading packages
library(naniar)
library(ggplot2)
library(scales)
library(e1071)
library(corrplot)
library(plyr)
library(ggrepel)

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
