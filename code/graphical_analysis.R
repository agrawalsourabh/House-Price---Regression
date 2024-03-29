# Print top 10 missing value
ggplot(data = missing_data[1:10,], mapping = aes(x = reorder(variable, perc), y = perc)) +
  geom_bar(stat = "identity", fill = "#f79567", col = "#e86427", alpha = 0.8) +
  coord_flip() +
  ggtitle("Top 10 variable contain missing value") +
  xlab("Variable") +
  ylab("Percentage") +
  geom_label(label = paste(missing_data$perc[1:10], "%"))

ggsave("plot/top_10_missing.png")

# Dependent Variable - SalePrice
skew = round(skewness(our.data$SalePrice[!is.na(our.data$SalePrice)]), digits = 2)
ggplot(data = our.data[!is.na(our.data), ], mapping = aes(x = SalePrice)) +
  geom_density(fill = "#90edad", col = "#4cef7f", alpha = 0.8) +
  scale_x_continuous(labels = comma) +
  annotate("text", -Inf, Inf, label = paste("Skewness = ", skew), 
           hjust = 0, vjust = 10)+
  ggtitle("Sale Price Density Graph")

ggsave("plot/saleprice_density.png")

# Overall Quality
ggplot(data = our.data[!is.na(our.data$SalePrice), ], 
       mapping = aes(x = factor(OverallQual), y = SalePrice)) +
  geom_boxplot(col = "#db471a") +
  ggtitle("Overall Quality") +
  xlab("Overall Quality") +
  ylab("Sale Price") +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), label = comma)

ggsave("plot/OverallQual_box.png")

# GrLivingArea - Above grade (ground) living area square feet
ggplot(data = our.data[!is.na(our.data$SalePrice), ], 
       mapping = aes(x = GrLivArea, y = SalePrice)) +
  geom_point(col = "#c43323") +
  geom_smooth(method = "lm", se = F, col = "#514f4f") +
  ggtitle(" Ground Living Area with Sale Price") +
  xlab(" Ground Living Area (Sq ft.)") +
  ylab(" Sale Price ") +
  scale_y_continuous(breaks = seq(0, 800000, 100000), label = comma) +
  geom_text_repel(aes(label = ifelse(our.data$GrLivArea[!is.na(our.data$SalePrice)]>4500, 
                                     rownames(our.data), '')))

ggsave("plot/GroundLivingArea_point.png")

# MiscFeature
ggplot(data = our.data[!is.na(our.data$SalePrice), ], 
       mapping = aes(x = MiscFeature, y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = "#f2cb3e", col = "#efc21f", alpha = 0.8) +
  ggtitle("Misc Feature") +
  xlab(" Misc Feature") +
  ylab(" Sale Price ") +
  scale_y_continuous(breaks = seq(0, 300000, by = 50000), labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..))

ggsave("plot/MiscFeature_bar.png")

#
# --------------------------------WORKSPACE SAVE------------------------
#

# Alley
ggplot(data = our.data[!is.na(our.data$SalePrice), ], 
       mapping = aes(x = Alley, y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = "#7c8ca5", col = "#476ba3", alpha = 0.8) +
  ggtitle("Alley - Sale Price") +
  xlab("Alley") +
  ylab("Sale Price") +
  scale_y_continuous(breaks = seq(0, 300000, by = 50000), labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..))

ggsave("plot/Alley_bar.png")

# Fence
ggplot(data = our.data[!is.na(our.data$SalePrice), ], 
       mapping = aes(x = Fence, y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = "#87edc9", col = "#036b46", alpha = 0.8) +
  ggtitle("Fence - Sale Price") +
  xlab("Fence") +
  ylab("Sale Price") +
  scale_y_continuous(breaks = seq(0, 300000, by = 50000), labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..))

ggsave("plot/Fence_bar.png")


# LotFrontage
ggplot(data = our.data[!is.na(our.data$SalePrice), ], 
       mapping = aes(x = LotFrontage, y = SalePrice)) +
  geom_point(col = "#b71d37") +
  ggtitle(" LotFrontage ") +
  geom_smooth(method = 'lm', se = F) +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma)

ggplot(data = our.data[!is.na(our.data$LotFrontage), ], 
       mapping = aes(x = as.factor(Neighborhood) , y = LotFrontage)) +
  geom_bar(stat = 'summary', fun.y = 'mean', fill = "#4da1e2", col = "#08416d", alpha = 0.8) +
  ggtitle(" LotFrontage Vs Neighbourhood") +
  xlab("Neighborhood") +
  ylab("Lot Frontage (Mean) ") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10))

ggsave("plot/LotFrontageVsNeighbourhood_bar.png")

# LotConfig
ggplot(data = our.data[!is.na(our.data$SalePrice), ], 
       mapping = aes(x = LotConfig, y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'mean', fill = "#e8bd74", col = "#e89d1e", alpha = 0.8) +
  ggtitle("LotConfig Vs Sale Price(Mean)") +
  xlab("Lot Config") +
  ylab("Sale Price") 

ggsave("plot/LotConfigVsSP(Mean)_bar.png")

# GarageQual
# Check GarageQual and mean Sale Price
ggplot(data = our.data[!is.na(our.data$SalePrice), ], 
       mapping = aes(x = factor(GarageQual), y = SalePrice, label = "abc")) +
  geom_bar(stat = 'summary', fun.y = 'mean', fill = "#b2593e", col = "#ad330d", alpha = 0.8) +
  ggtitle("Garage Quality vs Sale Price (Mean)") +
  xlab("Garage Quality") +
  ylab("Sale Price") +
  scale_y_continuous(breaks = seq(0, 300000, by = 50000), labels = comma) +
  scale_x_discrete( label = c('none', 'Po', 'Fa','TA', 'Gd', 'Ex'))

ggsave("plot/GarageQualVsSP(Mean)_bar.png")

# GarageType
# Check GarageType and mean Sale Price
ggplot(data = our.data[!is.na(our.data$SalePrice), ], 
       mapping = aes(x = GarageType, y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'mean', fill = "#f2ef5e", col = "#fcf828", alpha = 0.8) +
  ggtitle("Garage Type vs Sale Price (Mean)") +
  xlab("Garage Type") +
  ylab("Sale Price") +
  scale_y_continuous(breaks = seq(0, 300000, by = 50000), labels = comma)

ggsave("plot/GarageTypeVsSP(Mean)_bar.png")

#
# WORKSPACE SAVED
#

# Basement Quality
# Check Basement Quality and mean Sale Price
ggplot(data = our.data[!is.na(our.data$SalePrice), ], 
       mapping = aes(x = factor(BsmtQual), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'mean', fill = "#92e89f", col = "#23ed42", alpha = 0.8) +
  ggtitle("Basement Quality vs Sale Price (Mean)") +
  xlab("Basement Quality") +
  ylab("Sale Price") +
  scale_y_continuous(breaks = seq(0, 300000, by = 50000), labels = comma) +
  scale_x_discrete(label = c('none', 'Fa', 'TA', 'Gd', 'Ex'))

ggsave("plot/BasementQualityVsSP(Mean)_bar.png")

#BsmtCond
ggplot(data = our.data[!is.na(our.data$SalePrice), ], 
       mapping = aes(x = factor(BsmtCond), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'mean', fill = "#db3d3d", col = "#e00f0f", alpha = 0.8) +
  ggtitle("Basement Condition vs Sale Price (Mean)") +
  xlab("Basement Condition") +
  ylab("Sale Price") +
  scale_y_continuous(breaks = seq(0, 300000, by = 50000), labels = comma) +
  scale_x_discrete(label = c('none', 'Po', 'Fa', 'TA', 'Gd'))

ggsave("plot/BasementConditionVsSP(Mean)_bar.png")

# MasVnrType
ggplot(data = our.data[!is.na(our.data$SalePrice), ], 
       mapping = aes(x = MasVnrType, y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'mean', fill = "#d1d849", col = "#c3cc14", alpha = 0.8) +
  ggtitle("Masonry veneer type vs Sale Price (Mean)") +
  xlab("Masonry veneer type") +
  ylab("Sale Price") +
  scale_y_continuous(breaks = seq(0, 300000, by = 50000), labels = comma)

ggsave("plot/MasVnrTypeVsSP(Mean)_bar.png")

#
# WORKSPACE SAVED
#

# KitchenQual
ggplot(data = our.data[!is.na(our.data$SalePrice), ], 
       mapping = aes(x = factor(KitchenQual), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'mean', fill = "#e0bf5a", color = "#d1a315", alpha = 0.8) +
  ggtitle(" Kitchen Quality vs Sale Price (Mean)") +
  xlab("Kitchen Quality") +
  ylab("Sale Price") +
  scale_y_continuous(breaks = seq(0, 300000, by = 50000), labels = comma) +
  scale_x_discrete(label = c('Fair', 'Average', 'Good', 'Excellent'))

ggsave("plot/KitchenQualVsSP(Mean)_bar.png")

# Functional
ggplot(data = our.data[!is.na(our.data$SalePrice), ], 
       mapping = aes(x = factor(Functional), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'mean', fill = "#80f2b6", col = "#1ceb7e", alpha = "0.8") +
  ggtitle("Functional vs Sale Price(Mean)") +
  xlab("Functional") +
  ylab("Sale Price") +
  scale_y_continuous(breaks = seq(0, 250000, by = 50000), labels = comma) +
  scale_x_discrete(label = c('Sev', 'Maj2', 'Maj1', 'Mod', 'Min2', 'Min1', 'Typ'))

ggsave("plot/FunctionalVsSP(Mean)_bar.png")

# ExterQual
our.data$ExterQual

ggplot(data = our.data[!is.na(our.data$SalePrice), ], 
       mapping = aes(x = factor(ExterQual), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'mean', fill = "#de8585", col = "#e30e0e", alpha = "0.8") +
  ggtitle("ExterQual vs SalePrice(Mean)") +
  xlab("ExterQual") +
  ylab("Sale Price") +
  scale_y_continuous(breaks = seq(0, 400000, by = 50000), label = comma) +
  scale_x_discrete(label = c('Fa', 'TA', 'Gd', 'Ex'))

ggsave("plot/ExterQualVsSP(Mean)_bar.png")

# ExterCond
our.data$ExterCond

ggplot(data = our.data[!is.na(our.data$SalePrice), ], 
       mapping = aes(x = factor(ExterCond), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'mean', fill = "#e9ed6d", col = "#e9f016", alpha = "0.8") +
  ggtitle("ExterCond vs SalePrice(Mean)") +
  xlab("ExterCond") +
  ylab("Sale Price") +
  scale_y_continuous(breaks = seq(0, 400000, by = 50000), label = comma) +
  scale_x_discrete(label = c('Po', 'Fa', 'TA', 'Gd', 'Ex'))

ggsave("plot/ExterCondVsSP(Mean)_bar.png")

#
# WORKSPACE SAVED
# 

our.data$MoSold
# YrSold and MoSold
par(mfrow = c(2, 2))
ys = ggplot(data = our.data[!is.na(our.data$SalePrice ), ], 
       mapping = aes(x = YrSold, y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'mean', fill = '#83f7ea', col = '#16f7de', alpha = '0.8') +
  ggtitle("YrSold vs Mean Sale Price") +
  xlab("Year Sold") +
  ylab("Sale Price")

ms = ggplot(data = our.data[!is.na(our.data$SalePrice ), ], 
       mapping = aes(x = MoSold, y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'mean', fill = '#97b9e6', col = '#1771e8', alpha = '0.8') +
  ggtitle("MoSold vs Mean Sale Price") +
  xlab("Month Sold") +
  ylab("Sale Price") +
  scale_x_discrete(label = c('Jan', "Feb", "Mar", "Apr", "May", 
                             "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 45))

grid.arrange(ys, ms, widths = c(1, 2))

ggsave('plot/YrSold_MoSold Vs SalePrice.png')


#
# WORKSPACE SAVED
# 


# Variable Importance - Top 20

ggplot(data = imp_df[1:20, ], 
       mapping = aes(x = reorder(Variable, MSE), y = MSE, fill = MSE)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  ggtitle("Variable Importance - Top 20") +
  xlab("Variables") +
  ylab("MSE") +
  theme(legend.position = 'none') +
  scale_y_continuous(breaks = seq(0, 20, by = 2))

ggsave('plot/variable_importance.png')

#
# WORKSPACE SAVED
# 
