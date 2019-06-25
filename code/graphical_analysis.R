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

# LotArea  