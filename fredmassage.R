setwd("/Users/Charley/Downloads/Plotly/Plotly Data Link Test")

library(plyr)

library(dplyr)

hp <- read.csv("Housing_Prices.csv")
hp <- melt(hp, id.vars="DATE", variable.name="Mnemonic", value.name= "HPI")
hp$MetroCode <- factor(substr(hp$Mnemonic,1,2))
hp$TierCode <- factor(substr(hp$Mnemonic,5,5))
hp$Seasonality <- factor(substr(hp$Mnemonic,7,apply(array(hp$Mnemonic),1,nchar)))

metrotable <- data.frame(MetroCode = levels(hp$Metro), 
                         Metro = factor(c("Atlanta", "Boston", "Denver", "Las Vegas",
                                           "Los Angeles", "Miami", "Minneapolis", 
                                          "Phoenix","Portland", "San Diego", "Seattle", 
                                          "San Francisco", "Tampa", "Washington DC")))

hp <- select(merge(hp, metrotable, by= "MetroCode"), -(MetroCode))

tiertable <- data.frame(TierCode = levels(hp$TierCode), 
                        Tier = factor(c("High", "Low", "Middle")))

hp <- select(merge(hp, tiertable, by= "TierCode"), -c(TierCode, Mnemonic))

hp <- hp[,c(1,3,4,5,2)]

rm(metrotable, tiertable)

hpcast <- dcast(hp, DATE ~ Seasonality + Metro + Tier, value.var="HPI")

hpSAbos <- hp %>%
  filter(Seasonality == "SA", Metro == "Boston") %>%
  select(-c(Seasonality, Metro)) %>%
  arrange(Tier, DATE)

write.csv(hpSAbos, "hpsa.csv", row.names = FALSE)
