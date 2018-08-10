install.packages("ggmap")
install.packages("ggplot2")

library(ggmap)
library(ggplot2)

pharmacy <- read.csv("./DATA/MAC/busan_suyoung_pharmacy.csv", sep=",", header=TRUE)
View(pharmacy)
pharmacy$경도 = as.numeric(as.character(pharmacy$경도))
pharmacy$위도 = as.numeric(as.character(pharmacy$위도))

cent <- c(mean(pharmacy$경도),mean(pharmacy$위도))

map <- ggmap(get_googlemap(center = cp, zoom=13, maptype='roadmap', color='color', scale=2), extent='device')
map + geom_point(data=pharmacy, aes(x=경도, y=위도), colour='red', alpha=0.5)
