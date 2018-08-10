install.packages("ggmap")
install.packages("ggplot2")

library(ggmap)
library(ggplot2)

wifi <- read.csv("./DATA/MAC/busan_haeundae_free_wifi.csv", sep=",", header=TRUE)

str(wifi)

View(wifi)

public_wifi <- subset(wifi, select = c("위도", "경도", "관리기관명", "서비스제공사명"))

summary(public_wifi)

cent <- c(mean(public_wifi$경도),mean(public_wifi$위도))
map <- ggmap(get_googlemap(center = cent, zoom=13, maptype='roadmap', color='color', scale=2), extent='device')
map + geom_point(data=public_wifi, aes(x=경도, y=위도), colour='red', alpha=0.5)
