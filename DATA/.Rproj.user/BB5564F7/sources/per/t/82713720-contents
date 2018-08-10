
data()

?midwest

# 기본 설정
options(scipen=999) 
library(ggplot2)
data("midwest", package = "ggplot2") 

str(midwest)
summary(midwest)

ggplot(midwest, aes(x=area, y=poptotal))  # 배경만 설정됨
ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() # 배경 + 데이터 (점)


# 신뢰 구간을 표시하지 않을려면 se = FALSE를 설정
g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method="lm")  
plot(g)

# X축 범위 수정 (V1)
g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method="lm")  
g + xlim(c(0, 0.1)) + ylim(c(0, 1000000))  # 축 범위를 벗어나는 데이터는 삭제


# X축 범위 수정 (V2) - 확대
g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method="lm")  
g1 <- g + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))  
plot(g1)


# 정보 설정 (X, Y 축 라벨, Plot 제목 등)
g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method="lm") 
g1 <- g + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))

# 방법 1
g1 + labs(title="면적 Vs 인구수", subtitle="미국 중서부(midwest) 인구 통계 데이터 이용", y="인구수", x="지역", caption="미국 중서부 인구통계")

# 방법 2
g1 + ggtitle("면적 Vs 인구수", subtitle="미국 중서부(midwest) 인구 통계 데이터 이용") + xlab("지역") + ylab("인구수")



함

# 색상 변경 

ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(col="steelblue", size=3) +   # steelblue 설정 및 크기는 3으로 지정
  geom_smooth(method="lm", col="firebrick") +  # 선을 firebrick으로 설정
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="면적 Vs 인구수", subtitle="미국 중서부(midwest) 인구 통계 데이터 이용", y="인구수", x="지역", caption="미국 중서부 인구통계")

gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # 색상을 주별로 표현
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="면적 Vs 인구수", subtitle="미국 중서부(midwest) 인구 통계 데이터 이용", y="인구수", x="지역", caption="미국 중서부 인구통계")

plot(gg)


gg + theme(legend.position="None")  # 범례 제거
gg + scale_colour_brewer(palette = "Set1")  # 컬러 팔레트 변경

# 컬러 파레트 정보
library(RColorBrewer)
head(brewer.pal.info, 10)  # show 10 palettes
#>          maxcolors category colorblind
#> BrBG            11      div       TRUE
#> PiYG            11      div       TRUE
#> PRGn            11      div       TRUE
#> PuOr            11      div       TRUE
#> RdBu            11      div       TRUE
#> RdGy            11      div      FALSE
#> RdYlBu          11      div       TRUE
#> RdYlGn          11      div      FALSE
#> Spectral        11      div      FALSE
#> Accent           8     qual      FALSE

# X축 Tick 수정

gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="면적 Vs 인구수", subtitle="미국 중서부(midwest) 인구 통계 데이터 이용", y="인구수", x="지역", caption="미국 중서부 인구통계")

gg <- gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01)) # 0에서 0.1까지 0.01로 쪼갬


# 테마로 그래프 스타일 변경


options(scipen=999)
library(ggplot2)
data("midwest", package = "ggplot2")
theme_set(theme_bw()) # BW 테마 적용

gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
  labs(title="면적 Vs 인구수", subtitle="미국 중서부(midwest) 인구 통계 데이터 이용", y="인구수", x="지역", caption="미국 중서부 인구통계")

plot(gg)

