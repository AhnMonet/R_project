library(ggplot2)
library(gridExtra)
library(dplyr)

hot <- read.csv("Data/weather/hot_data.csv", header = T)
rain <- read.csv("Data/weather/rain.csv", header = T)
dust <- read.csv("Data/weather/yellowDust.csv", header = T)
avg <- read.csv("Data/weather/average30Years.csv", header = T)
temp90 <- read.csv("Data/weather/temp~1990.csv", header = T)
temp00 <- read.csv("Data/weather/temp~2000.csv", header = T)
temp10 <- read.csv("Data/weather/temp~2010.csv", header = T)
rainVar <- read.csv("Data/weather/rainVar.csv", header = T)

hot <- rename(hot,
              "hot" = "tot")
rain <- rename(rain,
              "rain" = "tot")
dust <- rename(dust,
              "dust" = "tot")

weather <- as.data.frame(c(rain[1], hot[14], rain[14], dust[14]))

windows()
par(mfrow = c(2, 2))

# 같이
plot(x = weather$year, y = weather$rain, type = "l", col = "skyblue", ylim = c(0, 130), 
     xlab = "", ylab = "", lwd = 2)
lines(x = weather$year, y = weather$dust, type = "l", col = "orange", lwd = 2)
lines(x = weather$year, y = weather$hot, type = "l", col = "red", lwd = 2)
title(main="1980 ~ 2017년 강수 / 황사 / 폭염 일수" , col.main="darkgreen",font.main=4)
title(xlab="YEAR", col.lab="black")
title(ylab="DAY",col.lab="black")

# 폭염
plot(x = weather$year, y = weather$hot, type = "l", col = "red",
     xlab = "", ylab = "", lwd = 2)
title(main="1980 ~ 2017년 폭염일수" , col.main="darkgreen",font.main=4)
title(xlab="YEAR", col.lab="black")
title(ylab="DAY",col.lab="black")

# 강수 
plot(x = weather$year, y = weather$rain, type = "l", col = "skyblue", 
     xlab = "", ylab = "", lwd = 2)
title(main="1980 ~ 2017년 강수일수" , col.main="darkgreen",font.main=4)
title(xlab="YEAR", col.lab="black")
title(ylab="DAY",col.lab="black")

# 황사
plot(x = weather$year, y = weather$dust, type = "l", col = "orange",
     xlab = "", ylab = "", lwd = 2)
title(main="1980 ~ 2017년 황사일수" , col.main="darkgreen",font.main=4)
title(xlab="YEAR", col.lab="black")
title(ylab="DAY",col.lab="black")


###

high_rain <- avg %>% 
  arrange(-rain) %>% 
  head(10)
row_rain <- avg %>% 
  arrange(rain) %>% 
  head(10)

windows()
p1 <- ggplot(high_rain, aes(region, rain, fill = region)) +
  geom_col() +
  labs(title = "강수량이 높은 지역 TOP 10",
       y = "강수량") +
  geom_hline(yintercept = mean(avg$rain), linetype = "dashed") +
  theme(legend.position = "left",
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank())

p2 <- ggplot(row_rain, aes(region, rain, fill = region)) +
  geom_col() +
  labs(title = "강수량이 낮은 지역 TOP 10",
       y = "강수량") +
  geom_hline(yintercept = mean(avg$rain), linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank())

grid.arrange(p1, p2, ncol=2)



##

temp90 <- temp90 %>%
  group_by(high.row) %>% 
  filter(region %in% c("서울", "부산", "인천", "대구", "대전", "광주", "울산"))
temp00 <- temp00 %>%
  group_by(high.row) %>% 
  filter(region %in% c("서울", "부산", "인천", "대구", "대전", "광주", "울산"))
temp10 <- temp10 %>%
  group_by(high.row) %>% 
  filter(region %in% c("서울", "부산", "인천", "대구", "대전", "광주", "울산"))


windows()
p1 <- ggplot(temp90, aes(x = region, y = temp, fill = high.row)) +
  geom_col() +
  labs(title = "1961 ~ 1990",
       y = "최고/최저 기온( ℃ )") +
  geom_hline(yintercept = mean(temp$temp), linetype = "dashed") +
  geom_hline(yintercept = 30, linetype = "dashed", col = "red") +
  geom_hline(yintercept = 10, linetype = "dashed", col = "blue") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_blank(),
        legend.position = "none")
p2 <- ggplot(temp00, aes(x = region, y = temp, fill = high.row)) +
  geom_col() +
  labs(title = "1971 ~ 2000",
       y = "최고/최저 기온( ℃ )") +
  geom_hline(yintercept = mean(temp$temp), linetype = "dashed") +
  geom_hline(yintercept = 30, linetype = "dashed", col = "red") +
  geom_hline(yintercept = 10, linetype = "dashed", col = "blue") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
p3 <- ggplot(temp10, aes(x = region, y = temp, fill = high.row)) +
  geom_col() +
  labs(title = "1981 ~ 2010",
       y = "최고/최저 기온( ℃ )") +
  geom_hline(yintercept = mean(temp$temp), linetype = "dashed") +
  geom_hline(yintercept = 30, linetype = "dashed", col = "red") +
  geom_hline(yintercept = 10, linetype = "dashed", col = "blue") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

grid.arrange(p1, p2, p3, ncol=3)


