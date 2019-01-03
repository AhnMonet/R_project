library(dplyr)
library(ggplot2)
library(tidyverse)
library(gridExtra)

# road Datas
dom.women <- read.csv("Data/theory_edu.viol/viol/law.dom.csv", header = T)
rape.women <- read.csv("Data/theory_edu.viol/viol/law.rape.csv", header = T)
harass.women <- read.csv("Data/theory_edu.viol/viol/law.harass.csv", header = T)

pre.women <- read.csv("Data/theory_edu.viol/viol/pre.viol.csv", header = T)
atu.women <- read.csv("Data/theory_edu.viol/viol/attitude.viol.csv", header = T)


# 전처리
pre.women <- as.data.frame(c(pre.women[1], pre.women[7]))
atu.women <- as.data.frame(c(atu.women[1], atu.women[7]))

#####################남자와 여자의 고등교육을 보여주는 그래프
try.level <- read.csv("Data/theory_edu.viol/edu/TRY.level.csv", header = T)

# Reorder data using average?
try.men <- try.level %>% 
  filter(GENDER == "MEN")
try.women <- try.level %>% 
  filter(GENDER == "WOMEN")
try.level <- merge(try.men, try.women, by = "LOCATION")
try.level <- as.data.frame(c(try.level[1], try.level[3], try.level[7], try.level[9], try.level[13]))

try.level <- try.level %>%
  rowwise() %>%
  arrange(Value.y) %>%
  mutate(LOCATION = factor(LOCATION, LOCATION))
try.level <- rename(try.level,
                    "MEN" = "GENDER.x",
                    "WOMEN" = "GENDER.y")
windows()
# With a bit more style
ggplot(try.level) +
  geom_segment( aes(x = LOCATION, xend = LOCATION, y = Value.x, yend = Value.y), col="grey") +
  geom_point( aes(x = LOCATION, y = Value.x, col = "MEN"), size=3 ) +
  geom_point( aes(x = LOCATION, y = Value.y, col = "WOMEN"), size=3 ) +
  scale_fill_discrete(limits=c("WOMEN", "MEN")) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        legend.title = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 45)) +
  labs(title = "OECD Country's Tertiay Percentage(2014)", x = "Country", y = "Tertiary(%)")


#################국가의 교육 수준
adult.level <- read.csv("Data/theory_edu.viol/edu/adult.level.csv", header = T)

adult.level <- as.data.frame(adult.level[-43,])
adult.level <- adult.level %>% 
  group_by(SUBJECT)

p <- ggplot(adult.level, aes(x = LOCATION, y = Value, fill = SUBJECT)) +
  scale_y_reverse() +
  scale_fill_discrete(limits=c("tertiary", "secondary", "no-secondary")) +
  geom_col()
windows()
p
# 고등교육
adult.try <- adult.level %>% 
  filter(SUBJECT == "tertiary") %>% 
  arrange(Value)

p1 <- ggplot(adult.try, aes(x = reorder(LOCATION, Value), y = Value)) +
  labs(title = "OECD Country's Education Level : Tertiary", x = "Country", y = "Percentage(%)") +
  coord_flip() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20)) +
  geom_col(fill = "pink")

# 중등교육
adult.sec <- adult.level %>% 
  filter(SUBJECT == "secondary") %>% 
  arrange(Value)

p2 <- ggplot(adult.sec, aes(x = reorder(LOCATION, Value), y = Value)) +
  labs(title = "OECD Country's Education Level : secondary", x = "Country", y = "Percentage(%)") +
  coord_flip() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20)) +
  geom_col(fill = "green")

# 중등교육 이하
adult.no <- adult.level %>% 
  filter(SUBJECT == "no-secondary") %>% 
  arrange(Value)

p3 <- ggplot(adult.no, aes(x = reorder(LOCATION, Value), y = Value)) +
  labs(title = "OECD Country's Education Level : under secondary", x = "Country", y = "Percentage(%)") +
  coord_flip() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20)) +
  geom_col(fill = "skyblue")

windows()
grid.arrange(p, p1, p2, p3)




######
adult.try$id <- c(1:34)

# Get the name and the y position of each label
label_data <- adult.try
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (adult.try$id - 0.5) / number_of_bar
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)

#
base_data <- adult.try %>% 
  summarise(start = min(Value), end = max(Value) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title = mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data=grid_data[-1,]

# Make the plot
p <- ggplot(adult.try, aes(x = as.factor(id), y = Value, fill = LOCATION)) +
  geom_col(aes(x=as.factor(id), y=Value, fill=LOCATION), position = "dodge", alpha=0.5) +
  ylim(-15, 55) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")) +
  coord_polar(start = 0) +
  geom_text(data = label_data, 
            aes(x = id, y = Value + 0.5, label = LOCATION, hjust = hjust),
            color = "black", 
            fontface = "bold",
            alpha = 0.6, 
            size = 3, 
            angle = label_data$angle)

p

######


###################여성의 교육수준과 여성들의 폭력에 대한 인식
atu.women
try.women

women.edu.viol <- merge(atu.women, try.women, by = "LOCATION")
women.edu.viol <- as.data.frame(c(women.edu.viol[1], women.edu.viol[2], women.edu.viol[8]))
women.edu.viol <- women.edu.viol %>% 
  rowwise() %>%
  arrange(Value) %>%
  mutate(LOCATION = factor(LOCATION, LOCATION))

windows()
ggplot(women.edu.viol) +
  geom_segment( aes(x = LOCATION, xend = LOCATION, y = atu, yend = Value), col="grey") +
  geom_point( aes(x = LOCATION, y = atu, col = "폭력을 타당하다는 여성"), size=3 ) +
  geom_point( aes(x = LOCATION, y = Value, col = "고등교육 받은 여성"), size=3 ) +
theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        legend.title = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 45)) +
  labs(title = "WOMEN EDU LEVEL / ATTITUDE VIOLENCE", x = "Country", y = "Attitudes towards violence(%)")


####
women.edu.viol$id <- c(1:22)

label_data <- women.edu.viol
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (women.edu.viol$id - 0.5) / number_of_bar
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)

windows()
ggplot(women.edu.viol, aes(x = as.factor(id), y = Value)) +
  geom_col(aes(x=as.factor(id), y=Value, fill="고등교육 받은 여성"), position = "dodge", alpha=0.5) +
  geom_col(aes(x=as.factor(id), y=atu, fill= "폭력을 타당하다는 여성"), position = "dodge", alpha=0.5) +
  ylim(-15, 60) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")) +
  coord_polar(start = 0) +
  geom_text(data = label_data, 
            aes(x = id, y = Value + 0.5, label = LOCATION, hjust = hjust),
            color = "black", 
            fontface = "bold",
            alpha = 0.6, 
            size = 3, 
            angle = label_data$angle)


### 여성 교육수준과 여성폭력 비율
edu.viol <- merge(pre.women, try.women, by = "LOCATION")
edu.viol <- edu.viol %>% 
  rowwise() %>%
  arrange(Value) %>%
  mutate(LOCATION = factor(LOCATION, LOCATION))

windows()
ggplot(edu.viol) +
  geom_segment( aes(x = LOCATION, xend = LOCATION, y = pre, yend = Value), col="grey") +
  geom_point( aes(x = LOCATION, y = pre, col = "폭력을 경험한 여성"), size=3 ) +
  geom_point( aes(x = LOCATION, y = Value, col = "고등교육을 받은 여성"), size=3 ) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        legend.title = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45)) +
  labs(title = "TERTIARY / PREVALENCE OF VIOLENCE (WOMEN)", y = "Prevalence of violence in the lifetime(%)")


####
edu.viol$id <- c(1:33)

label_data <- edu.viol
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (edu.viol$id - 0.5) / number_of_bar
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)

windows()
ggplot(edu.viol, aes(x = as.factor(id), y = Value)) +
  geom_col(aes(x=as.factor(id), y=Value, fill="고등교육을 받은 여성"), position = "dodge", alpha=0.5) +
  geom_col(aes(x=as.factor(id), y=pre, fill= "폭력을 경험한 여성"), position = "dodge", alpha=0.5) +
  ylim(-15, 60) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")) +
  coord_polar(start = 0) +
  geom_text(data = label_data, 
            aes(x = id, y = Value + 0.5, label = LOCATION, hjust = hjust),
            color = "black", 
            fontface = "bold",
            alpha = 0.6, 
            size = 3, 
            angle = label_data$angle)









####
# Value의 평균값을 기준으로 교육수준 상위와 하위를 나눈다(test.sample)
test <- women.edu.viol %>% 
  summarise(sum = sum(Value),
            mean = sum / 22)

test.sample <- merge(atu.women, try.women, by = "LOCATION")
test.sample <- as.data.frame(c(test.sample[1], test.sample[2], test.sample[8]))
test.sample$level <- ifelse(test.sample$Value >= 36.89, "upper", "lower")
test.sample.upper <- test.sample %>%
  filter(level == "upper")
test.sample.lower <- test.sample %>%
  filter(level == "lower")

## 가설1 : 여성 교육 수준과 폭력에 대한 의식이 관련이 있는가
# 영가설 : 여성 교육 수준과 폭력에 대한 여성의 의식은 관련이 없다.
# 대안가설 : 여성 교육 수준과 폭력에 대한 여성의 의식은 관련이 있다.

# 1. 정규성 검정(변수가 30개 이하일 경우)
shapiro.test(test.sample.upper$atu)    # p-value가 0.05보다 크므로 정규분포를 따른다
qqnorm(test.sample.upper$atu)          # 분위수를 통한 정규성 검정(대각선과 산점들이 가까울수록 정규성을 띈다고 판단)
qqline(test.sample.upper$atu, lwd = 2, col = "slateblue1")

shapiro.test(test.sample.lower$atu)  # p-value가 0.05보다 크므로 정규분포를 따른다
qqnorm(test.sample.lower$atu)
qqline(test.sample.lower$atu, lwd = 2, col = "tomato1")

# 2. 등분산성 검정
var.test(test.sample$atu ~ test.sample$level)    # p-value가 0.05보다 크므로 등분산성은 같다고 볼 수 있다

# 3. t.test
t.test(atu ~ level, data = test.sample, var.equal = TRUE, conf.level = 0.95) # p-value가 0.05보다 크므로 영가설 채택

## 회귀분석
par(mfrow = c(1, 2))
hist(test.sample$atu, col = "blue", breaks = 100)
hist(test.sample$Value, col = "blue", breaks = 100)

par(mfrow=c(1,1))

# 2. 상관관계 구하기
cor.test(test.sample$atu, test.sample$Value)   # 상관계수가 -0.37이며 p-value가 0.05보다 큰 걸 보아 두 변수간의 상관관계는 무의미하다 

# 3. 회귀식 구하기
women.test <- lm(test.sample$atu ~ test.sample$Value)
summary(women.test)


plot(atu ~ Value, data = test.sample)
abline(women.test, col = "red")

require(ggplot2)
ggplot(data = test.sample, aes(x = Value, y = atu)) +
  geom_count(size = 3, col = "tomato1") + 
  geom_smooth(method = "lm", col = "slateblue1", fill = "lightgrey") +
  theme_light()







## 가설2 : 교육 수준과 여성의 폭력당한 경험이 관련이 있는가
# 영가설 : 교육 수준과 여성의 폭력당한 경험은 관련이 없다.
# 대안가설 : 교육 수준과 여성의 폭력당한 경험은 관련이 있다.

# 데이터 가공
try.level.tot <- try.level %>% 
  group_by(LOCATION) %>% 
  mutate(tot = sum(Value.x, Value.y))

test.sample <- merge(pre.women, try.level.tot, by = "LOCATION")
test <- test.sample %>% 
  summarise(sum = sum(tot),
            mean = sum / 33)
test.sample$level <- ifelse(test.sample$tot >= 67.04, "upper", "lower")

##+++ 여성의 교육 수준을 upper 와 lower로 나눠 각기의 pre로 판단
test.sample.upper <- test.sample %>%
  filter(level == "upper")
test.sample.lower <- test.sample %>%
  filter(level == "lower")


# 1. 정규성 검정(변수가 30개 이하일 경우)
shapiro.test(test.sample.upper$pre)    # p-value가 0.05보다 크므로 정규분포를 따른다
qqnorm(test.sample.upper$pre)          # 분위수를 통한 정규성 검정(대각선과 산점들이 가까울수록 정규성을 띈다고 판단)
qqline(test.sample.upper$pre, lwd = 2, col = "tomato1")

shapiro.test(test.sample.lower$pre)  # p-value가 0.05보다 크므로 정규분포를 따른다
qqnorm(test.sample.lower$pre)
qqline(test.sample.lower$pre, lwd = 2, col = "slateblue1")

# 2. 등분산성 검정
var.test(test.sample$pre ~ test.sample$level)    # p-value가 0.05보다 크므로 등분산성은 같다고 볼 수 있다

# 3. t.test
t.test(pre ~ level, data = test.sample, var.equal = TRUE, conf.level = 0.95) # p-value가 0.05보다 크므로 영가설 채택

## 회귀분석
par(mfrow = c(1, 2))
hist(test.sample$pre, col = "blue", breaks = 100)
hist(test.sample$tot, col = "blue", breaks = 100)

par(mfrow=c(1,1))

# 2. 상관관계 구하기
cor.test(test.sample$pre, test.sample$tot)   # 상관계수가 -0.25이며 p-value가 0.05보다 큰 걸 보아 두 변수간의 상관관계는 무의미하다 

# 3. 회귀식 구하기 # 무의미하지만...
edu.test <- lm(test.sample$pre ~ test.sample$tot)
summary(edu.test)

plot(pre ~ tot, data = test.sample)
abline(edu.test, col = "tomato1")

require(ggplot2)
ggplot(data = test.sample, aes(x = tot, y = pre)) +
  geom_count(size = 3) + 
  geom_smooth(method = "lm")





#### 가설3 20대 남성 고등교육 인구와 20대 여성 고등교육 인구는 차이가 있을까?
# 영가설 : 20대 남성 고등교육 인구와 20대 여성 고등교육 인구는 차이가 없다.
# 대안가설 : 20대 남성 고등교육 인구와 20대 여성 고등교육 인구는 차이가 있다.

# 데이터 가공
y.men <- read.csv("Data/theory_edu.viol/edu/yM.csv", header = T)
y.women <- read.csv("Data/theory_edu.viol/edu/yW.csv", header = T)

y.men <- y.men %>% 
  filter(TIME == "2017")
y.women <- y.women %>% 
  filter(TIME == "2017")


edu.pop <- merge(y.men, y.women, by = "LOCATION")   # NA제거

edu.men <- data.frame(c(edu.pop[1], edu.pop[2], edu.pop[4]))
edu.men <- rename(edu.men,
                  "GENDER" = "GENDER.x",
                  "Value" = "Value.x")
write.csv(edu.men, file = "eud.men.csv", row.names = F)

edu.women <- data.frame(c(edu.pop[5], edu.pop[7]))
edu.women <- rename(edu.women,
                  "GENDER" = "GENDER.y",
                  "Value" = "Value.y")
write.csv(edu.women, file = "eud.women.csv", row.names = F)

edu.pop <- read.csv("Data/theory_edu.viol/eud.pop.csv", header = T)


# 1. 정규성 검정(변수가 30개 이하일 경우)
shapiro.test(edu.men$Value.x)    # p-value가 0.05보다 크므로 정규분포를 따른다
qqnorm(edu.men$Value.x)          # 분위수를 통한 정규성 검정(대각선과 산점들이 가까울수록 정규성을 띈다고 판단)
qqline(edu.men$Value.x, lwd = 2, col = "slateblue1")

shapiro.test(edu.women$Value.y)  # p-value가 0.05보다 크므로 정규분포를 따른다
qqnorm(edu.women$Value.y)
qqline(edu.women$Value.y, lwd = 2, col = "tomato1")

# 2. 등분산성 검정
var.test(edu.pop$Value ~ edu.pop$GENDER)    # p-value가 0.05보다 크므로 등분산성은 같다고 볼 수 있다

# 3. t.test
t.test(Value ~ GENDER, data = edu.pop, var.equal = TRUE, conf.level = 0.95) # p-value가 0.05보다 크므로 영가설 채택

## 회귀분석
par(mfrow = c(1, 2))
hist(edu.men$Value, col = "blue", breaks = 100)
hist(edu.women$Value, col = "blue", breaks = 100)

par(mfrow=c(1,1))

# 2. 상관관계 구하기
cor.test(edu.men$Value, edu.women$Value)   # 상관계수가 -0.25이며 p-value가 0.05보다 큰 걸 보아 두 변수간의 상관관계는 무의미하다 

# 3. 회귀식 구하기 # 무의미하지만...
edu.test <- lm(edu.pop$Value ~ edu.pop$GENDER)
summary(edu.test)

plot(Value ~ GENDER, data = edu.pop)
abline(edu.test, col = "tomato1")

require(ggplot2)
ggplot(data = edu.pop, aes(x = GENDER, y = Value)) +
  geom_count(size = 3) + 
  geom_smooth(method = "lm")





####
property <- read.csv("Data/theory_edu.viol/property.rate.csv", header = T)
politics <- read.csv("Data/theory_edu.viol/women.politics.csv", header = T)

## 빈곤율과 여성 폭력
prop.viol <- merge(property, pre.women, by = "LOCATION")
plot(x = prop.atu$Value, y = prop.atu$pre)

## 빈곤율과 교육 수준
prop.edu <- merge(property, try.level, by = "LOCATION")
prop.edu$tot <- prop.edu$Value.x + prop.edu$Value.y
plot(x = prop.edu$Value, y = prop.edu$tot)

## 빈곤율과 폭력에 대한 태도
prop.atu <- merge(property, atu.women, by = "LOCATION")
plot(x = prop.atu$Value, y = prop.atu$atu)

## 여성 국회의원과 폭력에 대한 태도
poli.atu <- merge(politics, atu.women, by = "LOCATION")

poli.atu <- poli.atu %>% 
  filter(SUBJECT == "POLREPRES")
plot(x = poli.atu$Value, y = poli.atu$atu)


test <- lm(poli.atu$atu ~ poli.atu$Value)
summary(test)

plot(poli.atu$atu ~ poli.atu$Value)
abline(test, col = "tomato1")

require(ggplot2)
ggplot(data = poli.atu, aes(x = Value, y = atu)) +
  geom_count(size = 2) + 
  geom_smooth(method = "lm")



test <- lm(poli.atu$atu ~ poli.atu$Value + I(poli.atu$Value^2))
summary(test)

plot(poli.atu$atu ~ poli.atu$Value)
abline(test, col = "tomato1")

require(ggplot2)
ggplot(data = poli.atu, aes(x = Value, y = atu)) +
  geom_count(size = 2) + 
  geom_smooth(method = "lm")
