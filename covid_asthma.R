install.packages("ggpubr")
install.packages("funtimes")
install.packages("dtw")
install.packages("dtwclust")

library(vars)
library(dtw)
library(dtwclust)
library(readxl)
library(ggplot2)
library(ggpubr)
library(funtimes)


data <- read_excel("C:/Users/홍성준/Desktop/자료 새로정리 230901.xlsx")
View(data)

# 1 week moving average asthma per 1000000 데이터 저장하기
movast <- data[,c(27,28,29,30)]
movast <- movast[-c(1:7),]
movast <- movast[-c(366,367),]
#View(movast)

start_date18 <- as.Date("2018-01-01")
end_date18 <- as.Date("2018-12-31")
date_sequence18 <- seq(from = start_date18, to = end_date18, by = "1 day")
value18 <- movast[,1]

start_date19 <- as.Date("2019-01-01")
end_date19 <- as.Date("2019-12-31")
date_sequence19 <- seq(from = start_date19, to = end_date19, by = "1 day")
value19 <- movast[,2]


#-------------------------------------------------------------------------------
# 18 asthma per 1000000 & 19 asthma per 1000000
value18 <- as.vector(movast[,1])
value19 <- as.vector(movast[,2])
value18_19 <- c(value18$`1 week moving average asthma per 1000000`, value19$...28)
start_date18_19 <- as.Date("2018-01-01")
end_date18_19 <- as.Date("2019-12-31")
date_sequence18_19 <- seq(from = start_date18_19, to = end_date18_19, by = "1 day")
df_value18_19 <- data.frame(Date = date_sequence18_19,values = value18_19)
head(df_value18_19)

# 20 asthma per 1000000 & 21 asthma per 1000000
value20 <- as.vector(movast[,3])
value21 <- as.vector(movast[,4])
value20_21 <- c(value20$...29, value21$...30)
start_date20_21 <- as.Date("2020-01-01")
end_date20_21 <- as.Date("2021-12-31")
date_sequence20_21 <- seq(from = start_date20_21, to = end_date20_21, by = "1 day")
# 2020년 2월 29일 제거
date_sequence20_21 <- date_sequence20_21[date_sequence20_21 != as.Date("2020-02-29")]
df_value20_21 <- data.frame(Date = date_sequence20_21,values = value20_21)
head(df_value20_21)


# ggplot을 사용하여 18-19 asthma per 1000000 그래프 그리기
ggplot(data = df_value18_19, aes(x = Date, y = values)) +
  geom_line() +
  labs(x = "날짜", y = "값", title = "18-19 asthma per 1000000") 
  

# ggplot을 사용하여 20-21 asthma per 1000000 그래프 그리기
ggplot(data = df_value20_21, aes(x = Date, y = values)) +
  geom_line() +
  labs(x = "날짜", y = "값",title = "20-21 asthma per 1000000")




#-------------------------------------------------------------------------------
# 1 week moving average subway per 1000 데이터 저장하기
movsub <- data[,c(31,32,33,34)]
movsub <- movsub[-c(1:7),]
movsub <- movsub[-c(366,367),]
#View(movsub)

# 18 subway per 1000 & 19 subway per 1000
valuesub_18 <- as.vector(movsub[,1])
valuesub_19 <- as.vector(movsub[,2])
valuesub18_19 <- c(valuesub_18$`1 week moving average subway user per 1000`, valuesub_19$...32)
sub_value18_19 <- data.frame(Date = date_sequence18_19,values = valuesub18_19)
head(sub_value18_19)

# 20 subway per 1000 & 21 subway per 1000
valuesub_20 <- as.vector(movsub[,3])
valuesub_21 <- as.vector(movsub[,4])
valuesub20_21 <- c(valuesub_20$...33, valuesub_21$...34)
sub_value20_21 <- data.frame(Date = date_sequence20_21,values = valuesub20_21)
head(sub_value20_21)

# ggplot을 사용하여 18-19 subway per 1000 그래프 그리기
ggplot(data = sub_value18_19, aes(x = Date, y = values)) +
  geom_line() +
  labs(x = "날짜", y = "값")

# ggplot을 사용하여 20-21 subway per 1000 그래프 그리기
ggplot(data = sub_value20_21, aes(x = Date, y = values)) +
  geom_line() +
  labs(x = "날짜", y = "값")

#-------------------------------------------------------------------------------

# 18 ~ 21 ast per 1000000
start_date18_21 <- as.Date("2018-01-01")
end_date18_21 <- as.Date("2021-12-31")
date_sequence18_21 <- seq(from = start_date18_21, to = end_date18_21, by = "1 day")
date_sequence18_21 <- date_sequence18_21[date_sequence18_21 != as.Date("2020-02-29")]


valueast18_21 <- c(value18$`1 week moving average asthma per 1000000`, value19$...28,value20$...29, value21$...30)
ast_value18_21 <- data.frame(Date = date_sequence18_21,values = valueast18_21)
head(ast_value18_21)


# 18 ~ 21 subway per 1000
valuesub18_21 <- c(valuesub_18$`1 week moving average subway user per 1000`, valuesub_19$...32,valuesub_20$...33, valuesub_21$...34)
sub_value18_21 <- data.frame(Date = date_sequence18_21,values = valuesub18_21)
head(sub_value18_21)


# ggplot을 사용하여 18~21 ast per 1000000 그래프 그리기
ggplot(data = ast_value18_21, aes(x = Date, y = values)) +
  geom_line() +
  labs(x = "날짜", y = "값")


# ggplot을 사용하여 18~21 sub per 1000 그래프 그리기
ggplot(data = sub_value18_21, aes(x = Date, y = values)) +
  geom_line() +
  labs(x = "날짜", y = "값")


# ggplot을 사용하여 두 시계열 그래프 나타내기
combined_data3 <- data.frame(
  Date = date_sequence18_21,
  Value1 = ast_value18_21$values,
  Value2 = sub_value18_21$values
)

gg1 <- ggplot(combined_data3, aes(x = Date)) +
  geom_line(aes(y = Value1, color = "asthma"), size = 0.5) +
  geom_line(aes(y = Value2, color = "subway"), size = 0.5) +
  labs(x = "날짜", y = "값") +
  scale_color_manual(values = c("asthma" = "blue", "subway" = "black")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))

ggsave("fullplot.png",plot = gg1,width = 10, height = 7)
#-------------------------------------------------------------------------------
# 1. pearson corr

pear18_19 <- data.frame(df_value18_19$values,sub_value18_19$values)
pear20_21 <- data.frame(df_value20_21$values,sub_value20_21$values)
pear18_21 <- data.frame(ast_value18_21$values,sub_value18_21$values)

# pearson 
ggscatter(pear18_19, x = "df_value18_19.values", y = "sub_value18_19.values",
          color = "black", cor.coef = TRUE, conf.int = TRUE,
          cor.method = "pearson",add = "reg.line",
          add.params = list(color = "navy", fill = "lightgray"),
          xlab = "Ast", ylab = "Sub")

ggscatter(pear20_21, x = "df_value20_21.values", y = "sub_value20_21.values",
          color = "black", cor.coef = TRUE, conf.int = TRUE,
          cor.method = "pearson",add = "reg.line",
          add.params = list(color = "navy", fill = "lightgray"),
          xlab = "Ast", ylab = "Sub")

ggscatter(pear18_21, x = "ast_value18_21.values", y = "sub_value18_21.values",
          color = "black", cor.coef = TRUE, conf.int = TRUE,
          cor.method = "pearson",add = "reg.line",
          add.params = list(color = "navy", fill = "lightgray"),
          xlab = "Ast", ylab = "Sub")


# kendall 
ggscatter(pear18_19, x = "df_value18_19.values", y = "sub_value18_19.values",
          color = "black", cor.coef = TRUE, conf.int = TRUE,
          cor.method = "kendall",add = "reg.line",
          add.params = list(color = "navy", fill = "lightgray"),
          xlab = "Ast", ylab = "Sub")

ggscatter(pear20_21, x = "df_value20_21.values", y = "sub_value20_21.values",
          color = "black", cor.coef = TRUE, conf.int = TRUE,
          cor.method = "kendall",add = "reg.line",
          add.params = list(color = "navy", fill = "lightgray"),
          xlab = "Ast", ylab = "Sub")

ggscatter(pear18_21, x = "ast_value18_21.values", y = "sub_value18_21.values",
          color = "black", cor.coef = TRUE, conf.int = TRUE,
          cor.method = "kendall",add = "reg.line",
          add.params = list(color = "navy", fill = "lightgray"),
          xlab = "Ast", ylab = "Sub")


#-------------------------------------------------------------------------------

# 2.Time Lagged Cross Correlation(TLCC)

x = pear18_21$ast_value18_21.values
time1 = date_sequence18_21

y = pear18_21$sub_value18_21.values
time2 = date_sequence18_21

ccf(pear18_19$df_value18_19.values, pear18_19$sub_value18_19.values)
print(ccf(x, y))

#-------------------------------------------------------------------------------
# 3.DTW
# DTW 거리 계산(18~21)
x1 = pear18_21$ast_value18_21.values
y1 = pear18_21$sub_value18_21.values
dtw_18_21 <- dtw(x1, y1)
dtw_18_21$normalizedDistance
plot(dtw(x1, y1,keep = T),type="threeway")

# DTW 거리 계산(18~19)
x2 = pear18_19$df_value18_19.values
y2 = pear18_19$sub_value18_19.values
dtw_18_19 <- dtw(x2, y2)
dtw_18_19$distance
dtw_18_19$normalizedDistance
plot(dtw(x2, y2,keep = TRUE),type="threeway")

# DTW 거리 계산(20~21)
x3 = pear20_21$df_value20_21.values
y3 = pear20_21$sub_value20_21.values
dtw_20_21 <- dtw(x3, y3)
dtw_20_21$distance
dtw_20_21$normalizedDistance
plot(dtw(x3, y3,keep = TRUE),type="threeway")

#-------------------------------------------------------------------------------
# data standardization

# Z-score 표준화 함수 정의
standardization <- function(x) {
  z <- (x - mean(x)) / sd(x)
  return(z)
}

# Z-score 표준화 적용
sx4 <- standardization(valueast18_21)
sx5 <- standardization(valuesub18_21)

normaldata1 <- matrix(data = sx4, nrow = 365)
normalast <- as.data.frame(normaldata1)
normaldata2 <- matrix(data = sx5, nrow = 365)
normalsub <- as.data.frame(normaldata2)


normaldata3 <- matrix(data = sx4, nrow = 730)
normalast2 <- as.data.frame(normaldata3)
normaldata4 <- matrix(data = sx5, nrow = 730)
normalsub2 <- as.data.frame(normaldata4)


combined_data4 <- data.frame(
  Date = date_sequence18_21,
  Value1 = sx4,
  Value2 = sx5
)

gg2 <- ggplot(combined_data4, aes(x = Date)) +
  geom_line(aes(y = Value1, color = "asthma"), size = 0.5) +
  geom_line(aes(y = Value2, color = "subway"), size = 0.5) +
  labs(x = "날짜", y = "값") +
  scale_color_manual(values = c("asthma" = "blue", "subway" = "black")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))

ggsave("zfullplot.png",plot = gg2,width = 10, height = 7)
#-------------------------------------------------------------------------------
# DTW

# DTW 거리 계산
result1 <- dtw(normalast$V1,normalsub$V1 ,keep = T)
result2 <- dtw(normalast$V2,normalsub$V2 ,keep = T)
result3 <- dtw(normalast$V3,normalsub$V3 ,keep = T)
result4 <- dtw(normalast$V4,normalsub$V4 ,keep = T)

# DTW 거리 및 경로 출력
print(paste("DTW 거리:", result1$distance))
print(paste("DTW 거리:", result2$distance))
print(paste("DTW 거리:", result3$distance))
print(paste("DTW 거리:", result4$distance))

# DTW 경로 그리기
plot(result1,type = "threeway")
plot(result2,type = "threeway")
plot(result3,type = "threeway")
plot(result4,type = "threeway")


result5 <- dtw(normalast2$V1,normalsub2$V1 ,keep = T)
result6 <- dtw(normalast2$V2,normalsub2$V2 ,keep = T)
result7 <- dtw(sx4,sx5 ,keep = T)
print(paste("DTW 거리:", result5$distance))
print(paste("DTW 거리:", result6$distance))
print(paste("DTW 거리:", result7$distance))
plot(result5,type = "threeway")
plot(result6,type = "threeway")
#-------------------------------------------------------------------------------
# 표준화 후 상관계수
# pearson 
scorr1 <- data.frame(value1 = normalast2$V1, value2 = normalsub2$V1)
scorr2 <- data.frame(value1 = normalast2$V2, value2 = normalsub2$V2)
scorr3 <- data.frame(value1 = sx4, value2 = sx5)

#18-19
gg3 <- ggscatter(scorr1, x = "value1", y = "value2",
          color = "black", cor.coef = TRUE, conf.int = TRUE,
          cor.method = "pearson",add = "reg.line",
          add.params = list(color = "navy", fill = "lightgray"),
          xlab = "Asthma", ylab = "Subway")

ggsave("pearson1819.png",plot = gg3,width = 10, height = 7)
#20-21
gg4 <- ggscatter(scorr2, x = "value1", y = "value2",
          color = "black", cor.coef = TRUE, conf.int = TRUE,
          cor.method = "pearson",add = "reg.line",
          add.params = list(color = "navy", fill = "lightgray"),
          xlab = "Asthma", ylab = "Subway")
ggsave("pearson2021.png",plot = gg4,width = 10, height = 7)
#18-21
gg5 <- ggscatter(scorr3, x = "value1", y = "value2",
          color = "black", cor.coef = TRUE, conf.int = TRUE,
          cor.method = "pearson",add = "reg.line",
          add.params = list(color = "navy", fill = "lightgray"),
          xlab = "Asthma", ylab = "Subway")
ggsave("pearson1821.png",plot = gg5,width = 10, height = 7)


# kendall
#18-19
gg6 <-ggscatter(scorr1, x = "value1", y = "value2",
          color = "black", cor.coef = TRUE, conf.int = TRUE,
          cor.method = "kendall",add = "reg.line",
          add.params = list(color = "navy", fill = "lightgray"),
          xlab = "Asthma", ylab = "Subway")
ggsave("kendall1819.png",plot = gg6,width = 10, height = 7)

#20-21
gg7 <- ggscatter(scorr2, x = "value1", y = "value2",
          color = "black", cor.coef = TRUE, conf.int = TRUE,
          cor.method = "kendall",add = "reg.line",
          add.params = list(color = "navy", fill = "lightgray"),
          xlab = "Asthma", ylab = "Subway")
ggsave("kendall2021.png",plot = gg7,width = 10, height = 7)

#18-21
gg8 <- ggscatter(scorr3, x = "value1", y = "value2",
          color = "black", cor.coef = TRUE, conf.int = TRUE,
          cor.method = "kendall",add = "reg.line",
          add.params = list(color = "navy", fill = "lightgray"),
          xlab = "Asthma", ylab = "Subway")
ggsave("kendall1821.png",plot = gg8,width = 10, height = 7)
#-------------------------------------------------------------------------------
# TLCC

a = combined_data4$Value1
b = combined_data4$Value2
time = date_sequence18_21
head(grd,10)
plot(ccf(a,a5),type = "l")
print(ccf(a, a5))

#18-19 ccf
cr1 <- ccf(scorr1$value1,scorr1$value2)
ccf_df <- data.frame(Lag = cr1$lag, CCF = cr1$acf)
gg9 <- ggplot(ccf_df, aes(x = Lag, y = CCF)) +
  geom_line() +  # 선 그래프로 그리기
  labs(title = "", x = "Lag", y = "CCF")
ggsave("ccf1819.png",plot = gg9,width = 10, height = 7)

#20-21 ccf
cr2 <- ccf(scorr2$value1,scorr2$value2)
ccf_df <- data.frame(Lag = cr2$lag, CCF = cr2$acf)
gg10 <- ggplot(ccf_df, aes(x = Lag, y = CCF)) +
  geom_line() +  # 선 그래프로 그리기
  labs(title = "", x = "Lag", y = "CCF")
ggsave("ccf2021.png",plot = gg10,width = 10, height = 7)

#18-21 ccf
cr3 <- ccf(scorr3$value1,scorr3$value2)
ccf_df <- data.frame(Lag = cr3$lag, CCF = cr3$acf)
gg11 <- ggplot(ccf_df, aes(x = Lag, y = CCF)) +
  geom_line() +  # 선 그래프로 그리기
  labs(title = "", x = "Lag", y = "CCF")
ggsave("ccf1821.png",plot = gg11,width = 10, height = 7)

#-------------------------------------------------------------------------------
#21-2-22 까지의 데이터로 나누기

da1 <- sx4[1:783]
da2 <- sx4[784:1460]
da3 <- sx5[1:783]
da4 <- sx5[784:1460]
daf1 <- data.frame(value1 = diff(da1), value2 = diff(da3))
daf2 <- data.frame(value1 = diff(da2), value2 = diff(da4))


#VAR 모델 적합
var_model <- VAR(daf1, p = 4)  # p는 VAR 모델의 차수

# 그레인저 인과관계 검정
grangertest(daf2$value1 ~ daf2$value2, order=4)
grangertest(daf2$value2 ~ daf2$value1, order=4)



granger_test1 <- causality(var_model, cause = "value1")  # "x"가 "y"에 대한 그레인저 인과관계 검정
granger_test1
granger_test2 <- causality(var_model, cause = "value2")  # "x"가 "y"에 대한 그레인저 인과관계 검정
granger_test2


#-------------------------------------------------------------------------------
# change point
#install.packages("changepoint")
library(changepoint)
#install.packages("bcp")
library(bcp)
#install.packages("strucchange")
library(strucchange)
#install.packages("segmented")
library(segmented)
library(zoo)


astdata <- ts(pear18_21$ast_value18_21, start = c(2018,1,1),frequency = 365)
subdata <- ts(pear18_21$sub_value18_21, start = c(2018,1,1),frequency = 365)
# 1.changepoint "AMOC"
#cpt2 <- cpt.mean(am, method = "PELT", penalty = "CROPS", pen.value = c(1,25))

#asthma
cpt.mean(astdata,method = "AMOC")
cpt.mean(am,method = "AMOC")
#subway
cpt.mean(subdata,method="AMOC")
cpt.mean(sm,method = "AMOC")

# 2. bcp

# 월별 데이터 불러오기
astmonth <- data[,c(37,38,39,40)]
astmonth <- astmonth[c(8:19),]
submonth <- data[,c(41,42,43,44)]
submonth <- submonth[c(8:19),]

am18 <- as.vector(astmonth[,1])
am19 <- as.vector(astmonth[,2])
am20 <- as.vector(astmonth[,3])
am21 <- as.vector(astmonth[,4])
am1 <- c(am18$...37, am19$...38)
am2 <- c(am20$...39, am21$...40)
am <-c(am18$...37, am19$...38,am20$...39, am21$...40)

sm18 <- as.vector(submonth[,1])
sm19 <- as.vector(submonth[,2])
sm20 <- as.vector(submonth[,3])
sm21 <- as.vector(submonth[,4])
sm1 <- c(sm18$...37, sm19$...38)
sm2 <- c(sm20$...39, sm21$...40)
sm <-c(sm18$...41, sm19$...42,sm20$...43, sm21$...44)

mdata <- data.frame(ast = am, sub = sm)

# asthoma
x<-ts(am,start= c(2018,1),frequency = 12)
bcp_x<-bcp(x,return.mcmc=TRUE,set.seed(123456))
plot(bcp_x)
bcp_sum1 <- as.data.frame(summary(bcp_x))
# Let's filter the data frame and identify the year:
bcp_sum1$id <- 1:length(x)
(sel <- bcp_sum1[which(bcp_x$posterior.prob > 0.6), ])
# Get the year:
time(x)[sel$id]

# subway
y<-ts(sm,start= c(2018,1),frequency = 12)
bcp_y<-bcp(y,return.mcmc=TRUE,set.seed(123456))
plot(bcp_y)
bcp_sum2 <- as.data.frame(summary(bcp_y))
# Let's filter the data frame and identify the year:
bcp_sum2$id <- 1:length(y)
(sel <- bcp_sum2[which(bcp_y$posterior.prob > 0.8), ])
# Get the year:
time(y)[sel$id]



# 3.Strucchange

# ast
ocus_ast<-efp(astdata~1,type="OLS-CUSUM")
opm<-par(mfrow=c(1,2))
plot(ocus_ast)

# changepoint summary
sctest(ocus_ast)

# F statistics
fs_ast<-Fstats(astdata~1)
summary(fs_ast)
plot(fs_ast)
# break point
bp_ast<-breakpoints(astdata~1)
plot(bp_ast)
summary(bp_ast)

# get best model
opt_bpts <- function(x) {
  #x = bpts_sum$RSS["BIC",]
  n <- length(x)
  lowest <- vector("logical", length = n-1)
  lowest[1] <- FALSE
  for (i in 2:n) {
    lowest[i] <- x[i] < x[i-1] & x[i] < x[i+1]
  }
  out <- as.integer(names(x)[lowest])
  return(out)
}

bpts_sum <- summary(bp_ast)
opt_brks <- opt_bpts(bpts_sum$RSS["BIC",])
opt_brks

ci_ast <- confint(bp_ast, breaks = 1)
par(mfrow=c(1,1))
plot(astdata)
lines(ci_ast)
ci_ast

#add the regression line
fm0<-lm(astdata~1)
coef(fm0)

ast_fac<-breakfactor(bp_ast,breaks=3)
ast_fac
fm1<-lm(astdata~ast_fac-1)
coef(fm1)

plot(astdata)
lines(ci_ast)
lines(ts(fitted(fm0),start=2018),col=3)
lines(ts(fitted(fm1),start=2018),col=4)
lines(bp_ast)






#sub
ocus_sub<-efp(subdata~1,type="OLS-CUSUM")
opm2<-par(mfrow=c(1,2))
plot(ocus_sub)

# changepoint summary
sctest(ocus_sub)

# F statistics
fs_sub<-Fstats(subdata~1)
plot(fs_sub)

# break point
bp_sub<-breakpoints(subdata~1)

plot(bp_sub)


# get best model
opt_bpts2 <- function(x) {
  #x = bpts_sum$RSS["BIC",]
  n <- length(x)
  lowest <- vector("logical", length = n-1)
  lowest[1] <- FALSE
  for (i in 2:n) {
    lowest[i] <- x[i] < x[i-1] & x[i] < x[i+1]
  }
  out <- as.integer(names(x)[lowest])
  return(out)
}
bpts_sum2 <- summary(bp_sub)
opt_brks2 <- opt_bpts2(bpts_sum2$RSS["BIC",])
opt_brks2

ci_sub <- confint(bp_sub, breaks = 1)
par(mfrow=c(1,1))
plot(subdata)
lines(ci_sub)
ci_sub

#add the regression line
fm2<-lm(subdata~1)
coef(fm2)

sub_fac<-breakfactor(bp_sub,breaks=1)
sub_fac
fm3<-lm(subdata~sub_fac-1)
coef(fm3)

plot(subdata)
lines(ci_sub)
lines(ts(fitted(fm2),start=2018),col=3)
lines(ts(fitted(fm3),start=2018),col=4)
lines(bp_sub)

