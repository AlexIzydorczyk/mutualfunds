mf <- read.csv('mf_25.csv')
library("xtable")
library("stargazer")

lagpad <- function(x, k) {
  c(rep(NA, k), x)[1 : length(x)] 
}

#get rid of data for funds that does not go back to to 2010/01
mf$mret <- as.numeric(as.character(mf$mret))
mf <- mf[-which(mf[,2] %in% mf[mf$mtna < 0,2]),]
length(unique(mf[,2]))
mf2 <- mf[-which(mf[,2] %in% mf[is.na(mf$mret),2]),]
length(unique(mf2[,2]))
length(mf2[substr(mf2$caldt,start=1,stop=6) == "201001",2])

###
#mf2 <- mf
#create lagged variables
for (i in c(1:12)){
  colname <- paste("tna_L",i,sep="")
  mf2[,colname] =  lagpad(mf2$mtna, i) 
}

for (i in c(1:12)){
  colname <- paste("R(t-",i,sep="")
  mf2[,colname] =  lagpad(mf2$mret, i) 
}

#create flow variable

dates2010 <- (unique(mf2$caldt[substr(mf2$caldt,start=1,stop=4) == "2003"]))
mf3 <- mf2[-which(mf2$caldt %in% dates2010),]

FLOW = mf3$mtna - mf3$tna_L1*(1+mf3$mret)
flow = FLOW/mf3$tna_L1
mf3$flow = flow

for (i in c(1:12)){
  colname <- paste("flow_t-",i,sep="")
  mf3[,colname] =  lagpad(mf3$flow, i) 
}


dates2011 <- (unique(mf3$caldt[substr(mf3$caldt,start=1,stop=4) == "2004"]))
mf4 <- mf3[-which(mf3$caldt %in% dates2011),]
length(unique(mf4[,2]))


#def_funds <- (mf4[mf4$flow > 1 | mf4$flow < -1, 2])
#mf5 <- mf4[-which(mf4[,2] %in% def_funds),]
#length(unique(mf5[,2]))

model <- mf4[,c(6,18:42)]
lm <- lm(flow~., data=model)
summary(lm(flow~.,data=model))

#regressions

#lagged net asset data
tna_L1 <- model[, c(1)]
#lagged return data
ret_data <- model[,c(2,3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)]
#lagged flow data
flow_data <- model[,c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)]
#combined
comb_data <- model[,c(2,3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)]

#flow regressions

#isolate data from data set
flow <- model[, c(14)]
ret_l1 <- model[, c(2)]
ret_l2 <- model[, c(3)]
ret_l3 <- model[, c(4)]
ret_l4 <- model[, c(5)]
ret_l5 <- model[, c(6)]
ret_l6 <- model[, c(7)]
ret_l7 <- model[, c(8)]
ret_l8 <- model[, c(9)]
ret_l9 <- model[, c(10)]
ret_l10 <- model[, c(11)]
ret_l11 <- model[, c(12)]
ret_l12 <- model[, c(13)]

flow_l1 <- model[, c(15)]
flow_l2 <- model[, c(16)]
flow_l3 <- model[, c(17)]
flow_l4 <- model[, c(18)]
flow_l5 <- model[, c(19)]
flow_l6 <- model[, c(20)]
flow_l7 <- model[, c(21)]
flow_l8 <- model[, c(22)]
flow_l9 <- model[, c(23)]
flow_l10 <- model[, c(24)]
flow_l11 <- model[, c(25)]
flow_l12 <- model[, c(26)]

lm_ret <- lm(flow~.,data=ret_data)
summary(lm_ret)
flow_by_return <- xtable(summary(lm_ret))
print(flow_by_return)

#flow by flow
lm_flow <- lm(flow~.,data=flow_data)
summary(lm_flow)
flow_by_flow <- xtable(summary(lm_flow))
print(flow_by_flow)

#flow by returns and flow
lm_comb <- lm(flow~.,data=comb_data)
summary(lm_comb)
flow_by_comb <- xtable(summary(lm_comb))
print(flow_by_comb)

lm_nav <- lm(flow~tna_L1)
summary(lm_nav)

#best fit would be AR fit on flow

#returns regression
ret <- mf4$mret

#returns on lagged returns
lm_r <- lm(ret~., data=ret_data)
summary(lm_r)
returns_by_returns <- xtable(summary(lm_r))
print(returns_by_returns)

#returns on flows
lm_f <- lm(ret~., data=flow_data)
summary(lm_f)
returns_by_flows <- xtable(summary(lm_f))
print(returns_by_flows)

#combined regression
lm_fr <- lm(ret~., comb_data)
summary(lm_fr)

#aic analysis - flow

#need to build out AIC separately for each
flow1 <- lm(flow ~ flow_l1)
flow2 <- lm(flow ~ flow_l1 + flow_l2)
flow3 <- lm(flow ~ flow_l1 + flow_l2 + flow_l3)
flow4 <- lm(flow ~ flow_l1 + flow_l2 + flow_l3 + flow_l4)
flow5 <- lm(flow ~ flow_l1 + flow_l2 + flow_l3 + flow_l4 + flow_l5)
flow6 <- lm(flow ~ flow_l1 + flow_l2 + flow_l3 + flow_l4 + flow_l5 + flow_l6)
flow7 <- lm(flow ~ flow_l1 + flow_l2 + flow_l3 + flow_l4 + flow_l5 + flow_l6 + flow_l7)
flow8 <- lm(flow ~ flow_l1 + flow_l2 + flow_l3 + flow_l4 + flow_l5 + flow_l6 + flow_l7 + flow_l8)
flow9 <- lm(flow ~ flow_l1 + flow_l2 + flow_l3 + flow_l4 + flow_l5 + flow_l6 + flow_l7 + flow_l8 + flow_l9)
flow10 <-lm(flow ~ flow_l1 + flow_l2 + flow_l3 + flow_l4 + flow_l5 + flow_l6 + flow_l7 + flow_l8 + flow_l9 + flow_l10)
flow11 <-lm(flow ~ flow_l1 + flow_l2 + flow_l3 + flow_l4 + flow_l5 + flow_l6 + flow_l7 + flow_l8 + flow_l9 + flow_l10 + flow_l11)
flow12 <-lm(flow ~ flow_l1 + flow_l2 + flow_l3 + flow_l4 + flow_l5 + flow_l6 + flow_l7 + flow_l8 + flow_l9 + flow_l10 + flow_l11 + flow_l12)

aic_flow <- c(AIC(flow1), AIC(flow2), AIC(flow3), AIC(flow4), AIC(flow5), AIC(flow6), AIC(flow7), AIC(flow8), AIC(flow9), AIC(flow10), AIC(flow11), AIC(flow12))

table_aic <- aic_flow[1:12]
mat <- matrix(aic_flow)
xtab <- xtable(mat)
print(xtab)

#correlations

#flow
flow_cor <- c()
for (i in 1:12) {
  var <- paste("flow_l",i,sep="")
  flow_cor <- rbind(flow_cor, cor(flow, get(var)))
}
plot(flow_cor, main="Flow ACF", xlab="Lag", ylab = "Flow Correlation")

#returns

return_cor <- c()
for (i in 1:12) {
  var <- paste("ret_l",i,sep="")
  return_cor <- rbind(return_cor, cor(ret, get(var)))
}
plot(return_cor, main="Return ACF", xlab="Lag")

flow_by_return_cor <- c()
for (i in 1:12) {
  var <- paste("ret_l",i,sep="")
  flow_by_return_cor <- rbind(flow_by_return_cor, cor(flow, get(var)))
}
plot(flow_by_return_cor, main="Flow by Return ACF", xlab="Lag Return", ylab="Flow by Return Autocorrelation")






