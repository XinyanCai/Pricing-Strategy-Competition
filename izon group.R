data <- read.csv("data1.csv")
da <- data[which(data$GL.Period.Year != '[N/A]'),]

da <- subset(data, GL.Period.Year == '2015')
percen <- weighted.mean(da$X.Invoice.Revenue..Invoice.Quantity,da$revenue)
total <- sum(da$revenue)
re <- percen*(sum(da$X.Invoice.Quantity))
percen1 <- mean(da$X.Invoice.Revenue..Invoice.Quantity)
re1 <- percen1*(sum(da$X.Invoice.Quantity))
resid <- re - total
resid1 <- re1- total

library(tidyverse)
data1 <- aggregate(da$X.Invoice.Revenue..Invoice.Quantity,by = list(da$Market.Name), FUN = mean)
data2 <- aggregate(da$X.Invoice.Quantity,by = list(da$Market.Name), FUN = sum)
prop <- data2$x/sum(da$X.Invoice.Quantity)
data3 <- aggregate(da$revenue,by = list(da$Market.Name), FUN = sum)
data4 <- aggregate(da$X.Invoice.Quantity,by = list(da$Market.Name), FUN = sum)
p<- data3$x/data4$x
data1$wighted.mean <- sum(prop*p)

dadada <- read.csv("groupdata.csv") 
dada <- subset(dadada, GLPeriodYear == '2019' & MarketName == 'LOS ANGELES')
dada <- subset(dadada,GLPeriodYear == '2019' & MarketName != 'LA - Acquired')
dada1 <- subset(dadada, GLPeriodYear == '2019' & MarketName == 'LA - Acquired')
storagequantity <- aggregate(dada$InvoiceQuantity,by = list(dada$CustomerImga,dada$BillCode), FUN = sum)
price <- aggregate(dada$RateAmount,by = list(dada$CustomerImga,dada$BillCode), FUN = mean )
cc <- subset(dada, select = c(CustomerImga,CustomerAccountName,BillCode,BillCodeType,MarketName,InvoiceQuantity,InvoiceRevenue,RateAmount))
cc1 <- subset(dada1, select = c(CustomerImga,CustomerAccountName,BillCode,BillCodeType,MarketName,InvoiceQuantity,InvoiceRevenue,RateAmount))
storagequantity$price <- price$x

library(plyr)
final <- ddply(storagequantity,.(Group.1),summarize,wm=weighted.mean(price, x))
colnames(final) <- c('CustomerIMGA','Weighted Average')
final1 <- aggregate(dada$RateAmount,by = list(dada$CustomerImga), FUN = mean )
colnames(final1) <- c('CustomerIMGA','Simple Average')
final[which(final$CustomerIMGA == 'IM_1017'),]
final1[which(final$CustomerIMGA == 'IM_1017'),]
q <- aggregate(storagequantity$x,by = list(storagequantity$Group.1), FUN = mean )
revenue1 <- q$x*final$wm   #weighted mean
revenue2 <- q$x*final1$x  #mean
revenue3 <- aggregate(dada$InvoiceRevenue,by = list(dada$CustomerImga), FUN = sum )
revenue3 <- revenue3$x

resi1 <- revenue1-revenue3
resi2 <- revenue2-revenue3
count(resi1 > 0)
count(resi2 > 0)
