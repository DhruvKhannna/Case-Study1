setwd(choose.dir())
case4 <- read.csv("F:\\DATA SCIENCE USING SAS AND R +EXCEL\\CASE STUDIES FOR SAS\\Linear Regression Case\\Linear Regression Case_csv.csv")
case4a <- case4[-c(1,7,16,20,22,87,89,92,94,97,99,102,104,107,109)]
case4a$total_spent <- case4$cardspent+case4$card2spent
case4a$total_items <- case4$card2items+case4$carditems

mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}
  var <- c( "townsize","tenure","card","card2" ,"address","jobcat" ,"employ" ,"retire" , "equipmon"  ,"income" ,"othdebt" , "reside", "carown"  , "total_items","reason" ,"creddebt","total_spent","equipmon","wiremon")
  str(case4a)
  
  
   num_var <- sapply(case4a, is.numeric)
 diag_stats <-  t(data.frame(apply(case4a[num_var],2,mystats)))
write.csv(diag_stats,"diag_stats.csv")



#outlier
case4a$employ[case4a$employ > 31 ] <-  31
case4a$income[case4a$income > 147 ] <-   147
case4a$debtinc[case4a$debtinc > 22.2 ] <-   22.2
case4a$creddebt[case4a$creddebt > 6.37150000000001 ] <-   6.37150000000001
case4a$othdebt[case4a$othdebt > 11.812 ] <-  11.812
case4a$spoused[case4a$spoused > 18 ] <-   18
case4a$carvalue[case4a$carvalue > 72 ] <-   72
case4a$longmon[case4a$longmon > 36.7575 ]  <- 36.7575
case4a$tollmon[case4a$tollmon > 43.5 ]  <- 43.5
case4a$tollten[case4a$tollten > 2620.2125 ] <-  2620.2125
case4a$equipmon[case4a$equipmon > 49.0525 ] <-  49.0525
case4a$equipten[case4a$equipten > 2600.99 ] <-  2600.99
case4a$cardmon[case4a$cardmon > 42 ] <-  42
case4a$wiremon[case4a$wiremon > 51.305 ]  <- 51.305
case4a$wireten[case4a$wireten > 2687.9225 ] <-  2687.9225
case4a$total_spent[case4a$total_spent > 1145.1465 ] <-  1145.1465
case4a$total_items[case4a$total_items > 22 ] <-  22


#dummies
case4a$dummy_jobcat1[case4a$jobcat==1] <- 1
case4a$dummy_jobcat1[case4a$jobcat!=1] <- 0
case4a$dummy_jobcat2[case4a$jobcat==2] <- 1
case4a$dummy_jobcat2[case4a$jobcat!=2] <- 0
case4a$dummy_jobcat3[case4a$jobcat==3] <- 1
case4a$dummy_jobcat3[case4a$jobcat!=3] <- 0
case4a$dummy_jobcat4[case4a$jobcat==4] <- 1
case4a$dummy_jobcat4[case4a$jobcat!=4] <- 0
case4a$dummy_jobcat5[case4a$jobcat==5] <- 1
case4a$dummy_jobcat5[case4a$jobcat!=5] <- 0



case4a$dummy_card1[case4a$card==1] <- 1
case4a$dummy_card1[case4a$card !=1] <- 0
case4a$dummy_card2[case4a$card==2] <- 1
case4a$dummy_card2[case4a$card!=2] <- 0
case4a$dummy_card3[case4a$card==3] <- 1
case4a$dummy_card3[case4a$card!=3] <- 0
case4a$dummy_card4[case4a$card==4] <- 1
case4a$dummy_card4[case4a$card!=4] <- 0

case4a$dummy_card2_1[case4a$card2==1] <- 1
case4a$dummy_card2_1[case4a$card2 !=1] <- 0
case4a$dummy_card2_2[case4a$card2==2] <- 1
case4a$dummy_card2_2[case4a$card2!=2] <- 0
case4a$dummy_card2_3[case4a$card2==3] <- 1
case4a$dummy_card2_3[case4a$card2!=3] <- 0
case4a$dummy_card2_4[case4a$card2==4] <- 1
case4a$dummy_card2_4[case4a$card2!=4] <- 0

case4a$dummy_reason1[case4a$reason==1] <- 1
case4a$dummy_reason1[case4a$reason !=1] <- 0
case4a$dummy_reason2[case4a$reason==2] <- 1
case4a$dummy_reason2[case4a$reason !=2] <- 0
case4a$dummy_reason3[case4a$reason==3] <- 1
case4a$dummy_reason3[case4a$reason !=3] <- 0
case4a$dummy_reason4[case4a$reason==4] <- 1
case4a$dummy_reason4[case4a$reason !=4] <- 0
case4a$dummy_reason5[case4a$reason==9] <- 1
case4a$dummy_reason5[case4a$reason !=9] <- 0

hist(case4a$total_spent)
hist(log(case4a$total_spent))
case4a$ln_total_spent <- log(case4a$total_spent)

#split the data in training and testing
set.seed(123)
training_select <- sample(1:nrow(case4a),size = floor(0.7*nrow(case4a)))
training <- case4a[training_select,]
testing <- case4a[-training_select,]

#multiple regression

fit <- lm(ln_total_spent~tenure+address+employ+retire+total_items
           +equipmon+income+othdebt+reside+carown+creddebt+wiremon
          +dummy_card1+dummy_card2+dummy_card3+dummy_card4+dummy_card2_1+dummy_card2_2+dummy_card2_3
          +dummy_card2_4+dummy_reason1+dummy_reason2+dummy_reason3+dummy_reason4+dummy_reason5+dummy_jobcat5
          +dummy_jobcat1+dummy_jobcat2+dummy_jobcat3+dummy_jobcat4,data=training)
summary(fit)





require(MASS)
step3<- stepAIC(fit,direction="both")
ls(step3)
step3$anova
fit_final <- lm(ln_total_spent ~ address + employ + retire + total_items + equipmon  +
                  income + carown + dummy_card1 + dummy_card4 + dummy_card2_1 + 
                  dummy_card2_4 + dummy_reason1 + dummy_reason2 + dummy_reason3 + 
                  dummy_reason4 + dummy_jobcat1 + dummy_jobcat2 + dummy_jobcat3,data=training)

summary(fit_final)
require(car)
vif(fit_final)
plot(fit_final$residuals)

plot(fit_final)



#scoring
pred_train<-cbind(training, pred_total_spend = exp(predict(fit_final,training)))
pred_train <- transform(pred_train,APE=abs(pred_total_spend-total_spent)/total_spent)


pred_test <- cbind(testing,pred_total_spend=exp(predict(fit_final,testing)))
pred_test <- transform(pred_test,APE=abs(pred_total_spend-total_spent)/total_spent)

# find the decile locations 
decLocations <- quantile(pred_train$pred_total_spend, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
pred_train$decile <- findInterval(pred_train$pred_total_spend,c(-Inf,decLocations, Inf))

require(sqldf)
pred_train_DA <- sqldf("select decile, count(decile) as count, avg(pred_total_spend) as avg_tot_pred_spd,avg(total_spent) as avg_tot_Actual_spt
               from pred_train
               group by decile
               order by decile desc")
write.csv(pred_train_DA,"mydata1_DA.csv")

decLocations2 <- quantile(pred_test$pred_total_spend,probs = seq(0.1,0.9,by=0.1))
pred_test$decile <- findInterval(pred_test$pred_total_spend,c(-Inf,decLocations2,Inf))


pred_test_DA <- sqldf("select decile,count(decile) as count, avg(pred_total_spend) as avg_tot_pre_spd,avg(total_spent) as avg_tot_Actual_spt
                     from pred_test
                      group by decile
                      order by decile desc")


write.csv(pred_test_DA,"mydata2_DA.CSV")


save.image("CASE4_enviroment.RData")

