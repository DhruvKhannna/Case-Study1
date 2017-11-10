setwd(choose.dir())
case4 <- read.csv("F:\\DATA SCIENCE USING SAS AND R +EXCEL\\CASE STUDIES FOR SAS\\Linear Regression Case\\Linear Regression Case_csv.csv")

case4$total_spent <- case4$cardspent+case4$card2spent
case4$total_items <- case4$card2items+case4$carditems
var <- c("region","townsize","age","ed","edcat", "jobcat", "employ","empcat","retire", "income","creddebt"
         ,"othdebt","marital","spoused","reside","homeown","hometype","address","cars","carown","commute"
         ,"commutecat", "reason","card","cardtype","card2","card2type","tenure","churn","longmon",
        "equip","equipmon","equipten","cardmon","wireless","wiremon","wireten","total_spent","total_items")
case4a <- subset(case4,select = var)

str(case4a)
case4a$townsize <-  as.numeric(case4a$townsize)
corrmtr <- cor(case4a)
write.csv(corrmtr,'correlation.csv')
View(corrmtr)
require(psych)
require(GPArotation)
scree(corrmtr,factors = T, pc=T,main="screen plot",hline=NULL,add =F )
eigen(corrmtr)$value
require(dplyr)
x <- data.frame(eigen(corrmtr)$values)
eigen_values <- mutate(x,cum_sum_eigen=cumsum(eigen.corrmtr..values)
                       , pct_var=eigen.corrmtr..values/sum(eigen.corrmtr..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrmtr..values))

write.csv(eigen_values,"eigenvalues.csv")
FA <- fa(r=corrmtr,nfactors=15,rotate ="varimax")
sort <- fa.sort(FA)
sort$loadings
loadings_file <- data.frame(sort$loadings[1:ncol(case4a),])
write.csv(loadings_file,"FA_final.csv")
save.image("envir.RData")


