library('dplyr')

dat <-read.csv("US_Census.csv")
str(dat)
dat <- dat[-c(2)]

# removing any datapoints with people younger than 18
dat.no18 <- subset(dat, AGE>=18)

# since we're trying to predict income of those who do make money
# we'll be removing those who reported $0 income
dattemp<- subset(dat.no18, INCOME!=0)
plot(dattemp$INCOME)

# getting range of income
table(dattemp$INCOME)
max(dattemp$INCOME)
median(dattemp$INCOME)
mean(dat$INCOME)


# we will be using log since it makes the data normally distributed
hist(log(dattemp$INCOME,base=exp(10)))
hist(log(dattemp$INCOME,base=exp(25)))

# checking to see significant variables
allin <- lm(INCOME~.,dattemp )
summary(allin)
names(dattemp)

# removing because of linear dependencies
# since parent is based on 18 or below, we'll be removing this variable 
# given that we removed all people under 18
drop <- c("PARENT", "CLSWKR", "Retirement_Source", "Poverty_Level")
dat1 = dattemp[,!(names(dattemp) %in% drop)]
#dat1 <- dattemp[,-c(9,11,18,22:24)]
str(dat1)
names(dat1)

# income of those who didn't finish HS are still valuable, so we'll keep 
min(dat1$HGA)
hist(dat1$HGA)
below39 <- subset(dat1, HGA<39)
hist(below39$INCOME)
table(below39$INCOME)
summary(below39$INCOME)
summary(below39$AGE)

newallin <-lm(INCOME~., dat1)
summary(newallin)
names(dat1)
dat1$disability <- if_else(dat1$PEDISOUT == 1 | dat1$PEDISREM == 1, 1, 0)
# making indicator variables
disability <- as.factor(dat1$disability)
marital <- as.factor(dat1$MARITAL)
sex <- as.factor(dat1$SEX) #1 male, 2 female
#sex <- as.factor(dat1$SEX -1) #0 male, 1 female
edu_attmt<- as.factor(dat1$HGA - 30)
prim_rel <- as.factor(dat1$PF)
insurance <- as.factor(dat1$NOW_DIR) #0 not in universe,1 yes, 2 no
asian_type <- as.factor(dat1$ASIAN)
maj_occ <- as.factor(dat1$MJ_OCC_SEC)
prof_cert <- as.factor(dat1$Professional_Certification)
first_job <- as.factor(dat1$First_Job)
maj_ind <- as.factor(dat1$Major_Industry)
clswkr <- as.factor(dat1$CLSWKR)
otr_inc <- as.factor(dat1$Other_Income_Sources)
region <- as.factor(dat1$Region)
unmem <- as.factor(dat1$Union_Member)
own_land <-as.factor(dat1$RNT_YN)

#replacing not in universe values of -1 with 0
dat1$Professional_Certification[dat1$Professional_Certification == -1] <- 0   
dat1$ASIAN[dat1$ASIAN == -1] <- 0
dat1$Union_Member[dat1$Union_Member == -1] <- 0
colSums(dat1 < 1)

head(dat1)
names(dat1)
tmp_sex <-data.frame(model.matrix(~sex -1))
#tmp_pedisout <-data.frame(model.matrix(~pedisout -1))
#tmp_pedisrem <-data.frame(model.matrix(~pedisrem -1))
tmp_marital <-data.frame(model.matrix(~marital -1))
tmp_edu_attmt <-data.frame(model.matrix(~edu_attmt -1))
tmp_prim_rel <-data.frame(model.matrix(~prim_rel -1))
tmp_insurance <-data.frame(model.matrix(~insurance -1))
tmp_asian_type <-data.frame(model.matrix(~asian_type -1))
tmp_maj_occ <-data.frame(model.matrix(~maj_occ -1))
tmp_prof_cert <-data.frame(model.matrix(~prof_cert -1))
tmp_first_job <-data.frame(model.matrix(~first_job -1))
tmp_maj_ind <-data.frame(model.matrix(~maj_ind -1))
#tmp_clswkr <-data.frame(model.matrix(~clswkr -1))
tmp_otr_inc <-data.frame(model.matrix(~otr_inc -1))
tmp_region <-data.frame(model.matrix(~region -1))
tmp_unmem <- data.frame(model.matrix(~unmem-1))
tmp_own_land <- data.frame(model.matrix(~own_land-1))

names(dat1)
dat2 <- cbind(dat1[, c(1,5)], dat1$disability,
              tmp_marital[,2:7], tmp_sex[,2], tmp_edu_attmt[,2:15], 
              tmp_prim_rel[,2:6], tmp_insurance[,2], tmp_asian_type[,2:7], 
              tmp_maj_occ[,2:11],tmp_prof_cert[,2:3], tmp_first_job[,2:9], 
              tmp_maj_ind[,2:14],tmp_unmem[,2:3], tmp_otr_inc[,2:12], tmp_region[,2:10])
head(dat2)
newallin <- lm(INCOME~.,dat2)
summary(newallin)

#looking at correlation of indicator variables
corrdf <- as.data.frame(as.table(cor(dat2)))
corrdf
str(cor(dat2))
corrdf %>% arrange(desc(Freq)) %>% filter(Freq< -0.6)
corrdf %>% arrange(desc(Freq)) %>% filter(Freq> 0.6) %>% filter(Freq != 1)
# Not in Universe and No are telling the same information, so we can combine NIU and No
# Same with prof_cert
dat2$disability <- paste(dat2$pedisout1, dat2$pedisout2, dat2$pedisrem1, dat2$pedisrem2)
head(dat2$disability)
#Major occupation is insignificant, so we will drop all of those dummies.
drop2 <- c("maj_occ1", "maj_occ2", "maj_occ3", "maj_occ4",
          "maj_occ5", "maj_occ6", "maj_occ7", "maj_occ8",
          "maj_occ9", "maj_occ10")
dat3 = dat2[,!(names(dat2) %in% drop2)]
newallin2 <- lm(log(INCOME)~.,dat3)
summary(newallin2)

library(leaps)
set.seed(36963508)
train <- sample(nrow(dat3),(nrow(dat3)/2)-1)
dat3.train <- dat3[train,]
dat3.test <- dat3[-train,]

regfit.full <- regsubsets(INCOME~., data = dat3.train, nvmax = 81, really.big = T,
                     method = "seqrep")
summary(regfit.full)
summary(regfit.full)$rsq
plot(summary(regfit.full)$rsq)

dat.test.mat <- model.matrix(INCOME~.,data = dat3.test)
coef35 <- coef(regfit.full,35)

coef20 <- coef(regfit.full,20)
yhat20 <- dat.test.mat[,names(coef20)] %*% coef20
MSE.bs20 <- mean((dat3.test$INCOME - yhat20)^2)
RMSE.bs20 <- MSE.bs20^0.5
RMSE.bs20

reg.summary <- summary(regfit.full)
names(reg.summary)
reg.summary$rsq
plot(reg.summary$rsq)

which.max(regfit.full$adjr2)
maxar2 <- which.max(reg.summary$adjr2)
points(maxar2,reg.summary$adjr2[maxar2], col = "red", cex = 2, pch = 20)
plot(reg.summary$cp, xlab = "Number of Variables", ylab ="Mallows Cp")
which.min(reg.summary$cp)
mincp <- which.min(reg.summary$cp)
points(mincp,reg.summary$cp[mincp], col = "blue", cex = 2, pch = 20)
plot(reg.summary$bic, xlab = "Number of Variables", 
     ylab ="Bayesian Info Crit")
which.min(reg.summary$bic)
minbic <- which.min(reg.summary$bic)
points(minbic,reg.summary$bic[minbic], col = "green", cex = 2, pch = 20)
par(mfrow=c(1,1))

reg.summary$bic
str(reg.summary)



'''
selected_var_reg <- lm(log(INCOME)~ AGE + marital7 + tmp_sex[, 2]` +
                         edu_attmt2 + edu_attmt3 + edu_attmt4 + edu_attmt5
                       + edu_attmt6 + eduattmt7 + edu_attmt8 + edu_attmt9
                       + edu_attmt10 + edu_attmt11 + edu_attmt12 +edu_attmt13
                       + edu_attmt14 + edu_attmt15 + prim_rel1
                       + prim_rel1 + prim_rel2 + first_job6 + maj_ind8
                       + maj_ind9, data=dat3)
summary(selected_var_reg)
'''

test_mod1 <- lm(log(INCOME) ~                
  AGE + marital7 + tmp_sex[,2] + edu_attmt2 + edu_attmt3 + edu_attmt4 + edu_attmt5 + 
  edu_attmt6  + edu_attmt7 +  edu_attmt8 +  edu_attmt9 + edu_attmt10 +  edu_attmt11 
+ edu_attmt12 + edu_attmt13 + edu_attmt14 + edu_attmt15 + prim_rel1 +  prim_rel2 + 
  `tmp_insurance[, 2]` + asian_type1 + prof_cert2  + first_job1 + first_job4 + 
  first_job5  +  first_job6 + maj_ind3 + maj_ind4 + maj_ind7 + maj_ind8 + maj_ind9 
+ maj_ind13 + region3 + region4 + region10, data = dat3)
  
summary(test_mod1)

test_mod_less_insig <- lm(log(INCOME) ~                
                  AGE + marital7 + tmp_sex[,2] + edu_attmt2 + edu_attmt3 + edu_attmt4 + edu_attmt5 + 
                  edu_attmt6  + edu_attmt7 +  edu_attmt8 +  edu_attmt9 + edu_attmt10 +  edu_attmt11 
                + edu_attmt12 + edu_attmt13 + edu_attmt14 + prim_rel1 +  prim_rel2 + 
                  `tmp_insurance[, 2]` + asian_type1 + prof_cert2  + first_job1 + first_job4 + 
                  first_job5  +  first_job6 + maj_ind3 + maj_ind4 + maj_ind7 + maj_ind8 + maj_ind9 
                + maj_ind13 + region4 + region10, data = dat3)               
                  
                  
            
summary(test_mod_less_insig)


list(coef35)
colname(coef35)



colnames(dat2)[colnames(dat2) == "dat1$disability"] <- "disability"
