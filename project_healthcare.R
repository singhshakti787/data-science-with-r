library(readxl)

#choose the file where you have saved

hospitalcosts <- read.csv(file.choose())

#for viewing the datasets in global environment
View(hospitalcosts)
summary(hospitalcosts)

#histogram of age of patients

hist(hospitalcosts$AGE)
summary(as.factor(hospitalcosts$AGE))

#calculating total charge of patients age wise

aggregate(TOTCHG ~ AGE, FUN = sum, data = hospitalcosts)
max(aggregate(TOTCHG ~ AGE, FUN = sum, data = hospitalcosts))

#disgnosis of patients their strength and charges
which.max(summary(as.factor(hospitalcosts$APRDRG)))
diagnosiscost <- aggregate(TOTCHG ~ APRDRG, FUN = sum, data = hospitalcosts)
diagnosiscost
diagnosiscost[which.max(diagnosiscost$TOTCHG),]


#changing race into factor and calculating the charges

summary(as.factor(hospitalcosts$RACE))
head(hospitalcosts)
hospitalcosts<-na.omit(hospitalcosts)
hospitalcosts$RACE<-as.factor(hospitalcosts$RACE)


#anova.....making linear regression models

model <- aov(TOTCHG ~ RACE, data = hospitalcosts)

model
summary(model)
summary(hospitalcosts$RACE)

#model1

model1 <- lm(TOTCHG ~ AGE + FEMALE, data = hospitalcosts)
hospitalcosts$FEMALE<-as.factor(hospitalcosts$FEMALE)
summary(model1)
summary(hospitalcosts$FEMALE)
head(hospitalcosts)
hospitalcosts$RACE<-as.factor(hospitalcosts$RACE)

#model2

model2 <- lm(TOTCHG ~ AGE + FEMALE + RACE, data = hospitalcosts)
summary(model2)

#model3

model3 <- lm(TOTCHG ~ ., data = hospitalcosts)
summary(model3)
