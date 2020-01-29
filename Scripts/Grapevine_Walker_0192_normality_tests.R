#input tidied data
source("Scripts/Grapevine_Walker_0192_load_tidy_data.R")


#analyse normality in individual treatment groups


#no heat & no salt
nhns <- chloride_3_tidy %>% 
  filter(treatment_heat == "no_heat", treatment_salt == "no_salt")

nhns1 <- nhns$cl_dry_weight_calc

#histogram
hist(nhns1, main = " ", xlab = "Leaf chloride content", freq=TRUE)


#density plot
hist(nhns1, main = " ", xlab = "Leaf chloride content", freq=FALSE)
curve(dnorm(x, mean=mean(nhns1), sd=sd(nhns1)), add=TRUE, col="red", lwd=2) 


#Kernel Density plots (more effective way to  view distribution of variable then histogram?)
plot(density(nhns1))
curve(dnorm(x, mean=mean(nhns1), sd=sd(nhns1)), add=TRUE, col="red", lwd=2) 


#qq plots

qqnorm(nhns1)
qqline(nhns1, col="red")




#heat & salt

hs <- chloride_3_tidy %>% 
  filter(treatment_heat == "heat", treatment_salt == "salt")

hs1 <- hs$cl_dry_weight_calc


#histogram
hist(hs1, main = " ", xlab = "Leaf chloride content", freq=TRUE)

#density plot
hist(hs1, main = " ", xlab = "Leaf chloride content", freq=FALSE)

curve(dnorm(x, mean=mean(hs1), sd=sd(hs1)), add=TRUE, col="red", lwd=2) 


#Kernel Density plots (more effective way to  view distribution of variable then histogram?)
plot(density(hs1))
curve(dnorm(x, mean=mean(hs1), sd=sd(hs1)), add=TRUE, col="red", lwd=2) 


#qq plots

qqnorm(hs1)
qqline(hs1, col="red")





#noheat & salt

nhs <- chloride_3_tidy %>% 
  filter(treatment_heat == "no_heat", treatment_salt == "salt")

nhs1 <- nhs$cl_dry_weight_calc


#histogram
hist(nhs1, main = " ", xlab = "Leaf chloride content", freq=TRUE)

#density plot
hist(nhs1, main = " ", xlab = "Leaf chloride content", freq=FALSE)

curve(dnorm(x, mean=mean(nhs1), sd=sd(nhs1)), add=TRUE, col="red", lwd=2) 


#Kernel Density plots (more effective way to  view distribution of variable then histogram?)
plot(density(nhs1))
curve(dnorm(x, mean=mean(nhs1), sd=sd(nhs1)), add=TRUE, col="red", lwd=2) 


#qq plots

qqnorm(nhs1)
qqline(nhs1, col="red")



#heat & nosalt

hns <- chloride_3_tidy %>% 
  filter(treatment_heat == "heat", treatment_salt == "no_salt")

hns1 <- hns$cl_dry_weight_calc


#histogram
hist(hns1, main = " ", xlab = "Leaf chloride content", freq=TRUE)

#density plot
hist(hns1, main = " ", xlab = "Leaf chloride content", freq=FALSE)

curve(dnorm(x, mean=mean(hns1), sd=sd(hns1)), add=TRUE, col="red", lwd=2) 


#Kernel Density plots (more effective way to  view distribution of variable then histogram?)
plot(density(hns1))
curve(dnorm(x, mean=mean(hns1), sd=sd(hns1)), add=TRUE, col="red", lwd=2) 


#qq plots

qqnorm(hns1)
qqline(hns1, col="red")








#Normality tests

#assign objects to 3 variables
laminae_sample_weight <- chloride_3_tidy$laminae_sample_weight
cl_sample_weight_mg <- chloride_3_tidy$cl_sample_weight_mg
cl_dry_weight_calc <- chloride_3_tidy$cl_dry_weight_calc


#normality of laminae-sample_weight

#data distribution

summary(laminae_sample_weight)
hist(laminae_sample_weight)
boxplot(laminae_sample_weight)




#Four normality tests

shapiro.test(laminae_sample_weight)
ad.test(laminae_sample_weight)
cvm.test(laminae_sample_weight)
lillie.test(laminae_sample_weight)

#graphical normality analysis

qqnorm(laminae_sample_weight)
qqline(laminae_sample_weight, col="red")


#normality of cl_sample_weight_mg

#data distribution

summary(cl_sample_weight_mg)
hist(cl_sample_weight_mg)
boxplot(cl_sample_weight_mg)
#Four normality tests

shapiro.test(cl_sample_weight_mg)
ad.test(cl_sample_weight_mg)
cvm.test(cl_sample_weight_mg)
lillie.test(cl_sample_weight_mg)

#graphical normality analysis

qqnorm(cl_sample_weight_mg)
qqline(cl_sample_weight_mg, col="red")



#normality of cl_dry_weight_calc

#data distribution
summary(cl_dry_weight_calc)
hist(cl_dry_weight_calc)
boxplot(cl_dry_weight_calc)

#four normality tests
shapiro.test(cl_dry_weight_calc)
ad.test(cl_dry_weight_calc)
cvm.test(cl_dry_weight_calc)
lillie.test(cl_dry_weight_calc)


#graphical normality analysis

qqnorm(cl_dry_weight_calc)
qqline(cl_dry_weight_calc, col="red")
