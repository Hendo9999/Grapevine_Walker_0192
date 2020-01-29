#input tidied data
source("Scripts/Grapevine_Walker_0192_load_tidy_data.R")


#analyse normality in individual treatment groups




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
