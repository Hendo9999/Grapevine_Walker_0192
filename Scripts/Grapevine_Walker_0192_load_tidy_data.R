#install and load packages
install.packages("janitor")
install.packages("dplyr")
install.packages("rowr")
install.packages("nortest")
install.packages("pca3d")
install.packages("ggfortify")
install.packages("Hmisc")

library(tidyverse) #for tidy stuff
library(ggplot2)
library(readxl) #for reading Excel files
library(plyr) #for join_all to join multiple dataframes, note stuffs up rename function in dplyr
library(dplyr)
library(janitor) #for tidying up column names
library(rowr) #for cbind.all function
library(nortest) #for normality test
library(pca3d) #for 3d PCA visualisation
library(ggfortify) #helps with PCA analysis
library(Hmisc) # for error bars in plots


###Standards data for each replicate_batch


##17.1mM Cl standards data
raw_r1b1_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 9, range = "F49:J67") 

#reformat standards data 
#extract vectors with relevant range of values from table as vectors
a_r1b1 <- raw_r1b1_17mM_st$...3[(1:5)] 
b_r1b1 <- raw_r1b1_17mM_st$`607 mg/L`[(1:5)] 
c_r1b1 <- raw_r1b1_17mM_st$...3[(8:17)]
d_r1b1 <- raw_r1b1_17mM_st$`607 mg/L`[(8:17)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r1b1 <- cbind.fill(a_r1b1, b_r1b1, c_r1b1, d_r1b1, fill = NA)

#rename columns in matrix
colnames(mx_r1b1) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r1b1 <- data.frame(mx_r1b1) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))


#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r1b1 <- df_1_r1b1 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r1b1_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 9, range = "M49:O57") 

#reformat bulk standards data 
#rename columns in dataframe, correct bulk standard reading & calc Cl% dry weight
bulk_r1b1 <- raw_r1b1_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r1b1)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r1b1[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r1b2_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 10, range = "F48:J66")


#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r1b2 <- raw_r1b2_17mM_st$...3[(1:5)] 
b_r1b2 <- raw_r1b2_17mM_st$`607 mg/L`[(1:5)] 
c_r1b2 <- raw_r1b2_17mM_st$...3[(8:17)]
d_r1b2 <- raw_r1b2_17mM_st$`607 mg/L`[(8:17)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r1b2 <- cbind.fill(a_r1b2, b_r1b2, c_r1b2, d_r1b2, fill = NA)

#rename columns in matrix
colnames(mx_r1b2) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r1b2 <- data.frame(mx_r1b2) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))


#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r1b2 <- df_1_r1b2 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))



###bulk grapevine data
raw_r1b2_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 10, range = "M48:O57") 

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r1b2 <- raw_r1b2_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r1b2)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r1b2[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r1b3_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 12, range = "B48:E66")


#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r1b3 <- raw_r1b3_17mM_st$...2[(1:5)] 
b_r1b3 <- raw_r1b3_17mM_st$`607 mg/L`[(1:5)] 
c_r1b3 <- raw_r1b3_17mM_st$...2[(8:16)]
d_r1b3 <- raw_r1b3_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r1b3 <- cbind.fill(a_r1b3, b_r1b3, c_r1b3, d_r1b3, fill = NA)

#rename columns in matrix
colnames(mx_r1b3) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r1b3 <- data.frame(mx_r1b3) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))


#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r1b3 <- df_1_r1b3 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))



###bulk grapevine data
raw_r1b3_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 12, range = "H48:J61")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r1b3 <- raw_r1b3_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r1b3)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r1b3[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))


##17.1mM Cl standards data
raw_r1b4_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 14, range = "B49:E67")


#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r1b4 <- raw_r1b4_17mM_st$...2[(1:5)] 
b_r1b4 <- raw_r1b4_17mM_st$`607 mg/L`[(1:5)] 
c_r1b4 <- raw_r1b4_17mM_st$...2[(8:16)]
d_r1b4 <- raw_r1b4_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r1b4 <- cbind.fill(a_r1b4, b_r1b4, c_r1b4, d_r1b4, fill = NA)

#rename columns in matrix
colnames(mx_r1b4) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r1b4 <- data.frame(mx_r1b4) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))


#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r1b4 <- df_1_r1b4 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))



###bulk grapevine data
raw_r1b4_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 14, range = "H49:J58")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r1b4 <- raw_r1b4_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r1b4)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r1b4[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))


##17.1mM Cl standards data
raw_r1b5_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 18, range = "B54:E72")


#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r1b5 <- raw_r1b5_17mM_st$...2[(1:5)] 
b_r1b5 <- raw_r1b5_17mM_st$`607 mg/L`[(1:5)] 
c_r1b5 <- raw_r1b5_17mM_st$...2[(8:17)]
d_r1b5 <- raw_r1b5_17mM_st$`607 mg/L`[(8:17)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r1b5 <- cbind.fill(a_r1b5, b_r1b5, c_r1b5, d_r1b5, fill = NA)

#rename columns in matrix
colnames(mx_r1b5) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r1b5 <- data.frame(mx_r1b5) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))


#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r1b5 <- df_1_r1b5 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r1b5_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 18, range = "H54:J65")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r1b5 <- raw_r1b5_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r1b5)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r1b5[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r1b6_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 20, range = "B38:E56")


#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r1b6 <- raw_r1b6_17mM_st$...2[(1:5)] 
b_r1b6 <- raw_r1b6_17mM_st$`607 mg/L`[(1:5)] 
c_r1b6 <- raw_r1b6_17mM_st$...2[(8:15)]
d_r1b6 <- raw_r1b6_17mM_st$`607 mg/L`[(8:15)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r1b6 <- cbind.fill(a_r1b6, b_r1b6, c_r1b6, d_r1b6, fill = NA)

#rename columns in matrix
colnames(mx_r1b6) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r1b6 <- data.frame(mx_r1b6) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))


#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r1b6 <- df_1_r1b6 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r1b6_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 20, range = "H38:J49")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r1b6 <- raw_r1b6_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r1b6)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r1b6[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r2b1_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 8, range = "F122:J140")

#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r2b1 <- raw_r2b1_17mM_st$...3[(1:5)] 
b_r2b1 <- raw_r2b1_17mM_st$`607 mg/L`[(1:5)] 
c_r2b1 <- raw_r2b1_17mM_st$...3[(8:15)]
d_r2b1 <- raw_r2b1_17mM_st$`607 mg/L`[(8:15)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r2b1 <- cbind.fill(a_r2b1, b_r2b1, c_r2b1, d_r2b1, fill = NA)

#rename columns in matrix
colnames(mx_r2b1) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r2b1 <- data.frame(mx_r2b1) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r2b1 <- df_1_r2b1 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r2b1_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 8, range = "M122:O131")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r2b1 <- raw_r2b1_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r2b1)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r2b1[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r2b2_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 9, range = "F118:J136")


#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r2b2 <- raw_r2b2_17mM_st$...3[(1:5)] 
b_r2b2 <- raw_r2b2_17mM_st$`607 mg/L`[(1:5)] 
c_r2b2 <- raw_r2b2_17mM_st$...3[(8:16)]
d_r2b2 <- raw_r2b2_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r2b2 <- cbind.fill(a_r2b2, b_r2b2, c_r2b2, d_r2b2, fill = NA)

#rename columns in matrix
colnames(mx_r2b2) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r2b2 <- data.frame(mx_r2b2) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r2b2 <- df_1_r2b2 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r2b2_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 9, range = "M118:O128")


#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r2b2 <- raw_r2b2_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r2b2)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r2b2[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r2b3_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 11, range = "C51:G69")

#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r2b3 <- raw_r2b3_17mM_st$...3[(1:5)] 
b_r2b3 <- raw_r2b3_17mM_st$`607 mg/L`[(1:5)] 
c_r2b3 <- raw_r2b3_17mM_st$...3[(8:16)]
d_r2b3 <- raw_r2b3_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r2b3 <- cbind.fill(a_r2b3, b_r2b3, c_r2b3, d_r2b3, fill = NA)

#rename columns in matrix
colnames(mx_r2b3) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r2b3 <- data.frame(mx_r2b3) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r2b3 <- df_1_r2b3 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r2b3_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 11, range = "J51:L61")


#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r2b3 <- raw_r2b3_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r2b3)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r2b3[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))




##17.1mM Cl standards data
raw_r2b4_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 13, range = "C74:G92")


#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r2b4 <- raw_r2b4_17mM_st$...3[(1:5)] 
b_r2b4 <- raw_r2b4_17mM_st$`607 mg/L`[(1:5)] 
c_r2b4 <- raw_r2b4_17mM_st$...3[(8:16)]
d_r2b4 <- raw_r2b4_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r2b4 <- cbind.fill(a_r2b4, b_r2b4, c_r2b4, d_r2b4, fill = NA)

#rename columns in matrix
colnames(mx_r2b4) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r2b4 <- data.frame(mx_r2b4) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r2b4 <- df_1_r2b4 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r2b4_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 13, range = "J74:L85")


#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r2b4 <- raw_r2b4_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r2b4)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r2b4[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))


##17.1mM Cl standards data
raw_r2b5_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 17, n_max = 47, range = "A52:E68")



#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r2b5 <- raw_r2b4_17mM_st$...3[(1:5)] 
b_r2b5 <- raw_r2b4_17mM_st$`607 mg/L`[(1:5)] 
c_r2b5 <- raw_r2b4_17mM_st$...3[(8:16)]
d_r2b5 <- raw_r2b4_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r2b5 <- cbind.fill(a_r2b5, b_r2b5, c_r2b5, d_r2b5, fill = NA)

#rename columns in matrix
colnames(mx_r2b5) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r2b5 <- data.frame(mx_r2b5) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r2b5 <- df_1_r2b5 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r2b5_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 17, n_max = 47, range = "H52:J68")#note: no measurement data available

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r2b5 <- raw_r2b5_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r2b5)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r2b5[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))

####No measured bulk standard data for rep_batch r2b5


##17.1mM Cl standards data
raw_r2b6_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 19, n_max = 53, range = "A57:E74")

#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r2b6 <- raw_r2b6_17mM_st$...3[(1:5)] 
b_r2b6 <- raw_r2b6_17mM_st$`607 mg/L`[(1:5)] 
c_r2b6 <- raw_r2b6_17mM_st$...3[(8:16)]
d_r2b6 <- raw_r2b6_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r2b6 <- cbind.fill(a_r2b6, b_r2b6, c_r2b6, d_r2b6, fill = NA)

#rename columns in matrix
colnames(mx_r2b6) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r2b6 <- data.frame(mx_r2b6) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r2b6 <- df_1_r2b6 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r2b6_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 19, n_max = 53, range = "H57:J68")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r2b6 <- raw_r2b6_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r2b6)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r2b6[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r3b1_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 8, n_max = 44, range = "F50:J65")

#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r3b1 <- raw_r3b1_17mM_st$...3[(1:5)] 
b_r3b1 <- raw_r3b1_17mM_st$`607 mg/L`[(1:5)] 
c_r3b1 <- raw_r3b1_17mM_st$...3[(8:15)]
d_r3b1 <- raw_r3b1_17mM_st$`607 mg/L`[(8:15)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r3b1 <- cbind.fill(a_r3b1, b_r3b1, c_r3b1, d_r3b1, fill = NA)

#rename columns in matrix
colnames(mx_r3b1) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r3b1 <- data.frame(mx_r3b1) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r3b1 <- df_1_r3b1 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r3b1_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 8, n_max = 44, range = "M50:O59")


#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r3b1 <- raw_r3b1_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r3b1)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r3b1[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r3b2_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 9, n_max = 43, range = "F48:J62")

#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r3b2 <- raw_r3b2_17mM_st$...3[(1:5)] 
b_r3b2 <- raw_r3b2_17mM_st$`607 mg/L`[(1:5)] 
c_r3b2 <- raw_r3b2_17mM_st$...3[(8:13)]
d_r3b2 <- raw_r3b2_17mM_st$`607 mg/L`[(8:13)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r3b2 <- cbind.fill(a_r3b2, b_r3b2, c_r3b2, d_r3b2, fill = NA)

#rename columns in matrix
colnames(mx_r3b2) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r3b2 <- data.frame(mx_r3b2) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r3b2 <- df_1_r3b2 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r3b2_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 9, n_max = 43, range = "M48:O58")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r3b2 <- raw_r3b2_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r3b2)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r3b2[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r3b3_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 11, n_max = 44, range = "C48:G64")

#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r3b3 <- raw_r3b3_17mM_st$...3[(1:5)] 
b_r3b3 <- raw_r3b3_17mM_st$`607 mg/L`[(1:5)] 
c_r3b3 <- raw_r3b3_17mM_st$...3[(8:16)]
d_r3b3 <- raw_r3b3_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r3b3 <- cbind.fill(a_r3b3, b_r3b3, c_r3b3, d_r3b3, fill = NA)

#rename columns in matrix
colnames(mx_r3b3) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r3b3 <- data.frame(mx_r3b3) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r3b3 <- df_1_r3b3 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r3b3_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 11, n_max = 44, range = "K48:M58")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r3b3 <- raw_r3b3_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r3b3)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r3b3[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))


##17.1mM Cl standards data
raw_r3b4_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 13, n_max = 48, range = "C53:G69")

#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r3b4 <- raw_r3b4_17mM_st$...3[(1:5)] 
b_r3b4 <- raw_r3b4_17mM_st$`607 mg/L`[(1:5)] 
c_r3b4 <- raw_r3b4_17mM_st$...3[(8:16)]
d_r3b4 <- raw_r3b4_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r3b4 <- cbind.fill(a_r3b4, b_r3b4, c_r3b4, d_r3b4, fill = NA)

#rename columns in matrix
colnames(mx_r3b4) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r3b4 <- data.frame(mx_r3b4) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r3b4 <- df_1_r3b4 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r3b4_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 13, n_max = 48, range = "J53:L63")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r3b4 <- raw_r3b4_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r3b4)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r3b4[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))




##17.1mM Cl standards data
raw_r3b5_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 17, n_max = 45, range = "B54:E69")


#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r3b5 <- raw_r3b5_17mM_st$...2[(1:5)] 
b_r3b5 <- raw_r3b5_17mM_st$`607 mg/L`[(1:5)] 
c_r3b5 <- raw_r3b5_17mM_st$...2[(8:15)]
d_r3b5 <- raw_r3b5_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r3b5 <- cbind.fill(a_r3b5, b_r3b5, c_r3b5, d_r3b5, fill = NA)

#rename columns in matrix
colnames(mx_r3b5) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r3b5 <- data.frame(mx_r3b5) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r3b5 <- df_1_r3b5 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r3b5_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 17, n_max = 45, range = "H54:J65")


#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r3b5 <- raw_r3b5_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r3b5)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r3b5[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))


##Note: r3b5 is last rep_batch for this experiment



####Process chloride data

#assign primary data files to variables


#files with chloride data for each replicate & batch
#import rows from specific sheets with chloride data for samples only (not standards)
#Biological replicate #1, sample batches #1 - #6, rows with pertinent data
raw_r1b1 <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 9, n_max = 45) %>%
  clean_names() %>% #replaces spaces and removes symbols from column names
  select(-c(2, 3, 4, 5, 18, 19, 22, 23, 26, 27, 28)) %>% #remove columns with analyses or duplicated info
  dplyr::rename(rep = rep_10, smarthouse = smarthouse_2, cl_rep1_reading = cl_r1_read, cl_rep_2_reading = cl_r2_read, 
                vial_number_1 = chloride_number_29, vial_number_2 = chloride_number_30) %>% #rename columns with variant names, need to force through dplyr because of plyr issue
  mutate("cl_rep1_reading_adj" =  as.numeric(cl_rep1_reading) - mean(c_r1b1), "cl_rep2_reading_adj" = as.numeric(cl_rep_2_reading) - mean(c_r1b1)) %>%
  mutate("cl_%_dry_weight_rep1" = (cl_rep1_reading_adj * cf_r1b1[1,1] * 35.5) / (as.numeric(cl_rep1_wt_mg) * 10),
         "cl_%_dry_weight_rep2" = (cl_rep2_reading_adj * cf_r1b1[1,1] * 35.5) / (as.numeric(cl_rep2_wt_mg) * 10))
raw_r1b1$rep_batch="r1b1" #add assay batch info


raw_r1b2 <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 10, n_max = 44) %>%
  clean_names() %>% #replaces spaces and removes symbols from column names
  select(-c(2, 3, 4, 5, 18, 19, 22, 23, 26, 27, 28)) %>% #remove columns with analyses or duplicated info
  dplyr::rename(rep = rep_10, smarthouse = smarthouse_2, 
                cl_rep1_reading = cl_r1_read, cl_rep_2_reading = cl_r2_read) %>% #rename columns with variant names, need to force through dplyr because of plyr issue
  mutate("cl_rep1_reading_adj" =  as.numeric(cl_rep1_reading) - mean(c_r1b2), "cl_rep2_reading_adj" = as.numeric(cl_rep_2_reading) - mean(c_r1b2)) %>%
  mutate("cl_%_dry_weight_rep1" = (cl_rep1_reading_adj * cf_r1b2[1,1] * 35.5) / (as.numeric(cl_rep1_wt_mg) * 10),
         "cl_%_dry_weight_rep2" = (cl_rep2_reading_adj * cf_r1b2[1,1] * 35.5) / (as.numeric(cl_rep2_wt_mg) * 10))
raw_r1b2$rep_batch="r1b2" #add assay batch info


raw_r1b3 <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 12, n_max = 43) %>%
  clean_names() %>% #replaces spaces and removes symbols from column names
  select(-c(14, 15, 18, 19, 22, 23, 24)) %>% #remove columns with analyses or duplicated info
  dplyr::rename(rep = rep_6, cl_rep1_reading = cl_r1_read, cl_rep_2_reading = cl_r2_read) %>% #rename columns with variant names, need to force through dplyr because of plyr issue
  mutate("cl_rep1_reading_adj" =  as.numeric(cl_rep1_reading) - mean(c_r1b3), "cl_rep2_reading_adj" = as.numeric(cl_rep_2_reading) - mean(c_r1b3)) %>%
  mutate("cl_%_dry_weight_rep1" = (cl_rep1_reading_adj * cf_r1b3[1,1] * 35.5) / (as.numeric(cl_rep1_wt_mg) * 10),
         "cl_%_dry_weight_rep2" = (cl_rep2_reading_adj * cf_r1b3[1,1] * 35.5) / (as.numeric(cl_rep2_wt_mg) * 10))
raw_r1b3$rep_batch="r1b3" #add assay batch info


raw_r1b4 <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 14, n_max = 41) %>%
  clean_names() %>% #replaces spaces and removes symbols from column names
  select(-c(14, 15, 18, 19, 22, 23, 24)) %>% #remove columns with analyses or duplicated info
  dplyr::rename(rep = rep_6, cl_rep1_reading = cl_r1_read, cl_rep_2_reading = cl_r2_read) %>% #rename columns with variant names, need to force through dplyr because of plyr issue
  mutate("cl_rep1_reading_adj" =  as.numeric(cl_rep1_reading) - mean(c_r1b4), "cl_rep2_reading_adj" = as.numeric(cl_rep_2_reading) - mean(c_r1b4)) %>%
  mutate("cl_%_dry_weight_rep1" = (cl_rep1_reading_adj * cf_r1b4[1,1] * 35.5) / (as.numeric(cl_rep1_wt_mg) * 10),
         "cl_%_dry_weight_rep2" = (cl_rep2_reading_adj * cf_r1b4[1,1] * 35.5) / (as.numeric(cl_rep2_wt_mg) * 10))
raw_r1b4$rep_batch="r1b4" #add assay batch info


raw_r1b5 <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 18, n_max = 48) %>%
  clean_names() %>% #replaces spaces and removes symbols from column names
  select(-c(16, 17, 20, 21, 22)) %>% #remove columns with analyses or duplicated info
  dplyr::rename(cl_rep1_reading = cl_r1_read, cl_rep_2_reading = cl_r2_read) %>% #rename columns with variant names, need to force through dplyr because of plyr issue
  mutate("cl_rep1_reading_adj" =  as.numeric(cl_rep1_reading) - mean(c_r1b5), "cl_rep2_reading_adj" = as.numeric(cl_rep_2_reading) - mean(c_r1b5)) %>%
  mutate("cl_%_dry_weight_rep1" = (cl_rep1_reading_adj * cf_r1b5[1,1] * 35.5) / (as.numeric(cl_rep1_wt_mg) * 10),
         "cl_%_dry_weight_rep2" = (cl_rep2_reading_adj * cf_r1b5[1,1] * 35.5) / (as.numeric(cl_rep2_wt_mg) * 10))
raw_r1b5$rep_batch="r1b5" #add assay batch info


raw_r1b6 <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 20, n_max = 30) %>%
  clean_names() %>% #replaces spaces and removes symbols from column names
  select(-c(16, 17, 20, 21, 22)) %>% #remove columns with analyses or duplicated info
  dplyr::rename(cl_rep1_reading = cl_r1_read, cl_rep_2_reading = cl_r2_read) %>% #rename columns with variant names, need to force through dplyr because of plyr issue
  mutate("cl_rep1_reading_adj" =  as.numeric(cl_rep1_reading) - mean(c_r1b6), "cl_rep2_reading_adj" = as.numeric(cl_rep_2_reading) - mean(c_r1b6)) %>%
  mutate("cl_%_dry_weight_rep1" = (cl_rep1_reading_adj * cf_r1b6[1,1] * 35.5) / (as.numeric(cl_rep1_wt_mg) * 10),
         "cl_%_dry_weight_rep2" = (cl_rep2_reading_adj * cf_r1b6[1,1] * 35.5) / (as.numeric(cl_rep2_wt_mg) * 10))
raw_r1b6$rep_batch="r1b6"#add assay batch info



#Biological replicate #2, sample batches #1 - #6
raw_r2b1 <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 8, cell_rows(73:115)) %>%
  clean_names() %>% #replaces spaces and removes symbols from column names
  select(-c(2, 3, 4, 5, 18, 19, 23, 24, 28, 29, 30)) %>% #remove columns with analyses or duplicated info
  dplyr::rename(smarthouse = smarthouse_2, count_number = count) %>% #rename columns with variant names, need to force through dplyr because of plyr issue
  mutate("cl_rep1_reading_adj" =  as.numeric(cl_rep1_reading) - mean(c_r2b1), "cl_rep2_reading_adj" = as.numeric(cl_rep_2_reading) - mean(c_r2b1)) %>%
  mutate("cl_%_dry_weight_rep1" = (cl_rep1_reading_adj * cf_r2b1[1,1] * 35.5) / (as.numeric(cl_rep1_wt_mg) * 10),
         "cl_%_dry_weight_rep2" = (cl_rep2_reading_adj * cf_r2b1[1,1] * 35.5) / (as.numeric(cl_rep2_wt_mg) * 10))
raw_r2b1$rep_batch="r2b1" #add assay batch info


raw_r2b2 <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 9, cell_rows(71:114)) %>%
  clean_names() %>% #replaces spaces and removes symbols from column names
  select(-c(2, 3, 4, 5, 18, 19, 23, 24, 28, 29, 30))  %>% #remove columns with analyses or duplicated info
  dplyr::rename(rep = rep_10, smarthouse = smarthouse_2) %>% #rename columns with variant names, need to force through dplyr because of plyr issue
  mutate("cl_rep1_reading_adj" =  as.numeric(cl_rep1_reading) - mean(c_r2b2), "cl_rep2_reading_adj" = as.numeric(cl_rep_2_reading) - mean(c_r2b2)) %>%
  mutate("cl_%_dry_weight_rep1" = (cl_rep1_reading_adj * cf_r2b2[1,1] * 35.5) / (as.numeric(cl_rep1_wt_mg) * 10),
         "cl_%_dry_weight_rep2" = (cl_rep2_reading_adj * cf_r2b2[1,1] * 35.5) / (as.numeric(cl_rep2_wt_mg) * 10))
raw_r2b2$rep_batch="r2b2" #add assay batch info


raw_r2b3 <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 11, n_max = 47) %>%
  clean_names() %>% #replaces spaces and removes symbols from column names
  select(-c(14, 15, 19, 20, 24, 25, 26)) %>% #remove columns with analyses or duplicated info
  dplyr::rename(rep = rep_6) %>% #rename columns with variant names, need to force through dplyr because of plyr issue
  mutate("cl_rep1_reading_adj" =  as.numeric(cl_rep1_reading) - mean(c_r2b3), "cl_rep2_reading_adj" = as.numeric(cl_rep_2_reading) - mean(c_r2b3)) %>%
  mutate("cl_%_dry_weight_rep1" = (cl_rep1_reading_adj * cf_r2b3[1,1] * 35.5) / (as.numeric(cl_rep1_wt_mg) * 10),
         "cl_%_dry_weight_rep2" = (cl_rep2_reading_adj * cf_r2b3[1,1] * 35.5) / (as.numeric(cl_rep2_wt_mg) * 10))
raw_r2b3$rep_batch="r2b3" #add assay batch info


raw_r2b4 <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 13, cell_rows(29:69)) %>%
  clean_names() %>% #replaces spaces and removes symbols from column names
  select(-c(14, 15, 19, 20, 24, 25, 26)) %>% #remove columns with analyses or duplicated info
  dplyr::rename(rep = rep_6) %>% #rename columns with variant names, need to force through dplyr because of plyr issue
  mutate("cl_rep1_reading_adj" =  as.numeric(cl_rep1_reading) - mean(c_r2b4), "cl_rep2_reading_adj" = as.numeric(cl_rep_2_reading) - mean(c_r2b4)) %>%
  mutate("cl_%_dry_weight_rep1" = (cl_rep1_reading_adj * cf_r2b4[1,1] * 35.5) / (as.numeric(cl_rep1_wt_mg) * 10),
         "cl_%_dry_weight_rep2" = (cl_rep2_reading_adj * cf_r2b4[1,1] * 35.5) / (as.numeric(cl_rep2_wt_mg) * 10))
raw_r2b4$rep_batch="r2b4" #add assay batch info


raw_r2b5 <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 17, cell_rows(74:120)) %>%
  clean_names() %>% #replaces spaces and removes symbols from column names
  select(-c(14, 15)) %>% #remove columns with analyses or duplicated info
  dplyr::rename(rep = rep_6) %>% #rename columns with variant names, need to force through dplyr because of plyr issue
  mutate("cl_rep1_reading_adj" =  as.numeric(cl_rep1_reading) - mean(c_r2b5), "cl_rep2_reading_adj" = as.numeric(cl_rep_2_reading) - mean(c_r2b5)) %>%
  mutate("cl_%_dry_weight_rep1" = (cl_rep1_reading_adj * cf_r2b5[1,1] * 35.5) / (as.numeric(cl_rep1_wt_mg) * 10),
         "cl_%_dry_weight_rep2" = (cl_rep2_reading_adj * cf_r2b5[1,1] * 35.5) / (as.numeric(cl_rep2_wt_mg) * 10))
raw_r2b5$rep_batch="r2b5" #add assay batch info


raw_r2b6 <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 19, n_max = 53) %>%
  clean_names() %>% #replaces spaces and removes symbols from column names
  select(-c(14, 15, 19, 20, 24, 25, 26)) %>% #remove columns with analyses or duplicated info
  dplyr::rename(rep = rep_6) %>% #rename columns with variant names, need to force through dplyr because of plyr issue
  mutate("cl_rep1_reading_adj" =  as.numeric(cl_rep1_reading) - mean(c_r2b5), "cl_rep2_reading_adj" = as.numeric(cl_rep_2_reading) - mean(c_r2b5)) %>%
  mutate("cl_%_dry_weight_rep1" = (cl_rep1_reading_adj * cf_r2b5[1,1] * 35.5) / (as.numeric(cl_rep1_wt_mg) * 10),
         "cl_%_dry_weight_rep2" = (cl_rep2_reading_adj * cf_r2b5[1,1] * 35.5) / (as.numeric(cl_rep2_wt_mg) * 10))
raw_r2b6$rep_batch="r2b6" #add assay batch info




#Biological replicate #3, sample batches #1 - #6
raw_r3b1 <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 8, n_max = 44) %>%
  clean_names() %>% #replaces spaces and removes symbols from column names
  select(-c(2, 3, 4, 5, 18, 19, 23, 24, 28, 29, 30))  %>% #remove columns with analyses or duplicated info
  dplyr::rename(smarthouse = smarthouse_2, count_number = count) %>% #rename columns with variant names, need to force through dplyr because of plyr issue
  mutate("cl_rep1_reading_adj" =  as.numeric(cl_rep1_reading) - mean(c_r3b1), "cl_rep2_reading_adj" = as.numeric(cl_rep_2_reading) - mean(c_r3b1)) %>%
  mutate("cl_%_dry_weight_rep1" = (cl_rep1_reading_adj * cf_r3b1[1,1] * 35.5) / (as.numeric(cl_rep1_wt_mg) * 10),
         "cl_%_dry_weight_rep2" = (cl_rep2_reading_adj * cf_r3b1[1,1] * 35.5) / (as.numeric(cl_rep2_wt_mg) * 10))
raw_r3b1$rep_batch="r3b1" #add assay batch info


raw_r3b2 <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 9, n_max = 43) %>%
  clean_names() %>% #replaces spaces and removes symbols from column names
  select(-c(2, 3, 4, 5, 18, 19, 23, 24, 28, 29, 30))  %>% #remove columns with analyses or duplicated info
  dplyr::rename(rep = rep_10, smarthouse = smarthouse_2) %>% #rename columns with variant names, need to force through dplyr because of plyr issue
  mutate("cl_rep1_reading_adj" =  as.numeric(cl_rep1_reading) - mean(c_r3b2), "cl_rep2_reading_adj" = as.numeric(cl_rep_2_reading) - mean(c_r3b2)) %>%
  mutate("cl_%_dry_weight_rep1" = (cl_rep1_reading_adj * cf_r3b2[1,1] * 35.5) / (as.numeric(cl_rep1_wt_mg) * 10),
         "cl_%_dry_weight_rep2" = (cl_rep2_reading_adj * cf_r3b2[1,1] * 35.5) / (as.numeric(cl_rep2_wt_mg) * 10))
raw_r3b2$rep_batch="r3b2" #add assay batch info

raw_r3b3 <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 11, n_max = 44) %>%
  clean_names() %>% #replaces spaces and removes symbols from column names
  select(-c(4, 15, 16, 20, 21, 25, 26, 27)) %>% #remove columns with analyses or duplicated info
  dplyr::rename(rep = rep_7) %>% #rename columns with variant names, need to force through dplyr because of plyr issue
  mutate("cl_rep1_reading_adj" =  as.numeric(cl_rep1_reading) - mean(c_r3b3), "cl_rep2_reading_adj" = as.numeric(cl_rep_2_reading) - mean(c_r3b3)) %>%
  mutate("cl_%_dry_weight_rep1" = (cl_rep1_reading_adj * cf_r3b3[1,1] * 35.5) / (as.numeric(cl_rep1_wt_mg) * 10),
         "cl_%_dry_weight_rep2" = (cl_rep2_reading_adj * cf_r3b3[1,1] * 35.5) / (as.numeric(cl_rep2_wt_mg) * 10))
raw_r3b3$rep_batch="r3b3" #add assay batch info


raw_r3b4 <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 13, n_max = 48) %>%
  clean_names() %>% #replaces spaces and removes symbols from column names
  select(-c(14, 15, 19, 20, 24, 25, 26)) %>% #remove columns with analyses or duplicated info
  dplyr::rename(rep = rep_6) %>% #rename columns with variant names, need to force through dplyr because of plyr issue
  mutate("cl_rep1_reading_adj" =  as.numeric(cl_rep1_reading) - mean(c_r3b4), "cl_rep2_reading_adj" = as.numeric(cl_rep_2_reading) - mean(c_r3b4)) %>%
  mutate("cl_%_dry_weight_rep1" = (cl_rep1_reading_adj * cf_r3b4[1,1] * 35.5) / (as.numeric(cl_rep1_wt_mg) * 10),
         "cl_%_dry_weight_rep2" = (cl_rep2_reading_adj * cf_r3b4[1,1] * 35.5) / (as.numeric(cl_rep2_wt_mg) * 10))
raw_r3b4$rep_batch="r3b4" #add assay batch info


raw_r3b5 <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 17, n_max = 45) %>%
  clean_names() %>% #replaces spaces and removes symbols from column names
  select(-c(16, 17, 20, 21, 22)) %>% #remove columns with analyses or duplicated info
  dplyr::rename(cl_rep1_reading = cl_r1_read, cl_rep_2_reading = cl_r2_read) %>% #rename columns with variant names, need to force through dplyr because of plyr issue
  mutate("cl_rep1_reading_adj" =  as.numeric(cl_rep1_reading) - mean(c_r3b5), "cl_rep2_reading_adj" = as.numeric(cl_rep_2_reading) - mean(c_r3b5)) %>%
  mutate("cl_%_dry_weight_rep1" = (cl_rep1_reading_adj * cf_r3b5[1,1] * 35.5) / (as.numeric(cl_rep1_wt_mg) * 10),
         "cl_%_dry_weight_rep2" = (cl_rep2_reading_adj * cf_r3b5[1,1] * 35.5) / (as.numeric(cl_rep2_wt_mg) * 10))
raw_r3b5$rep_batch="r3b5" #add assay batch info


#joining data together

dfs <- list(raw_r1b1, raw_r1b2, raw_r1b3, raw_r1b4, raw_r1b5, raw_r1b6, 
            raw_r2b1, raw_r2b2, raw_r2b3, raw_r2b4, raw_r2b5, raw_r2b6,
            raw_r3b1, raw_r3b2, raw_r3b3, raw_r3b4, raw_r3b5)#list of dataframes with raw data
chloride_dfs <- join_all(dfs, type = "full") #join dataframes together 



#renaming columns in combined dataframe
chloride_1_data <- chloride_dfs %>%
  dplyr::rename(bio_rep = rep, treatment_heat = treat_1_h, treatment_salt = treat_2_s, 
                cl_sample_weight_mg_rep1 = cl_rep1_wt_mg, cl_read_rep1 = cl_rep1_reading,
                cl_sample_weight_mg_rep2 = cl_rep2_wt_mg, cl_read_rep2 = cl_rep_2_reading,
                cl_read_adj_rep1 = cl_rep1_reading_adj, cl_read_adj_rep2 = cl_rep2_reading_adj,
                cl_dry_weight_calc_rep1 = "cl_%_dry_weight_rep1", cl_dry_weight_calc_rep2 = "cl_%_dry_weight_rep2",
                vial_number_rep1 = vial_number_1, vial_number_rep2 = vial_number_2) #rename columns with variant names, need to force through dplyr because of plyr issue


#changing treatment value names
chloride_1_data$treatment_salt[is.na(chloride_1_data$treatment_salt)] <- "no_salt"
chloride_1_data$treatment_salt[str_detect(chloride_1_data$treatment_salt, "Salt")] <- "salt"
chloride_1_data$treatment_heat[str_detect(chloride_1_data$treatment_heat, "CONT.")] <- "no_heat"
chloride_1_data$treatment_heat[str_detect(chloride_1_data$treatment_heat, "HEAT")] <- "heat"


#changing some renamed columns to numerical data
chloride_1_data$cl_sample_weight_mg_rep1 <- as.numeric(as.character(chloride_1_data$cl_sample_weight_mg_rep1))
chloride_1_data$cl_sample_weight_mg_rep2 <- as.numeric(as.character(chloride_1_data$cl_sample_weight_mg_rep2))
chloride_1_data$cl_read_rep1 <- as.numeric(as.character(chloride_1_data$cl_read_rep1))
chloride_1_data$cl_read_rep2 <- as.numeric(as.character(chloride_1_data$cl_read_rep2))          
chloride_1_data$bio_rep <- as.character((chloride_1_data$bio_rep))          



#add harvester data
raw_harvester <- read_xlsx("Data/0192 Mature laminae chloride summary with harvester info.xlsx") %>% 
  clean_names() %>% 
  select(code, harvester) 
chloride_data_harv <- left_join(chloride_1_data, raw_harvester, by="code") 


#add laminae sampling weight data
raw_laminae <- read_xlsx("Data/0192 Mature Laminae dry wts.xlsx") %>% 
  clean_names() %>%
  dplyr::rename(laminae_sample_weight = dry_wt_lam_ion) %>% 
  select(code, laminae_sample_weight)
chloride_data_harv_lam <- left_join(chloride_data_harv, raw_laminae, by = "code")
chloride_data_harv_lam$laminae_sample_weight <- as.numeric(as.character(chloride_data_harv_lam$laminae_sample_weight))

#remove columns no longer required
chloride_data_harv_lam1 <- chloride_data_harv_lam %>% 
  select(-c(vial_number_rep1, vial_number_rep2, cl_read_rep1, cl_read_rep2, cl_read_adj_rep1, cl_read_adj_rep2))


#using gather, separate and spread
chloride_1_tidy <- chloride_data_harv_lam1 %>%
  #gather both variables/duplicates into key and value columns
  gather(key = assay_rep, value = value, "cl_sample_weight_mg_rep1", "cl_sample_weight_mg_rep2", "cl_dry_weight_calc_rep1", "cl_dry_weight_calc_rep2") %>%
  #separate the gathered column variable into two columns, one with the assay info and the other with replicate info
  separate(col = assay_rep, into = c("assay", "tech_rep"), sep = "_rep") %>%
  #spread assay column into two columns with assay info
  spread(key = assay, value = value)


#cleanup to remove NA values in sample weight and cl dry weight columns
chloride_2_tidy <- chloride_1_tidy %>%
  #  mutate(tech_rep = as.numeric(tech_rep)) %>% 
  filter(!is.na(cl_sample_weight_mg) & !is.na(cl_dry_weight_calc))


#remove outlier in cl_sample_weight_mg tech_rep1.
chloride_3_tidy <- chloride_2_tidy %>% 
  filter(cl_sample_weight_mg <70)

#combine treatments into a single column
chloride_4_tidy <- chloride_3_tidy %>%
  unite(col = "treatment", "treatment_heat", "treatment_salt", sep = "_&_")