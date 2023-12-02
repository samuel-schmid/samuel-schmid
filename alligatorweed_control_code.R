install.packages('ggplot2')
install.packages('tidyverse')
install.packages('plotrix')
install.packages('car')

library('ggplot2')
library('tidyverse')
library('plotrix')
library('car')

# RELEVENT STATISTICAL ANALYSIS FOR HERBICIDE TRIALS

df1 <- read.csv('herbicide_data.csv', header = T)
df1[,'trial']<-factor(df1[,'trial'])
df1[,'treat_num']<-factor(df1[,'treat_num'])
df1[,'treatment']<-factor(df1[,'treatment'])

summary(df1)
head(df1)

fit1<- lm(biomass_reduc ~ treatment + trial, data = df1)
summary(fit1)
df1 %>%
  group_by(treatment) %>%
  summarise(means = mean(biomass_reduc), SE = std.error(biomass_reduc))
df1 %>%
  group_by(trial) %>%
  summarise(means = mean(biomass_reduc), SE = std.error(biomass_reduc))

summary(aov1 <- aov (biomass_reduc ~ treat_num + trial, data = df1))
TukeyHSD(aov1, "treat_num", ordered = "T")

# test assumptions for linear regression

herb01 <- df1 %>% filter(df1$treat_num == "1")
herb02 <- df1 %>% filter(df1$treat_num == "2")
herb03 <- df1 %>% filter(df1$treat_num == "3")
herb04 <- df1 %>% filter(df1$treat_num == "4")
herb05 <- df1 %>% filter(df1$treat_num == "5")
herb06 <- df1 %>% filter(df1$treat_num == "6")
herb07 <- df1 %>% filter(df1$treat_num == "7")
herb08 <- df1 %>% filter(df1$treat_num == "8")
herb09 <- df1 %>% filter(df1$treat_num == "9")
herb10 <- df1 %>% filter(df1$treat_num == "10")
herb11 <- df1 %>% filter(df1$treat_num == "11")
herb12 <- df1 %>% filter(df1$treat_num == "12")

shapiro.test(herb01$biomass_reduc)
shapiro.test(herb02$biomass_reduc)
shapiro.test(herb03$biomass_reduc)
shapiro.test(herb04$biomass_reduc)
shapiro.test(herb05$biomass_reduc)
shapiro.test(herb06$biomass_reduc)
shapiro.test(herb07$biomass_reduc)
shapiro.test(herb08$biomass_reduc)
shapiro.test(herb09$biomass_reduc)
shapiro.test(herb10$biomass_reduc)
shapiro.test(herb11$biomass_reduc)
shapiro.test(herb12$biomass_reduc)

leveneTest(biomass_reduc ~ treat_num, data = df1, center = mean)


# RELEVENT STATISTICAL ANALYSIS FOR HERBICIDE-THRIPS TRIALS

df2 <- read.csv('thrips_data.csv', header = T)
df2[,'trial']<-factor(df2[,'trial'])
df2[,'treat_num']<-factor(df2[,'treat_num'])
df2[,'chemistry']<-factor(df2[,'chemistry'])

summary(df2)
head(df2)

fit2<- lm(biomass_reduc ~ chemistry + thrips + trial, data = df2)
summary(fit2)
df2 %>%
  group_by(chemistry) %>%
  summarise(means = mean(biomass_reduc), SE = std.error(biomass_reduc))
df2 %>%
  group_by(thrips) %>%
  summarise(means = mean(biomass_reduc), SE = std.error(biomass_reduc))
df2 %>%
  group_by(trial) %>%
  summarise(means = mean(biomass_reduc), SE = std.error(biomass_reduc))

summary(aov2 <- aov (biomass_reduc ~ treat_num + trial, data = df1))
TukeyHSD(aov1, "treat_num", ordered = "T")

# test assumptions for linear regression

thrp01 <- df2 %>% filter(df2$treat_num == "1")
thrp02 <- df2 %>% filter(df2$treat_num == "2")
thrp03 <- df2 %>% filter(df2$treat_num == "3")
thrp04 <- df2 %>% filter(df2$treat_num == "4")
thrp05 <- df2 %>% filter(df2$treat_num == "5")
thrp06 <- df2 %>% filter(df2$treat_num == "6")
thrp07 <- df2 %>% filter(df2$treat_num == "7")
thrp08 <- df2 %>% filter(df2$treat_num == "8")
thrp09 <- df2 %>% filter(df2$treat_num == "9")
thrp10 <- df2 %>% filter(df2$treat_num == "10")
thrp11 <- df2 %>% filter(df2$treat_num == "11")
thrp12 <- df2 %>% filter(df2$treat_num == "12")
thrp13 <- df2 %>% filter(df2$treat_num == "13")
thrp14 <- df2 %>% filter(df2$treat_num == "14")

shapiro.test(thrp01$biomass_reduc)
shapiro.test(thrp02$biomass_reduc)
shapiro.test(thrp03$biomass_reduc)
shapiro.test(thrp04$biomass_reduc)
shapiro.test(thrp05$biomass_reduc)
shapiro.test(thrp06$biomass_reduc)
shapiro.test(thrp07$biomass_reduc)
shapiro.test(thrp08$biomass_reduc)
shapiro.test(thrp09$biomass_reduc)
shapiro.test(thrp10$biomass_reduc)
shapiro.test(thrp11$biomass_reduc)
shapiro.test(thrp12$biomass_reduc)
shapiro.test(thrp13$biomass_reduc)
shapiro.test(thrp14$biomass_reduc)

leveneTest(biomass_reduc ~ treat_num, data = df2, center = mean)
