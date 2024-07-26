library(dplyr)
library(ggplot2)
library(forcats)
# library(DIMPLE)
# line for installing MItools; only need to run once
# devtools::install_github("junsoukchoi/MItools", force = TRUE)
library(spatstat.explore)
library(spatstat.geom)
library(MItools)
library(survival)
library(glmnet)
library(tidyr)

ovarianData <- readRDS(
  "/Users/derekyao/Documents/BDSI/Cancer Data Science/Data/ovarianData.RDS"
)
lungData <- readRDS(
  "/Users/derekyao/Documents/BDSI/Cancer Data Science/Data/lungData.RDS"
)

lungDatacombined <- lungData$lung_cells %>% left_join(lungData$lung_metadata, 
                                                      by = 'image_id'

lungKdata <- read.csv("/Users/derekyao/Documents/BDSI/Cancer Data Science/Data/lungKdata.csv")

lungKdata <- lungKdata %>% group_by(patient_id) %>% mutate(
  imageMax = ifelse(totalCell == max(totalCell) , 1, 0)
)

modData <- lungKdata %>%
  group_by(patient_id) %>%
  slice(which.max(totalCell)) %>%
  ungroup()

# Kaplan-Meier Curves
kmObj <- survfit(
  Surv(time = survival_days, event = survival_status) ~ 1, data = lungMeta
)
plot(kmObj)
summary(kmObj)$surv

# Cox Proportional Hazards Model
names(modData)

#define x values
lung_k <- modData %>%
  select(k_CK,   
         k_CD8,    
         k_CD19,     
         k_CD14,
         k_CD4,
         k_CK_CD8,
         k_CK_CD19,
         k_CK_CD14,
         k_CK_CD4,
         k_CD19_CD8,
         k_CD19_CD14,
         k_CD19_CD4,
         k_CD8_CD14,
         k_CD8_CD4,
         k_CD14_CD4) %>%
  filter(complete.cases(.))

#takes it down to a sample size of 146/153


#remove rows from total data set
filtered_modData <- modData[complete.cases(modData %>%
                                             select(k_CK,   
                                                    k_CD8,    
                                                    k_CD19,     
                                                    k_CD14,
                                                    k_CD4,
                                                    k_CK_CD8,
                                                    k_CK_CD19,
                                                    k_CK_CD14,
                                                    k_CK_CD4,
                                                    k_CD19_CD8,
                                                    k_CD19_CD14,
                                                    k_CD19_CD4,
                                                    k_CD8_CD14,
                                                    k_CD8_CD4,
                                                    k_CD14_CD4)), ] 


filtered_modData


#fit
x <- lung_k

x

y <- filtered_modData %>%
  mutate(time=survival_days,
         status=survival_status) %>%
  select(time, status) %>%
  as.matrix()
y


#fit model
fit <- glmnet(x, y, family = "cox")

plot(fit)
coef(fit, s = 0.03)

x_mat<-as.matrix(x)

cvfit <- cv.glmnet(x_mat, y, family = "cox", type.measure = "C")

plot(cvfit)

coef(cvfit, s = -2.5)

# fitting lasso using sample CDS code
modData <- lungKdata %>%
  group_by(patient_id) %>%
  slice(which.max(totalCell)) %>%
  ungroup()

modData1 <- modData %>%
  mutate(mchII= case_when(mhcII_status=="low" ~ 0,
                          mhcII_status=="high" ~ 1),
         gender_1= case_when(gender=="F" ~ 0,
                             gender=="M" ~ 1),
         age_at_diagnosis = as.numeric(age_at_diagnosis))





