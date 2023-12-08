heart_failure_clinical_records_dataset <- read.csv("~/Desktop/DANA-Qualt/Final project/heart_failure_clinical_records_dataset.csv")
attach(heart_failure_clinical_records_dataset)
head(heart_failure_clinical_records_dataset)
heart <- heart_failure_clinical_records_dataset
model_null <- glm(DEATH_EVENT ~ 1, family=binomial, data=heart)
summary(model_null)

#step1-testing individual aganist null model

model_age <- glm(DEATH_EVENT ~ age, family=binomial, data=heart)
anova(model_null,model_age,test='LRT')
# age is significant

model_anaemia <- glm(DEATH_EVENT ~ anaemia, family=binomial, data=heart)
anova(model_null,model_anaemia,test='LRT')#anemia is not significant

model_creatinine_phosphokinase <- glm(DEATH_EVENT ~ creatinine_phosphokinase, family=binomial, data=heart)
anova(model_null,model_creatinine_phosphokinase,test='LRT') #creatinine_phosphokinase is not significant

model_diabetes <- glm(DEATH_EVENT ~ diabetes, family=binomial, data=heart)
anova(model_null,model_diabetes,test='LRT') #diabetes is not significant

model_ejection_fraction <- glm(DEATH_EVENT ~ ejection_fraction, family=binomial, data=heart)
anova(model_null,model_ejection_fraction,test='LRT') #ejection_fraction is  significant

model_high_blood_pressure <- glm(DEATH_EVENT ~ high_blood_pressure, family=binomial, data=heart)
anova(model_null,model_high_blood_pressure,test='LRT') #high_blood_pressure is  significant

model_platelets <- glm(DEATH_EVENT ~ platelets, family=binomial, data=heart)
anova(model_null,model_platelets,test='LRT') #platelets is not significant

model_serum_creatinine <- glm(DEATH_EVENT ~ serum_creatinine, family=binomial, data=heart)
anova(model_null,model_serum_creatinine,test='LRT') #serum_creatinine is  significant

model_serum_sodium <- glm(DEATH_EVENT ~ serum_sodium, family=binomial, data=heart)
anova(model_null,model_serum_sodium,test='LRT') #serum_sodium is  significant

model_sex <- glm(DEATH_EVENT ~ sex, family=binomial, data=heart)
anova(model_null,model_sex,test='LRT') #sex is not significant

model_smoking <- glm(DEATH_EVENT ~ smoking, family=binomial, data=heart)
anova(model_null,model_smoking,test='LRT') #smoking is not significant

model_time <- glm(DEATH_EVENT ~ time, family=binomial, data=heart)
anova(model_null,model_time,test='LRT') #time is  significant

#final model for now
models_df <- data.frame(
  Model = c("Age", "Anaemia", "CPK", "Diabetes", 
            "EjectionFraction", "HighBloodPressure", "Platelets", 
            "SerumCreatinine", "SerumSodium", "Sex", "Smoking", "Time")
)

anova_results <- list(
  age = anova(model_null, model_age, test = 'LRT'),
  anaemia = anova(model_null, model_anaemia, test = 'LRT'),
  creatinine_phosphokinase = anova(model_null, model_creatinine_phosphokinase, test = 'LRT'),
  diabetes = anova(model_null, model_diabetes, test = 'LRT'),
  ejection_fraction = anova(model_null, model_ejection_fraction, test = 'LRT'),
  high_blood_pressure = anova(model_null, model_high_blood_pressure, test = 'LRT'),
  platelets = anova(model_null, model_platelets, test = 'LRT'),
  serum_creatinine = anova(model_null, model_serum_creatinine, test = 'LRT'),
  serum_sodium = anova(model_null, model_serum_sodium, test = 'LRT'),
  sex = anova(model_null, model_sex, test = 'LRT'),
  smoking = anova(model_null, model_smoking, test = 'LRT'),
  time = anova(model_null, model_time, test = 'LRT')
)

anova_results

result_df <- data.frame(
  PValue = sapply(anova_results, function(result) result$"Pr(>Chi)"[2]),
  Significance = ifelse(sapply(anova_results, function(result) result$"Pr(>Chi)"[2] > 0.2), "Not Significant", "Significant")
)

# Display the results
print(result_df)


#new model with the significant variables from step1
model_initial <- glm(DEATH_EVENT ~ age+ejection_fraction+high_blood_pressure+serum_creatinine+serum_sodium+time, family = binomial, data=heart)
summary(model_initial)

#step2
#drop time
model_dropTime <- glm(DEATH_EVENT ~ age+ejection_fraction+high_blood_pressure+serum_creatinine+serum_sodium, family = binomial, data=heart)
anova_dropTime = anova(model_dropTime,model_initial,test='LRT') 
anova_dropTime
#### time is significant 

#drop serum_sodium
model_dropSerum_sodium <- glm(DEATH_EVENT ~ age+ejection_fraction+high_blood_pressure+serum_creatinine+time, family = binomial, data=heart)
anova_dropSerum_sodium = anova(model_dropSerum_sodium,model_initial,test='LRT') 
anova_dropSerum_sodium
### Serum_sodium not significant

#drop serum_creatinine
model_dropSerum_creatinine <- glm(DEATH_EVENT ~ age+ejection_fraction+high_blood_pressure+serum_sodium+time, family = binomial, data=heart)
anova_dropSerum_creatinine = anova(model_dropSerum_creatinine,model_initial,test='LRT') 
anova_dropSerum_creatinine
### serum_creatinine is significant

#drop high_blood_pressure
model_drophigh_blood_pressure <- glm(DEATH_EVENT ~ age+ejection_fraction+serum_creatinine+serum_sodium+time, family = binomial, data=heart)
anova_drophigh_blood_pressure = anova(model_drophigh_blood_pressure,model_initial,test='LRT') 
anova_drophigh_blood_pressure
### high_blood_pressure is not significant

#drop ejection_fraction
model_dropejection_fraction <- glm(DEATH_EVENT ~ age+high_blood_pressure+serum_creatinine+serum_sodium+time, family = binomial, data=heart)
anova_dropejection_fraction = anova(model_dropejection_fraction,model_initial,test='LRT') 
anova_dropejection_fraction
### ejection_fraction is  significant

#drop age
model_dropage <- glm(DEATH_EVENT ~ ejection_fraction+high_blood_pressure+serum_creatinine+serum_sodium+time, family = binomial, data=heart)
anova_dropage = anova(model_dropage,model_initial,test='LRT') 
anova_dropage
### age is  significant


# Extract p-values and add Significance column
p_values_anova <- c(
  age = anova_dropage$"Pr(>Chi)"[2],
  ejection_fraction = anova_dropejection_fraction$"Pr(>Chi)"[2],
  high_blood_pressure = anova_drophigh_blood_pressure$"Pr(>Chi)"[2],
  serum_creatinine = anova_dropSerum_creatinine$"Pr(>Chi)"[2],
  serum_sodium = anova_dropSerum_sodium$"Pr(>Chi)"[2],
  time = anova_dropTime$"Pr(>Chi)"[2]
)

# Create a data frame with PValues and Significance
anova_result_df <- data.frame(
  PValue = p_values_anova,
  Significance = ifelse(p_values_anova < 0.05, "Significant", "Not Significant")
)

# Display the results
print(anova_result_df)

#useful model until now
model_step2 <- glm(DEATH_EVENT ~ age+ejection_fraction+serum_creatinine+time, family = binomial, data=heart)
summary(model_step2)

#add anaemia
model_addAnaemia <- glm(DEATH_EVENT ~ age+ejection_fraction+serum_creatinine+time+anaemia, family = binomial, data=heart)
anova_addAnaemia = anova(model_step2,model_addAnaemia,test='LRT') 
anova_addAnaemia
#anaemia is not significant

#add creatinine_phosphokinase
model_addCreatinine_phosphokinase<- glm(DEATH_EVENT ~ age+ejection_fraction+serum_creatinine+time+creatinine_phosphokinase, family = binomial, data=heart)
anova_addCreatinine_phosphokinase = anova(model_step2,model_addCreatinine_phosphokinase,test='LRT') 
anova_addCreatinine_phosphokinase

#creatinine_phosphokinase is not significant

#add diabetes
model_addDiabetes<- glm(DEATH_EVENT ~ age+ejection_fraction+serum_creatinine+time+diabetes, family = binomial, data=heart)
anova_ddDiabetes = anova(model_step2,model_addDiabetes,test='LRT') 
anova_ddDiabetes
#diabetes is not significant

#add platelets
model_addPlatelets<- glm(DEATH_EVENT ~ age+ejection_fraction+serum_creatinine+time+platelets, family = binomial, data=heart)
anova_addPlatelets = anova(model_step2,model_addPlatelets,test='LRT') 
anova_addPlatelets
#platelets is not significant

#add sex
model_addSex<- glm(DEATH_EVENT ~ age+ejection_fraction+serum_creatinine+time+sex, family = binomial, data=heart)
anova_addSex = anova(model_step2,model_addSex,test='LRT') 
anova_addSex
#sex is not significant

#add smoking
model_addsmoking<- glm(DEATH_EVENT ~ age+ejection_fraction+serum_creatinine+time+smoking, family = binomial, data=heart)
anova_addsmoking = anova(model_step2,model_addsmoking,test='LRT') 
anova_addsmoking
#smoking is not significant

p_values_anova_additions <- c(
  anaemia = anova_addAnaemia$"Pr(>Chi)"[2],
  creatinine_phosphokinase = anova_addCreatinine_phosphokinase$"Pr(>Chi)"[2],
  diabetes = anova_ddDiabetes$"Pr(>Chi)"[2],
  platelets = anova_addPlatelets$"Pr(>Chi)"[2],
  sex = anova_addSex$"Pr(>Chi)"[2],
  smoking = anova_addsmoking$"Pr(>Chi)"[2]
)

# Create a data frame with PValues and Significance
anova_result_additions_df <- data.frame(
  PValue = p_values_anova_additions,
  Significance = ifelse(p_values_anova_additions < 0.05, "Significant", "Not Significant")
)

# Display the results
print(anova_result_additions_df)

#step 4

# interaction age & ejection
model_ageAndejection <- glm(DEATH_EVENT ~ age+ejection_fraction+serum_creatinine+time+age:ejection_fraction, family = binomial, data=heart)
anova_ageAndejection = anova(model_step2,model_ageAndejection,test='LRT') 
anova_ageAndejection
#interaction age & ejection not significant

# interaction age & serum_creatinine
model_ageAndserum_creatinine <- glm(DEATH_EVENT ~ age+ejection_fraction+serum_creatinine+time+age:serum_creatinine, family = binomial, data=heart)
anova_ageAndserum_creatinine = anova(model_step2,model_ageAndserum_creatinine,test='LRT') 
anova_ageAndserum_creatinine
#interaction age & serum_creatinine not significant

# interaction age & time
model_ageAndtime <- glm(DEATH_EVENT ~ age+ejection_fraction+serum_creatinine+time+age:time, family = binomial, data=heart)
anova_ageAndtime = anova(model_step2,model_ageAndtime,test='LRT') 
anova_ageAndtime
#interaction age & time not significant

# interaction ejection_fraction & serum_creatinine
model_ejectionserum_creatinine <- glm(DEATH_EVENT ~ age+ejection_fraction+serum_creatinine+time+ejection_fraction:serum_creatinine, family = binomial, data=heart)
anova_ejectionserum_creatinine = anova(model_step2,model_ejectionserum_creatinine,test='LRT') 
anova_ejectionserum_creatinine

# interaction ejection_fraction & time
model_ejectiontime <- glm(DEATH_EVENT ~ age+ejection_fraction+serum_creatinine+time+ejection_fraction:time, family = binomial, data=heart)
anova_ejectiontime = anova(model_step2,model_ejectiontime,test='LRT') 
anova_ejectiontime
#interaction ejection_fraction & time is  significant

# interaction serum_creatinine & time
model_serum_creatininetime <- glm(DEATH_EVENT ~ age+ejection_fraction+serum_creatinine+time+serum_creatinine:time, family = binomial, data=heart)
anova_serum_creatininetime = anova(model_step2,model_serum_creatininetime,test='LRT') 
anova_serum_creatininetime
#interaction serum_creatinine & time is not significant

# ... Your previous code ...

# Extract p-values and add Significance column for interaction terms
p_values_interactions <- c(
  ageAndejection = anova_ageAndejection$"Pr(>Chi)"[2],
  ageAndserum_creatinine = anova_ageAndserum_creatinine$"Pr(>Chi)"[2],
  ageAndtime = anova_ageAndtime$"Pr(>Chi)"[2],
  ejectionserum_creatinine = anova_ejectionserum_creatinine$"Pr(>Chi)"[2],
  ejectiontime = anova_ejectiontime$"Pr(>Chi)"[2],
  serum_creatininetime = anova_serum_creatininetime$"Pr(>Chi)"[2]
)

# Create a data frame with PValues and Significance
anova_result_interactions_df <- data.frame(
  PValue = p_values_interactions,
  Significance = ifelse(p_values_interactions < 0.05, "Significant", "Not Significant")
)

# Display the results
print(anova_result_interactions_df)

model1 <- glm(DEATH_EVENT ~ age+ejection_fraction+serum_creatinine+time+ejection_fraction:time, family = binomial, data=heart)
summary(model1)


##bestglm with BIC
heart2=data.frame(age,anaemia,creatinine_phosphokinase,diabetes,ejection_fraction,high_blood_pressure,platelets,serum_creatinine,serum_sodium,sex,smoking,time,DEATH_EVENT) 
library(bestglm)
library(leaps)

bestglm(heart2, family=binomial, IC="BIC")

model2 <- glm(DEATH_EVENT ~ age+ejection_fraction+serum_creatinine+time, family = binomial, data=heart)
summary(model2)


library(pROC)
rocplot_1 <- roc(DEATH_EVENT ~ fitted(model1), data=heart)
rocplot_2 <- roc(DEATH_EVENT ~ fitted(model2), data=heart)


plot.roc(rocplot_1, legacy.axes=TRUE, col ="red")
plot.roc(rocplot_2, legacy.axes=TRUE, add = TRUE, col="blue")
legend("topleft",c(deparse(model1$formula),
                   deparse(model2$formula),
                  deparse(model1$formula)),
       lty=c(1, 1), col=c("red", "blue","green"), cex=0.4)

sprintf("Area under ROC curve from model1 = %f",auc(rocplot_1))
sprintf("Area under ROC curve from model2 = %f",auc(rocplot_2))


mod1 <-glm(DEATH_EVENT ~ ., family = binomial, data=heart)
summary(mod1) 

library(MASS)
stepAIC(mod1)
rocplot_3 <- roc(DEATH_EVENT ~ fitted(mod1), data=heart)
plot.roc(rocplot_3, legacy.axes=TRUE, add = TRUE, col="green")
legend("topleft", 
       legend = c("Model 1", "Model 2", "Model 3"),
       col = c("red", "blue", "green"), 
       lty = 1)

sprintf("Area under ROC curve from model3 = %f",auc(rocplot_3))
