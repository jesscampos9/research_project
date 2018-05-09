library(tidyverse)
library(rsq)
library(ggplot2)

# load data set
fafa_df <- read_csv("./data/data.csv")

# view structure
str(fafa_df)
glimpse(fafa_df)
head(fafa_df)

# center the cont var & dummy code cat var
fafa_c <- fafa_df %>%
  mutate(., proficiency_c = proficiency - mean(proficiency)) %>%
  mutate(., ifelse(condition == "fafa", 0,
            ifelse(condition == "fafal", 1, NA))) # did i do this correctly? the world may never know 
head(fafa_c) # shows fafa = 0
tail(fafa_c) # shows fafal = 1

# Plot
## plot with condition
span_stress <- fafa_c %>%
  ggplot(., aes(x = proficiency_c, y = propOxytone, color = condition)) + 
  geom_point() +
  geom_smooth(method = lm)

ggsave( "./figs/span_stress6x4.png",
       plot = span_stress,
        width = 6, height = 4, 
       unit = "in")

# Model
## general linear model
mod_null <- glm(propOxytone ~ 1, 
              data = fafa_c,                            
              family = gaussian(link = "identity"))
summary(mod_null) 
summary(mod_null)$coef 

mod_prof <- glm(propOxytone ~ proficiency_c,
              data = fafa_c,                            
              family = gaussian(link = "identity"))
summary(mod_prof)
summary(mod_prof)$coef
rsq(mod_prof) # R2 = 0.1153

mod_cond <- glm(propOxytone ~ condition,
              data = fafa_c,                            
              family = gaussian(link = "identity"))
summary(mod_cond)
summary(mod_cond)$coef
rsq(mod_cond) # R2 =  0.4841

mod_add <- glm(propOxytone ~ proficiency_c + condition,
              data = fafa_c,                            
              family = gaussian(link = "identity"))
summary(mod_add)
summary(mod_add)$coef 
rsq(mod_add) #R2 = 0.5994 

mod_int <- glm(propOxytone ~ proficiency_c + condition + proficiency_c:condition,
              data = fafa_c,                            
              family = gaussian(link = "identity"))
summary(mod_int)
sum_best_mod_int <- summary(mod_int)$coef


# doing this manually bc R hates me
null_dev <- 2.84096 
resid_dev <- 0.36247
R2_mod_int <- 1 - (resid_dev / null_dev) # R^2 = 0.8724128
rsq(mod_int) # R^2 = 0.8724 #found the code, package = rsq
# best model, most variance explained 
confint(mod_int) #fafal: LB = -0.2187, UB = -0.18349
                 #fafa:  LB = 0.6452,  UB = 0.6701

# nested model comparisons
anova(mod_null, mod_prof, mod_cond, mod_add, mod_int, test ="Chisq")
# all but 3 significant.
# 3 isn't significant bc its not being nested
# do mini anova() (below)

anova(mod_null, mod_cond, test ="Chisq") # chi^2 = 1.3752 p = 2.2e-16
anova(mod_null, mod_prof, test ="Chisq") # chi^2 = 0.3277 p = 2.915e-05
anova(mod_add, mod_int,   test ="Chisq") # chi^2 = 0.7756 p = 2.2e-16
# these nnumbers are small prob bc propOxytone uses log units? idk

# main effect of prof
# main effect of condition
# interaction of prof x cond

# Model Diagnostics
## 1. check for linearity 
span_stress # linear

## 2. mean of residuals = 0
mean(mod_int$residuals) # basically 0

## 3. homoskedasticity
plot(fitted(mod_int), residuals(mod_int)) # clustering on the leftside
                                          # does this always happen w/ interactions?
## 4. no autocorrelation of residuals
acf(mod_int$residuals)  # visual inspection: so far so good except for 0 but das ok bc interaction
lmtest::dwtest(mod_int) # formal test: durbin-watson test
                        # check p-val less than 0.05 = significant
                        # DW = 2.0536, p-val = 0.5213?? ?das not ok? why is this happening?
                        # DW test val of 2 = no autocorrelation 

## 5. normality of residuals
hist(residuals(mod_int)) # slightly skewed to the right but still normal
qqnorm(residuals(mod_int))
qqline(residuals(mod_int))


