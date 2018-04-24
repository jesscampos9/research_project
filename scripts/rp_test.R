library(tidyverse)

# load data set
fafa_df <- read_csv("./data/data.csv")

# view structure
str(fafa_df)
glimpse(fafa_df)
head(fafa_df)

# use mutate to center the continuous variable
# mean = 0
fafa_c <- fafa_df %>%
  mutate(., proficiency_c = proficiency - mean(proficiency))
head(fafa_c)
----
# Plot the data

# plot with condition (fafa + fafal)
span_stress <- fafa_c %>%
  ggplot(., aes(x = proficiency_c, y = propOxytone, color = condition)) + 
  geom_point() +
  geom_smooth(method = lm) # it looks better without geom_smooth

# condition: fafa 
fafa_only <- fafa_c %>%
  filter(., condition == "fafa") %>%
  ggplot(., aes(x = proficiency_c, y = propOxytone, color = condition)) + 
  geom_point() +
  geom_smooth(method = lm)
fafa_only <- fafa_c %>%
  filter(., condition == "fafa") %>%
cor.test(proficiency_c, propOxytone)

# r for fafa is higher than r for fafal

# condition: fafal
fafal_only <- fafa_c %>%
  filter(., condition == "fafal") %>%
  ggplot(., aes(x = proficiency_c, y = propOxytone, color = condition)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(proficiency)

ggsave(span_stress = "./figs/span_stress.png",
       plot = span_stress,
       width = 10, height = 4, 
       unit = "in")

---
# Models
## general linear model
mod_null <- glm(propOxytone ~ 1, 
              data = fafa_c,                            
              family = gaussian(link = "identity"))
summary(mod_null)

mod_prof <- glm(propOxytone ~ proficiency_c,
              data = fafa_c,                            
              family = gaussian(link = "identity"))
summary(mod_prof)

mod_cond <- glm(propOxytone ~ condition,
              data = fafa_c,                            
              family = gaussian(link = "identity"))
summary(mod_cond)

mod_add <- glm(propOxytone ~ proficiency_c + condition,
              data = fafa_c,                            
              family = gaussian(link = "identity"))
summary(mod_add)

mod_int <- glm(propOxytone ~ proficiency_c + condition + proficiency_c:condition,
              data = fafa_c,                            
              family = gaussian(link = "identity"))
summary(mod_int)

anova(mod_null, mod_prof, mod_cond, mod_add, mod_int, test ="Chisq")

# is this only for poisson?
anova(mod_null, mod_cond, test ="Chisq")
anova(mod_null, mod_prof, test ="Chisq")
anova(mod_add, mod_int,   test ="Chisq")

## linear model
lm_mod_int   <-  lm(propOxytone ~ proficiency_c + condition + proficiency_c:condition, data = fafa_c)
lm_mod_add   <-  lm(propOxytone ~ proficiency_c + condition                          , data = fafa_c)
lm_mod_prof  <-  lm(propOxytone ~ proficiency_c                                      , data = fafa_c)
lm_mod_cond  <-  lm(propOxytone ~ condition                                          , data = fafa_c)
lm_mod_null  <-  lm(propOxytone ~ 1                                                  , data = fafa_c)

anova(lm_mod_null, lm_mod_cond, lm_mod_prof, lm_mod_add, lm_mod_int)

anova(lm_mod_int, lm_mod_add)  # test int
anova(lm_mod_add, lm_mod_prof) # test prof
anova(lm_mod_add, lm_mod_cond) # test cond


# main effect of prof
# main effect of condition
# interaction of prof x cond

# which is the best model?
# how can you to tell when theres an interaction? Main effect? Using summary not pictures

----
# Model Diagnostics (use the best model for this obv...... right?)
# 1. check for linearity 
span_stress %>%
  ggplot(., aes(x = proficiency, y = propOxytone, color = condition)) +
  geom_point() +
  geom_smooth(method = lm)
# same as the original

# 2. mean of residuals = 0
mean(modx$residuals)

# 3. homoskedasticity
autoplot(mod1, which = c(1,3)) #check numbers. copied from diagnostics r script

# 4. no autocorrelation of residuals
acf(modx$residuals)  #visual inspection
lmtest::dwtest(modx) #formal test: durbin-watson test
                     #check p-val less than 0.05 = significant

# 5. predictors and residuals are uncorrelated
cor.test(og_dataframe$x, modx$residuals)

# 6. normality of residuals
qqplots(modx)
qqnorm(modx)
qqline(modx)

----------
# "write up"
## general:
general linear model (why? cat & cont vars)
criterion: propOxytone which means ...........
predictors: condition + proficiency (centered)
centered prof pred. + coded cat var (not yet tho)
model assumptions: 
  main effects:
  interaction:
  alpha: 0.05 (i think)

##results:
mod fit:
  main effects:
  interactions:
  interpretations: 
  - directionality
- B weights
- SE
- CI: 95% (i think)
- p-val:
  
