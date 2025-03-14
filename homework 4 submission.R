## Homework 4 Submission ## 


library(MASS)
library(performance)
library(marginaleffects)
library(modelr)
library(ggplot2)
library(interactions)
library(pscl)



#For the questions below, you may use any tools we have discussed in class to examine 
#or visualize model results (e.g. summary(), coef(), predict(), plot_model, link functions, etc).


#Question 1:
#The authors of this study assessed the impacts of mistletoe on forest regeneration, 
#measuring the abundance of seedlings below trees that were either parasitized by 
#mistletoe or not parasitized. 
#They hypothesized that mistletoe changes light availability and soil fertility beneath 
#the canopies of parasitized trees, facilitating recruitment of seedlings below these weakened trees 
#and shifting the community composition in these forests.


mistletoe <- read.csv("mistletoes.csv")
head(mistletoe)
View(mistletoe)

#The full paper includes many variables, but we will focus just on the following variables:
#TREATMENT = A categorical variable describing whether a tree is parasitized by mistletoe or not.
#SEEDLINGS = A count of the number of seedlings in 5m x 5m plots beneath parasitized 
#and unparasitized trees, during the authors’ 2011 and 2012 surveys.
#YEAR = Whether the survey was taken in 2011 or 2012.


#Question 1: Mistletoe impacts on seedlings:
#1a) (5 pts) Fit a glm assessing evidence for the following hypothesis: 
#Seedling density is increased beneath trees experiencing mistletoe infection. 
#Describe the rationale you used in selecting a glm structure and probability distribution. 
#Calculate model fit using MAE.

hypo_glm <- glm(formula = Seedlings ~ Treatment, family = "poisson", data = mistletoe)
summary(hypo_glm)

#Plot the model:
interact_plot(hypo_glm, pred = Treatment, modx = Treatment, plot.points = TRUE)

#Calculate MAE for the model fit:
#Let's get the predicted values of Seedlings from the negative binomial model:
predictions_hypo_glm <- predict(hypo_glm, type = "response")
#print(predictions_hypo_glm)
#And now calculate the MAE:
mae_hypo_glm <- mean(abs(mistletoe$Seedlings - predictions_hypo_glm))
print(mae_hypo_glm)
mean(mistletoe$Seedlings)
var(mistletoe$Seedlings)

#The MAE (145.841) is very close to the mean (160.6591) that might suggest the model fit is not good.
#However since the variance is really > mean, I can conclude that the mistletoe data 
#is highly overdispersed.


#Now let's check for overdispersion:
library(AER)
dispersion_test <- dispersiontest(hypo_glm)
dispersion_test
#Because p-value = 0.0005626 is <0.05, overdispersion is detectable and it would be better
#to use negative binomial model.


#How about fitting a Negative Binomial GLM:
hypo_fit_nb <- glm.nb(Seedlings ~ Treatment , data = mistletoe)
summary(hypo_fit_nb)

#Now compare the Poisson and Negative Binomial models:
AIC(hypo_glm, hypo_fit_nb)

#The lower AIC for the negative binomial model suggest that it's a better fit than the glm.

#Now let's check for overdispersion:
overdispersion_ratio_nb <- sum(residuals(hypo_fit_nb, type = "pearson")^2) / hypo_fit_nb$df.residual
print(overdispersion_ratio_nb)

#Now that the overdispersion_ratio = 1.078919 & is close to 1, a negative binomial model is be better.

#Plot the model:
interact_plot(hypo_fit_nb, pred = Treatment, modx = Treatment, plot.points = TRUE)
#I did not find any differences between the plots!

#Let's get the predicted values of Seedlings from the negative binomial model:
predictions_hypo_fit_nb <- predict(hypo_fit_nb, type = "response")

#And now calculate the MAE:
mae_hypo_fit_nb <- mean(abs(mistletoe$Seedlings - predictions_hypo_glm))
print(mae_hypo_fit_nb)
mean(mistletoe$Seedlings)
var(mistletoe$Seedlings)

#The MAE (145.841) is very close to the mean (160.6591) that might suggest the model fit is not good.
#However since the variance is really > mean, I can conclude that the mistletoe data 
#is highly overdispersed, that being said, negative binomial model is still a good fit.
#But adding more data to that (such as soil moisture) might improve the model.


#Now I plot the residuals to check if the model is under or over predicting:
plot(resid(hypo_fit_nb, type = "pearson"), main="Residual Plot")
abline(h=0, col="red", lwd=2)


#Use visual (e.g. a marginal effects plot):
#install.packages("marginaleffects")  
#install.packages("ggeffects")

library(marginaleffects)
library(ggplot2)
library(ggeffects)


#Compute Marginal Effects:
mep <- marginaleffects(hypo_fit_nb)
#This function is not available for me
#Let's install it for GitHub:
#install.packages("devtools")
#devtools::install_github("vincentarelbundock/marginaleffects")
library(marginaleffects)
#install.packages("margins")  
library(margins) 
mep <- margins(hypo_fit_nb)
plot(mep)
summary(mep)
marginal_data <- ggpredict(hypo_fit_nb, terms = "Treatment")
ggplot(marginal_data, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0.2, color = "brown") +
  labs(title = "Marginal Effect of Mistletoe Infection on Seedling Numbers",
       x = "Treatment",
       y = "Seedling") +
  theme_minimal()

#The wide error bar for the parasitized trees indicates big uncertainty in the prediction
#which could be due to the data variablity. However, error bars for the unparasitized
#trees might be due to less diverse data,


#During the course of this study, 2012 was an atypically rainy year, compared to 2011. 
#Fit an additional glm that quantifies how the effect of mistletoe differs between the two years in this study. 
#Write ~2 new sentences that summarize the results of the new model and their biological implications.

#Now let's include the Year:
fit_nb <- glm.nb(Seedlings ~ Treatment + Year, data = mistletoe)
summary(fit_nb)


##If I want to compare both models (without including the Year and with including the Year)
#based on their standard error values, for the first model fit it is 0.0330 and for 
#the second standard model fit it is 0.0299. 
#the higher value suggests that the estimate in the first model is a bit more uncertain or has bigger variability 
#compared to the second model with lower value for the standard error.

##Comparing the two models based on the magnitude of coefficient values, -3.1575 (model-1)
#compared to -3.3644  (model-2) is slightly less negative which can suggest stronger
#effect of Treatment in model-2 when Year data is included.



AIC(hypo_fit_nb, fit_nb)
##Based on the AIC values, 2079.3 (model-2) and AIC: 2094 (model-1), the lower AIC value for model-2, however
#it's slightly lower, can suggest that model-2 with adding an extra layer (Year) is a better fit.


##Plot the model:
interact_plot(fit_nb, pred = Year, modx = Treatment, plot.points = TRUE)
#Based on the plot it's obvious that more precipitation (rain) had no effect on 
#seedling numbers under unparasitized trees while increase the seedling numbers
#under the parasitized trees. Maybe the higher moisture increased the infection rate and
#therefore, the seedling counts went up too. 




#Question 2:
#Questions 2 uses the “treemortality” dataset in this repository. 
#Forest thinning is a forest management approach that aims to remove woody fuels from forests before wildfires to reduce subsequent severity of these disturbances. 
#This data set examines the effects of a forest thinning treatment on tree mortality in a subsequent wildfire in forests in the Sierra Nevada mountains.
#Fit a glm (using a probability distribution of your choice) that reflects the following research question (including thinning as your only predictor and mortality as your response): 
#Do forest thinning treatments reduce the probability of tree mortality?

#First, bring the data in:
treemortality <- read.csv("treemortality.csv")
head(treemortality)
View(treemortality)

#2a: fit a glm:
thinning_mortality <- glm(mortality ~ thinning, data = treemortality, family = binomial())
summary(thinning_mortality)

#The negative coefficient for thinning ( -1.8559) means that thinning reduces the mortality.
#The p-value (<2e-16) also shows that thinning has a significant effect on tree mortality.
#Also this p-value enable us to reject the null hypothesis (there is no relationship between thinning and tree mortality).

#To better understand the effect size, we can also calculate the odds ratio:
exp(coef(thinning_mortality))

#Now let's convert it to Probabilities:
probability_thinning_mortality <- data.frame(thinning = c(0, 1))
print(probability_thinning_mortality)

#Predict probability using inverse logit transformation:
probability_thinning_mortality$predicted_prob <- predict(thinning_mortality, newdata = probability_thinning_mortality, type = "response")
print(probability_thinning_mortality)

#0.7297297 or 73% mortality probability for unthinned trees.
#0.2967960 or 30% mortality probability for thinned trees.
#I conclude that the thinning highly impacts the tree mortality.


#2b: The researchers explicitly considered the potential for confounding relationships related to tree size in their design and randomized their post-fire sampling by tree size. Given this information, do the researchers need to incorporate tree size into their glm to accurately estimate the effect of thinning? Why or why not?

#Let's include tree size in the glm model fit:
treesize_mode <- glm(mortality ~ thinning + treesize, data = treemortality, family = binomial())
summary(treesize_mode)

#Coefficients for treesize is -0.04423, compare it to -1.89930 for thinning, it shows that
#tree size does not have as significant effect as the thinning on tree mortality.
#However after adding the tree size, thinning coef changes from - 1.8559to -1.89930 
#which indicates that by including tree size the effect of thinning on tree mortality becomes a bit stronger.



#2c:Refit the model from 2a to include the necessary variables to minimize bias in the estimation of the “thinning” variable, based on the reviewer’s proposed DAG. 
#Update the model fit with adding slope and road-distance:
# Fit logistic regression model with thinning, slope, and roaddist as predictors
model_slope_roaddist <- glm(mortality ~ thinning + slope + roaddist, 
                            data = treemortality, 
                            family = binomial())

summary(model_slope_roaddist)

#After including values for slope and distance fro the road, the coef for thinning came down to -0.91627
#while this value for slope is 0.82480 and for distance is 0.54468. All suggest that
#slope has more significant impact on tree mortality compared to distance from roads. 
#Also by adding the slope the impact of thinning on tree mortality becomes less biased.

#AIC:
AIC(thinning_mortality,model_slope_roaddist)
print(AIC)
#Lower values for AIC suggest better fit model.
#p-values for thinning, slope, and distance from road are significantly < 0.05
#All in all, I conclude that slope and distance impact on increasing tree mortality.

exp(coef(model_slope_roaddist))
#Now with the value of 4.000076e-01 for thinning, it shows that 40% of thinned trees
#are prone to die while 60% of thinned trees are probably safe.
#With the value of 2.281429e+00 for slope, it can be concluded that trees on steeper slopes
#are 2.28 times more prone to die. 
#With the value of 1.724060e+00 for distance from roads, it can be concluded that for
#every 1K increase in distance from roads, tree mortality increases by 1.72 times.

#Let's create a new DAG model:
library(dagitty)
dag_model <- dagitty("dag {
  Slope -> thinning -> mortality
  roaddist -> thinning
  slope -> mortality
  roaddist -> mortality
}")

plot(dag_model)


#include any biological/ecological hypotheses here that require additional or deeper knowledge about forest fires, thinning treatments, etc – assume that the DAG proposed is complete. Though feel free to come up with a biological explanation too, if you’d like
#I would also include soil temperature data and the plant community composition of the understory too.
#Including soil temperature data would also come useful in estimating the likelihood of fire frequency and intensity.
#If the understory consists of dry vegetation cover, the forest will be more prone to fire.
#Moreover, having orientation data might be also helpful, N-facing slope are less likely to
#have dry soil and understory, that can be less fuel for fire.

dag_model <- dagitty("dag {
  slope -> thinning -> mortality
  roaddist -> thinning
  slope -> mortality
  understory -> mortality
  soilmoisture -> uderstorycomposition -> mortality
  slope -> soilmoisture -> uderstorycomposition 
  roaddist -> mortality
}")
plot(dag_model)