## glmmeg.R: R code demonstrating how to fit a logistic regression model, with a random intercept term, to randomly generated overdispersed binomial data. 

## David I. Warton and Francis K. C. Hui
## School of Mathematics and Statistics
## The University of New South Wales
## Last modified: 27/7/10

## Some brief details: GLMM’s are fitted in the following code using the lme4 package on R, which you will need to have installed from CRAN. This package fits GLMM’s using Laplace quadrature, which usually provides a good approximation, particularly when fitting a model with one or two random effects terms. If you get a warning that convergence has not been achieved, try using the nAGQ argument (e.g. add ", nAGQ=4" to the line where you call the glmer function) to fit the model using a more accurate but more computationally intensive approach known as adaptive quadrature.

## REFERENCES ##

## For a general introduction to GLMM:
## Benjamin M. Bolker, Mollie E. Brooks, Connie J. Clark, Shane W. Geange, John R. Poulsen, M. Henry H. Stevens and Jada-Simone S. White (2009) Generalized linear mixed models: a practical guide for ecology and evolution. Trends in Ecology & Evolution, 24 (3), 127-135.

## For further details on implementing GLMM in R or S-Plus:
## José C. Pinheiro, Douglas M. Bates (2009) Mixed-Effects Models in S and S-PLUS, Second edition.  Springer-Verlag, New York, USA.

## For details on implementing GLMM in SAS:
## Ramon C. Littell, George A. Milliken, Walter W. Stroup, Russell D. Wolfinger, Oliver Schabenberber (2006) SAS for Mixed Models, Second Edition. SAS Institute, Cary, USA.

## NOTE - the below code currently does not run when using R 2.9.1 or a later version, instead returning the error "Number of levels of a grouping factor for the random effects must be less than the number of observations". This error message should not appear, and if it does appear the problem can be avoided by expanding the dataset out into a Bernoulli response (see end of code), or by downloading and installing an older version of the package:
## Matrix Package:- R package version 0.999375-24
## lme4 Package: - R package version 0.999375-31

## Enjoy massaging your data!

#########################################################################################

## GENERATING AN EXAMPLE DATASET FOR ANALYSIS ##

## In order to illustrate the used of GLMM, over-dispersed binomial data are generated here according to a balanced one-way ANOVA design, with 15 “species” at each of four levels of the factor “location”.

species     = 1:60 # We assume that the 60 rows of the dataset correspond to 60 different species.
location    = c(rep("australia",15), rep("canada",15), rep("argentina",15), rep("china",15)) 
sample.size = 12
p           = c(rep(0.3,15), rep(0.4,15), rep(0.5,15), rep(0.6,15))
eta         = log( p/(1-p) ) + rnorm(60)
p           = exp(eta) / ( exp(eta) + 1 )

success     = rbinom(60, size=sample.size, prob=p) 	
failure     = sample.size - success
location    = factor(location)
dataset     = data.frame(location, species, sample.size, success, failure)
rm(location, species, success, failure)


#########################################################################################

## ANALYSIS OF EXAMPLE DATASET ##

## Load data
  
  head(daysMerge[,c('SITESTN','nights_STOC_ANY','duration','Distance','dist_intended')])
  daysMerge$SITESTN <- factor(daysMerge$SITESTN)

## Plot the sample proportions against variable(s)
  plot(nights_STOC_ANY/duration ~ reproState, data = daysMerge)
  plot(nights_STOC_ANY/duration ~ Distance, data = daysMerge)


## Logistic regression (fixed effects)
  glm.fe = glm(cbind(nights_STOC_ANY, duration - nights_STOC_ANY) ~ reproState, family = binomial, data = daysMerge)
  anova(glm.fe, test = 'Chisq')
  
  #Residual deviance >>> residual df. Overdispersed
  
## Re-scale continuous variable
  daysMerge$distScaled <- scale(daysMerge$Distance)
  hist(daysMerge$Distance)  
  hist(daysMerge$distScaled)
  
## GLMM (fixed + random effects)
  glm.null  = glmer(cbind(nights_STOC_ANY, duration - nights_STOC_ANY) ~ 1 + (1|SITESTN),
                    family = binomial, data = daysMerge)
  glm.repro = glmer(cbind(nights_STOC_ANY, duration - nights_STOC_ANY) ~ reproState + (1|SITESTN), 
                    family = binomial, data = daysMerge)
  glm.dist  = glmer(cbind(nights_STOC_ANY, duration - nights_STOC_ANY) ~ Distance + (1|SITESTN),
                    family = binomial, data = daysMerge)
  glm.dist  = glmer(cbind(nights_STOC_ANY, duration - nights_STOC_ANY) ~ distScaled + (1|SITESTN),
                    family = binomial, data = daysMerge)
  glm.repro.dist  = glmer(cbind(nights_STOC_ANY, duration - nights_STOC_ANY) ~ reproState * Distance + (1|SITESTN),
                          family = binomial, data = daysMerge)
  
  anova(glm.null, glm.repro, glm.dist, repro.dist)


fit.glmm           = glmer(cbind(success, failure) ~ location + (1|species), family = binomial, data=dataset)
fit.glmm.intercept = glmer(cbind(success, failure) ~ 1 + (1|species), family = binomial, data=dataset)
anova(fit.glmm.intercept, fit.glmm)
## Note the significant evidence of a location effect.
## If you got the error message "Number of levels of a grouping factor for the random effects must be less than the number of observations", see code at the end.


#########################################################################################

## PRODUCING DIAGNOSTIC PLOTS ##

## Logistic regression
plot(glm.fe$fitted.values, residuals(glm.fe), pch = 19, las = 1, cex = 1.4)
abline(0, 0, lwd = 1.5)
  ## check for no pattern

## GLMM
par(mfrow = c(1,2))
m = model.matrix(glm.repro)
ft.fix = m%*%fixef(glm.repro)
plot(ft.fix, ranef(glm.repro, drop = TRUE)$SITESTN, pch = 19, las = 1, cex = 1.4)
abline(0, 0, lwd = 1.5)

qqnorm(ranef(glm.repro, drop = TRUE)$SITESTN, pch = 19, las = 1, cex = 1.4)


