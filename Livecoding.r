##
pacman::p_load(
  tidyverse,
  here,
  rethinking,
  brms
)

## Load the data
d <- read_csv("AnalysisData.csv") %>%
  mutate(
    CB = (CollectiveBenefit_v2 - mean(CollectiveBenefit_v2, na.rm=T))/sd(CollectiveBenefit_v2, na.rm=T),
    SR = (SimilarityRatio_v2 - mean(SimilarityRatio_v2, na.rm=T))/sd(SimilarityRatio_v2, na.rm=T)
  )


# 1. Define your model
## Outcome distribution
ggplot(d, aes(CB)) +
  theme_classic() +
  geom_density()

CB_f0 <- bf(CB ~ 1)
## Distribution of the outcome conditioned on the predictors
CB_f1 <- bf(CB ~ 1 + SR)

get_prior(
  CB_f0,
  d,
  family=gaussian
)

CB_p0 <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(1, 0.5), class = sigma)
)

dens(rnorm(1e4, 0, 1))
dens(rnorm(1e4, 1, 0.5))

dens(rnorm(1e4, 0 + rnorm(1e4, 1, 0.5), 1))

get_prior(
  CB_f1,
  d,
  family=gaussian
)

CB_p1 <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(1, 0.5), class = sigma),
  prior(normal(0, 0.2), class = b, coef = SR )
)

## Define and visualize your priors
### Outcome alone



### Effects of the predictors

# 2. Visualize the joint expectations of the model (prior + likelihood function)
## Predictive priors (ppcheck, but also conditional effects)
## Fiddle til you’re satisfied, but be transparent!

CB_m0_prior <- brm(
  formula = CB_f0,
  data = d,
  family = gaussian,
  prior = CB_p0,
  sample_prior="only"##,
  #backend="cmdstanr",
  #threads = threading(2)
)

pp_check(CB_m0_prior, nsamples=100)

CB_m1_prior <- brm(
  formula = CB_f1,
  data = d,
  family = gaussian,
  prior = CB_p1,
  sample_prior="only",
  file = "CB_m1_prior"
  #backend="cmdstanr",
  #threads = threading(2)
)

p3 <- pp_check(CB_m1_prior, nsamples=100)
p3

# 3. Fit the model
##Warnings

CB_m0 <- brm(
  formula = CB_f0,
  data = d,
  family = gaussian,
  prior = CB_p0,
  sample_prior=T,
  backend="cmdstanr",
  threads = threading(2)
)

CB_m1_v2 <- brm(
  formula = CB_f1,
  data = d,
  family = gaussian,
  prior = CB_p1,
  sample_prior=T##,
  #backend="cmdstanr",
  #threads = threading(2)
)
p4<- pp_check(CB_m1, nsamples=100)
p4<- pp_check(CB_m1_v2, nsamples=100)
p3 + p4
# 4. Assess model quality
## Predictive posterior

library(patchwork)
p1<-pp_check(CB_m0_prior, nsamples=100)
p2<-pp_check(CB_m0, nsamples=100)
p1 + p2

## Posterior learning (against prior)

posterior <- posterior_samples(CB_m0)

ggplot(posterior) +
  theme_classic() +
  geom_density(aes(prior_Intercept), fill="red", alpha=0.3) +
  geom_density(aes(b_Intercept), fill="blue", alpha=0.5)

ggplot(posterior) +
  theme_classic() +
  geom_density(aes(prior_sigma), fill="red", alpha=0.3) +
  geom_density(aes(sigma), fill="blue", alpha=0.5) 

### 
posterior <- posterior_samples(CB_m1_v2)

ggplot(posterior) +
  theme_classic() +
  geom_density(aes(prior_Intercept), fill="red", alpha=0.3) +
  geom_density(aes(b_Intercept), fill="blue", alpha=0.5)

ggplot(posterior) +
  theme_classic() +
  geom_density(aes(prior_sigma), fill="red", alpha=0.3) +
  geom_density(aes(sigma), fill="blue", alpha=0.5) 

ggplot(posterior) +
  theme_classic() +
  geom_density(aes(prior_b_SR), fill="red", alpha=0.3) +
  geom_density(aes(b_SR), fill="blue", alpha=0.5)

## Plotting model 
conditional_effects(CB_m1_v2)
plot(conditional_effects(CB_m1_v2), points=T)

plot(conditional_effects(CB_m1_v2, spaghetti=T, nsamples=100, method = "fitted"), points=T)

plot(conditional_effects(CB_m1_v2, spaghetti=T, nsamples=100, method = "predict"), points=T)

# Hypothesis
hypothesis(CB_m1_v2, "SR > 0")
hypothesis(CB_m1, "SR > 0")


### How do we test for robustness to priors?
prior0 <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(1, 0.5), class = sigma),
  prior(normal(0, 1), class = b, coef = SR )
)
prior1 <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(1, 0.5), class = sigma),
  prior(normal(0, .8), class = b, coef = SR )
)
prior2 <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(1, 0.5), class = sigma),
  prior(normal(0, .6), class = b, coef = SR )
)

prior3 <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(1, 0.5), class = sigma),
  prior(normal(0, .4), class = b, coef = SR )
)

prior4 <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(1, 0.5), class = sigma),
  prior(normal(0, .2), class = b, coef = SR )
)
prior5 <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(1, 0.5), class = sigma),
  prior(normal(0, .05), class = b, coef = SR )
)

CB_m1_p0 <- brm(
  formula = CB_f1,
  data = d,
  family = gaussian,
  prior = prior0,
  sample_prior=T,
  chains=2,
  cores=2,
  backend="cmdstanr",
  threads = threading(2)
)

CB_m1_p1 <- update(CB_m1_p0, prior=prior1)
CB_m1_p2 <- update(CB_m1_p0, prior=prior2)
CB_m1_p3 <- update(CB_m1_p0, prior=prior3)
CB_m1_p4 <- update(CB_m1_p0, prior=prior4)
CB_m1_p5 <- update(CB_m1_p0, prior=prior5)

posterior0 <- posterior_samples(CB_m1_p0, pars=c("prior_b", "b_SR")) %>% mutate(prior=1)
posterior1 <- posterior_samples(CB_m1_p1, pars=c("prior_b", "b_SR")) %>% mutate(prior=0.8)
posterior2 <- posterior_samples(CB_m1_p2, pars=c("prior_b", "b_SR")) %>% mutate(prior=0.6)
posterior3 <- posterior_samples(CB_m1_p3, pars=c("prior_b", "b_SR")) %>% mutate(prior=0.4)
posterior4 <- posterior_samples(CB_m1_p4, pars=c("prior_b", "b_SR")) %>% mutate(prior=0.2)
posterior5 <- posterior_samples(CB_m1_p5, pars=c("prior_b", "b_SR")) %>% mutate(prior=0.05)

posterior <- rbind(
  posterior0,
  posterior1,
  posterior2,
  posterior3,
  posterior4,
  posterior5
)

ggplot(posterior, aes(prior,b_SR)) +
  geom_point(alpha=0.05) +
  geom_smooth(method=lm) + 
  theme_classic()


