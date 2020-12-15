library(lme4)
library(lmerTest)
library(tidyverse)

sim_panel <- function(group_name, n_patients, patient_offset, days, fe, re_sd, eps_sd) {
  #' Simulate a patient panel
  #' 
  #' @description Simulate a longitudional panel with fixed and random slopes and intercepts
  #' 
  #' @param group_name name (factor) to identify group
  #' @param n_patients number of patients in group
  #' @param patient_offset first patient id
  #' @param days number of days of follow-up (total would be days+1)
  #' @param fe fixedd effect coefficients - (intercept, slope)
  #' @param re_sd standard deviations of the random effect (intercept, slope)
  #' @param eps_sd residual variance
  ids <- factor(rep(patient_offset:(patient_offset + n_patients - 1), each = days + 1))
  re_int <- rnorm(n_patients, sd = re_sd[1])
  re_eff <- rnorm(n_patients, sd = re_sd[2])
  eps <- rnorm(n_patients * (days + 1), sd = eps_sd)
  t <- rep(seq(0, days), n_patients)
  y <- (fe[1] + re_int[ids]) + (fe[2] + re_eff[ids]) * t + eps
  tibble(group = group_name, id = ids, t = t, y = y)
}

sim_experiment <- function(n_placebo, n_treatment, days, param) {
  #' Simulate an experiemnt with placebo and treatment groups
  bind_rows(
    sim_panel('Treatment', n_treatment, 1, days,
              fe = c(param$base, param$trt$eff), 
              re_sd = c(param$trt$base_sd, param$trt$eff_sd),
              eps_sd = param$eps_sd),
    sim_panel('Placebo', n_placebo, n_treatment + 1, days,
              fe = c(param$base, param$plc$eff), 
              re_sd = c(param$plc$base_sd, param$plc$eff_sd),
              eps_sd = param$eps_sd),
  )
}

sim_analysis <- function(n_placebo = 10, n_treatment = 10, days = 5) {
  #' Simulate an analysis
  #' @return inferred fixed effect per day and p-value (H0 - no effect)
  
  # Fixed effects:
  # intercept for both groups is 0 (standardized)
  # Treatment: 0.05 standard deviations per day 
  #   ~2% per days,  50th -> 93rd percentile in 30 days
  # Placebo: no effect
  
  # Random effects:
  # Baseline: +/- 1SD for both placebo and treatment
  # Treatment: each responder: +/- half of the effect (0.025)
  
  # Residual: +/- .5SD
  param <- list(eps_sd = .5, base = 0,
                trt = list(eff = .05, base_sd = 1, eff_sd = .025),
                plc = list(eff = 0,  base_sd = 1, eff_sd = 0))
  
  df <- sim_experiment(n_placebo, n_treatment, days, param)

  # Fit a linear mixed model
  model <- lmer(y ~ t:group + (t | id), data = df)
  
  # Pull out effect estimate and p-value (from lmerTest)
  # could use broom::tidy here, but this seems to be faster
  summary(model)$coefficients['t:groupTreatment', c('Estimate', 'Pr(>|t|)')] %>% 
    set_names(c('effect', 'p_value'))
}