# Setup ----

library(brms)
library(rstan)
options(mc.cores = parallel::detectCores()) 
rstan_options(auto_write = TRUE)
library(correlation)
library(tidyverse)
library(tidybayes)
library(ggthemes)
library(bayestestR)

load("DoPL_DOSPERT Results.RData")

 # General Correlations----
gen_cor_tab = correlation(experiment_dataset_analysis[, c(2,8:16)],
                          bayesian = T)
gen_cor_tab[c(1:3,5:9)] # get estimates and HDIs (probability of direction & ROPE)

# Regression m1: General risk preference across DOPL motives, gendet and age + interactions ----

# General risk preference across DOPL motives, gender and age
m1 <- brm(generalRiskPreference ~ dominanceSum + prestigeSum + leadershipSum + Gender + Age, data = experiment_dataset_analysis, warmup = 500, iter = 8000,
          prior = c(prior(normal(0, 1), class = "Intercept"),
                    prior(normal(3, 1), class = "b", coef = "dominanceSum"),
                    prior(normal(0, 1), class = "b", coef = "prestigeSum"), 
                    prior(normal(-2, 1), class = "b", coef = "leadershipSum"),
                    prior(normal(-3, 1), class = "b", coef = "Gender1"), 
                    prior(normal(-3, 1), class = "b", coef = "Age")),
          save_pars = save_pars(all = T))
summary(m1)

# m1 + interaction between gender and each of the DoPL motives
m1_int <- brm(generalRiskPreference ~ dominanceSum*Gender+ prestigeSum*Gender + leadershipSum*Gender + Age, data=experiment_dataset_analysis, warmup = 500, iter = 8000,
              prior=c(prior(normal(0,1), class="Intercept"),
                      prior(normal(3,1), class="b", coef="dominanceSum"),
                      prior(normal(0,1), class="b", coef="prestigeSum"),
                      prior(normal(-2,1), class="b", coef="leadershipSum"),
                      prior(normal(-3,1), class="b", coef="Gender1"),
                      prior(normal(-3,1), class="b", coef="Age"),
                      prior(normal(0,1), class="b", coef="dominanceSum:Gender1"),
                      prior(normal(0,1), class="b", coef="Gender1:leadershipSum"),
                      prior(normal(0,1), class="b", coef="Gender1:prestigeSum")),
              save_pars = save_pars(all = T))

summary(m1_int)

# model comparisons
loo(m1, m1_int) # equal
bfs_gen = bayesfactor_models(m1, m1_int, denominator = 2) # equal

# HDI m1 + DOPL:gender interactions
m1_hdi <- hdi(m1, effects = "fixed", component = "conditional", ci = .95)
m1_hdi[sign(m1_hdi$CI_low) == sign(m1_hdi$CI_high),
           c('Parameter', 'CI','CI_low', 'CI_high')]

# general mediation of components of DOSPERT

d1 = experiment_dataset_analysis[complete.cases(experiment_dataset_analysis),]
m1mod = bf(riskPerceptionSum ~ dominanceSum + prestigeSum + leadershipSum)
m2mod = bf(riskBenefitSum ~ dominanceSum + prestigeSum + leadershipSum)
ymod = bf(riskSum ~ riskBenefitSum + riskPerceptionSum + dominanceSum +
                  prestigeSum + leadershipSum + Gender + Age)
med_model = brm(ymod + m1mod + m2mod + set_rescor(FALSE), warmup = 1000,
                iter = 8000, data = d1, save_pars = save_pars(all = TRUE))



m1mod.2 = bf(riskPerceptionSum ~ dominanceSum * Gender + prestigeSum * Gender + 
                     leadershipSum * Gender)
m2mod.2 = bf(riskBenefitSum ~ dominanceSum * Gender + prestigeSum * Gender + 
                     leadershipSum * Gender)
ymod.2 = bf(riskSum ~ riskBenefitSum + riskPerceptionSum + dominanceSum * Gender +
                    prestigeSum * Gender + leadershipSum * Gender + Age)
med_model.2 = brm(ymod.2 + m1mod.2 + m2mod.2 + set_rescor(FALSE), 
                  warmup = 1000, iter = 8000,
                  data = d1, save_pars = save_pars(all = TRUE))


m1mod.3 = bf(riskPerceptionSum ~ dominanceSum + prestigeSum + leadershipSum 
             * Gender)
m2mod.3 = bf(riskBenefitSum ~ dominanceSum + prestigeSum + leadershipSum 
             + Gender)
ymod.3 = bf(riskSum ~ riskBenefitSum + riskPerceptionSum + dominanceSum * Gender
            + Age)
med_model.3 = brm(ymod.3 + m1mod.3 + m2mod.3 + set_rescor(FALSE), 
                  warmup = 1000, iter = 8000,
                  data = d1, save_pars = save_pars(all = TRUE))
                # No mediation for above specifications with riskPerception.

m2mod.4 = bf(riskBenefitSum ~ dominanceSum + prestigeSum + leadershipSum)
ymod.4 = bf(riskSum ~ riskBenefitSum + dominanceSum + prestigeSum +
                    leadershipSum + Gender + Age)
med_model.4 = brm(ymod.4 + m2mod.4 + set_rescor(FALSE), 
                  warmup = 1000, iter = 8000,
                  data = d1, save_pars = save_pars(all = TRUE))

m2mod.5 = bf(riskBenefitSum ~ dominanceSum + prestigeSum + leadershipSum 
             + Gender + Age)
ymod.5 = bf(riskSum ~ riskBenefitSum + dominanceSum + prestigeSum +
                    leadershipSum + Gender + Age)
med_model.5 = brm(ymod.5 + m2mod.5 + set_rescor(FALSE), 
                  warmup = 1000, iter = 8000,
                  data = d1, save_pars = save_pars(all = TRUE))

m2mod.6 = bf(riskBenefitSum ~ dominanceSum * Gender + prestigeSum + leadershipSum 
             + Age)
ymod.6 = bf(riskSum ~ riskBenefitSum + dominanceSum * Gender + prestigeSum +
                    leadershipSum + Age)
med_model.6 = brm(ymod.6 + m2mod.6 + set_rescor(FALSE), 
                  warmup = 1000, iter = 8000,
                  data = d1, save_pars = save_pars(all = TRUE))

m2mod.7 = bf(riskBenefitSum ~ dominanceSum * Gender + prestigeSum * Gender + 
                     leadershipSum * Gender + Age)
ymod.7 = bf(riskSum ~ riskBenefitSum + dominanceSum * Gender + prestigeSum * 
                    Gender + leadershipSum * Gender + Age)
med_model.7 = brm(ymod.7 + m2mod.7 + set_rescor(FALSE), 
                  warmup = 1000, iter = 8000,
                  data = d1, save_pars = save_pars(all = TRUE))
                # No mediation in above models but possible to condition
                # on joint distributions?

# compare possible mediation models
med_loo = loo(med_model, med_model.2, med_model.3, med_model.4) # m3
        # 5 (though too close)
med_comp = bayesfactor_models(med_model, med_model.2, med_model.3, 
                              med_model.4, denominator = 4) # m3

med_loo2 = loo(med_model.4, med_model.5, med_model.6, med_model.7)
med_comp2 = bayesfactor_models(med_model.4, med_model.5, med_model.6,
                               med_model.7, denominator = med_model.4) # m6
bayes_R2(med_model.3) # riskSum = .49, percept = .14, benefit = .16
hdi.3 = hdi(med_model.3, effects = 'fixed', component = 'conditional', ci = .95)
hdi.3[sign(hdi.3$CI_low)==sign(hdi.3$CI_high), 
          c('Parameter','CI','CI_low','CI_high')]
# plot R^2 values for best mediation model
bayes_R2(med_model.3, summary = F) %>%
        data.frame() %>%
        pivot_longer(everything()) %>%
        mutate(name = str_remove(name, 'R2')) %>%
        
        ggplot(aes(x = value, color = name, fill = name)) +
        geom_density(alpha = .5) + 
        scale_color_ptol() +
        scale_fill_ptol() +
        scale_y_continuous(NULL, breaks = NULL) +
        coord_cartesian(xlim = 0:1) +
        labs(title = expression(paste('Our ', italic(R)^{2}, 'distributions')), x = NULL) +
        theme_minimal() +
        theme(legend.title = element_blank())

# extract posterior info for computing mediation for Dominance
# notation, c' = direct effect; c = total effect.
post = posterior_samples(med_model.3)

post = post %>%
        mutate(a1 = b_riskPerceptionSum_dominanceSum,
               a2 = b_riskBenefitSum_dominanceSum,
               b1 = b_riskSum_riskPerceptionSum,
               b2 = b_riskSum_riskBenefitSum,
               c_prime = b_riskSum_dominanceSum) %>%
        
        mutate(a1b1 = a1 * b1,
               a2b2 = a2 * b2) %>%
        
        mutate(c = c_prime + a1b1 + a2b2) %>%
        mutate(total_indirect_effect = a1b1 + a2b2) %>%
        mutate(c_minus_c_prime = c - c_prime)

post %>%
        
        pivot_longer(a1:c_minus_c_prime) %>%
        group_by(name) %>%
        median_qi(value) %>%
        mutate_if(is_double, round, digits = 3)
        # dom -> benefit -> riskSum (marginal indirect effect)

# visualise results from tibble
post %>%
        pivot_longer(c(c, c_prime)) %>%
        
ggplot(aes(x = value, fill = name, color = name)) +
        geom_vline(xintercept = 0, color = 'black') +
        geom_density(alpha = .5) +
        scale_color_ptol(NULL) +
        scale_fill_ptol(NULL) +
        scale_y_continuous(NULL, breaks = NULL) +
        # labs + 
        coord_cartesian(xlim = c(-1.5, 1.5)) +
        theme_minimal()

post %>%
        pivot_longer(c(a1b1, a2b2)) %>%
        
        ggplot(aes(x = value, fill = name, color = name)) +
        geom_vline(xintercept = 0, color = 'black') +
        geom_density(alpha = .5) +
        scale_color_ptol(NULL) +
        scale_fill_ptol(NULL) +
        scale_y_continuous(NULL, breaks = NULL) +
        # labs + 
        coord_cartesian(xlim = c(-1.5, 1.5)) +
        theme_minimal()

# extract posterior info for computing mediation for Prestige
# notation, c' = direct effect; c = total effect.
post = post %>%
        mutate(a1 = b_riskPerceptionSum_prestigeSum,
               a2 = b_riskBenefitSum_prestigeSum,
               b1 = b_riskSum_riskPerceptionSum,
               b2 = b_riskSum_riskBenefitSum,
               c_prime = b_riskSum_prestigeSum) %>%
        
        mutate(a1b1 = a1 * b1,
               a2b2 = a2 * b2) %>%
        
        mutate(c = c_prime + a1b1 + a2b2) %>%
        mutate(total_indirect_effect = a1b1 + a2b2) %>%
        mutate(c_minus_c_prime = c - c_prime)

post %>%
        
        pivot_longer(a1:c_minus_c_prime) %>%
        group_by(name) %>%
        median_qi(value) %>%
        mutate_if(is_double, round, digits = 3)
# No mediation

# extract posterior info for computing mediation
# notation, c' = direct effect; c = total effect.
post = post %>%
        mutate(a1 = b_riskPerceptionSum_dominanceSum,
               a2 = b_riskBenefitSum_leadershipSum,
               b1 = b_riskSum_riskPerceptionSum,
               b2 = b_riskSum_riskBenefitSum,
               c_prime = b_riskSum_leadershipSum) %>%
        
        mutate(a1b1 = a1 * b1,
               a2b2 = a2 * b2) %>%
        
        mutate(c = c_prime + a1b1 + a2b2) %>%
        mutate(total_indirect_effect = a1b1 + a2b2) %>%
        mutate(c_minus_c_prime = c - c_prime)

post %>%
        
        pivot_longer(a1:c_minus_c_prime) %>%
        group_by(name) %>%
        median_qi(value) %>%
        mutate_if(is_double, round, digits = 3)
# No mediation

# plot R^2 values for best mediation model
bayes_R2(med_model.6, summary = F) %>%
        data.frame() %>%
        pivot_longer(everything()) %>%
        mutate(name = str_remove(name, 'R2')) %>%
        
        ggplot(aes(x = value, color = name, fill = name)) +
        geom_density(alpha = .5) + 
        scale_color_ptol() +
        scale_fill_ptol() +
        scale_y_continuous(NULL, breaks = NULL) +
        coord_cartesian(xlim = 0:1) +
        labs(title = expression(paste('Our ', italic(R)^{2}, 'distributions')), x = NULL) +
        theme_minimal() +
        theme(legend.title = element_blank())

# extract posterior info for computing mediation for Dominance
# notation, c' = direct effect; c = total effect.
post2 = posterior_samples(med_model.6)

post2 = post2 %>%
        mutate(a1 = b_riskBenefitSum_dominanceSum,
               b1 = b_riskSum_riskBenefitSum,
               c_prime = b_riskSum_dominanceSum) %>%
        
        mutate(a1b1 = a1 * b1) %>%
        
        mutate(c = c_prime + a1b1) %>%
        mutate(total_indirect_effect = a1b1) %>%
        mutate(c_minus_c_prime = c - c_prime)

post2 %>%
        
        pivot_longer(a1:c_minus_c_prime) %>%
        group_by(name) %>%
        median_qi(value) %>%
        mutate_if(is_double, round, digits = 3)
# dom -> benefit -> riskSum (marginal indirect effect)

# visualise results from tibble
post %>%
        pivot_longer(c(c, c_prime)) %>%
        
        ggplot(aes(x = value, fill = name, color = name)) +
        geom_vline(xintercept = 0, color = 'black') +
        geom_density(alpha = .5) +
        scale_color_ptol(NULL) +
        scale_fill_ptol(NULL) +
        scale_y_continuous(NULL, breaks = NULL) +
        # labs + 
        coord_cartesian(xlim = c(-1.5, 1.5)) +
        theme_minimal()

post %>%
        pivot_longer(c(a1b1, a2b2)) %>%
        
        ggplot(aes(x = value, fill = name, color = name)) +
        geom_vline(xintercept = 0, color = 'black') +
        geom_density(alpha = .5) +
        scale_color_ptol(NULL) +
        scale_fill_ptol(NULL) +
        scale_y_continuous(NULL, breaks = NULL) +
        # labs + 
        coord_cartesian(xlim = c(-1.5, 1.5)) +
        theme_minimal()



# Regression m2-m3: Domain specific risk preference across DOPL motives, gender and age ----

# Additive model (m2)
m2 <- brm(mvbind(ethicalPreference, financialPreference, socialPreference, healthAndSafetyPreference, recreationalPreference) ~ dominanceSum + prestigeSum + leadershipSum + Gender + Age, data = experiment_dataset_analysis, save_all_pars = T, iter = 10000,
          
          prior = c(prior(normal(0, 1), coef = "Age", resp = "ethicalPreference"),
                    prior(normal(0, 1), coef = "Gender1", resp = "ethicalPreference"),
                    prior(normal(2, 1), coef = "dominanceSum", resp = "ethicalPreference"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "ethicalPreference"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "ethicalPreference"),
                    prior(normal(0, 1), class = "Intercept", resp = "ethicalPreference"),
                    prior(normal(0, 1), class = "sigma", resp = "ethicalPreference"),
                    #
                    prior(normal(0, 1), coef = "Age", resp = "financialPreference"),
                    prior(normal(0, 1), coef = "Gender1", resp = "financialPreference"),
                    prior(normal(2, 1), coef = "dominanceSum", resp = "financialPreference"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "financialPreference"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "financialPreference"),
                    prior(normal(0, 1), class = "Intercept", resp = "financialPreference"),
                    prior(normal(0, 1), class = "sigma", resp = "financialPreference"),
                    #
                    prior(normal(0, 1), coef = "Age", resp = "healthAndSafetyPreference"),
                    prior(normal(0, 1), coef = "Gender1", resp = "healthAndSafetyPreference"),
                    prior(normal(2, 1), coef = "dominanceSum", resp = "healthAndSafetyPreference"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "healthAndSafetyPreference"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "healthAndSafetyPreference"),
                    prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyPreference"),
                    prior(normal(0, 1), class = "sigma", resp = "healthAndSafetyPreference"),
                    #
                    prior(normal(0, 1), coef = "Age", resp = "recreationalPreference"),
                    prior(normal(0, 1), coef = "Gender1", resp = "recreationalPreference"),
                    prior(normal(2, 1), coef = "dominanceSum", resp = "recreationalPreference"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "recreationalPreference"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "recreationalPreference"),
                    prior(normal(0, 1), class = "Intercept", resp = "recreationalPreference"),
                    prior(normal(0, 1), class = "sigma", resp = "recreationalPreference"),
                    #
                    prior(normal(0, 1), coef = "Age", resp = "socialPreference"),
                    prior(normal(0, 1), coef = "Gender1", resp = "socialPreference"),
                    prior(normal(2, 1), coef = "dominanceSum", resp = "socialPreference"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "socialPreference"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "socialPreference"),
                    prior(normal(0, 1), class = "Intercept", resp = "socialPreference"), 
                    prior(normal(0, 1), class = "sigma", resp = "socialPreference")))


summary(m2)

# HDI m2
m2_hdi <- hdi(m2, effects = "fixed", component = "conditional", ci = .95)
m2_hdi[sign(m2_hdi$CI_low) == sign(m2_hdi$CI_high),
       c('Parameter', 'CI','CI_low', 'CI_high')]


# m2 + Gender-DOPL interactions
m3 <- brm(mvbind(ethicalPreference, financialPreference, socialPreference, healthAndSafetyPreference, recreationalPreference) ~ dominanceSum*Gender + prestigeSum*Gender + leadershipSum*Gender + Age, data = experiment_dataset_analysis, save_all_pars = T, iter = 10000,
          
          prior(
            prior(normal(0, 1), coef = "Age", resp = "ethicalPreference"),
            prior(normal(0, 1), coef = "Gender1", resp = "ethicalPreference"),
            prior(normal(2, 1), coef = "dominanceSum", resp = "ethicalPreference"),
            prior(normal(0, 1), coef = "leadershipSum", resp = "ethicalPreference"),
            prior(normal(0, 1), coef = "prestigeSum", resp = "ethicalPreference"),
            prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "ethicalPreference"),
            prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "ethicalPreference"),
            prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "ethicalPreference"),
            prior(normal(0, 1), class = "Intercept", resp = "ethicalPreference"), 
            prior(normal(0, 1), class = "sigma", resp = "ethicalPreference"),
            #
            prior(normal(0, 1), coef = "Age", resp = "financialPreference"),
            prior(normal(0, 1), coef = "Gender1", resp = "financialPreference"),
            prior(normal(2, 1), coef = "dominanceSum", resp = "financialPreference"),
            prior(normal(0, 1), coef = "leadershipSum", resp = "financialPreference"),
            prior(normal(0, 1), coef = "prestigeSum", resp = "financialPreference"),
            prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "financialPreference"),
            prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "financialPreference"),
            prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "financialPreference"),
            prior(normal(0, 1), class = "Intercept", resp = "financialPreference"),
            prior(normal(0, 1), class = "sigma", resp = "financialPreference"),
            #
            prior(normal(0, 1), coef = "Age", resp = "healthAndSafetyPreference"),
            prior(normal(0, 1), coef = "Gender1", resp = "healthAndSafetyPreference"),
            prior(normal(2, 1), coef = "dominanceSum", resp = "healthAndSafetyPreference"),
            prior(normal(0, 1), coef = "leadershipSum", resp = "healthAndSafetyPreference"),
            prior(normal(0, 1), coef = "prestigeSum", resp = "healthAndSafetyPreference"),
            prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "healthAndSafetyPreference"),
            prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "healthAndSafetyPreference"),
            prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "healthAndSafetyPreference"),
            prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyPreference"),
            prior(normal(0, 1), class = "sigma", resp = "healthAndSafetyPreference"),
            #
            prior(normal(0, 1), coef = "Age", resp = "recreationalPreference"),
            prior(normal(0, 1), coef = "Gender1", resp = "recreationalPreference"),
            prior(normal(2, 1), coef = "dominanceSum", resp = "recreationalPreference"),
            prior(normal(0, 1), coef = "leadershipSum", resp = "recreationalPreference"),
            prior(normal(0, 1), coef = "prestigeSum", resp = "recreationalPreference"),
            prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "recreationalPreference"),
            prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "recreationalPreference"),
            prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "recreationalPreference"),
            prior(normal(0, 1), class = "Intercept", resp = "recreationalPreference"),
            prior(normal(0, 1), class = "sigma", resp = "recreationalPreference"),
            #
            prior(normal(0, 1), coef = "Age", resp = "socialPreference"),
            prior(normal(0, 1), coef = "Gender1", resp = "socialPreference"),
            prior(normal(2, 1), coef = "dominanceSum", resp = "socialPreference"),
            prior(normal(0, 1), coef = "leadershipSum", resp = "socialPreference"),
            prior(normal(0, 1), coef = "prestigeSum", resp = "socialPreference"),
            prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "socialPreference"),
            prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "socialPreference"),
            prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "socialPreference"),
            prior(normal(0, 1), class = "Intercept", resp = "socialPreference"), 
            prior(normal(0, 1), class = "sigma", resp = "socialPreference")))


summary(m3)

# HDI m3
m3_hdi <- hdi(m3, effects = "fixed", component = "conditional", ci = .95)
m3_hdi[sign(m3_hdi$CI_low) == sign(m3_hdi$CI_high),
       c('Parameter', 'CI','CI_low', 'CI_high')]

# Model Comparison (m2 and m3)
co <- loo(m2,m3)

looic_ <- c(co$loos$m2$looic, co$loos$m3$looic)
looic_se <- c(co$loos$m2$se_looic, co$loos$m3$se_looic)
looic_elpd <- c(co$loos$m2$elpd_loo, co$loos$m3$elpd_loo)
looic_elpd_se <- c(co$loos$m2$se_elpd_loo, co$loos$m3$se_elpd_loo)

loo_table <- data.frame(looic, looic_se, looic_elpd,looic_elpd_se, row.names = c("m2", "m3"))
loo_table

bayes_R2(m2)
bayes_R2(m3)


# Regression m4-m5: Benefit, perception and risk taking across subdomains for DOPL motives, age and sex ----







# Additive model
m4 <-  brm(mvbind(riskSum, riskPerceptionSum, riskBenefitSum) ~ dominanceSum + prestigeSum + leadershipSum + Age + Gender, data= experiment_dataset_analysis, save_all_pars = T, iter = 5000,
           prior = c(prior(normal(0, 1), class = "Intercept", resp = "riskSum"), 
                     prior(normal(0, 1), class = "sigma", resp = "riskSum"), 
                     prior(normal(-3, 1),  coef = "Age", resp = "riskSum"),
                     prior(normal(-3, 1),  coef = "Gender1", resp = "riskSum"),
                     prior(normal(3, 1),  coef = "dominanceSum", resp = "riskSum"),
                     prior(normal(-2, 1),  coef = "leadershipSum", resp = "riskSum"),
                     prior(normal(0, 1),  coef = "prestigeSum", resp = "riskSum"),
                     #
                     prior(normal(0, 1), class = "Intercept", resp = "riskPerceptionSum"),
                     prior(normal(0, 1), class = "sigma", resp = "riskPerceptionSum"), 
                     prior(normal(3, 1),  coef = "Age", resp = "riskPerceptionSum"),
                     prior(normal(3, 1),  coef = "Gender1", resp = "riskPerceptionSum"),
                     prior(normal(-3, 1),  coef = "dominanceSum", resp = "riskPerceptionSum"),
                     prior(normal(2, 1),  coef = "leadershipSum", resp = "riskPerceptionSum"),
                     prior(normal(0, 1),  coef = "prestigeSum", resp = "riskPerceptionSum"),
                     #
                     prior(normal(0, 1), class = "Intercept", resp = "riskBenefitSum"),
                     prior(normal(0, 1), class = "sigma", resp = "riskBenefitSum"),
                     prior(normal(-3, 1),  coef = "Age", resp = "riskBenefitSum"),
                     prior(normal(-3, 1),  coef = "Gender1", resp = "riskBenefitSum"),
                     prior(normal(3, 1),  coef = "dominanceSum", resp = "riskBenefitSum"),
                     prior(normal(-2, 1),  coef = "leadershipSum", resp = "riskBenefitSum"),
                     prior(normal(0, 1),  coef = "prestigeSum", resp = "riskBenefitSum")))
 

summary(m4) 

# HDI m4
m4_hdi <- hdi(m4, effects = "fixed", component = "conditional", ci = .95)
m4_hdi[sign(m4_hdi$CI_low) == sign(m4_hdi$CI_high),
       c('Parameter', 'CI','CI_low', 'CI_high')]

# m4 + DOPL motives:gender interactions
m5 <-  brm(mvbind(riskSum, riskPerceptionSum, riskBenefitSum) ~ dominanceSum*Gender + prestigeSum*Gender + leadershipSum*Gender + Age, data= experiment_dataset_analysis, iter = 6000, save_all_pars = T,
           prior = c(prior(normal(0, 1), class = "Intercept", resp = "riskSum"), 
                     prior(normal(0, 1), class = "sigma", resp = "riskSum"), 
                     prior(normal(-3, 1), coef = "Age", resp = "riskSum"),
                     prior(normal(-3, 1), coef = "Gender1", resp = "riskSum"),
                     prior(normal(3, 1), coef = "dominanceSum", resp = "riskSum"),
                     prior(normal(-2, 1), coef = "leadershipSum", resp = "riskSum"),
                     prior(normal(0, 1), coef = "prestigeSum", resp = "riskSum"),
                     prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "riskSum"),
                     prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "riskSum"),
                     prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "riskSum"),
                     #
                     prior(normal(0, 1), class = "Intercept", resp = "riskPerceptionSum"),
                     prior(normal(0, 1), class = "sigma", resp = "riskPerceptionSum"), 
                     prior(normal(3, 1), coef = "Age", resp = "riskPerceptionSum"),
                     prior(normal(3, 1), coef = "Gender1", resp = "riskPerceptionSum"),
                     prior(normal(-3, 1), coef = "dominanceSum", resp = "riskPerceptionSum"),
                     prior(normal(2, 1), coef = "leadershipSum", resp = "riskPerceptionSum"),
                     prior(normal(0, 1), coef = "prestigeSum", resp = "riskPerceptionSum"),
                     prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "riskPerceptionSum"),
                     prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "riskPerceptionSum"),
                     prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "riskPerceptionSum"),
                     #
                     prior(normal(0, 1), class = "Intercept", resp = "riskBenefitSum"),
                     prior(normal(0, 1), class = "sigma", resp = "riskBenefitSum"),
                     prior(normal(0, 1), resp = "riskBenefitSum"),
                     prior(normal(-3, 1), coef = "Age", resp = "riskBenefitSum"),
                     prior(normal(-3, 1), coef = "Gender1", resp = "riskBenefitSum"),
                     prior(normal(3, 1), coef = "dominanceSum", resp = "riskBenefitSum"),
                     prior(normal(-2, 1), coef = "leadershipSum", resp = "riskBenefitSum"),
                     prior(normal(0, 1), coef = "prestigeSum", resp = "riskBenefitSum"),
                     prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "riskBenefitSum"),
                     prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "riskBenefitSum"),
                     prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "riskBenefitSum")))

summary(m5) 

# HDI m5
m5_hdi <- hdi(m5, effects = "fixed", component = "conditional", ci = .95)
m5_hdi[sign(m5_hdi$CI_low) == sign(m5_hdi$CI_high),
       c('Parameter', 'CI','CI_low', 'CI_high')]

# conditional effects plot
plot(conditional_effects(m5, resp = "riskPerceptionSum"), points = T)

# Model comparison
co2 <- loo(m4,m5)

looic_2 <- c(co2$loos$m4$looic, co2$loos$m5$looic)
looic_2_se <- c(co2$loos$m4$se_looic, co2$loos$m5$se_looic)
looic_elpd_2 <- c(co2$loos$m4$elpd_loo, co2$loos$m5$elpd_loo)
looic_elpd_2_se <- c(co2$loos$m4$se_elpd_loo, co2$loos$m5$se_elpd_loo)

loo_table2 <- data.frame(looic_2, looic_2_se, looic_elpd_2,looic_elpd_2_se, row.names = c("m4", "m5"))
loo_table2

bayes_R2(m4)
bayes_R2(m5)

pp_check(m5, resp="riskSum", nsamples = 1000)
pp_check(m5, resp="riskPerceptionSum", nsamples = 1000)
pp_check(m5, resp="riskBenefitSum", nsamples = 1000)

# Regression m6: DOPL across risk domains ----

m6 <- brm(mvbind(dominanceSum, prestigeSum, leadershipSum) ~ ethicalPreference + financialPreference + socialPreference + healthAndSafetyPreference + recreationalPreference, data = experiment_dataset_analysis,
                       iter = 5000, warmup = 500,
                       prior = c(prior(normal(0, 1), class = "Intercept"),
                                 prior(normal(0, 1), class = "sigma", resp = "dominanceSum"),
                                 prior(normal(0, 1), class = "sigma", resp = "prestigeSum"),
                                 prior(normal(0, 1), class = "sigma", resp = "leadershipSum"),
                                 prior(normal(0, 1), class = "b", coef = "ethicalPreference", resp = "dominanceSum"),
                                 prior(normal(0, 1), class = "b", coef = "financialPreference", resp = "dominanceSum"),
                                 prior(normal(0, 1), class = "b", coef = "healthAndSafetyPreference", resp = "dominanceSum"), 
                                 prior(normal(0, 1), class = "b", coef = "recreationalPreference", resp = "dominanceSum"), 
                                 prior(normal(0, 1), class = "b", coef = "socialPreference", resp = "dominanceSum"),
                                 prior(normal(0, 1), class = "b", coef = "ethicalPreference", resp = "prestigeSum"),
                                 prior(normal(0, 1), class = "b", coef = "financialPreference", resp = "prestigeSum"),
                                 prior(normal(0, 1), class = "b", coef = "healthAndSafetyPreference", resp = "prestigeSum"), 
                                 prior(normal(0, 1), class = "b", coef = "recreationalPreference", resp = "prestigeSum"), 
                                 prior(normal(0, 1), class = "b", coef = "socialPreference", resp = "prestigeSum"),
                                 prior(normal(0, 1), class = "b", coef = "ethicalPreference", resp = "leadershipSum"),
                                 prior(normal(0, 1), class = "b", coef = "financialPreference", resp = "leadershipSum"),
                                 prior(normal(0, 1), class = "b", coef = "healthAndSafetyPreference", resp = "leadershipSum"), 
                                 prior(normal(0, 1), class = "b", coef = "recreationalPreference", resp = "leadershipSum"), 
                                 prior(normal(0, 1), class = "b", coef = "socialPreference", resp = "leadershipSum")),
                       save_all_pars = T)
summary(m6)

# HDI m6
m6_hdi <- hdi(m6, effects = "fixed", component = "conditional", ci = .95)
m6_hdi[sign(m6_hdi$CI_low) == sign(m6_hdi$CI_high),
       c('Parameter', 'CI','CI_low', 'CI_high')]

# Regression m7: DOSPERT domains in each risk category across DOPL motives, age and gender ----

m7 <- brm(mvbind(ethicalQuestionsRiskSum, ethicalQuestionsPerceptionSum, ethicalQuestionsBenefitSum, 
                 financialQuestionsRiskSum, financialQuestionsPerceptionSum, financialQuestionsBenefitSum,
                 healthAndSafetyQuestionsRiskSum, healthAndSafetyQuestionsPerceptionSum, 
                 healthAndSafetyQuestionsBenefitSum,recreationalQuestionsRiskSum, recreationalQuestionsPerceptionSum, 
                recreationalQuestionsBenefitSum, socialQuestionsRiskSum, socialQuestionsPerceptionSum, socialQuestionsBenefitSum) ~ dominanceSum*Gender + prestigeSum*Gender + leadershipSum*Gender + Age, data= experiment_dataset_analysis, iter = 8000, warmup = 500, save_all_pars = T, cores = 6,
          prior = c(prior(normal(0, 1), class = "Intercept", resp = "ethicalQuestionsRiskSum"), 
                    prior(normal(0, 1), class = "sigma", resp = "ethicalQuestionsRiskSum"), 
                    prior(normal(-2, 1), coef = "Age", resp = "ethicalQuestionsRiskSum"),
                    prior(normal(-2, 1), coef = "Gender1", resp = "ethicalQuestionsRiskSum"),
                    prior(normal(2, 1), coef = "dominanceSum", resp = "ethicalQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "ethicalQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "ethicalQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "ethicalQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "ethicalQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "ethicalQuestionsRiskSum"),
                    #
                    prior(normal(0, 1), class = "Intercept", resp = "ethicalQuestionsPerceptionSum"), 
                    prior(normal(0, 1), class = "sigma", resp = "ethicalQuestionsPerceptionSum"), 
                    prior(normal(2, 1), coef = "Age", resp = "ethicalQuestionsPerceptionSum"),
                    prior(normal(2, 1), coef = "Gender1", resp = "ethicalQuestionsPerceptionSum"),
                    prior(normal(-2, 1), coef = "dominanceSum", resp = "ethicalQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "ethicalQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "ethicalQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "ethicalQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "ethicalQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "ethicalQuestionsPerceptionSum"),
                    #
                    prior(normal(0, 1), class = "Intercept", resp = "ethicalQuestionsBenefitSum"), 
                    prior(normal(0, 1), class = "sigma", resp = "ethicalQuestionsBenefitSum"), 
                    prior(normal(-2, 1), coef = "Age", resp = "ethicalQuestionsBenefitSum"),
                    prior(normal(-2, 1), coef = "Gender1", resp = "ethicalQuestionsBenefitSum"),
                    prior(normal(2, 1), coef = "dominanceSum", resp = "ethicalQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "ethicalQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "ethicalQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "ethicalQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "ethicalQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "ethicalQuestionsBenefitSum"),
                    #
                    prior(normal(0, 1), class = "Intercept", resp = "financialQuestionsRiskSum"), 
                    prior(normal(0, 1), class = "sigma", resp = "financialQuestionsRiskSum"), 
                    prior(normal(-2, 1), coef = "Age", resp = "financialQuestionsRiskSum"),
                    prior(normal(-2, 1), coef = "Gender1", resp = "financialQuestionsRiskSum"),
                    prior(normal(2, 1), coef = "dominanceSum", resp = "financialQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "financialQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "financialQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "financialQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "financialQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "financialQuestionsRiskSum"),
                    #
                    prior(normal(0, 1), class = "Intercept", resp = "financialQuestionsPerceptionSum"), 
                    prior(normal(0, 1), class = "sigma", resp = "financialQuestionsPerceptionSum"), 
                    prior(normal(2, 1), coef = "Age", resp = "financialQuestionsPerceptionSum"),
                    prior(normal(2, 1), coef = "Gender1", resp = "financialQuestionsPerceptionSum"),
                    prior(normal(-2, 1), coef = "dominanceSum", resp = "financialQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "financialQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "financialQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "financialQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "financialQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "financialQuestionsPerceptionSum"),
                    #
                    prior(normal(0, 1), class = "Intercept", resp = "financialQuestionsBenefitSum"), 
                    prior(normal(0, 1), class = "sigma", resp = "financialQuestionsBenefitSum"), 
                    prior(normal(-2, 1), coef = "Age", resp = "financialQuestionsBenefitSum"),
                    prior(normal(-2, 1), coef = "Gender1", resp = "financialQuestionsBenefitSum"),
                    prior(normal(2, 1), coef = "dominanceSum", resp = "financialQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "financialQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "financialQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "financialQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "financialQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "financialQuestionsBenefitSum"),
                    #
                    prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyQuestionsRiskSum"), 
                    prior(normal(0, 1), class = "sigma", resp = "healthAndSafetyQuestionsRiskSum"), 
                    prior(normal(-2, 1), coef = "Age", resp = "healthAndSafetyQuestionsRiskSum"),
                    prior(normal(-2, 1), coef = "Gender1", resp = "healthAndSafetyQuestionsRiskSum"),
                    prior(normal(2, 1), coef = "dominanceSum", resp = "healthAndSafetyQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "healthAndSafetyQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "healthAndSafetyQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "healthAndSafetyQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "healthAndSafetyQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "healthAndSafetyQuestionsRiskSum"),
                    #
                    prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyQuestionsPerceptionSum"), 
                    prior(normal(0, 1), class = "sigma", resp = "healthAndSafetyQuestionsPerceptionSum"), 
                    prior(normal(2, 1), coef = "Age", resp = "healthAndSafetyQuestionsPerceptionSum"),
                    prior(normal(2, 1), coef = "Gender1", resp = "healthAndSafetyQuestionsPerceptionSum"),
                    prior(normal(-2, 1), coef = "dominanceSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "healthAndSafetyQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "healthAndSafetyQuestionsPerceptionSum"),
                    #
                    prior(normal(0, 1), class = "Intercept", resp = "healthAndSafetyQuestionsBenefitSum"), 
                    prior(normal(0, 1), class = "sigma", resp = "healthAndSafetyQuestionsBenefitSum"), 
                    prior(normal(-2, 1), coef = "Age", resp = "healthAndSafetyQuestionsBenefitSum"),
                    prior(normal(-2, 1), coef = "Gender1", resp = "healthAndSafetyQuestionsBenefitSum"),
                    prior(normal(2, 1), coef = "dominanceSum", resp = "healthAndSafetyQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "healthAndSafetyQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "healthAndSafetyQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "healthAndSafetyQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "healthAndSafetyQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "healthAndSafetyQuestionsBenefitSum"),
                    #
                    prior(normal(0, 1), class = "Intercept", resp = "recreationalQuestionsRiskSum"), 
                    prior(normal(0, 1), class = "sigma", resp = "recreationalQuestionsRiskSum"), 
                    prior(normal(-2, 1), coef = "Age", resp = "recreationalQuestionsRiskSum"),
                    prior(normal(-2, 1), coef = "Gender1", resp = "recreationalQuestionsRiskSum"),
                    prior(normal(2, 1), coef = "dominanceSum", resp = "recreationalQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "recreationalQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "recreationalQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "recreationalQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "recreationalQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "recreationalQuestionsRiskSum"),
                    #
                    prior(normal(0, 1), class = "Intercept", resp = "recreationalQuestionsPerceptionSum"), 
                    prior(normal(0, 1), class = "sigma", resp = "recreationalQuestionsPerceptionSum"), 
                    prior(normal(2, 1), coef = "Age", resp = "recreationalQuestionsPerceptionSum"),
                    prior(normal(2, 1), coef = "Gender1", resp = "recreationalQuestionsPerceptionSum"),
                    prior(normal(-2, 1), coef = "dominanceSum", resp = "recreationalQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "recreationalQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "recreationalQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "recreationalQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "recreationalQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "recreationalQuestionsPerceptionSum"),
                    #
                    prior(normal(0, 1), class = "Intercept", resp = "recreationalQuestionsBenefitSum"), 
                    prior(normal(0, 1), class = "sigma", resp = "recreationalQuestionsBenefitSum"), 
                    prior(normal(-2, 1), coef = "Age", resp = "recreationalQuestionsBenefitSum"),
                    prior(normal(-2, 1), coef = "Gender1", resp = "recreationalQuestionsBenefitSum"),
                    prior(normal(2, 1), coef = "dominanceSum", resp = "recreationalQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "recreationalQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "recreationalQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "recreationalQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "recreationalQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "recreationalQuestionsBenefitSum"),
                    #
                    prior(normal(0, 1), class = "Intercept", resp = "socialQuestionsRiskSum"), 
                    prior(normal(0, 1), class = "sigma", resp = "socialQuestionsRiskSum"), 
                    prior(normal(-2, 1), coef = "Age", resp = "socialQuestionsRiskSum"),
                    prior(normal(-2, 1), coef = "Gender1", resp = "socialQuestionsRiskSum"),
                    prior(normal(2, 1), coef = "dominanceSum", resp = "socialQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "socialQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "socialQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "socialQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "socialQuestionsRiskSum"),
                    prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "socialQuestionsRiskSum"),
                    #
                    prior(normal(0, 1), class = "Intercept", resp = "socialQuestionsPerceptionSum"), 
                    prior(normal(0, 1), class = "sigma", resp = "socialQuestionsPerceptionSum"), 
                    prior(normal(2, 1), coef = "Age", resp = "socialQuestionsPerceptionSum"),
                    prior(normal(2, 1), coef = "Gender1", resp = "socialQuestionsPerceptionSum"),
                    prior(normal(-2, 1), coef = "dominanceSum", resp = "socialQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "socialQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "socialQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "socialQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "socialQuestionsPerceptionSum"),
                    prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "socialQuestionsPerceptionSum"),
                    #
                    prior(normal(0, 1), class = "Intercept", resp = "socialQuestionsBenefitSum"), 
                    prior(normal(0, 1), class = "sigma", resp = "socialQuestionsBenefitSum"), 
                    prior(normal(-2, 1), coef = "Age", resp = "socialQuestionsBenefitSum"),
                    prior(normal(-2, 1), coef = "Gender1", resp = "socialQuestionsBenefitSum"),
                    prior(normal(2, 1), coef = "dominanceSum", resp = "socialQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "leadershipSum", resp = "socialQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "prestigeSum", resp = "socialQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "dominanceSum:Gender1", resp = "socialQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "Gender1:prestigeSum", resp = "socialQuestionsBenefitSum"),
                    prior(normal(0, 1), coef = "Gender1:leadershipSum", resp = "socialQuestionsBenefitSum")))
summary(m7)

m7_hdi <- hdi(m7, effects = "fixed", component = "conditional", ci = .95)
m7_hdi[sign(m7_hdi$CI_low) == sign(m7_hdi$CI_high),
       c('Parameter', 'CI','CI_low', 'CI_high')]

