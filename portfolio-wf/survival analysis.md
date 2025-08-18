# Packages
library(survival)
library(survminer)

# Jeu de données de survie exemple
data(lung)
head(lung)

# Création de l’objet de survie
# time = durée de suivi, status = évènement (1 = décès, 0 = censuré)
surv_obj <- Surv(time = lung$time, event = lung$status)

# Kaplan-Meier global
km_fit <- survfit(surv_obj ~ 1, data = lung)

# Plot courbe de survie
ggsurvplot(km_fit, conf.int = TRUE, risk.table = TRUE,
           title = "Courbe de survie globale (Kaplan-Meier)",
           xlab = "Temps (jours)", ylab = "Probabilité de survie")

# Kaplan-Meier par groupe (sexe dans lung: 1=male, 2=female)
km_fit_group <- survfit(surv_obj ~ sex, data = lung)
ggsurvplot(km_fit_group, data = lung, pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, legend.title = "Sexe",
           legend.labs = c("Homme", "Femme"))

# Modèle de Cox
cox_fit <- coxph(surv_obj ~ age + sex, data = lung)
summary(cox_fit)





