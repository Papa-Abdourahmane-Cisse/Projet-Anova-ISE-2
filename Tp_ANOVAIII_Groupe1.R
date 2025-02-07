##################################################################"
#                                                                #
#    Cas pratique: ANOVA à trois facteurs cas non balancés       #
#                     07/02/2025                                 #
#######################################################"#########"                                                                

# Chargement des packages nécessaires
library(readxl)
library(dplyr)
library(tidyverse)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(emmeans)

# Chargement de la base

BASE <- read_excel("C:/Users/Anna/OneDrive/Documents/ISE2_suite/ANOVA/ANOVA/anova.xlsx")

# Afficher les premières 10 lignes du dataframe
head(BASE, 10)

##################### Statistiques descriptives########################### 
 
# Calculons la moyenne et l'écart-type du rendement par groupe 
resume <-BASE %>%
  group_by(facteur_1, facteur_2, facteur_3) %>%
  get_summary_stats(cible, type = "mean_sd")
View(resume)

# Boxplot 
bxp <- ggboxplot(
  BASE, x = "facteur_3", y = "cible", 
  color = "facteur_2", palette = "jco", facet.by = "facteur_1"
)
bxp

############## Vérification des  hypothèses#########################

# 1- test des valeurs aberrantes
val <-BASE %>%
  group_by(facteur_1, facteur_2, facteur_3) %>%
  identify_outliers(cible)
val

# 2- Hypothèse de normalité
model  <- lm(cible ~ facteur_1*facteur_2*facteur_3, data = BASE)

# 2-1 Créer un QQ plot des résidus
qqnorm(residuals(model), col = "turquoise", main = "QQ-Plot des Résidus",
       xlab = "Quantiles théoriques", ylab = "Quantiles observés")
qqline(residuals(model), col = "purple")

# 2-2 Calculer le test de normalité de Shapiro-Wilk des residus
shapiro_test <- shapiro.test(residuals(model))
shapiro_test

# 2-3 Vérification de la normalité par groupe (cellule)
ggqqplot(BASE, "cible", ggtheme = theme_bw()) +
  facet_grid(facteur_2 + facteur_3 ~ facteur_1, labeller = "label_both")

# 3-L’hypothèse d’homogénéité des variances

test_var<-levene_test(cible ~ facteur_1*facteur_2*facteur_3, data = BASE)
test_var

#######################Calcul de l'ANOVA###################################

res.aov <- BASE %>% anova_test(cible ~ facteur_1*facteur_2*facteur_3,type = 3)
res.aov

#######################Tests post-hoc######################################

# 1-Calculer des interactions à deux facteurs

BASE %>%
  group_by(facteur_1) %>%
  anova_test(cible ~ facteur_2*facteur_3, error = model)

# ############Calculs des effets principaux############################## 

#Regroupons les données par le facteur_1 et facteur_2  et analysons les effets 

# 2-principaux du facteur_3 sur la variable cible

traitement_effect <- BASE%>%
  group_by(facteur_1, facteur_2) %>%
  anova_test(cible ~ facteur_3, error = model)
traitement_effect

# 3-Comparaisons entre les groupes 

#Comparons le facteur_3 par facteur_1 et facteur_2

pwc <- BASE %>%
  group_by(facteur_1, facteur_2) %>%
  emmeans_test(cible ~ facteur_3, p.adjust.method = "bonferroni") %>%
  select(-df, -statistic, -p) # Supprimer les détails
view(pwc)

# 3-Moyennes marginales estimées (c.-à-d. moyenne ajustée) avec un intervalle de confiance à 95%
view(get_emmeans(pwc))


#############################################################
