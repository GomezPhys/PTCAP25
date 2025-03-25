library(readxl) ## To load excel sheet
library(dplyr) # Data grammar and manipulation
library(rstatix) # Shapiro Wilk and effect size
library(psych) #descriptives
library(kableExtra) #tables
library(lme4) #linear mixed effects models (LMM)
library(lmerTest) #anova like output for LMM
library(ggplot2) #data visualization
library(ggpubr)#data visualization
library(ggprism)##makes plots look like graphad
library(table1) #for descriptives


#####Uppper####


Df <- read_excel("~/DAPTs.xlsx",
                 sheet = "upper50")
View(Df)


Df$Set <- as.factor(Df$Set)

## Order conditions
Df$Set <- ordered(Df$Set,
                        levels = c("0", "1",
                                   "2","3"))


################# DATA NORMALITY TEST ###############

##ESS normality
Df %>% group_by(Set) %>%
  shapiro_test(ESS_Ant)

Df %>% group_by(Set) %>%
  shapiro_test(ESS_Retro)

##Re normality
Df %>% group_by(Set) %>%
  shapiro_test(Re_Ant)

Df %>% group_by(Set) %>%
  shapiro_test(Re_Retro)


###### Linear Mixed models ESS Antegrade
lmModel = lmer(ESS_Ant ~ Set + (1|Subject_ID),
               data=Df, REML=FALSE)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df %>%
  pairwise_t_test(ESS_Ant ~ Set, paired = TRUE,
                  p.adjust.method	= "holm")
pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(ESS_Ant ~ Set,
                paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Set")
# Boxplot of ESS
Antegrade_ESS_plot <- ggboxplot(Df, x = "Set", y = "ESS_Ant",
                                color = "Set", palette = get_palette("Set1", 4),
                                ylab = "ESS Antegrade (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()
#Save Plot
ggsave("ESS_Antegrade.png")


####retro
###### Linear Mixed models ESS Antegrade
lmModel = lmer(ESS_Retro ~ Set + (1|Subject_ID),
               data=Df, REML=FALSE)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df %>%
  pairwise_t_test(ESS_Retro ~ Set, paired = TRUE,
                  p.adjust.method	= "holm")
pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(ESS_Retro ~ Set,
                paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Set")
# Boxplot of ESS
retrograde_ESS_plot <- ggboxplot(Df, x = "Set", y = "ESS_Retro",
                                color = "Set", palette = get_palette("Set1", 4),
                                ylab = "ESS Retrograde (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()



#####lower50#####

Df2 <- read_excel("~/DAPTs.xlsx",
                 sheet = "lower50")
View(Df2)


Df2$Set <- as.factor(Df2$Set)

## Order conditions
Df$Set <- ordered(Df$Set,
                  levels = c("0", "1",
                             "2","3"))



##ESS normality
Df2 %>% group_by(Set) %>%
  shapiro_test(ESS_Ant)

Df2 %>% group_by(Set) %>%
  shapiro_test(ESS_Retro)

##Re normality
Df2 %>% group_by(Set) %>%
  shapiro_test(Re_Ant)

Df2 %>% group_by(Set) %>%
  shapiro_test(Re_Retro)


###### Linear Mixed models ESS Antegrade
lmModel = lmer(ESS_Ant ~ Set + (1|Subject_ID),
               data=Df2, REML=FALSE)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df2 %>%
  pairwise_t_test(ESS_Ant ~ Set, paired = F,
                  p.adjust.method	= "holm")
pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df2 %>% cohens_d(ESS_Ant ~ Set,
                paired = F, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Set")
# Boxplot of ESS
Antegrade_ESS_plot <- ggboxplot(Df2, x = "Set", y = "ESS_Ant",
                                color = "Set", palette = get_palette("Set1", 4),
                                ylab = "ESS Antegrade (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()
#Save Plot
ggsave("ESS_Antegrade.png")


####retro
###### Linear Mixed models ESS Antegrade
lmModel = lmer(ESS_Retro ~ Set + (1|Subject_ID),
               data=Df2, REML=FALSE)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df2 %>%
  pairwise_t_test(ESS_Retro ~ Set, paired = F,
                  p.adjust.method	= "holm")
pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df2 %>% cohens_d(ESS_Retro ~ Set,
                paired = F, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Set")
# Boxplot of ESS
retrograde_ESS_plot <- ggboxplot(Df2, x = "Set", y = "ESS_Retro",
                                 color = "Set", palette = get_palette("Set1", 4),
                                 ylab = "ESS Retrograde (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()


#####Combined50#####

Df3 <- read_excel("~/DAPTs.xlsx",
                  sheet = "combined50")
View(Df3)


Df3$Set <- as.factor(Df3$Set)

## Order conditions
Df$Set <- ordered(Df$Set,
                  levels = c("0", "1",
                             "2","3"))



##ESS normality
Df3 %>% group_by(Set) %>%
  shapiro_test(ESS_Ant)

Df3 %>% group_by(Set) %>%
  shapiro_test(ESS_Retro)

##Re normality
Df3 %>% group_by(Set) %>%
  shapiro_test(Re_Ant)

Df3 %>% group_by(Set) %>%
  shapiro_test(Re_Retro)


###### Linear Mixed models ESS Antegrade
lmModel = lmer(ESS_Ant ~ Set + (1|Subject_ID),
               data=Df3, REML=FALSE)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df3 %>%
  pairwise_t_test(ESS_Ant ~ Set, paired = T,
                  p.adjust.method	= "holm")
pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df3 %>% cohens_d(ESS_Ant ~ Set,
                 paired = T, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Set")
# Boxplot of ESS
Antegrade_ESS_plot <- ggboxplot(Df3, x = "Set", y = "ESS_Ant",
                                color = "Set", palette = get_palette("Set1", 4),
                                ylab = "ESS Antegrade (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()
#Save Plot
ggsave("ESS_Antegrade.png")


####retro
###### Linear Mixed models ESS Antegrade
lmModel = lmer(ESS_Retro ~ Set + (1|Subject_ID),
               data=Df3, REML=FALSE)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df3 %>%
  pairwise_t_test(ESS_Retro ~ Set, paired = F,
                  p.adjust.method	= "holm")
pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df3 %>% cohens_d(ESS_Retro ~ Set,
                 paired = F, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Set")
# Boxplot of ESS
retrograde_ESS_plot <- ggboxplot(Df3, x = "Set", y = "ESS_Retro",
                                 color = "Set", palette = get_palette("Set1", 4),
                                 ylab = "ESS Retrograde (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()



##############80_PERCENT###########

#####Uppper80####


Df4 <- read_excel("~/DAPTs.xlsx",
                 sheet = "upper80")
View(Df4)


Df4$Set <- as.factor(Df4$Set)

## Order conditions
Df4$Set <- ordered(Df4$Set,
                  levels = c("0", "1",
                             "2","3"))


################# DATA NORMALITY TEST ###############

##ESS normality
Df4 %>% group_by(Set) %>%
  shapiro_test(ESS_Ant)

Df4 %>% group_by(Set) %>%
  shapiro_test(ESS_Retro)

##Re normality
Df4 %>% group_by(Set) %>%
  shapiro_test(Re_Ant)

Df4 %>% group_by(Set) %>%
  shapiro_test(Re_Retro)


###### Linear Mixed models ESS Antegrade
lmModel = lmer(ESS_Ant ~ Set + (1|Subject_ID),
               data=Df4, REML=FALSE)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df4 %>%
  pairwise_t_test(ESS_Ant ~ Set, paired = F,
                  p.adjust.method	= "holm")
pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df4 %>% cohens_d(ESS_Ant ~ Set,
                paired = F, hedges.correction = F)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Set")
# Boxplot of ESS
Antegrade_ESS_plot <- ggboxplot(Df4, x = "Set", y = "ESS_Ant",
                                color = "Set", palette = get_palette("Set1", 4),
                                ylab = "ESS Antegrade (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()
#Save Plot
ggsave("ESS_Antegrade.png")


####retro
###### Linear Mixed models ESS Antegrade
lmModel = lmer(ESS_Retro ~ Set + (1|Subject_ID),
               data=Df4, REML=FALSE)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df4 %>%
  pairwise_t_test(ESS_Retro ~ Set, paired = F,
                  p.adjust.method	= "holm")
pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df4 %>% cohens_d(ESS_Retro ~ Set,
                paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Set")
# Boxplot of ESS
retrograde_ESS_plot <- ggboxplot(Df4, x = "Set", y = "ESS_Retro",
                                 color = "Set", palette = get_palette("Set1", 4),
                                 ylab = "ESS Retrograde (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()



#####lower80#####

Df5 <- read_excel("~/DAPTs.xlsx",
                  sheet = "lower80")
View(Df5)


Df5$Set <- as.factor(Df5$Set)

## Order conditions
Df5$Set <- ordered(Df5$Set,
                  levels = c("0", "1",
                             "2","3"))



##ESS normality
Df5 %>% group_by(Set) %>%
  shapiro_test(ESS_Ant)

Df5 %>% group_by(Set) %>%
  shapiro_test(ESS_Retro)

##Re normality
Df5 %>% group_by(Set) %>%
  shapiro_test(Re_Ant)

Df5 %>% group_by(Set) %>%
  shapiro_test(Re_Retro)


###### Linear Mixed models ESS Antegrade
lmModel = lmer(ESS_Ant ~ Set + (1|Subject_ID),
               data=Df5, REML=FALSE)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)


# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df5 %>%
  pairwise_t_test(ESS_Ant ~ Set, paired = F,
                  p.adjust.method	= "holm")
pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df5 %>% cohens_d(ESS_Ant ~ Set,
                 paired = F, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Set")
# Boxplot of ESS
Antegrade_ESS_plot <- ggboxplot(Df5, x = "Set", y = "ESS_Ant",
                                color = "Set", palette = get_palette("Set1", 4),
                                ylab = "ESS Antegrade (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()
#Save Plot
ggsave("ESS_Antegrade.png")


####retro
###### Linear Mixed models ESS Antegrade
lmModel = lmer(ESS_Retro ~ Set + (1|Subject_ID),
               data=Df5, REML=FALSE)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df5 %>%
  pairwise_t_test(ESS_Retro ~ Set, paired = F,
                  p.adjust.method	= "holm")
pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df5 %>% cohens_d(ESS_Retro ~ Set,
                 paired = F, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Set")
# Boxplot of ESS
retrograde_ESS_plot <- ggboxplot(Df5, x = "Set", y = "ESS_Retro",
                                 color = "Set", palette = get_palette("Set1", 4),
                                 ylab = "ESS Retrograde (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()


#####Combined80#####

Df6 <- read_excel("~/DAPTs.xlsx",
                  sheet = "combined80")
View(Df6)


Df6$Set <- as.factor(Df6$Set)

## Order conditions
Df6$Set <- ordered(Df6$Set,
                  levels = c("0", "1",
                             "2","3"))



##ESS normality
Df6 %>% group_by(Set) %>%
  shapiro_test(ESS_Ant)

Df6 %>% group_by(Set) %>%
  shapiro_test(ESS_Retro)

##Re normality
Df6 %>% group_by(Set) %>%
  shapiro_test(Re_Ant)

Df6 %>% group_by(Set) %>%
  shapiro_test(Re_Retro)


###### Linear Mixed models ESS Antegrade
lmModel = lmer(ESS_Ant ~ Set + (1|Subject_ID),
               data=Df6, REML=FALSE)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df6 %>%
  pairwise_t_test(ESS_Ant ~ Set, paired = T,
                  p.adjust.method	= "holm")
pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df6 %>% cohens_d(ESS_Ant ~ Set,
                 paired = T, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Set")
# Boxplot of ESS
Antegrade_ESS_plot <- ggboxplot(Df6, x = "Set", y = "ESS_Ant",
                                color = "Set", palette = get_palette("Set1", 4),
                                ylab = "ESS Antegrade (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()
#Save Plot
ggsave("ESS_Antegrade.png")


####retro
###### Linear Mixed models ESS Antegrade
lmModel = lmer(ESS_Retro ~ Set + (1|Subject_ID),
               data=Df6, REML=FALSE)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df6 %>%
  pairwise_t_test(ESS_Retro ~ Set, paired = F,
                  p.adjust.method	= "holm")
pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df6 %>% cohens_d(ESS_Retro ~ Set,
                 paired = F, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Set")
# Boxplot of ESS
retrograde_ESS_plot <- ggboxplot(Df6, x = "Set", y = "ESS_Retro",
                                 color = "Set", palette = get_palette("Set1", 4),
                                 ylab = "ESS Retrograde (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()



