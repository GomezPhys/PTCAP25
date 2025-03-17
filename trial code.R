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
library(tidyverse)
library(tidyr)
library(irr)

df <- read_excel("~/NTJC_manny.xlsx", sheet = "NTJC_manny")
View (df)

df <- df %>%
  mutate(
    Condition = factor(Condition, levels = c("Upper", "Lower", "Combined")),
    Percent = factor(Percent, levels = c("50", "80")),
    Set = factor(Set, levels = c("0", "1", "2", "3")),  # Ensure 0 is Baseline
    Subject_ID = factor(Subject_ID)  # Ensure subject IDs are factors
  )

model <- lmer(ESS_Ant ~ Condition * Percent * Set + (1 | Subject_ID), data = df)
summary(model)
library(emmeans)
emmeans(model, pairwise ~ Condition * Percent * Set, adjust = "tukey")
ggplot(df, aes(x = Set, y = ESS_Ant, color = Condition, group = Condition)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Percent) +
  labs(title = "ESS_Ant Across Conditions and Sets", y = "ESS_Ant", x = "Set")

ggplot(df, aes(x = Set, y = ESS_Ant, fill = Percent)) +
  geom_boxplot() +
  facet_wrap(~Condition) +  # Separate plots per condition
  labs(title = "ESS_Ant Distribution Across Sets",
       y = "ESS_Ant",
       x = "Set",
       fill = "Percent (%)") +
  theme_minimal()

# Format the data to match ggpubr requirements
pairwise_df <- pairwise_df %>%
  mutate(
    group1 = gsub(" - .*", "", contrast),  # Extract first group
    group2 = gsub(".* - ", "", contrast),  # Extract second group
    p.adj.signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ "ns"  # Not significant
    )
  )

# Generate boxplot with significance markers
ggplot(df, aes(x = Set, y = ESS_Ant, fill = Percent)) +
  geom_boxplot() +
  facet_wrap(~Condition) +  # Separate plots per condition
  stat_pvalue_manual(pairwise_df, label = "p.adj.signif", hide.ns = TRUE) +  # Add significance labels
  labs(title = "ESS_Ant Distribution Across Sets",
       y = "ESS_Ant",
       x = "Set",
       fill = "Percent (%)") +
  theme_minimal()


# Perform pairwise comparisons with Tukey adjustment
comparisons <- emmeans(model, pairwise ~ Condition * Percent * Set, adjust = "tukey")

# Extract results as a data frame
comparison_results <- as.data.frame(comparisons$contrasts)


library(dplyr)
library(knitr)
library(kableExtra)

# Select relevant columns and rename for clarity
comparison_results %>%
  select(contrast, estimate, SE, df, t.ratio, p.value) %>%
  rename(Comparison = contrast,
         Mean_Difference = estimate,
         Std_Error = SE,
         t_Value = t.ratio,
         P_Value = p.value) %>%
  arrange(P_Value) %>%  # Sort by significance
  kable(digits = 4, format = "html") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))


# Display the table
library(ace_tools)
ace_tools::display_dataframe_to_user(name = "Pairwise Comparisons Table", dataframe = comparison_results)
3


library(effectsize)
cohens_d(model)


# Perform pairwise comparisons
pairwise_results <- emmeans(model, pairwise ~ Condition * Percent * Set, adjust = "tukey")

# Extract pairwise comparisons and format as data frame
pairwise_df <- as.data.frame(pairwise_results$contrasts)

# Create a boxplot with significance markers
ggplot(df, aes(x = Set, y = ESS_Ant, fill = Condition)) +
  geom_boxplot() +
  stat_pvalue_manual(pairwise_df, label = "p.value", hide.ns = TRUE) + # Show significant comparisons
  facet_wrap(~ Percent) +
  labs(title = "ESS_Ant Differences by Condition and Percent",
       y = "ESS_Ant",
       x = "Set") +
  theme_minimal()

ggplot(df, aes(x = Set, y = ESS_Ant, color = Condition, group = Condition)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Percent) +
  geom_text(data = pairwise_df, aes(x = as.numeric(Set), y = max(df$ESS_Ant), label = p.value),
            inherit.aes = FALSE, vjust = -1) + # Show p-values on top
  labs(title = "ESS_Ant Trends with Significant Differences",
       y = "ESS_Ant",
       x = "Set") +
  theme_minimal()

library(multcompView)

# Convert p-values to significance letters
pairwise_df$significance <- multcompLetters(pairwise_df$p.value)$Letters

# Merge back with original data
df <- merge(df, pairwise_df, by = c("Condition", "Percent", "Set"))

# Plot with significance letters
ggplot(df, aes(x = Set, y = ESS_Ant, fill = Condition)) +
  geom_boxplot() +
  geom_text(aes(label = significance), position = position_dodge(width = 0.8), vjust = -1) +
  facet_wrap(~ Percent) +
  labs(title = "ESS_Ant Differences by Condition and Percent",
       y = "ESS_Ant",
       x = "Set") +
  theme_minimal()

#Perform pairwise comparisons using emmeans (Tukey correction)
pairwise_results <- emmeans(model, pairwise ~ Condition * Percent * Set, adjust = "tukey")

# Convert results to a data frame
pairwise_df <- as.data.frame(pairwise_results$contrasts)

# Extract comparison groups and significance markers
pairwise_df <- pairwise_df %>%
  mutate(
    group1 = gsub(" - .*", "", contrast),  # Extract first group
    group2 = gsub(".* - ", "", contrast),  # Extract second group
    p.adj.signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ "ns"  # Not significant
    )
  )

# Set y.position for significance markers (adjust based on your data range)
max_y <- max(df$ESS_Ant, na.rm = TRUE)  # Get max Y value
pairwise_df$y.position <- max_y * 1.05  # Position markers slightly above max

# Generate boxplot with significance markers
ggplot(df, aes(x = Set, y = ESS_Ant, fill = Percent)) +
  geom_boxplot() +
  facet_wrap(~Condition) +  # Separate plots per condition
  stat_pvalue_manual(pairwise_df, label = "p.adj.signif", hide.ns = TRUE) +  # Add significance markers
  labs(title = "ESS_Ant Distribution Across Sets",
       y = "ESS_Ant",
       x = "Set",
       fill = "Percent") +
  theme_minimal()

####trial
colnames(df)
str(df)
# Convert categorical variables to factors (Ensure proper levels)
df <- df %>%
  mutate(
    Condition = factor(Condition, levels = c("Upper", "Lower", "Combined")),
    Percent = factor(Percent, levels = c("50", "80")),
    Set = factor(Set, levels = c("0", "1", "2", "3"))
  )

df <- df %>%
  mutate(
    Condition = factor(Condition, levels = c("Upper", "Lower", "Combined")),
    Percent = factor(Percent, levels = c(50, 80)),  # Ensure Percent is a factor
    Set = factor(Set, levels = c("0", "1", "2", "3"))
  )

# Run pairwise comparisons using Tukey's adjustment
pairwise_results <- emmeans(model, pairwise ~ Condition * Percent * Set, adjust = "tukey")

# Convert results into a data frame
pairwise_df <- as.data.frame(pairwise_results$contrasts)

# Extract first and second groups from the contrast column
pairwise_df <- pairwise_df %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  mutate(
    p.adj.signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ "ns"
    )
  )

# Define y.position dynamically to place significance markers above each box
max_y <- max(df$ESS_Ant, na.rm = TRUE)
pairwise_df$y.position <- max_y * 1.1  # Adjust spacing above the highest value

# Create the boxplot with significant comparisons
ggplot(df, aes(x = Set, y = ESS_Ant, fill = Percent)) +
  geom_boxplot() +
  facet_wrap(~Condition) +  # Separate plots per condition
  stat_pvalue_manual(pairwise_df, label = "p.adj.signif", hide.ns = TRUE) +  # Add significance markers
  labs(title = "ESS_Ant Distribution Across Sets",
       y = "ESS_Ant",
       x = "Set",
       fill = "Percent (%)") +
  theme_minimal()

colnames(df)
df <- df %>%
  mutate(
    Condition = factor(Condition, levels = c("Upper", "Lower", "Combined")),
    Percent = factor(Percent, levels = c("50", "80")),  # Convert Percent to a factor
    Set = factor(Set, levels = c("0", "1", "2", "3"))
  )

pairwise_results <- emmeans(model, pairwise ~ Condition * Percent * Set, adjust = "tukey")

# Convert results into a data frame
pairwise_df <- as.data.frame(pairwise_results$contrasts)

# Extract first and second groups from the contrast column
pairwise_df <- pairwise_df %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  mutate(
    p.adj.signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ "ns"
    )
  )

# Define y.position dynamically to place significance markers above each box
max_y <- max(df$ESS_Ant, na.rm = TRUE)
pairwise_df$y.position <- max_y * 1.1  # Adjust spacing above the highest value

# Create the boxplot with significant comparisons
ggplot(df, aes(x = Set, y = ESS_Ant, fill = Percent)) +
  geom_boxplot() +
  facet_wrap(~Condition) +  # Separate plots per condition
  stat_pvalue_manual(pairwise_df, label = "p.adj.signif", hide.ns = TRUE) +  # Add significance markers
  labs(title = "ESS_Ant Distribution Across Sets",
       y = "ESS_Ant",
       x = "Set",
       fill = "Percent (%)") +
  theme_minimal()



# Ensure Percent is a factor
df <- df %>%
  mutate(
    Condition = factor(Condition, levels = c("Upper", "Lower", "Combined")),
    Percent = factor(Percent, levels = c(50, 80)),  # Ensure Percent is a factor
    Set = factor(Set, levels = c("0", "1", "2", "3"))
  )

# Run pairwise comparisons using Tukey's adjustment
pairwise_results <- emmeans(model, pairwise ~ Condition * Percent * Set, adjust = "tukey")

# Convert results into a data frame
pairwise_df <- as.data.frame(pairwise_results$contrasts)

# Extract first and second groups from the contrast column
pairwise_df <- pairwise_df %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  mutate(
    p.adj.signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ "ns"
    )
  )

# Define y.position dynamically to place significance markers above each box
max_y <- max(df$ESS_Ant, na.rm = TRUE)
pairwise_df$y.position <- max_y * 1.1  # Adjust spacing above the highest value

# Create the boxplot with significant comparisons
ggplot(df, aes(x = Set, y = ESS_Ant, fill = Percent)) +
  geom_boxplot() +
  facet_wrap(~Condition) +  # Separate plots per condition
  stat_pvalue_manual(pairwise_df, label = "p.adj.signif", hide.ns = TRUE) +  # Add significance markers
  labs(title = "ESS_Ant Distribution Across Sets",
       y = "ESS_Ant",
       x = "Set",
       fill = "Percent (%)") +
  theme_minimal()
str(df)
df$Percent <- as.factor(df$Percent)
ggplot(df, aes(x = Set, y = ESS_Ant, fill = Percent)) +
  geom_boxplot() +
  facet_wrap(~Condition) +
  theme_minimal()

print(df$Percent)



# Convert Percent explicitly to factor (Ensures ggplot recognizes it)
df <- df %>%
  mutate(
    Condition = factor(Condition, levels = c("Upper", "Lower", "Combined")),
    Percent = as.factor(as.character(Percent)),  # Explicitly fix Percent
    Set = factor(Set, levels = c("0", "1", "2", "3"))
  )

# Run pairwise comparisons using Tukey's adjustment
pairwise_results <- emmeans(model, pairwise ~ Condition * Percent * Set, adjust = "tukey")

# Convert results into a data frame
pairwise_df <- as.data.frame(pairwise_results$contrasts)

# Extract first and second groups from the contrast column
pairwise_df <- pairwise_df %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  mutate(
    p.adj.signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ "ns"
    )
  )

# Define y.position dynamically to place significance markers above each box
max_y <- max(df$ESS_Ant, na.rm = TRUE)
pairwise_df$y.position <- max_y * 1.1  # Adjust spacing above the highest value

# Create the boxplot with significance markers
ggplot(df, aes(x = Set, y = ESS_Ant, fill = Percent)) +
  geom_boxplot() +
  facet_wrap(~Condition) +  # Separate plots per condition
  stat_pvalue_manual(pairwise_df, label = "p.adj.signif", hide.ns = TRUE) +  # Add significance markers
  labs(title = "ESS_Ant Distribution Across Sets",
       y = "ESS_Ant",
       x = "Set",
       fill = "Percent (%)") +
  theme_minimal()


print(pairwise_df)
pairwise_df <- as.data.frame(emmeans(model, pairwise ~ Condition * Percent * Set, adjust = "tukey")$contrasts)


# Run pairwise comparisons
pairwise_results <- emmeans(model, pairwise ~ Condition * Percent * Set, adjust = "tukey")

# Convert results into a data frame
pairwise_df <- as.data.frame(pairwise_results$contrasts)

# Extract comparison groups
pairwise_df <- pairwise_df %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  mutate(
    p.adj.signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ "ns"
    )
  )

# Check pairwise_df
print(pairwise_df)

# Define y.position dynamically
max_y <- max(df$ESS_Ant, na.rm = TRUE)
pairwise_df$y.position <- max_y * 1.1  

# Generate Boxplot with Significance
ggplot(df, aes(x = Set, y = ESS_Ant, fill = Percent +
  geom_boxplot() +
  facet_wrap(~Condition) +  
  stat_pvalue_manual(pairwise_df, label = "p.adj.signif", hide.ns = F) +  
  labs(title = "ESS_Ant Distribution Across Sets",
       y = "ESS_Ant",
       x = "Set",
       fill = "Percent") +
  theme_minimal()
  
  
####trial 2
library(dplyr)

df <- df %>%
  mutate(Condition_Percent = interaction(Percent, Condition, sep = "_")) # Combine Percent & Condition


pairwise_results <- emmeans(model, pairwise ~ Condition_Percent * Set, adjust = "tukey")

# Convert to a dataframe
pairwise_df <- as.data.frame(pairwise_results$contrasts)

# Extract group names for plotting
library(tidyr)
pairwise_df <- pairwise_df %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  mutate(
    p.adj.signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ "ns"
    )
  )

# Define y.position dynamically
max_y <- max(df$ESS_Ant, na.rm = TRUE)
pairwise_df$y.position <- max_y * 1.1  # Position markers above highest value
