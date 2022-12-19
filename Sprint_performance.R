library(tidyr)
library(dplyr)
library(ggplot2)
library(nlme)
library(RColorBrewer)
library(emmeans)

# Importing the thermal performance data
sprint_df <- read.csv('C:/Users/colin/Dropbox/USDA_Chameleon_Report/QA3214_Data/4_Thermal Performance/Sprint_df.csv')

# Importing the morphometric data
morphometrics_df <- read.csv('C:/Users/colin/Dropbox/USDA_Chameleon_Report/QA3214_Data/4_Thermal Performance/Morphometrics_df.csv')

# Importing critical thermal limits data
ct_lim_df <- read.csv('C:/Users/colin/Dropbox/USDA_Chameleon_Report/QA3214_Data/4_Thermal Performance/Thermal_limits_for_performance.csv')

# Importing dataframe with Q10 values for each interval
q10_df <- read.csv('C:/Users/colin/Dropbox/USDA_Chameleon_Report/QA3214_Data/4_Thermal Performance/q10_df.csv')

# Generating a new dataframe that takes the best performance of each individual across each temperature
max_perf_df <- data.frame(
  sprint_df %>% 
    filter(trial.successful == "Y") %>% 
    group_by(Season, cham_id, trial_temp) %>% 
    summarise(max_maxvel_25_ms = max(max_vel_25cm_mps))
)
max_perf_df

# Merging the morphometrics and performance dataframes
max_perf_morph_df <- merge(morphometrics_df, max_perf_df, 
                       by = c("cham_id", "Season"))
max_perf_morph_df


# Merging the morphometrics and q10 dataframes
q10_morph_df <- merge(morphometrics_df, q10_df, by = c("cham_id", "Season"))

# Observing the distribution of q10 values
hist(q10_morph_df$q10)

# Calculating the natural log of all q10 values to meet assumptions of normality
log_q10_morph_df <- q10_morph_df %>% 
  mutate(log_q10 = log(q10))

# Re-checking the distribution of q10 values
hist(log_q10_morph_df$log_q10)

# q10 model: accounting for repeated measures
q10_model <- lme(log_q10 ~ Season + svl.mm. + sex + factor(Temp_range), random=~1|cham_id, 
          data = log_q10_morph_df, method='REML')
summary(q10_model)
anova(q10_model)
# q10_temp_range_tuk<-lsmeans(q10_model, pairwise ~ Temp_range, adjust="tukey")
# q10_temp_range_tuk

#Generating a dataframe with the max performance (standardized by percentage of maximum)/ sandard error at each temp
tpc_df <- data.frame(
  max_perf_morph_df %>% 
    group_by(Season, cham_id) %>% 
    mutate(max_sprint_perc = max_maxvel_25_ms / max(max_maxvel_25_ms)) %>% 
    group_by(Season, trial_temp) %>% 
    summarise(mean_sprint_perc = mean(max_sprint_perc),
              sd_sprint_perc = sd(max_sprint_perc),
              sample_size = length(unique(cham_id))) %>%
    group_by(Season, trial_temp) %>%
    mutate(sd_err_perf = 1.96 * (sd_sprint_perc / sample_size))
)
tpc_df

# Generating a dataframe based on just the critical thermal limits, with standard error and sample sizes
ct_lims_2 <- data.frame(
  ct_lim_df %>%
    group_by(Season,trial_type) %>%
    summarise(trial_temp = mean(cham_temp_end),
              trial_temp_sd = sd(cham_temp_end),
              sample_size = length(unique(cham_id))) %>% 
    group_by(Season,trial_type) %>% 
    mutate(sd_err_temp = 1.96 * (trial_temp_sd / sample_size))
)
ct_lims_2

# Making sure all of the columns match in order to bind the CT limits dataframe with the max performance dataframe, 
  ## and making sure CTmin and CTgape have 0 velocity
ct_lims_2$mean_sprint_perc <- 0
ct_lims_2$sd_err_perf <- 0

tpc_df$sd_err_temp <- 0

ct_lims_2 <- data.frame(
  ct_lims_2$Season, 
  ct_lims_2$trial_temp,
  ct_lims_2$mean_sprint_perc,
  ct_lims_2$trial_temp_sd,
  ct_lims_2$sample_size,
  ct_lims_2$sd_err_perf,
  ct_lims_2$sd_err_temp)

names(ct_lims_2) <- names(tpc_df)

# Combining the perf/ CT lims dataframes and 
tpc_full_df <- rbind(tpc_df, ct_lims_2)
tpc_full_df <- tpc_full_df %>% 
  mutate(mean_sprint_perc = mean_sprint_perc *100,
         sd_err_perf = sd_err_perf *100)

# Making sure the seasons appear in the correct order for the plot
tpc_full_df$Season <- factor(tpc_full_df$Season,
                             levels = c("Fall", "Winter", "Summer"))

# Generating the TPC plot
TPC_plot <- ggplot(data = tpc_full_df, aes(x = trial_temp, y = mean_sprint_perc), col = Season) +
  geom_line(aes(group = Season, linetype = Season, color = Season), size = 2) +
  scale_color_manual(values = c("#9999CC", "#0000CC", "#CC3300")) +
  geom_point(aes(group = Season, shape = Season), cex = 4.5) +
  geom_errorbar(aes(ymin = mean_sprint_perc - sd_err_perf, 
                    ymax = mean_sprint_perc + sd_err_perf,
                    xmin = sd_err_temp - sd_err_temp, 
                    xmax = sd_err_temp + sd_err_temp), 
                width = 0.5, size = 1.25)+
  xlab("Acclimation Temperature (°C)") +
  ylab("Percentage of Maximum Sprint Speed") +
  theme_classic(base_size = 18) +
  theme(legend.key.width = unit(5, "line")) +
  theme(legend.position="bottom")

TPC_plot


#q10 graph

# Making sure that the seasons appear in the correct order
q10_morph_df$Season <- factor(q10_df$Season,
                        levels = c("Fall", "Winter", "Summer"))

# Generating labels of the facet grid
Q10_facetgrid_names <- list(
  "15–20" = "15–20 °C",
  "20–25" = "20–25 °C",
  "25–30" = "25–30 °C",
  "30–35" = "30–35 °C"
)


# Creating a label so that the q10 values are grouped and labelled within their respective temperature ranges
Q10_facetgrid_labeller <- function(variable,value){
  return(value)
}

# Generating the q10 plot
q10_plot <- ggplot(data=q10_morph_df, aes(x = Season, y = q10, fill = Season)) +
  geom_boxplot(size=1, alpha=0.8,color="black", outlier.shape = NA)+
  geom_point(color="black", shape=21, size=3.5) +
  ylim(c(0, 12)) +
  theme_classic(base_size = 18) +
  scale_fill_manual(values = c("#9999CC", "#0000CC", "#CC3300"))+
  xlab("Season") +
  ylab(expression(italic(Q)[10] ~ "Sprint Speed")) +
  theme(legend.key.width = unit(3, "line")) +
  theme(legend.key.height = unit(2, "line")) +
  theme(legend.position="bottom",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  theme(axis.text.x = element_text(angle = 40, hjust=1)) +
  facet_grid(~Temp_range, labeller = Q10_facetgrid_labeller) 

q10_plot

