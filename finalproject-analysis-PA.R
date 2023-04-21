## Title: ECON 470 Final Project
## Author: Ben Yang
## Date Created: 4/10/2023
## Date Edited: 4/10/2023
## Descriptions: This file renders/runs all relevant R code for the assignment

## Preliminaries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, ggpubr, cobalt, dplyr, broom, cobalt, MatchIt,
               knitr, data.table, kableExtra, tinytex, scales,  
               lubridate, stringr, gdata,
               readxl, 
               rdrobust, rddensity, estimatr,
               modelsummary, fixest, AER)

## Read data and set workspace for knitr ---------------------------------------

BRFSS <- read_csv("data/input/Behavioral_Risk_Factor_Surveillance_System__BRFSS__Prevalence_Data__2011_to_present_.csv") 
acs_medicaid <- read_tsv("data/output/acs_medicaid.txt")

## Merge Data Set --------------------------------------------------------------

acs_medicaid_BRFSS <- acs_medicaid %>%
  left_join(BRFSS, by = c("State" = "Locationdesc", "year" = "Year"))

## Health Care Coverage by Overall ---------------------------------------------

hcc_o <- acs_medicaid_BRFSS %>%
  filter(Class == "Health Care Access/Coverage", Topic == "Health Care Coverage") %>%
  filter(QuestionID == "HLTHPLN1", Response == "Yes") %>%
  filter(Break_Out_Category == "Overall") %>%
  select(year, State, expand, `Crude Prevalence` = Data_value)

hcc_o_data <- hcc_o %>% 
  group_by(year, expand) %>%
  summarise(Avg_CP = mean(`Crude Prevalence`))

hcc_o_plot <- hcc_o_data %>%
  ggplot(aes(x = year, y = Avg_CP, color = expand)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(2012:2020)) +
  scale_color_manual(name = "Expand", values = c("dodgerblue4", "dodgerblue1")) +
  geom_text(aes(label = paste0(round(Avg_CP,1), "%")), size = 3, nudge_x = 0, nudge_y = 0.3, check_overlap = TRUE, show_guide = FALSE) +
  labs(x = "Year", y = "Health Care Coverage Rate (%)", Title = "Average Health Care Coverage Rate by Medicaid Expansion Status from 2012 to 2019") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

hcc_o_sum <- hcc_o %>%
  mutate(year = factor(year)) %>%
  group_by(year, expand) %>% 
  summarise(`Mean` = mean(`Crude Prevalence`),
            `Min` = min(`Crude Prevalence`),
            `Q2` = quantile(`Crude Prevalence`, 0.25),
            `Median` = median(`Crude Prevalence`),
            `Q3` = quantile(`Crude Prevalence`, 0.75),
            `Max` = max(`Crude Prevalence`)) %>%
  pivot_wider(names_from = expand, values_from = c("Mean", "Min", "Q2", "Median", "Q3", "Max")) %>%
  select(year, Mean_FALSE, Min_FALSE, Q2_FALSE, Median_FALSE, Q3_FALSE, Max_FALSE, Mean_TRUE, Min_TRUE, Q2_TRUE, Median_TRUE, Q3_TRUE, Max_TRUE)
colnames(hcc_o_sum) <- c("year", rep(c("Mean", "Min", "Q2", "Median", "Q3", "Max"), 2))

hcc_o_dist <- hcc_o %>%
  ggplot(aes(x = factor(year), y = `Crude Prevalence`, color = expand)) +
  geom_boxplot(size = 0.5, alpha = 0.25, position = position_dodge(0.8), outlier.shape = NA) +
  stat_summary(fun.y = "mean", geom = "point", shape = 16, size = 4, position = position_dodge(0.8)) +
  scale_color_manual(name = "Expand", values = c("dodgerblue4", "dodgerblue1")) +
  labs(x = "Year", y = "Health Care Coverage Rate by State", Title = "Distribution of Health Care Coverage Rate by State") +
  ylim(70,100) +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Health Care Coverage by Gender ----------------------------------------------

hcc_g <- acs_medicaid_BRFSS %>%
  filter(Class == "Health Care Access/Coverage", Topic == "Health Care Coverage") %>%
  filter(QuestionID == "HLTHPLN1", Response == "Yes") %>%
  filter(Break_Out_Category == "Gender") %>%
  select(year, State, expand, Gender = Break_Out, `Crude Prevalence` = Data_value)

hcc_g_data <- hcc_g %>% 
  group_by(year, expand, Gender) %>%
  summarise(Avg_CP = mean(`Crude Prevalence`)) %>%
  mutate(expand_Gender = paste0(expand, "_", Gender))

hcc_g_plot <- hcc_g_data %>%
  ggplot(aes(x = year, y = Avg_CP, color = expand_Gender)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(2012:2020)) +
  scale_color_manual(name = "Expand Gender", values = c("firebrick4", "dodgerblue4", "firebrick1", "dodgerblue1")) +
  geom_text(aes(label = paste0(round(Avg_CP,1), "%")), size = 3, nudge_x = 0, nudge_y = 0.3, check_overlap = TRUE, show_guide = FALSE) +
  labs(x = "Year", y = "Health Care Coverage Rate (%)", Title = "Average Health Care Coverage Rate by Gender and Medicaid Expansion Status from 2012 to 2019") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Health Care Coverage by Household Income ------------------------------------

hcc_hi <- acs_medicaid_BRFSS %>%
  filter(Class == "Health Care Access/Coverage", Topic == "Health Care Coverage") %>%
  filter(QuestionID == "HLTHPLN1", Response == "Yes") %>%
  filter(Break_Out_Category == "Household Income") %>%
  select(year, State, expand, `Household Income` = Break_Out, `Crude Prevalence` = Data_value)

hcc_hi_data <- hcc_hi %>% 
  group_by(year, expand, `Household Income`) %>%
  summarise(Avg_CP = mean(`Crude Prevalence`)) %>%
  mutate(expand_hi = paste0(expand, "_", `Household Income`))

hcc_hi_plot <- hcc_hi_data %>%
  ggplot(aes(x = year, y = Avg_CP, color = expand_hi)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(2012:2020)) +
  scale_color_manual(name = "Expand Household Income", values = c("dodgerblue4", "firebrick4", "darkorange3", "goldenrod3", "darkgreen",
                                                                  "dodgerblue1", "firebrick1", "darkorange1", "goldenrod1", "forestgreen")) +
  #geom_text(aes(label = paste0(round(Avg_CP,1), "%")), size = 3, nudge_x = 0, nudge_y = 0.3, check_overlap = TRUE, show_guide = FALSE) +
  labs(x = "Year", y = "Health Care Coverage Rate (%)", Title = "Average Health Care Coverage Rate by Household Income from 2012 to 2019") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Health Status Response Rate -------------------------------------------------

hcc_hs <- acs_medicaid_BRFSS %>%
  filter(Class == "Health Status", Topic == "Overall Health") %>%
  filter(QuestionID == "GENHLTH") %>%
  filter(Break_Out_Category == "Overall") %>%
  select(year, State, expand, Response, `Crude Prevalence` = Data_value)

hcc_hs_data <- hcc_hs %>%
  group_by(year, expand, Response) %>%
  mutate(Response = factor(Response)) %>%
  summarise(Avg_CP = mean(`Crude Prevalence`))
levels(hcc_hs_data$Response) <- c("Excellent", "Very good", "Good", "Fair", "Poor")

hcc_hs_plot <- hcc_hs_data %>%
  ggplot(aes(x = year, y = Avg_CP, fill = Response)) +
  geom_col(color = "white") +
  facet_wrap(vars(expand)) +
  scale_fill_manual(name = "Response", values = c("dodgerblue1", "dodgerblue3", "dodgerblue4", "dimgray", "darkgray")) +
  geom_text(aes(label = paste0(round(Avg_CP), "%")), size = 3, hjust = 0.5, vjust = 2, position = "stack", color = "white") +
  labs(x = "Year", y = "Health Care Coverage Rate (%)", Title = "Health Status by Response from 2012 to 2019") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

hcc_hsEX_sum <- hcc_hs %>%
  mutate(year = factor(year)) %>%
  filter(Response == "Excellent") %>%
  group_by(year, expand) %>% 
  summarise(`Mean` = mean(`Crude Prevalence`),
            `Min` = min(`Crude Prevalence`),
            `Q2` = quantile(`Crude Prevalence`, 0.25),
            `Median` = median(`Crude Prevalence`),
            `Q3` = quantile(`Crude Prevalence`, 0.75),
            `Max` = max(`Crude Prevalence`)) %>%
  pivot_wider(names_from = expand, values_from = c("Mean", "Min", "Q2", "Median", "Q3", "Max")) %>%
  select(year, Mean_FALSE, Min_FALSE, Q2_FALSE, Median_FALSE, Q3_FALSE, Max_FALSE, Mean_TRUE, Min_TRUE, Q2_TRUE, Median_TRUE, Q3_TRUE, Max_TRUE)
colnames(hcc_hsEX_sum) <- c("year", rep(c("Mean", "Min", "Q2", "Median", "Q3", "Max"), 2))

hcc_hsPO_sum <- hcc_hs %>%
  mutate(year = factor(year)) %>%
  filter(Response == "Poor") %>%
  group_by(year, expand) %>% 
  summarise(`Mean` = mean(`Crude Prevalence`),
            `Min` = min(`Crude Prevalence`),
            `Q2` = quantile(`Crude Prevalence`, 0.25),
            `Median` = median(`Crude Prevalence`),
            `Q3` = quantile(`Crude Prevalence`, 0.75),
            `Max` = max(`Crude Prevalence`)) %>%
  pivot_wider(names_from = expand, values_from = c("Mean", "Min", "Q2", "Median", "Q3", "Max")) %>%
  select(year, Mean_FALSE, Min_FALSE, Q2_FALSE, Median_FALSE, Q3_FALSE, Max_FALSE, Mean_TRUE, Min_TRUE, Q2_TRUE, Median_TRUE, Q3_TRUE, Max_TRUE)
colnames(hcc_hsPO_sum) <- c("year", rep(c("Mean", "Min", "Q2", "Median", "Q3", "Max"), 2))

levels(factor(BRFSS$Topic))

## Save data for markdown ------------------------------------------------------

rm(list=c("hcc_gender_plotData"))
save.image("FP_workspace.Rdata")
