## Title: ECON 470 Final Project
## Author: Ben Yang
## Date Created: 4/21/2023
## Date Edited: 4/24/2023
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
MAExp <- read_tsv("data/output/medicaid_expansion.txt")

## Merge Data Set --------------------------------------------------------------

MAExp_BRFSS <- MAExp %>%
  filter(! State %in% c("District of Columbia")) %>%
  left_join(BRFSS, by = c("State" = "Locationdesc")) %>%
  mutate(expand_year = year(date_adopted),
         expand = (Year >= expand_year & !is.na(expand_year))) %>%
  rename(expand_ever=expanded) %>%
  filter(Year %in% 2011:2019) %>%
  arrange(State, Year)

## Health Care Coverage by Overall ---------------------------------------------

hcc_o <- MAExp_BRFSS %>%
  filter(expand_year == 2014 | is.na(expand_year)) %>%
  filter(Class == "Health Care Access/Coverage", Topic == "Health Care Coverage") %>%
  filter(QuestionID == "HLTHPLN1", Response == "Yes") %>%
  filter(Break_Out_Category == "Overall") %>%
  select(Year, State, expand, expand_ever, `Crude Prevalence` = Data_value)

hcc_o_data <- hcc_o %>% 
  group_by(Year, expand_ever) %>%
  summarise(Avg_CP = mean(`Crude Prevalence`))

hcc_o_plot <- hcc_o_data %>%
  ggplot(aes(x = Year, y = Avg_CP, color = expand_ever)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(2011:2019)) +
  scale_color_manual(name = "Expand", values = c("dodgerblue4", "dodgerblue1")) +
  geom_text(aes(label = paste0(round(Avg_CP,1), "%")), size = 3, nudge_x = 0, nudge_y = 0.3, check_overlap = TRUE, show_guide = FALSE) +
  labs(x = "Year", y = "Health Care Coverage Rate (%)", Title = "Average Health Care Coverage Rate by Medicaid Expansion Status from 2011 to 2019") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

hcc_o_sum <- hcc_o %>%
  mutate(Year = factor(Year)) %>%
  group_by(Year, expand_ever) %>% 
  summarise(`Mean` = mean(`Crude Prevalence`),
            `Min` = min(`Crude Prevalence`),
            `Q2` = quantile(`Crude Prevalence`, 0.25),
            `Median` = median(`Crude Prevalence`),
            `Q3` = quantile(`Crude Prevalence`, 0.75),
            `Max` = max(`Crude Prevalence`)) %>%
  pivot_wider(names_from = expand_ever, values_from = c("Mean", "Min", "Q2", "Median", "Q3", "Max")) %>%
  select(Year, Mean_FALSE, Min_FALSE, Q2_FALSE, Median_FALSE, Q3_FALSE, Max_FALSE, Mean_TRUE, Min_TRUE, Q2_TRUE, Median_TRUE, Q3_TRUE, Max_TRUE)
colnames(hcc_o_sum) <- c("Year", rep(c("Mean", "Min", "Q2", "Median", "Q3", "Max"), 2))

hcc_o_dist <- hcc_o %>%
  ggplot(aes(x = factor(Year), y = `Crude Prevalence`, color = expand_ever)) +
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

hcc_g <- MAExp_BRFSS %>%
  filter(expand_year == 2014 | is.na(expand_year)) %>%
  filter(Class == "Health Care Access/Coverage", Topic == "Health Care Coverage") %>%
  filter(QuestionID == "HLTHPLN1", Response == "Yes") %>%
  filter(Break_Out_Category == "Gender") %>%
  select(Year, State, expand, expand_ever, Gender = Break_Out, `Crude Prevalence` = Data_value)

hcc_g_data <- hcc_g %>% 
  group_by(Year, expand_ever, Gender) %>%
  summarise(Avg_CP = mean(`Crude Prevalence`)) %>%
  mutate(expand_Gender = paste0(expand_ever, "_", Gender))

hcc_g_plot <- hcc_g_data %>%
  ggplot(aes(x = Year, y = Avg_CP, color = expand_Gender)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(2011:2019)) +
  scale_color_manual(name = "Expand Gender", values = c("firebrick4", "dodgerblue4", "firebrick1", "dodgerblue1")) +
  geom_text(aes(label = paste0(round(Avg_CP,1), "%")), size = 3, nudge_x = 0, nudge_y = 0.3, check_overlap = TRUE, show_guide = FALSE) +
  labs(x = "Year", y = "Health Care Coverage Rate (%)", Title = "Average Health Care Coverage Rate by Gender and Medicaid Expansion Status from 2011 to 2019") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Health Care Coverage by Household Income ------------------------------------

hcc_hi <- MAExp_BRFSS %>%
  filter(expand_year == 2014 | is.na(expand_year)) %>%
  filter(Class == "Health Care Access/Coverage", Topic == "Health Care Coverage") %>%
  filter(QuestionID == "HLTHPLN1", Response == "Yes") %>%
  filter(Break_Out_Category == "Household Income") %>%
  select(Year, State, expand, expand_ever, `Household Income` = Break_Out, `Crude Prevalence` = Data_value)

hcc_hi_data <- hcc_hi %>% 
  group_by(Year, expand_ever, `Household Income`) %>%
  summarise(Avg_CP = mean(`Crude Prevalence`)) %>%
  mutate(expand_hi = paste0(expand_ever, "_", `Household Income`))

hcc_hi_plot <- hcc_hi_data %>%
  ggplot(aes(x = Year, y = Avg_CP, color = expand_hi)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(2011:2019)) +
  scale_color_manual(name = "Expand Household Income", values = c("dodgerblue4", "firebrick4", "darkorange3", "goldenrod3", "darkgreen",
                                                                  "dodgerblue1", "firebrick1", "darkorange1", "goldenrod1", "forestgreen")) +
  #geom_text(aes(label = paste0(round(Avg_CP,1), "%")), size = 3, nudge_x = 0, nudge_y = 0.3, check_overlap = TRUE, show_guide = FALSE) +
  labs(x = "Year", y = "Health Care Coverage Rate (%)", Title = "Average Health Care Coverage Rate by Household Income from 2011 to 2019") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Health Status Response Rate -------------------------------------------------

hs <- MAExp_BRFSS %>%
  filter(expand_year == 2014 | is.na(expand_year)) %>%
  filter(Class == "Health Status", Topic == "Overall Health") %>%
  filter(QuestionID == "GENHLTH") %>%
  filter(Break_Out_Category == "Overall") %>%
  select(Year, State, expand, expand_ever, Response, `Crude Prevalence` = Data_value)

hs_data <- hs %>%
  group_by(Year, expand_ever, Response) %>%
  mutate(Response = factor(Response, levels = c("Excellent", "Very good", "Good", "Fair", "Poor"))) %>%
  summarise(Avg_CP = mean(`Crude Prevalence`))

hs_plot <- hs_data %>%
  ggplot(aes(x = Year, y = Avg_CP, fill = Response)) +
  geom_col(color = "white") +
  facet_wrap(vars(expand_ever)) +
  scale_fill_manual(name = "Response", values = c("dodgerblue1", "dodgerblue3", "dodgerblue4", "dimgray", "darkgray")) +
  scale_x_continuous(breaks = c(2011:2019)) +
  geom_text(aes(label = paste0(round(Avg_CP), "%")), size = 3, hjust = 0.5, vjust = 2, position = "stack", color = "white") +
  labs(x = "Year", y = "Health Care Coverage Rate (%)", Title = "Health Status by Response from 2011 to 2019") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

hsEX_sum <- hs %>%
  mutate(Year = factor(Year)) %>%
  filter(Response == "Excellent") %>%
  group_by(Year, expand_ever) %>% 
  summarise(`Mean` = mean(`Crude Prevalence`),
            `Min` = min(`Crude Prevalence`),
            `Q2` = quantile(`Crude Prevalence`, 0.25),
            `Median` = median(`Crude Prevalence`),
            `Q3` = quantile(`Crude Prevalence`, 0.75),
            `Max` = max(`Crude Prevalence`)) %>%
  pivot_wider(names_from = expand_ever, values_from = c("Mean", "Min", "Q2", "Median", "Q3", "Max")) %>%
  select(Year, Mean_FALSE, Min_FALSE, Q2_FALSE, Median_FALSE, Q3_FALSE, Max_FALSE, Mean_TRUE, Min_TRUE, Q2_TRUE, Median_TRUE, Q3_TRUE, Max_TRUE)
colnames(hsEX_sum) <- c("Year", rep(c("Mean", "Min", "Q2", "Median", "Q3", "Max"), 2))

hsPO_sum <- hs %>%
  mutate(Year = factor(Year)) %>%
  filter(Response == "Poor") %>%
  group_by(Year, expand_ever) %>% 
  summarise(`Mean` = mean(`Crude Prevalence`),
            `Min` = min(`Crude Prevalence`),
            `Q2` = quantile(`Crude Prevalence`, 0.25),
            `Median` = median(`Crude Prevalence`),
            `Q3` = quantile(`Crude Prevalence`, 0.75),
            `Max` = max(`Crude Prevalence`)) %>%
  pivot_wider(names_from = expand_ever, values_from = c("Mean", "Min", "Q2", "Median", "Q3", "Max")) %>%
  select(Year, Mean_FALSE, Min_FALSE, Q2_FALSE, Median_FALSE, Q3_FALSE, Max_FALSE, Mean_TRUE, Min_TRUE, Q2_TRUE, Median_TRUE, Q3_TRUE, Max_TRUE)
colnames(hsPO_sum) <- c("Year", rep(c("Mean", "Min", "Q2", "Median", "Q3", "Max"), 2))

## Health Status Response Rate Fair or Poor ------------------------------------

hsfp_hi15 <- MAExp_BRFSS %>%
  filter(Class == "Health Status", Topic == "Fair or Poor Health") %>%
  filter(QuestionID == "_RFHLTH", Response == "Fair or Poor Health") %>%
  filter(Break_Out_Category == "Household Income", Break_Out == "Less than $15,000") %>%
  select(Year, State, expand_year, expand_ever, expand, Response, Rate = Data_value)

## Regression Data

hsfp_hi15_reg.data <- hsfp_hi15 %>%
  filter(expand_year == 2014 | is.na(expand_year)) %>%
  mutate(post = (Year >= 2014), treat = post * expand_ever)

hsfp_hi15_reg.data2 <- hsfp_hi15 %>%
  mutate(treat = case_when(
    Year >= expand_year & !is.na(expand_year) ~ 1,
    is.na(expand_year) ~ 0,
    Year < expand_year & !is.na(expand_year) ~ 0)
  ) %>%
  mutate(time_to_treat = ifelse(expand_ever == TRUE, Year - expand_year, -1),
         time_to_treat = ifelse(time_to_treat <= -4, -4, time_to_treat))
# not enough data points for time_to_treat beyond 4 years in the past,
# since most states that have ever adopted expansion expanded in 2014

## Summary Statistics

hsfp_hi15_reg.data_sum <- hsfp_hi15_reg.data %>%
  mutate(Year = factor(Year)) %>%
  group_by(Year, expand_ever) %>% 
  summarise(`Mean` = mean(Rate),
            `Min` = min(Rate),
            `Q2` = quantile(Rate, 0.25),
            `Median` = median(Rate),
            `Q3` = quantile(Rate, 0.75),
            `Max` = max(Rate)) %>%
  pivot_wider(names_from = expand_ever, values_from = c("Mean", "Min", "Q2", "Median", "Q3", "Max")) %>%
  select(Year, Mean_FALSE, Min_FALSE, Q2_FALSE, Median_FALSE, Q3_FALSE, Max_FALSE, Mean_TRUE, Min_TRUE, Q2_TRUE, Median_TRUE, Q3_TRUE, Max_TRUE)
colnames(hsfp_hi15_reg.data_sum) <- c("Year", rep(c("Mean", "Min", "Q2", "Median", "Q3", "Max"), 2))

hsfp_hi15_plot <- hsfp_hi15_reg.data %>%
  group_by(Year, expand_ever) %>%
  summarise(Avg_Rate = mean(Rate)) %>%
  ggplot(aes(x = Year, y = Avg_Rate, color = expand_ever)) +
  geom_line(size = 1) + geom_point(size = 2) +
  geom_vline(xintercept = 2013.5, size = 1, color = "firebrick", linetype = "longdash") + 
  scale_x_continuous(breaks = c(2011:2019)) +
  scale_color_manual(name = "Medicaid Expansion Status", values = c("dodgerblue4", "dodgerblue1"), labels = c("Non-Expansion", "Expansion")) +
  geom_text(aes(label = paste0(round(Avg_Rate, 1), "%")), size = 3, nudge_x = 0, nudge_y = 0.5, check_overlap = TRUE, color = "black") +
  labs(x = "Year", y = "Avg Percent of People (%)", Title = "Avg Percent of Individuals of Household Income Less Than 15,000 with Poor or Fair Health Status") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Basic 2x2 DD Table

hsfp_hi15_dd.table <- hsfp_hi15_reg.data %>%
  group_by(expand_ever, post) %>%
  summarise(avg_Rate = mean(Rate)) %>%
  pivot_wider(names_from = "post", names_prefix = "post", values_from = "avg_Rate") %>%
  ungroup() %>%
  mutate(expand_ever = case_when(
    expand_ever == FALSE ~ "Non-Expansion",
    expand_ever == TRUE ~ "Expansion")
  ) %>% 
  rename(Group = expand_ever, `Pre-Period` = postFALSE, `Post-Period` = postTRUE)

## Difference-in-Difference Model

hsfp_hi15_dd.est <- lm(formula = Rate ~ post + expand_ever + treat, data = hsfp_hi15_reg.data)
hsfp_hi15_fe.est <- feols(fm = Rate ~ treat | State + Year, data = hsfp_hi15_reg.data)
hsfp_hi15_fe.est2 <- feols(fm = Rate ~ treat | State + Year, data = hsfp_hi15_reg.data2)

## Event Study

hsfp_hi15_mod.twfe <- feols(fm = Rate ~ i(Year, expand_ever, ref = 2013) | State + Year, cluster = ~State, data = hsfp_hi15_reg.data)
hsfp_hi15_mod.twfe2 <- feols(fm = Rate ~ i(time_to_treat, expand_ever, ref = -1) | State + Year, cluster = ~State, data = hsfp_hi15_reg.data2)

hsfp_hi15_mod.twfe.plot <- data.frame(hsfp_hi15_mod.twfe$coeftable) %>%
  select(Estimate, SE = Std..Error) %>%
  mutate(Year = c(2011:2012, 2014:2019)) %>%
  rbind(., "Year::2013:expand_ever" = c(0, 0, 2013)) %>%
  ggplot(aes(x = Year, y = Estimate)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "black", linetype = 1) +
  geom_vline(xintercept = 2013, color = "black", linetype = 2) +
  geom_errorbar(aes(x = Year, ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE), width = .1) +
  scale_x_continuous(breaks = 2011:2019) +
  labs(x = "Time to Treatment", y = "Estimate and 95 Percent Confidence Interval", Title = "Event Study for Effects of Medicaid Expansion - States that Expanded in 2014") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

hsfp_hi15_mod.twfe2.plot <- data.frame(hsfp_hi15_mod.twfe2$coeftable) %>%
  select(Estimate, SE = Std..Error) %>%
  mutate(Year = c(-4:-2, 0:5)) %>%
  rbind(., "time_to_treat::-1:expand_ever" = c(0, 0, -1)) %>%
  ggplot(aes(x = Year, y = Estimate)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "black", linetype = 1) +
  geom_vline(xintercept = -1, color = "black", linetype = 2) +
  geom_errorbar(aes(x = Year, ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE), width = .1, position = position_dodge(width = 0.5)) +
  scale_x_continuous(breaks = -4:5) +
  labs(x = "Time to Treatment", y = "Estimate and 95 Percent Confidence Interval", Title = "Event Study for Effects of Medicaid Expansion - All ExpansionStates") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Chronic Obstructive Pulmonary Disease, or COPD ------------------------------

copd_hi15 <- MAExp_BRFSS %>%
  filter(Class == "Chronic Health Indicators", Topic == "COPD") %>%
  filter(Question == "Ever told you have COPD?", Response == "Yes") %>%
  filter(Break_Out_Category == "Household Income", Break_Out == "Less than $15,000") %>%
  select(Year, State, expand_year, expand_ever, expand, Response, Rate = Data_value)

## Regression Data

copd_hi15_reg.data <- copd_hi15 %>%
  filter(expand_year == 2014 | is.na(expand_year)) %>%
  mutate(post = (Year >= 2014), treat = post * expand_ever)

copd_hi15_reg.data2 <- copd_hi15 %>%
  mutate(treat = case_when(
    Year >= expand_year & !is.na(expand_year) ~ 1,
    is.na(expand_year) ~ 0,
    Year < expand_year & !is.na(expand_year) ~ 0)
  ) %>%
  mutate(time_to_treat = ifelse(expand_ever == TRUE, Year - expand_year, -1),
         time_to_treat = ifelse(time_to_treat <= -4, -4, time_to_treat))
# not enough data points for time_to_treat beyond 4 years in the past,
# since most states that have ever adopted expansion expanded in 2014

## Summary Statistics

copd_hi15_reg.data_sum <- copd_hi15_reg.data %>%
  mutate(Year = factor(Year)) %>%
  group_by(Year, expand_ever) %>% 
  summarise(`Mean` = mean(Rate, na.rm=TRUE),
            `Min` = min(Rate, na.rm=TRUE),
            `Q2` = quantile(Rate, 0.25, na.rm=TRUE),
            `Median` = median(Rate, na.rm=TRUE),
            `Q3` = quantile(Rate, 0.75, na.rm=TRUE),
            `Max` = max(Rate, na.rm=TRUE)) %>%
  pivot_wider(names_from = expand_ever, values_from = c("Mean", "Min", "Q2", "Median", "Q3", "Max")) %>%
  select(Year, Mean_FALSE, Min_FALSE, Q2_FALSE, Median_FALSE, Q3_FALSE, Max_FALSE, Mean_TRUE, Min_TRUE, Q2_TRUE, Median_TRUE, Q3_TRUE, Max_TRUE)
colnames(copd_hi15_reg.data_sum) <- c("Year", rep(c("Mean", "Min", "Q2", "Median", "Q3", "Max"), 2))

copd_hi15_plot <- copd_hi15_reg.data %>%
  group_by(Year, expand_ever) %>%
  summarise(Avg_Rate = mean(Rate, na.rm=TRUE)) %>%
  ggplot(aes(x = Year, y = Avg_Rate, color = expand_ever)) +
  geom_line(size = 1) + geom_point(size = 2) +
  geom_vline(xintercept = 2013.5, size = 1, color = "firebrick", linetype = "longdash") + 
  scale_x_continuous(breaks = c(2011:2019)) +
  scale_color_manual(name = "Medicaid Expansion Status", values = c("dodgerblue4", "dodgerblue1"), labels = c("Non-Expansion", "Expansion")) +
  geom_text(aes(label = paste0(round(Avg_Rate, 1), "%")), size = 3, nudge_x = 0, nudge_y = 0.5, check_overlap = TRUE, color = "black") +
  labs(x = "Year", y = "Avg Percent of People (%)", Title = "Avg Percent of Individuals of Household Income Less Than 15,000 with Poor or Fair Health Status") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Basic 2x2 DD Table 

copd_hi15_dd.table <- copd_hi15_reg.data %>%
  group_by(expand_ever, post) %>%
  summarise(avg_Rate = mean(Rate, na.rm=TRUE)) %>%
  pivot_wider(names_from = "post", names_prefix = "post", values_from = "avg_Rate") %>%
  ungroup() %>%
  mutate(expand_ever = case_when(
    expand_ever == FALSE ~ "Non-Expansion",
    expand_ever == TRUE ~ "Expansion")
  ) %>% 
  rename(Group = expand_ever, `Pre-Period` = postFALSE, `Post-Period` = postTRUE)

## Difference-in-Difference Model

copd_hi15_dd.est <- lm(formula = Rate ~ post + expand_ever + treat, data = copd_hi15_reg.data)
copd_hi15_fe.est <- feols(fm = Rate ~ treat | State + Year, data = copd_hi15_reg.data)
copd_hi15_fe.est2 <- feols(fm = Rate ~ treat | State + Year, data = copd_hi15_reg.data2)

## Event Study

copd_hi15_mod.twfe <- feols(fm = Rate ~ i(Year, expand_ever, ref = 2013) | State + Year, cluster = ~State, data = copd_hi15_reg.data)
copd_hi15_mod.twfe2 <- feols(fm = Rate ~ i(time_to_treat, expand_ever, ref = -1) | State + Year, cluster = ~State, data = copd_hi15_reg.data2)

copd_hi15_mod.twfe.plot <- data.frame(copd_hi15_mod.twfe$coeftable) %>%
  select(Estimate, SE = Std..Error) %>%
  mutate(Year = c(2011:2012, 2014:2019)) %>%
  rbind(., "Year::2013:expand_ever" = c(0, 0, 2013)) %>%
  ggplot(aes(x = Year, y = Estimate)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "black", linetype = 1) +
  geom_vline(xintercept = 2013, color = "black", linetype = 2) +
  geom_errorbar(aes(x = Year, ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE), width = .1) +
  scale_x_continuous(breaks = 2011:2019) +
  labs(x = "Time to Treatment", y = "Estimate and 95 Percent Confidence Interval", Title = "Event Study for Effects of Medicaid Expansion - States that Expanded in 2014") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

copd_hi15_mod.twfe2.plot <- data.frame(copd_hi15_mod.twfe2$coeftable) %>%
  select(Estimate, SE = Std..Error) %>%
  mutate(Year = c(-4:-2, 0:5)) %>%
  rbind(., "time_to_treat::-1:expand_ever" = c(0, 0, -1)) %>%
  ggplot(aes(x = Year, y = Estimate)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "black", linetype = 1) +
  geom_vline(xintercept = -1, color = "black", linetype = 2) +
  geom_errorbar(aes(x = Year, ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE), width = .1, position = position_dodge(width = 0.5)) +
  scale_x_continuous(breaks = -4:5) +
  labs(x = "Time to Treatment", y = "Estimate and 95 Percent Confidence Interval", Title = "Event Study for Effects of Medicaid Expansion - All ExpansionStates") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Depression ------------------------------------------------------------------

depr_hi15 <- MAExp_BRFSS %>%
  filter(Class == "Chronic Health Indicators", Topic == "Depression") %>%
  filter(Question == "Ever told you that you have a form of depression?", Response == "Yes") %>%
  filter(Break_Out_Category == "Household Income", Break_Out == "Less than $15,000") %>%
  select(Year, State, expand_year, expand_ever, expand, Response, Rate = Data_value)

## Regression Data

depr_hi15_reg.data <- depr_hi15 %>%
  filter(expand_year == 2014 | is.na(expand_year)) %>%
  mutate(post = (Year >= 2014), treat = post * expand_ever)

depr_hi15_reg.data2 <- depr_hi15 %>%
  mutate(treat = case_when(
    Year >= expand_year & !is.na(expand_year) ~ 1,
    is.na(expand_year) ~ 0,
    Year < expand_year & !is.na(expand_year) ~ 0)
  ) %>%
  mutate(time_to_treat = ifelse(expand_ever == TRUE, Year - expand_year, -1),
         time_to_treat = ifelse(time_to_treat <= -4, -4, time_to_treat))
# not enough data points for time_to_treat beyond 4 years in the past,
# since most states that have ever adopted expansion expanded in 2014

## Summary Statistics

depr_hi15_reg.data_sum <- depr_hi15_reg.data %>%
  mutate(Year = factor(Year)) %>%
  group_by(Year, expand_ever) %>% 
  summarise(`Mean` = mean(Rate, na.rm=TRUE),
            `Min` = min(Rate, na.rm=TRUE),
            `Q2` = quantile(Rate, 0.25, na.rm=TRUE),
            `Median` = median(Rate, na.rm=TRUE),
            `Q3` = quantile(Rate, 0.75, na.rm=TRUE),
            `Max` = max(Rate, na.rm=TRUE)) %>%
  pivot_wider(names_from = expand_ever, values_from = c("Mean", "Min", "Q2", "Median", "Q3", "Max")) %>%
  select(Year, Mean_FALSE, Min_FALSE, Q2_FALSE, Median_FALSE, Q3_FALSE, Max_FALSE, Mean_TRUE, Min_TRUE, Q2_TRUE, Median_TRUE, Q3_TRUE, Max_TRUE)
colnames(depr_hi15_reg.data_sum) <- c("Year", rep(c("Mean", "Min", "Q2", "Median", "Q3", "Max"), 2))

depr_hi15_plot <- depr_hi15_reg.data %>%
  group_by(Year, expand_ever) %>%
  summarise(Avg_Rate = mean(Rate, na.rm=TRUE)) %>%
  ggplot(aes(x = Year, y = Avg_Rate, color = expand_ever)) +
  geom_line(size = 1) + geom_point(size = 2) +
  geom_vline(xintercept = 2013.5, size = 1, color = "firebrick", linetype = "longdash") + 
  scale_x_continuous(breaks = c(2011:2019)) +
  scale_color_manual(name = "Medicaid Expansion Status", values = c("dodgerblue4", "dodgerblue1"), labels = c("Non-Expansion", "Expansion")) +
  geom_text(aes(label = paste0(round(Avg_Rate, 1), "%")), size = 3, nudge_x = 0, nudge_y = 0.5, check_overlap = TRUE, color = "black") +
  labs(x = "Year", y = "Avg Percent of People (%)", Title = "Avg Percent of Individuals of Household Income Less Than 15,000 with Poor or Fair Health Status") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Basic 2x2 DD Table 

depr_hi15_dd.table <- depr_hi15_reg.data %>%
  group_by(expand_ever, post) %>%
  summarise(avg_Rate = mean(Rate, na.rm=TRUE)) %>%
  pivot_wider(names_from = "post", names_prefix = "post", values_from = "avg_Rate") %>%
  ungroup() %>%
  mutate(expand_ever = case_when(
    expand_ever == FALSE ~ "Non-Expansion",
    expand_ever == TRUE ~ "Expansion")
  ) %>% 
  rename(Group = expand_ever, `Pre-Period` = postFALSE, `Post-Period` = postTRUE)

## Difference-in-Difference Model

depr_hi15_dd.est <- lm(formula = Rate ~ post + expand_ever + treat, data = depr_hi15_reg.data)
depr_hi15_fe.est <- feols(fm = Rate ~ treat | State + Year, data = depr_hi15_reg.data)
depr_hi15_fe.est2 <- feols(fm = Rate ~ treat | State + Year, data = depr_hi15_reg.data2)

## Event Study

depr_hi15_mod.twfe <- feols(fm = Rate ~ i(Year, expand_ever, ref = 2013) | State + Year, cluster = ~State, data = depr_hi15_reg.data)
depr_hi15_mod.twfe2 <- feols(fm = Rate ~ i(time_to_treat, expand_ever, ref = -1) | State + Year, cluster = ~State, data = depr_hi15_reg.data2)

depr_hi15_mod.twfe.plot <- data.frame(depr_hi15_mod.twfe$coeftable) %>%
  select(Estimate, SE = Std..Error) %>%
  mutate(Year = c(2011:2012, 2014:2019)) %>%
  rbind(., "Year::2013:expand_ever" = c(0, 0, 2013)) %>%
  ggplot(aes(x = Year, y = Estimate)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "black", linetype = 1) +
  geom_vline(xintercept = 2013, color = "black", linetype = 2) +
  geom_errorbar(aes(x = Year, ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE), width = .1) +
  scale_x_continuous(breaks = 2011:2019) +
  labs(x = "Time to Treatment", y = "Estimate and 95 Percent Confidence Interval", Title = "Event Study for Effects of Medicaid Expansion - States that Expanded in 2014") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

depr_hi15_mod.twfe2.plot <- data.frame(depr_hi15_mod.twfe2$coeftable) %>%
  select(Estimate, SE = Std..Error) %>%
  mutate(Year = c(-4:-2, 0:5)) %>%
  rbind(., "time_to_treat::-1:expand_ever" = c(0, 0, -1)) %>%
  ggplot(aes(x = Year, y = Estimate)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "black", linetype = 1) +
  geom_vline(xintercept = -1, color = "black", linetype = 2) +
  geom_errorbar(aes(x = Year, ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE), width = .1, position = position_dodge(width = 0.5)) +
  scale_x_continuous(breaks = -4:5) +
  labs(x = "Time to Treatment", y = "Estimate and 95 Percent Confidence Interval", Title = "Event Study for Effects of Medicaid Expansion - All ExpansionStates") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Save data for markdown ------------------------------------------------------

rm(list=c("hcc_gender_plotData"))
save.image("FP_FA_workspace.Rdata")

levels(factor(BRFSS$Topic))
"Arthritis", "Asthma", "Cardiovascular Disease", "COPD", "Diabetes", "Kidney", "Other Cancer", "Skin Cancer"
"Depression"
"BMI Categories", "Disability status", "Fair or Poor Health", "Overall Health"

levels(factor((MAExp_BRFSS %>% filter(Topic == "Cardiovascular Disease"))$Break_Out))
