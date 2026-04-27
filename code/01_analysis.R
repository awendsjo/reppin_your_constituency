## Replication code 'Reppin' your Constituency?'
## 01_analysis.R
## This scripts creates the tables and figures of the manuscript.
options(scipen=999)

# load packages

library(stargazer)
library(fixest)
library(openxlsx)
library(marginaleffects)
library(tidyverse)
library(pscl)
library(pscl)
library(sandwich)
library(lmtest)
library(modelsummary)

# load data

data <- read.csv("../data/dataset.csv")

# remove obs with missing county identifier, and those who are references to the other MP (location identifiers)
# also create binary vars

data <- data %>%
  filter(!is.na(county)) %>%
  filter(location_identifier != 1) %>%
  mutate(female = 
           case_when(
             gender == "woman" ~ 1,
             gender == "man" ~ 0,
             .default =  NA
           ),
         rural_born = case_when(
           urban_rural_born == "Rural" ~ 1,
           urban_rural_born == "Urban" ~ 0,
           .default = NA
         )
  )

# descriptive statistics table

vars <- c("is_minister","female","age","rural_born",
          "is_representative","is_born","urban_rural_binary","rural_pop_prop",
          "relative_unemployment",
          "Economy","Crime","Education","Environment","Healthcare")

var_labels <- c(
  "is_minister" = "Minister Status",
  "female" = "Female",
  "age" = "Age",
  "rural_born" = "Rural Background",
  "is_representative" = "Representative",
  "is_born" = "Born in County",
  "urban_rural_binary" = "Rural Location",
  "rural_pop_prop" = "Proportion Rural Population",
  "relative_unemployment" = "Relative Unemployment",
  "Economy" = "Economic Focus",
  "Crime" = "Crime Focus",
  "Education" = "Education Focus",
  "Environment" = "Environment Focus",
  "Healthcare" = "Healthcare Focus"
)

desc_data <- data[, vars]
names(desc_data) <- var_labels[vars]
stargazer(as.data.frame(desc_data),
          type = "html",
          summary.stat = c("mean", "sd", "min", "max"),
          title = "Descriptive Statistics",
          label = "tab:descriptives",
          digits = 2,
          out = "../outputs/tables/table2.html")

# how many are representative mentioning the county they rep?

mean(data$is_representative,na.rm=T)

# how many are representatives mentioning the county they were born?

mean(data$is_born,na.rm=T)

# how many mentions are to rural?

mean(data$urban_rural_binary,na.rm=T)

# average county rurality mention?

mean(data$rural_pop_prop)

# create data on politician, county, year level.

data_pcy <- data %>%
  filter(location_identifier == 0) %>%
  group_by(who, county, year) %>%
  summarise(
    n_mentions = n(),
    age = first(age),
    gender = first(gender),
    is_minister = first(is_minister),
    urban_rural_born = first(urban_rural_born),
    county_rep = first(county_rep),
    county_born = first(county_born),
    party = first(party),
    rural_pop_prop = first(rural_pop_prop),
    total_population = first(total_population),
    n_speeches = first(n_speeches),
    .groups = "drop"
  ) %>%
  group_by(who, year) %>%
  complete(
    county = unique(data$county),
    fill = list(n_mentions = 0)
  ) %>%
  ungroup() %>%
  group_by(who) %>%
  fill(age, gender, is_minister, urban_rural_born, county_rep, county_born,party,
       .direction = "downup") %>%
  ungroup() %>%
  group_by(county, year) %>%
  fill(rural_pop_prop, total_population, .direction = "downup") %>%
  group_by(who, year) %>%
  fill(n_speeches, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    is_representative = ifelse(is.na(county_rep), NA, as.numeric(county == county_rep)),
    is_born = ifelse(is.na(county_born), NA, as.numeric(county == county_born))
  )

data_pcy$any_mention <- as.numeric(data_pcy$n_mentions > 0)

# only non-zero values about 23% of the time

mean(data_pcy$any_mention)

# create binary vars

data_pcy <- data_pcy %>%
  mutate(female = 
           case_when(
    gender == "woman" ~ 1,
    gender == "man" ~ 0,
    .default =  NA
  ),
  rural_born = case_when(
    urban_rural_born == "Rural" ~ 1,
    urban_rural_born == "Urban" ~ 0,
    .default = NA
  )
  )

# factors

data_pcy$county <- factor(data_pcy$county)
data_pcy$year <- factor(data_pcy$year)
data_pcy$party <- factor(data_pcy$party)

# estimate models for table 3

m1 <- feols(n_mentions~is_representative,
            data = data_pcy,
            cluster = ~ who+county^year)
m2 <- feols(n_mentions~is_representative*rural_pop_prop+rural_born,
            data = data_pcy,
            cluster = ~ who+county^year)
m3 <- feols(n_mentions~is_representative*rural_pop_prop+
              rural_born + age + female + is_minister + is_born,
            data = data_pcy,
            cluster = ~ who+county^year)
m4 <- feols(n_mentions~is_representative*rural_pop_prop + 
              rural_born+ age + female + is_minister + is_born | year+county+party,
            data = data_pcy,
            cluster = ~ who+county^year)

etable(m1,m2,m3,m4,
       cluster = ~who + county^year,
       fixef.group = list("County" = "county", "Year" = "year"),
       signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
       adjustbox = TRUE,
       file = "../outputs/tables/table3.tex")

# estimate models for table 6

data_pcy$county_year <- interaction(data_pcy$year,data_pcy$county)

m4_zinb_offset <- zeroinfl(
  n_mentions ~ is_representative + is_representative:rural_pop_prop + rural_pop_prop +
    age + female + is_minister + rural_born+is_born+
    offset(log(n_speeches)),
  data = data_pcy,
  dist = "negbin",
  link = "logit"
)

rows <- as.integer(rownames(model.frame(m4_zinb_offset)))

m4_zinb_offset_vcov <- vcovCL(
  m4_zinb_offset,
  cluster = data.frame(data_pcy[rows,]$who,data_pcy[rows,]$county_year)
)

coeftest(m4_zinb_offset, vcov = m4_zinb_offset_vcov)

modelsummary(
  m4_zinb_offset,
  vcov      = m4_zinb_offset_vcov,
  stars     = c("." = 0.1, "*" = 0.05, "**" = 0.01, "***" = 0.001),
  notes     = "Offset: log(n_speeches). Clustered standard erorrs (by politician and county-year) in parantheses.",
  output="../outputs/tables/table6.docx" 
)

# mention level analysis

# set as factors

data$county <- factor(data$county)
data$year <- factor(data$year)
data$party <- factor(data$party)

# estimate models for table 5

m5 <- feols(Economy ~ relative_unemployment,
            data = data,
            cluster = ~who+county^year)

m6 <- feols(Economy ~ relative_unemployment*is_representative,
            data = data,
            cluster = ~who+county^year)

m7 <- feols(Economy ~ relative_unemployment*is_representative+
              age+female+is_minister+is_born,
            data = data,
            cluster = ~who+county^year)

m8 <- feols(Economy ~ relative_unemployment*is_representative+
              age+female+is_minister+is_born
            | year + county+party,
            data = data,
            cluster = ~who+county^year)

etable(m5, m6, m7, m8,
       cluster = ~who + county^year,
       fixef.group = list("County" = "county", "Year" = "year"),
       signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
       adjustbox = TRUE,
       tex = T,
       file = "../outputs/tables/table5.tex")

# estimate models for Figure 5

# fit model with robust standard errors

data$is_representative <- factor(data$is_representative)
levels(data$is_representative) <- c("Not Representative","Representative")

m9_robust <- feols(Economy ~ relative_unemployment*is_representative*rural_pop_prop+
                     age+gender+is_minister+is_born
                   | county+year+party,
                   data = data,
                   vcov="hetero")

# fit model with clustered standard errors
m9_cluster <- feols(Economy ~ relative_unemployment*is_representative*rural_pop_prop+
                      age+gender+is_minister+is_born
                    | county+year+party,
                    data = data,
                    cluster=~who+county^year)

etable(m9_robust, m9_cluster,
       fixef.group = list("County" = "county", "Year" = "year", "Party" = "party"),
       signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
       adjustbox = TRUE,
       tex = T,
       headers = c("Robust SE", "Clustered SE"),
       file = "../outputs/tables/table_appendix_1.tex")

# Get marginal effects for both

mfx_robust <- slopes(m9_robust, variables = "relative_unemployment", 
                     newdata = datagrid(is_representative = c("Not Representative","Representative"),
                                        rural_pop_prop = c(0.09,0.34,0.46,0.67)),
                     conf_level = 0.95)
mfx_robust$se_type <- "Robust"

mfx_cluster <- slopes(m9_cluster, variables = "relative_unemployment", 
                      newdata = datagrid(is_representative = c("Not Representative","Representative"),
                                         rural_pop_prop = c(0.09,0.34,0.46,0.67)),
                      conf_level = 0.95)
mfx_cluster$se_type <- "Clustered"

mfx_df <- rbind(mfx_robust, mfx_cluster)
mfx_df$dodge_pos <- ifelse(mfx_df$is_representative == "Not Representative", -0.015, 0.015)
mfx_df$x_dodged <- mfx_df$rural_pop_prop + mfx_df$dodge_pos

png(file="../outputs/figures/figure5.png",
    width=12, height=10, units="cm", res=600)
ggplot(mfx_df, aes(x = x_dodged, y = estimate, 
                   color = factor(is_representative), fill = factor(is_representative))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, 
                      linewidth = se_type)) + 
  geom_line(aes(linetype = se_type, group = interaction(is_representative, se_type))) +
  scale_linewidth_manual(values = c("Robust" = 1.2, "Clustered" = 0.4)) +
  scale_linetype_manual(values = c("Robust" = "solid", "Clustered" = "dashed")) +
  scale_x_continuous(breaks = c(0.09, 0.34, 0.46, 0.67),
                     labels = c("0.09\nStockholm",
                                "0.34\nVästra Götaland\n",
                                "0.46\nBlekinge",
                                "0.67\nJämtland")) +
  theme_bw() + 
  ylab("Marginal effect of Unemployment") +
  xlab("Proportion of Rural Population") + 
  labs(color = "", fill = "") +
  guides(linewidth = "none", linetype = "none") +
  theme(legend.position = "top")
dev.off()

# estimate models for Figure 8 and get marginal effects

topics <- c("Economy", "Crime", "Education", "Environment", "Healthcare")

results_list <- lapply(topics, function(topic) {
  formula <- as.formula(paste0(topic, " ~ relative_unemployment*is_representative + 
                                age + gender + is_minister + is_born | 
                                year+county+party"))
  
  model_robust <- feols(formula, data = data, vcov = "hetero")
  
  model_cluster <- feols(formula, data = data, cluster = ~who+county^year)
  
  mfx_robust <- slopes(model_robust, 
                       variables = "relative_unemployment", 
                       newdata = datagrid(is_representative = c("Not Representative","Representative")),
                       conf_level = 0.95)
  mfx_robust$se_type <- "Robust"
  mfx_robust$topic <- topic
  
  mfx_cluster <- slopes(model_cluster, 
                        variables = "relative_unemployment", 
                        newdata = datagrid(is_representative = c("Not Representative","Representative")),
                        conf_level = 0.95)
  mfx_cluster$se_type <- "Clustered"
  mfx_cluster$topic <- topic
  
  mfx_combined <- rbind(as.data.frame(mfx_robust), as.data.frame(mfx_cluster))
  
  return(mfx_combined)
})

mfx_all <- bind_rows(results_list)

mfx_all$topic <- factor(mfx_all$topic, levels = topics)
mfx_all$topic_num <- as.numeric(mfx_all$topic)
mfx_all$dodge_pos <- ifelse(mfx_all$is_representative == "Not Representative", -0.25, 0.25)
mfx_all$x_dodged <- mfx_all$topic_num + mfx_all$dodge_pos

png(file="../outputs/figures/figure8.png",
    width=12, height=10, units="cm", res=600)
ggplot(mfx_all, aes(x = x_dodged, y = estimate, 
                    color = factor(is_representative), 
                    fill = factor(is_representative))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high,
                      linewidth = se_type),
                  position = position_identity()) +
  scale_linewidth_manual(values = c("Robust" = 1.2, "Clustered" = 0.4)) +
  scale_x_continuous(breaks = 1:length(topics),
                     labels = topics) +
  theme_bw() + 
  ylab("Marginal effect of Unemployment") + 
  xlab("Topic") + 
  labs(color = "", fill = "") +
  guides(linewidth = "none") +
  theme(legend.position = "top")
dev.off()

# estimate models for table 4

# explaining urban-rural appeals, mention level

# get rurality for representative county
county_year_pop <- read.csv("../data/county_rurality.csv")

county_year_pop$rural_pop_prop_rep <- county_year_pop$rural_pop_prop

data <- merge(data,county_year_pop[c("county","year","rural_pop_prop_rep")],by.x=c("county_rep","year"),by.y=c("county","year"),all.x=T)

m10 <- feols(urban_rural_binary ~ rural_pop_prop+rural_pop_prop_rep,
             data = data,
             cluster = ~who + county^year)

m11 <- feols(urban_rural_binary ~ rural_pop_prop + rural_born +
               is_representative + age + gender + is_minister + is_born,
             data = data,
             cluster = ~who + county^year)

m12 <- feols(urban_rural_binary ~ rural_born + rural_pop_prop +
               is_representative + age + gender + is_minister + is_born+rural_pop_prop_rep
             |  year+party,
             data = data,
             cluster = ~who + county^year)

etable(m10, m11, m12,
       cluster = ~who + county^year,
       fixef.group = list("County" = "county", "Year" = "year"),
       signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
       adjustbox = TRUE,
       tex = T,
       file = "../outputs/tables/table4.tex")

data$urban_rural_born <- factor(data$urban_rural_born)

levels(data$urban_rural_born) <- c("Rural Background","Urban Background")

m13_robust <- feols(Economy ~ relative_unemployment*is_representative*rural_pop_prop*urban_rural_born+
                      age+gender+is_minister+is_born
                    | county+year,
                    data = data,
                    vcov="hetero")
m13_cluster <- feols(Economy ~ relative_unemployment*is_representative*rural_pop_prop*urban_rural_born+
                       age+gender+is_minister+is_born
                     | county+year,
                     data = data,
                     cluster = ~who + county^year)

etable(m13_robust, m13_cluster,
       fixef.group = list("County" = "county", "Year" = "year"),
       signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
       adjustbox = TRUE,
       tex = TRUE,
       headers = c("Robust SE", "Clustered SE"),
       file = "../outputs/tables/table_appendix2.tex")


mfx_robust <- slopes(m13_robust, variables = "relative_unemployment", 
                     newdata = datagrid(is_representative = c("Not Representative","Representative"),
                                        rural_pop_prop = c(0.09,0.34,0.46,0.67),
                                        urban_rural_born=c("Rural Background","Urban Background")),
                     conf_level = 0.95)
mfx_robust$se_type <- "Robust"

mfx_cluster <- slopes(m13_cluster, variables = "relative_unemployment", 
                      newdata = datagrid(is_representative = c("Not Representative","Representative"),
                                         rural_pop_prop = c(0.09,0.34,0.46,0.67),
                                         urban_rural_born=c("Rural Background","Urban Background")),
                      conf_level = 0.95)
mfx_cluster$se_type <- "Clustered"

mfx_df <- rbind(mfx_robust, mfx_cluster)

mfx_df$dodge_pos <- ifelse(mfx_df$is_representative == "Not Representative", -0.015, 0.015)
mfx_df$x_dodged <- mfx_df$rural_pop_prop + mfx_df$dodge_pos

png(file="../outputs/figures/figure6.png",
    width=22, height=10, units="cm", res=600)
ggplot(mfx_df, aes(x = x_dodged, y = estimate, 
                   color = factor(is_representative), fill = factor(is_representative))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, 
                      linewidth = se_type)) + 
  geom_line(lty="dashed") +
  scale_linewidth_manual(values = c("Robust" = 1.2, "Clustered" = 0.4)) +
  scale_x_continuous(breaks = c(0.09, 0.34, 0.46, 0.67),
                     labels = c("0.09\nStockholm",
                                "0.34\nVästra Götaland\n",
                                "0.46\nBlekinge",
                                "0.67\nJämtland")) +
  theme_bw() + 
  ylab("Marginal effect of Unemployment") +
  xlab("Proportion of Rural Population") + 
  labs(color = "", fill = "") +
  guides(linewidth = "none", alpha = "none", linetype = "none") +
  theme(legend.position = "top") +
  facet_wrap(~urban_rural_born, nrow=1)
dev.off()

# party heterogenity

parties <- c("Vänsterpartiet","Socialdemokraterna","Miljöpartiet","Centerpartiet","Liberalerna",
             "Moderaterna","Kristdemokraterna","Sverigedemokraterna","Ny demokrati")

parties_short <- c("V","S","MP","C","L",
                   "M","KD","SD","NyD")

m14_robust <- feols(Economy ~ relative_unemployment*is_representative*party+
                      age+gender+is_minister+is_born
                    | county+year,
                    data = data,
                    vcov="hetero")

m14_cluster <- feols(Economy ~ relative_unemployment*is_representative*party+
                       age+gender+is_minister+is_born
                     | county+year,
                     data = data,
                     cluster = ~who + county^year)

mfx_robust <- slopes(m14_robust, variables = "relative_unemployment", 
                     newdata = datagrid(is_representative = c("Not Representative","Representative"),
                                        party = parties),
                     conf_level = 0.95)
mfx_robust$se_type <- "Robust"

mfx_cluster <- slopes(m14_cluster, variables = "relative_unemployment", 
                      newdata = datagrid(is_representative = c("Not Representative","Representative"),
                                         party = parties),
                      conf_level = 0.95)
mfx_cluster$se_type <- "Clustered"

mfx_party <- rbind(mfx_robust, mfx_cluster)
mfx_party$party <- factor(mfx_party$party, levels = parties)
mfx_party$party_num <- as.numeric(mfx_party$party)
mfx_party$dodge_pos <- ifelse(mfx_party$is_representative == "Not Representative", -0.1, 0.1)
mfx_party$x_dodged <- mfx_party$party_num + mfx_party$dodge_pos

png(file="../outputs/figures/figure7a.png",
    width=10, height=8, units="cm", res=600)
ggplot(mfx_party, aes(x = x_dodged, y = estimate, 
                      color = factor(is_representative), 
                      fill = factor(is_representative))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, 
                      linewidth = se_type)) +
  scale_linewidth_manual(values = c("Robust" = 1.2, "Clustered" = 0.4)) +
  scale_x_continuous(breaks = 1:length(parties),
                     labels = parties_short) +
  theme_bw() + 
  ylab("Marginal effect of Unemployment") + 
  xlab("Party") + 
  labs(color = "", fill = "") +
  guides(linewidth = "none") +
  theme(legend.position = "top")
dev.off()

m15_robust <- feols(n_mentions~is_representative*party+ 
                      age + gender + is_minister + is_born | year+county,
                    data = data_pcy,
                    vcov="hetero")

m15_cluster <- feols(n_mentions~is_representative*party+ 
                       age + gender + is_minister + is_born | year+county,
                     data = data_pcy,
                     cluster = ~ who+county^year)

mfx_robust <- slopes(m15_robust, variables = "is_representative", 
                     newdata = datagrid(party = parties),
                     conf_level = 0.95)
mfx_robust$se_type <- "Robust"
mfx_cluster <- slopes(m15_cluster, variables = "is_representative", 
                      newdata = datagrid(party = parties),
                      conf_level = 0.95)
mfx_cluster$se_type <- "Clustered"
mfx_party <- rbind(mfx_robust, mfx_cluster)
mfx_party$party <- factor(mfx_party$party, levels = parties)
mfx_party$party_num <- as.numeric(mfx_party$party)

png(file="../outputs/figures/figure7b.png",
    width=10, height=6.5, units="cm", res=600)
ggplot(mfx_party, aes(x = party_num, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, 
                      linewidth = se_type)) +
  scale_linewidth_manual(values = c("Robust" = 1.2, "Clustered" = 0.4)) +
  scale_x_continuous(breaks = 1:length(parties),
                     labels = parties_short) +
  theme_bw() + 
  ylab("Effect of Being Representative") + 
  xlab("Party") + 
  guides(linewidth = "none") +
  theme(legend.position = "bottom")
dev.off()