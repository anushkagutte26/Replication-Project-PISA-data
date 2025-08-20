# Libraries from original paper and for replicatiopn
library(dplyr)        # data wrangling
library(tidyr)        # data cleaning
library(ggplot2)      # visualization
library(scales)       # axis & scale formatting
library(gridExtra)    # arrange multiple plots

library(haven)        # import Stata/SPSS/SAS data
library(openxlsx)     # export tables to Excel
library(writexl)      # alternative Excel export

library(lme4)         # mixed-effects models
library(WeMix)        # weighted mixed models
library(broom)        # tidy model outputs
library(broom.mixed)  # tidy mixed-model outputs

library(modelsummary) # regression tables


#Data
wd <- "/Users/apple/Downloads/dataverse_files SES and school"
setwd(wd)
d <- wd

md_completo <- readRDS("md_lat_2018_completo.rds")
md          <- readRDS("md_lat_2018.rds")


# Variables of interest
va  <- c("Disciplina","Apoyo","Dirigida","Retroalimentacion","Estimulacion",
         "Adaptacion","Entusiasmo","Materiales","Personal")

va2 <- paste0(va, "xESECB")

va3 <- c(va, "Edad","Repeticion","Disfrutar_lectura","ESECB")


# Data processing (from the paper)

md <- md %>%
  mutate(CNT = recode(CNT,
                      "ARG" = "Argentina", 
                      "BRA" = "Brasil",
                      "CHL" = "Chile",
                      "COL" = "Colombia",
                      "URY" = "Uruguay"))

md2 <- md %>%
  mutate(across(matches("ESECB"), as.factor),
         ESECB = recode(ESECB, "0" = "Medio y alto", "1" = "Bajo"),
         ESECB = relevel(ESECB, ref = "Bajo"))


# Benchmark setting

ben <- pisa.ben.pv(pvlabel = paste0("PV",1:10,"READ"), 
                   cutoff = c(552.88,552.889), 
                   atlevel=TRUE, by = c("CNT","ESECB"), data = md2)

ben2 <- ben %>%
  filter(Benchmarks != "(552.88,552.889]") %>%
  mutate(`Alto desempeño` = case_when(
            Benchmarks == "<= 552.88"  ~ "Bajo desempeño",
            Benchmarks == "> 552.889" ~ "Alto desempeño"),
         `Alto desempeño` = relevel(as.factor(`Alto desempeño`), ref="Alto desempeño")) %>%
  group_by(CNT,ESECB) %>%
  mutate(posy = cumsum(Percentage)-4) %>%
  drop_na(`Alto desempeño`)


# Bivariate logistic models

mod_bi <- lapply(va, function(v) {
  pisa.log.pv(pvlabel=paste0("PV",1:10,"READ"),
              x=v, cutoff=552.89, data=md, export=FALSE)
})
names(mod_bi) <- va

#  results
bi_results <- do.call(rbind, lapply(mod_bi, function(m) m[2,1:3]))


# Descriptives (mean & SD)

md_dct <- lapply(va, function(v) {
  dc <- pisa.mean(variable=v, by=c("CNT","ESECB"), data=md2) %>%
    mutate(Variable = v) %>%
    select(Variable, CNT, ESECB, Mean, SD) %>%
    pivot_wider(names_from = ESECB, values_from = c("Mean","SD"))
  dc
})
md_dct_m_sd <- bind_rows(md_dct)

# Univariate regressions
mdctt <- lapply(va, function(v) {
  mc <- pisa.reg(y=v, x="ESECB", by="CNT", data=md2)
  out <- bind_rows(lapply(names(mc), function(ctry) {
    data <- mc[[ctry]]$reg
    data <- data[row.names(data)=="ESECBMedio y alto", 
                 c("Estimate","t value")]
    data$Variable <- v
    data$CNT <- ctry
    data
  }))
  out
})
mdctt <- bind_rows(mdctt)

mdctt$sig <- case_when(
  abs(mdctt$`t value`) >= 2.576 ~ "**",
  abs(mdctt$`t value`) >= 1.960 ~ "*",
  abs(mdctt$`t value`) >= 1.645 ~ "+",
  TRUE ~ ""
)

# Merge descriptives + univariate models
mdctt_completo <- inner_join(md_dct_m_sd, mdctt, by=c("Variable","CNT")) %>%
  rename("País"="CNT") %>%
  select(Variable, País, starts_with("Mean"), Estimate, sig, starts_with("SD"))

write.xlsx(mdctt_completo, file.path(d,"Descriptivos por ESECB y CNT.xlsx"),
           rowNames=FALSE, overwrite=TRUE)


# Multivariate logistic models - recreating main table from the paper - code self

mod1 <- pisa.log.pv(pvlabel=paste0("PV",1:10,"READ"),
                    x=c("Disciplina",
                        "Apoyo",
                        "Dirigida",
                        "Retroalimentacion",
                        "Estimulacion",
                        "Adaptacion", 
                        "Entusiasmo",
                        "Materiales",
                        "Personal",
                        "ESECB",
                        "GRADE",
                        "Femenino",
                        "Publico"),
                    by="CNT", cutoff=552.89,data=md)

summary(mod1)
########## Working with same data and applying multilevel modelling #################

# Chile subset

chile_data <- md %>% filter(CNT == "Chile") %>%
  mutate(norm_weight = W_FSTUWT/mean(W_FSTUWT))

# Binary PVs
pv_vars <- paste0("PV",1:10,"READ")
chile_data <- chile_data %>%
  mutate(across(all_of(pv_vars), ~ ifelse(. > 552.89, 1, 0), 
                .names="high_{.col}"))

pv_binary <- paste0("high_", pv_vars)

# Example multilevel logistic (PV1 only)
model_pv1 <- glmer(
  high_PV1READ ~ ESECB + Disciplina + Apoyo + Dirigida + Retroalimentacion +
    Estimulacion + Adaptacion + Entusiasmo + Materiales + Personal +
    Femenino + GRADE + Publico + (1 | CNTSCHID),
  data = chile_data, family = binomial(link="logit"),
  weights = norm_weight)
                                    
summary(model_pv1)
