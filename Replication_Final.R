
library(lme4)
library(broom.mixed)


library(gridExtra)
library(tidyr)
library(intsvy)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(stargazer)
library(writexl)
library(officer)
library(rvg)
library(scales)
library(haven)

getAnywhere(recode)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


#3) Directorios de trabajo y de graficos------
wd <- "/Users/apple/Downloads/dataverse_files SES and school"
setwd(wd)

d <- "/Users/apple/Downloads/dataverse_files SES and school"

#4) Cargar marco de datos----
md_completo <- readRDS(file = "md_lat_2018_completo.rds")

md <- readRDS(file = "md_lat_2018.rds")

#Variables de inter?s
va <- c("Disciplina","Apoyo","Dirigida","Retroalimentacion",
        "Estimulacion","Adaptacion", 
        "Entusiasmo","Materiales", "Personal")
va2 <- c("DisciplinaxESECB",
         "ApoyoxESECB","DirigidaxESECB",
         "RetroalimentacionxESECB","EstimulacionxESECB",
         "AdaptacionxESECB","EntusiasmoxESECB", 
         "MaterialesxESECB","PersonalxESECB")
va3 <- c("Disciplina","Apoyo","Dirigida","Retroalimentacion",
         "Estimulacion","Adaptacion", 
         "Entusiasmo","Materiales", "Personal",
         "Edad","Repeticion","Disfrutar_lectura","ESECB")


#N?mero de personas en muestra original
summary(factor(md_completo$CNT))
#N?mero de personas en muestra final
summary(factor(md$CNT))
#Porcentaje de muestra 
round(summary(factor(md$CNT))/summary(factor(md_completo$CNT)),2)
#Porcentaje de la muestra final respecto a la original por sexo
round(100*(table(md$CNT,md$Femenino)/table(md_completo$CNT,md_completo$Femenino)),1)

#5) Procesar datos------
md <- md %>%
  mutate(CNT = dplyr::recode(CNT,
                             "ARG" = "Argentina", 
                             "BRA" = "Brasil",
                             "CHL" = "Chile",
                             "COL" = "Colombia",
                             "URY" = "Uruguay"))

md2 <- md %>%
  mutate(across(matches("ESECB"), as.factor),
         ESECB= dplyr::recode(ESECB, "0" = "Medio y alto", 
                              "1" = "Bajo"),
         ESECB = relevel(ESECB,levels,ref="Bajo"))



#6) Analisis exploratorios---------

#a) Porcentaje de estudiantes por pa?s y por ESECB por niveles de desempe?o
ben<- pisa.ben.pv(pvlabel = paste0("PV",1:10,"READ"), cutoff = c(552.88,552.889), 
                  atlevel=TRUE,by = c("CNT","ESECB"), data = md2)
ben2 <- ben %>%
  filter(Benchmarks != "(552.88,552.889]") %>%
  mutate(`Alto desempeño` = case_when(Benchmarks == "<= 552.88" ~ "Bajo desempeño",
                                      Benchmarks == "> 552.889" ~ "Alto desempeño"),
         across(matches("Alto desempeño"),as.factor),
         `Alto desempeño` = relevel(`Alto desempeño`,levels,
                                    ref="Alto desempeño")) %>%
  group_by(CNT,ESECB) %>%
  mutate(posy = cumsum(Percentage)-4) %>%
  na.omit(`Alto desempeño`)


#c) Modelos logit bivariados

#Disciplina
modbi1 <- pisa.log.pv(pvlabel=paste0("PV",1:10,"READ"),
                      x=c("Disciplina"), 
                      cutoff=552.89,data=md,
                      export=F)
modbi1

#Apoyo
modbi2 <- pisa.log.pv(pvlabel=paste0("PV",1:10,"READ"), 
                      x=c("Apoyo"), 
                      cutoff=552.89,data=md,
                      export=F)
modbi2

#Dirigida
modbi3 <- pisa.log.pv(pvlabel=paste0("PV",1:10,"READ"), 
                      x=c("Dirigida"), 
                      cutoff=552.89,data=md,
                      export=F)
modbi3

#Retroalimentacion
modbi4 <- pisa.log.pv(pvlabel=paste0("PV",1:10,"READ"),
                      x=c("Retroalimentacion"),
                      cutoff=552.89,data=md,
                      export=F)

modbi4

#Estimulacion
modbi5 <- pisa.log.pv(pvlabel=paste0("PV",1:10,"READ"),
                      x=c("Estimulacion"),
                      cutoff=552.89,data=md,
                      export=F)

modbi5


#Adaptacion
modbi6 <- pisa.log.pv(pvlabel=paste0("PV",1:10,"READ"),
                      x=c("Adaptacion"),
                      cutoff=552.89,data=md,
                      export=T,name="model",folder=d)

modbi6

#Entusiasmo
modbi7 <- pisa.log.pv(pvlabel=paste0("PV",1:10,"READ"),
                      x=c("Entusiasmo"),
                      cutoff=552.89,data=md,
                      export=T,name="model",folder=d)

modbi7

# Materiales
modbi8 <- pisa.log.pv(pvlabel=paste0("PV",1:10,"READ"),
                      x=c("Materiales"),
                      cutoff=552.89,data=md,
                      export=T,name="model",folder=d)

modbi8

# Personal
modbi9 <- pisa.log.pv(pvlabel=paste0("PV",1:10,"READ"),
                      x=c("Personal"),
                      cutoff=552.89,data=md,
                      export=T,name="model",folder=d)

modbi9

#Juntar todas las correlaciones bivariadas
rbind(modbi1[2,1:3],
      modbi2[2,1:3],
      modbi3[2,1:3],
      modbi4[2,1:3],
      modbi5[2,1:3],
      modbi6[2,1:3],
      modbi7[2,1:3],
      modbi8[2,1:3],
      modbi9[2,1:3])

#d)Medias y desviaciones estandares de las variables de clima escolar por ESECB
#1) Calcular descriptivos mean y sd
#Objeto nulo
md_dct=NULL

for(i in 1:9) {
  dc<-pisa.mean(variable=va[i],
                by=c("CNT","ESECB"),
                data=md2)
  dc <- dc %>%
    mutate(Variable = va[i] ) %>%
    dplyr::select(Variable, CNT, ESECB, Mean, SD)%>%
    pivot_wider(names_from = ESECB,
                values_from = c("Mean","SD"))
  
  md_dct[[i]] <- dc
  print(i)
}

#Rbind todos los elmentos de la lista anterior
md_dct_m_sd<-bind_rows(md_dct)

#Modelos univariados
#Objeto nulo
mdct=NULL
mdctt = NULL
#Estimar modelos univariados y extraer datos de interes
for(i in 1:9) {
  mc <- pisa.reg(y=va[i],x="ESECB",by="CNT",
                 data=md2)
  for (j in 1:length(mc)){
    data <- mc[[j]]$reg
    data <- data[,c("Estimate", "t value")]
    data$variable <- row.names(data)
    data$Variable = va[i]
    data$CNT <- names(mc)[j]
    data <- filter(data,variable=="ESECBMedio y alto")
    data <- data[,c("Estimate","t value","CNT","Variable")]
    mdct[[j]] <-  data
  }
  mdctt[[i]]<-bind_rows(mdct)
  
  print(i)
}

#Rbind todos los elementos de la lista anterior
mdctt<-bind_rows(mdctt)
#NUll nombres de filas del marco de datos anterior
rownames(mdctt) <- NULL


mdctt$sig <- ifelse(abs(mdctt$`t value`) >= 1.645 & 
                      abs(mdctt$`t value`)<1.960,"+",
                    ifelse( abs(mdctt$`t value`) >= 1.960 & 
                              abs(mdctt$`t value`)<2.576,"*",
                            ifelse(abs(mdctt$`t value`) >= 2.576,"**","")))


#Juntar los descriptivos y los modelos univariados
mdctt_completo <- inner_join(md_dct_m_sd,mdctt,
                             by = c("Variable", "CNT"))

#Formatear el marco de datos mctt_completo
mdctt_completo <- mdctt_completo %>%
  rename("País"="CNT",
         "Media grupo ESECB = 1" = "Mean_Bajo",
         "Media grupo ESECB = 0" = "Mean_Medio y alto",
         "DE grupo ESECB = 1"="SD_Bajo",
         "DE grupo ESECB = 0"="SD_Medio y alto",
         "Diferencia estimada de medias" ="Estimate",
         "Valor t" ="t value") %>%
  dplyr::select(Variable,`País`,
                `Media grupo ESECB = 0`,`Media grupo ESECB = 1`,
                `Diferencia estimada de medias`,sig,
                `DE grupo ESECB = 0`,`DE grupo ESECB = 1`)



write.xlsx(mdctt_completo, paste(d,"Descriptivos por ESECB y CNT.xlsx",sep=""),
           rowNames=F,overwrite = T)


#7)Modelos logisticos------
#Modelo por paises
#Contabiliizar tiempo
tiempo_inicio <- Sys.time()
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
                    by="CNT", cutoff=552.89,data=md,
                    export=T,name="model",folder=d)


################################################################################
######################### CHILE DATA - HEIRARCHICAL MODEL ######################
################################################################################


# Prepare data
chile_data <- md %>% filter(CNT == "Chile") 
chile_data$norm_weight <- chile_data$W_FSTUWT/mean(chile_data$W_FSTUWT)

# Create binary outcomes for all PVs
pv_vars <- paste0("PV", 1:10, "READ")
chile_data <- chile_data %>% 
  mutate(across(all_of(pv_vars), ~ ifelse(. > 552.89, 1, 0), .names = "high_{.col}"))

pv_binary <- paste0("high_PV", 1:10, "READ")

# Store results
results <- list()

for (pv in pv_binary) {
  formula <- as.formula(paste(pv, "~ ESECB + Disciplina + Apoyo + Dirigida + Retroalimentacion + Estimulacion + Adaptacion + Entusiasmo + 
                      Materiales + Personal + Femenino + GRADE + Publico +  (1 | CNTSCHID)"))
  
  model <- glmer(formula, 
                 data = chile_data,
                 family = binomial,
                 weights = norm_weight,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)))
  
  results[[pv]] <- tidy(model, effects = "fixed", conf.int = TRUE)
}

uncond_model <- glmer(
  high_PV1READ ~ 1 + (1 | CNTSCHID), data = chile_data, family = binomial(link = "logit")
)

summary(uncond_model)

# Combine results
final_results <- bind_rows(results, .id = "PV")

# Calculate average coefficients across PVs
pooled_results <- final_results %>%
  group_by(term) %>%
  summarize(estimate = mean(estimate),
            std.error = mean(std.error))


library(dplyr)
library(broom)

results <- list()

for (pv in pv_binary) {
  formula2 <- as.formula(paste(
    pv, "~ ESECB + Disciplina + Apoyo + Dirigida + Retroalimentacion + Estimulacion + Adaptacion + Entusiasmo + 
          Materiales + Personal + Femenino + GRADE + Publico"
  ))
  
  model <- glm(
    formula2,
    data = chile_data,
    family = binomial,
    weights = norm_weight
  )
  
  results[[pv]] <- tidy(model, conf.int = TRUE)
}

# Combine results
final_results2 <- bind_rows(results, .id = "PV")

# Calculate average coefficients across PVs
pooled_results2 <- final_results %>%
  group_by(term) %>%
  summarize(
    estimate = mean(estimate, na.rm = TRUE),
    std.error = mean(std.error, na.rm = TRUE)
  )




chile_data <- chile_data %>%
  dplyr::mutate(meanPVREAD = rowMeans(select(., starts_with("PV") & ends_with("READ")), na.rm = TRUE))

chile_data <- chile_data %>%
  mutate(meanPVREAD = rowMeans(across(PV1READ:PV10READ), na.rm = TRUE))

chile_data <- chile_data %>%
  mutate(HighRead = ifelse(meanPVREAD > 552.89, 1, 0))


chile_model <- glmer(HighRead ~ ESECB + Disciplina + Apoyo + Dirigida + Retroalimentacion + Estimulacion + Adaptacion + Entusiasmo + 
                      Materiales + Personal + Femenino + GRADE + Publico + (1 | CNTSCHID), 
                    data = chile_data, family = binomial)

chile_model_linear <- lmer(meanPVREAD ~ ESECB + Disciplina + Apoyo + Dirigida + Retroalimentacion + Estimulacion + Adaptacion + Entusiasmo + 
                       Materiales + Personal + Femenino + GRADE + Publico + (1 | CNTSCHID), 
                     data = chile_data)

chile_model_original <- glm(HighRead ~ ESECB + Disciplina + Apoyo + Dirigida + Retroalimentacion + Estimulacion + Adaptacion + Entusiasmo + 
                              Materiales + Personal + Femenino + GRADE + Publico, 
                            data = chile_data, family = binomial)



summary(chile_model)

summary(chile_model_original)

summary(pooled_results)









# Define list of variables
school_vars <- c("Disciplina", "Apoyo", "Dirigida", "Retroalimentacion", "Estimulacion", 
                 "Adaptacion", "Entusiasmo", "Materiales", "Personal")

# Plausible values
pv_binary <- paste0("PV", 1:10, "READ")

# List to store results
results_bivariate <- list()

# Bivariate logistic regression loop
for (var in school_vars) {
  model_list <- list()
  
  for (pv in pv_binary) {
    formula <- as.formula(paste0(pv, " ~ ", var))
    
    model <- glm(
      formula,
      data = chile_data,
      family = binomial,
      weights = norm_weight
    )
    
    model_list[[pv]] <- tidy(model, conf.int = TRUE)
  }
  
  # Average the results across PVs
  combined <- bind_rows(model_list, .id = "PV") %>%
    group_by(term) %>%
    summarize(estimate = mean(estimate, na.rm = TRUE),
              std.error = mean(std.error, na.rm = TRUE))
  
  results_bivariate[[var]] <- combined
}

# Example: View results for "Disciplina"
results_bivariate[["Disciplina"]]







mod_chile_og <- pisa.log.pv(pvlabel=paste0("PV",1:10,"READ"),
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
                        "Publico"), cutoff=552.89,data=chile_data,
                    export=T,name="model_chl",folder=d)



library(lme4)

# Model formula: outcome (e.g., PV values) ~ predictors + (1|grouping_variable)
# In this case, I'm assuming `ESECB` is a grouping variable (like schools or regions),
# but you can use another grouping factor like `CNT` or `ESECB` based on your structure.

# We’ll use PV reading scores and the predictors like you have.

# Multilevel Logistic Regression Model
mod_chile_multilevel <- glmer(
  formula = paste0("high_PV", 1:10, "READ ~ Disciplina + Apoyo + Dirigida + Retroalimentacion + 
                   Estimulacion + Adaptacion + Entusiasmo + Materiales + Personal + 
                   ESECB + GRADE + Femenino + Publico + (1|CNTSCHID)"),
  data = chile_data, 
  family = binomial(link = "logit"),
  weights = norm_weight
)

# View model summary
summary(mod_chile_multilevel)



install.packages("MLMusingR")
library(WeMix) #for mixed models with weights at different levels
library(modelsummary) #for output tables
library(MLMusingR)
library(broom) #needed for output tables
source("https://raw.githubusercontent.com/flh3/pubdata/main/mixPV/mixPV.R")
chile_data$weight_dummy <- 1


# Create 10 binary variables: 1 if above cutoff, 0 otherwise
chile_data <- chile_data %>%
  mutate(
    PV1READ_bin = ifelse(PV1READ >= 552.89, 1, 0),
    PV2READ_bin = ifelse(PV2READ >= 552.89, 1, 0),
    PV3READ_bin = ifelse(PV3READ >= 552.89, 1, 0),
    PV4READ_bin = ifelse(PV4READ >= 552.89, 1, 0),
    PV5READ_bin = ifelse(PV5READ >= 552.89, 1, 0),
    PV6READ_bin = ifelse(PV6READ >= 552.89, 1, 0),
    PV7READ_bin = ifelse(PV7READ >= 552.89, 1, 0),
    PV8READ_bin = ifelse(PV8READ >= 552.89, 1, 0),
    PV9READ_bin = ifelse(PV9READ >= 552.89, 1, 0),
    PV10READ_bin = ifelse(PV10READ >= 552.89, 1, 0)
  )

chile_data <- chile_data[!is.na(chile_data$CNTSCHID), ]  # remove missing school ID
chile_data$CNTSCHID <- as.factor(chile_data$CNTSCHID)     # make sure schoolid is a factor


mix_mod <- mixPV(
  PV1READ_bin + PV2READ_bin + PV3READ_bin + PV4READ_bin + PV5READ_bin +
    PV6READ_bin + PV7READ_bin + PV8READ_bin + PV9READ_bin + PV10READ_bin ~
    Disciplina +
    Apoyo +
    Dirigida +
    Retroalimentacion +
    Estimulacion +
    Adaptacion +
    Entusiasmo +
    Materiales +
    Personal +
    ESECB +
    GRADE +
    Femenino +
    Publico + 
    (1 | CNTSCHID),
  data = chile_data,
  weights = c('W_FSTUWT', 'weight_dummy')
   # for logistic regression
)



mix_mod2 <- mixPV(
  PV1READ + PV2READ + PV3READ + PV4READ + PV5READ +
    PV6READ + PV7READ + PV8READ + PV9READ + PV10READ ~
    Disciplina +
    Apoyo +
    Dirigida +
    Retroalimentacion +
    Estimulacion +
    Adaptacion +
    Entusiasmo +
    Materiales +
    Personal +
    ESECB +
    GRADE +
    Femenino +
    Publico + 
    (1 | CNTSCHID),
  data = chile_data,
  weights =
)

model_pv1 <- glmer(PV1READ_bin ~ Disciplina + Apoyo + Dirigida + Retroalimentacion +
                   Estimulacion + Adaptacion + Entusiasmo + Materiales +
                   Personal + ESECB + GRADE + Femenino + Publico + 
                    (1 | CNTSCHID),
                 family = binomial(link = "logit"),
                 data = chile_data)

model_pv2 <- glmer(PV2READ_bin ~ Disciplina + Apoyo + Dirigida + Retroalimentacion +
                   Estimulacion + Adaptacion + Entusiasmo + Materiales +
                   Personal + ESECB + GRADE + Femenino + Publico+ 
                    (1 | CNTSCHID),
                 family = binomial(link = "logit"),
                 data = chile_data)

model_pv3 <- glmer(PV3READ_bin ~ Disciplina + Apoyo + Dirigida + Retroalimentacion +
                   Estimulacion + Adaptacion + Entusiasmo + Materiales +
                   Personal + ESECB + GRADE + Femenino + Publico+ 
                    (1 | CNTSCHID),
                 family = binomial(link = "logit"),
                 data = chile_data)

model_pv4 <- glmer(PV4READ_bin ~ Disciplina + Apoyo + Dirigida + Retroalimentacion +
                   Estimulacion + Adaptacion + Entusiasmo + Materiales +
                   Personal + ESECB + GRADE + Femenino + Publico+ 
                    (1 | CNTSCHID),
                 family = binomial(link = "logit"),
                 data = chile_data)
model_pv5 <- glmer(PV5READ_bin ~ Disciplina + Apoyo + Dirigida + Retroalimentacion +
                   Estimulacion + Adaptacion + Entusiasmo + Materiales +
                   Personal + ESECB + GRADE + Femenino + Publico+ 
                    (1 | CNTSCHID),
                 family = binomial(link = "logit"),
                 data = chile_data)
model_pv6 <- glmer(PV6READ_bin ~ Disciplina + Apoyo + Dirigida + Retroalimentacion +
                   Estimulacion + Adaptacion + Entusiasmo + Materiales +
                   Personal + ESECB + GRADE + Femenino + Publico+ 
                    (1 | CNTSCHID),
                 family = binomial(link = "logit"),
                 data = chile_data)
model_pv7 <- glmer(PV7READ_bin ~ Disciplina + Apoyo + Dirigida + Retroalimentacion +
                   Estimulacion + Adaptacion + Entusiasmo + Materiales +
                   Personal + ESECB + GRADE + Femenino + Publico+ 
                    (1 | CNTSCHID),
                 family = binomial(link = "logit"),
                 data = chile_data)
model_pv8 <- glmer(PV8READ_bin ~ Disciplina + Apoyo + Dirigida + Retroalimentacion +
                   Estimulacion + Adaptacion + Entusiasmo + Materiales +
                   Personal + ESECB + GRADE + Femenino + Publico+ 
                    (1 | CNTSCHID),
                 family = binomial(link = "logit"),
                 data = chile_data)

model_pv9 <- glmer(PV9READ_bin ~ Disciplina + Apoyo + Dirigida + Retroalimentacion +
                   Estimulacion + Adaptacion + Entusiasmo + Materiales +
                   Personal + ESECB + GRADE + Femenino + Publico+ 
                    (1 | CNTSCHID),
                 family = binomial(link = "logit"),
                 data = chile_data)

model_pv10 <- glmer(PV10READ_bin ~ Disciplina + Apoyo + Dirigida + Retroalimentacion +
                   Estimulacion + Adaptacion + Entusiasmo + Materiales +
                   Personal + ESECB + GRADE + Femenino + Publico+ 
                     (1 | CNTSCHID),
                 family = binomial(link = "logit"),
                 data = chile_data)

summary(model_pv10)

coefs_list <- list(
  coef(model_pv1),
  coef(model_pv2),
  coef(model_pv3),
  coef(model_pv4),
  coef(model_pv5),
  coef(model_pv6),
  coef(model_pv7),
  coef(model_pv8),
  coef(model_pv9),
  coef(model_pv10)
)

coef_matrix <- do.call(rbind, coefs_list)
coef_mean <- colMeans(coef_matrix)



library(dplyr)

data_ch <- chile_data %>%
  group_by(CNTSCHID) %>%
  summarize(
    n_students = n(),
    n_0 = sum(PV1READ_bin == 0, na.rm = TRUE),
    n_1 = sum(PV1READ_bin == 1, na.rm = TRUE)
  ) %>%
  filter(n_0 == 0 | n_1 == 0)  # Find problematic schools

unique(chile_data$CNTSCHID)
library(intsvy)

chile_data_clean <- chile_data %>%
  filter(!is.na(PV1READ_bin)) %>%
  filter(!is.na(Disciplina) & !is.na(Apoyo) & !is.na(Dirigida) & 
           !is.na(Retroalimentacion) & !is.na(Estimulacion) & !is.na(Adaptacion) &
           !is.na(Entusiasmo) & !is.na(Materiales) & !is.na(Personal) &
           !is.na(ESECB) & !is.na(GRADE) & !is.na(Femenino) & !is.na(Publico) &
           !is.na(CNTSCHID))

mix_mod <- mixPV(
  PV1READ_bin + PV2READ_bin + PV3READ_bin + PV4READ_bin + PV5READ_bin +
    PV6READ_bin + PV7READ_bin + PV8READ_bin + PV9READ_bin + PV10READ_bin ~
    Disciplina + Apoyo + Dirigida + Retroalimentacion + 
    Estimulacion + Adaptacion + Entusiasmo + Materiales + 
    Personal + ESECB + GRADE + Femenino + Publico + 
    (1 | CNTSCHID), 
  data = chile_data_clean,
  weights = c("W_FSTUWT", "weight_dummy"),   # optional
  family = binomial(link = "logit")
)

summary(mix_mod)

sum(complete.cases(chile_data[, c("PV1READ_bin",
                                  "Disciplina", "Apoyo", "Dirigida", "Retroalimentacion",
                                  "Estimulacion", "Adaptacion", "Entusiasmo", "Materiales",
                                  "Personal", "ESECB", "GRADE", "Femenino", "Publico", "CNTSCHID")]))


library(dplyr)



# Step 1: Check variation across ALL 10 plausible values
good_schools <- chile_data %>%
  group_by(CNTSCHID) %>%
  summarize(across(
    ends_with("READ_bin"), 
    ~ (sum(.x == 0, na.rm = TRUE) > 0) & (sum(.x == 1, na.rm = TRUE) > 0),
    .names = "ok_{.col}"
  )) %>%
  # Keep only schools where all PVs have variation
  filter(if_all(starts_with("ok_"), ~ .x == TRUE)) %>%
  pull(CNTSCHID)

# Step 2: Filter your data
chile_data_clean <- chile_data %>%
  filter(CNTSCHID %in% good_schools)


# Step 2: Filter dataset
chile_data_clean <- chile_data %>%
  filter(CNTSCHID %in% good_schools)

# Step 3: Run multilevel logistic model
mix_mod2 <- mixPV(
  PV1READ_bin + PV2READ_bin + PV3READ_bin + PV4READ_bin + PV5READ_bin +
    PV6READ_bin + PV7READ_bin + PV8READ_bin + PV9READ_bin + PV10READ_bin ~
    Disciplina +
    Apoyo +
    Dirigida +
    Retroalimentacion +
    Estimulacion +
    Adaptacion +
    Entusiasmo +
    Materiales +
    Personal +
    ESECB +
    GRADE +
    Femenino +
    Publico +
    (1 | CNTSCHID),
  data = chile_data_clean,    # Use cleaned data
  weights = c("W_FSTUWT", "weight_dummy"), 
  family = binomial()
)

# Step 4: See results
summary(mix_mod2)


mod_chile_ols_og <- intsvy.reg.pv (pvlabel=paste0("PV",1:10,"READ"),
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
                                "Publico"), data=chile_data,
                            export=T,name="model_chl",folder=d)

mod_chile_ols <- intsvy.reg.pv(
  x = c(
    "Disciplina", "Apoyo", "Dirigida", "Retroalimentacion", 
    "Estimulacion", "Adaptacion", "Entusiasmo", "Materiales", 
    "Personal", "ESECB", "GRADE", "Femenino", "Publico"
  ),
  pvnames = "PV1READ",     # NOT paste0("PV",1:10,"READ") !!
  data = chile_data,
  export = TRUE,
  name = "model_chile",
  folder = d,
  config = pisa_conf   # you must pass config, like pisa2012_conf
)




library(lme4)
library(broom.mixed)
library(dplyr)

# 1. Prepare data (using Chile as example)
chile_data <- md %>% filter(CNT == "Chile") 

df$W_FSTUWT <- as.integer(df$W_FSTUWT)
# 2. Create binary outcomes for all PVs
library(lme4)
library(broom.mixed)
library(dplyr)

# Step 1: Define PV variables
pv_vars <- paste0("PV", 1:10, "READ")

# Step 2: Create binary 'high achievement' dummies for each PV
library(lme4)
library(broom.mixed)
library(dplyr)
library(tidyr)
library(purrr)

library(dplyr)
library(lme4)
library(broom.mixed)


# 2. Create binary outcomes for all PVs
pv_vars <- paste0("PV", 1:10, "READ")
df <- df %>%
  mutate(across(all_of(pv_vars), ~ ifelse(. > 552.89, 1, 0), .names = "high_{.col}"))
         
# 3. Initialize storage for results
         pv_estimates <- list()
         sampling_vars <- list()
         imputation_vars <- list()
         
         # 4. Analyze each plausible value separately
         for (i in 1:10) {
           pv <- paste0("high_PV", i, "READ")
           
           # Run multilevel model for this PV
           model <- glmer(as.formula(paste(pv, "~ Disciplina + Apoyo + Dirigida + 
                                Retroalimentacion + Estimulacion + Adaptacion + 
                                Entusiasmo + Materiales + Personal + 
                                Femenino + Publico + (1 | CNTSCHID)")),
                          data = df,
                          family = binomial,
                          weights = W_FSTUWT,
                          control = glmerControl(optimizer = "bobyqa"))
           
           # Store point estimates
           pv_estimates[[i]] <- tidy(model, effects = "fixed") %>% 
             select(term, estimate, std.error)
           
           # Store sampling variance from first PV only (unbiased estimate)
           if (i == 1) {
             sampling_vars <- pv_estimates[[1]] %>% 
               select(term, sampling_var = std.error) %>%
               mutate(sampling_var = sampling_var^2)
           }
         }
         
         # 5. Combine results across PVs
         combined_results <- bind_rows(pv_estimates, .id = "pv_num")
         
         
 # 6. Calculate final estimates
final_results <- combined_results %>%
  group_by(term) %>%
           summarize(
             # Final point estimate = mean across PVs
             final_estimate = mean(estimate),
             
             # Imputation variance = between-PV variance
             between_var = var(estimate),
             
             # Average within-PV variance
             avg_within_var = mean(std.error^2)
           ) %>%
           left_join(sampling_vars, by = "term") %>%
           mutate(
             # Total variance = within + (1 + 1/M)*between
             total_var = sampling_var + (1 + 1/10)*between_var,
             
             # Final standard error
             final_se = sqrt(total_var),
             
             # 95% confidence intervals
             conf_low = final_estimate - 1.96 * final_se,
             conf_high = final_estimate + 1.96 * final_se
           )
         
         # 7. View final results
final_results2 <- final_results %>%
  select(term, final_estimate, final_se, conf_low, conf_high)






mix_mod2 <- mixPV(
  PV1READ + PV2READ + PV3READ + PV4READ + PV5READ +
    PV6READ + PV7READ + PV8READ + PV9READ + PV10READ ~
    Disciplina +
    Apoyo +
    Dirigida +
    Retroalimentacion +
    Estimulacion +
    Adaptacion +
    Entusiasmo +
    Materiales +
    Personal +
    ESECB +
    GRADE +
    Femenino +
    Publico + 
    (1 | CNTSCHID),
  weights = c('W_FSTUWT', 'weight_dummy'),
  data = chile_data
)

summary(mix_mod2)



(2227.43)^2 / ((2227.43)^2 + (4777.94)^2)

chile_data <- chile_data %>%
  mutate(
    PV1READ_bin = ifelse(PV1READ >= 552.89, 1, 0),
    PV2READ_bin = ifelse(PV2READ >= 552.89, 1, 0),
    PV3READ_bin = ifelse(PV3READ >= 552.89, 1, 0),
    PV4READ_bin = ifelse(PV4READ >= 552.89, 1, 0),
    PV5READ_bin = ifelse(PV5READ >= 552.89, 1, 0),
    PV6READ_bin = ifelse(PV6READ >= 552.89, 1, 0),
    PV7READ_bin = ifelse(PV7READ >= 552.89, 1, 0),
    PV8READ_bin = ifelse(PV8READ >= 552.89, 1, 0),
    PV9READ_bin = ifelse(PV9READ >= 552.89, 1, 0),
    PV10READ_bin = ifelse(PV10READ >= 552.89, 1, 0)
  )

mix_mod <- mixPV(
  PV1READ_bin + PV2READ_bin + PV3READ_bin + PV4READ_bin + PV5READ_bin +
    PV6READ_bin + PV7READ_bin + PV8READ_bin + PV9READ_bin + PV10READ_bin ~
    Disciplina +
    Apoyo +
    Dirigida +
    Retroalimentacion +
    Estimulacion +
    Adaptacion +
    Entusiasmo +
    Materiales +
    Personal +
    ESECB +
    GRADE +
    Femenino +
    Publico + 
    (1 | CNTSCHID),
  data = chile_data,
  weights = c('W_FSTUWT', 'weight_dummy')
  # for logistic regression
)

mix_mod3 <- mixPV(
  high_PV1READ + high_PV2READ + high_PV3READ + high_PV4READ + high_PV5READ +
    high_PV6READ + high_PV7READ + high_PV8READ + high_PV9READ + high_PV10READ ~
    Disciplina +
    Apoyo +
    Dirigida +
    Retroalimentacion +
    Estimulacion +
    Adaptacion +
    Entusiasmo +
    Materiales +
    Personal +
    ESECB +
    GRADE +
    Femenino +
    Publico + 
    (1 | CNTSCHID),
  data = chile_data,
  weights = c('W_FSTUWT', 'weight_dummy'),
  family = binomial(link = "logit")
  # for logistic regression
)

summary(mix_mod)
chile_data$W_FSTUWT <- as.integer(chile_data$W_FSTUWT)
chile_data$weight_dummy <- as.integer(chile_data$weight_dummy)

library(lme4)
library(dplyr)

# 1. Calculate ICC for each PV model
icc_results <- lapply(1:10, function(i) {
  pv <- paste0("high_PV", i, "READ")
  
  model <- glmer(as.formula(paste(pv, "~ Disciplina + Apoyo + Dirigida + 
                                Retroalimentacion + Estimulacion + Adaptacion + 
                                Entusiasmo + Materiales + Personal + 
                                Femenino + Publico + (1 | CNTSCHID)")),
                 data = df,
                 family = binomial,
                 weights = W_FSTUWT,
                 control = glmerControl(optimizer = "bobyqa"))
  
  # Extract variance components
  school_var <- as.data.frame(VarCorr(model))$vcov[1]
  icc <- school_var / (school_var + (3.29)/3)
  
  return(data.frame(PV = i, 
                    school_var = school_var,
                    icc = icc))
}) %>% bind_rows()

# 2. Pool ICC estimates across PVs (mean and SE)
final_icc <- icc_results %>%
  summarize(
    mean_school_var = mean(school_var),
    mean_icc = mean(icc),
    se_icc = sd(icc)/sqrt(n())  # Standard error of ICC estimate
  )

# 3. Print results
print(icc_results)  # ICC for each PV
print(final_icc)    # Pooled estimates

# 4. Visualization (optional)
library(ggplot2)
ggplot(icc_results, aes(x = PV, y = icc)) +
  geom_point() +
  geom_hline(yintercept = final_icc$mean_icc, linetype = "dashed") +
  labs(title = "ICC Across Plausible Values",
       x = "Plausible Value",
       y = "ICC") +
  ylim(0, 0.3)




# Extract variance components
var_between <- mix_mod2$estimates["CNTSCHID.(Intercept)", "Estimate"]
var_within <- mix_mod2$estimates["Residual", "Estimate"]

# Calculate ICC
icc <- var_between / (var_between + var_within)
icc

mix_mod2 <- testEstimates(models)

