library(dplyr)
library(tidyr)
library(tmap)
library(rgdal)
library(rgeos)
library(gdata)
library(stringr)
source("./scripts/ventiler.R")
load("./data/P2012BV.Rdata")
load("./data/communes_ident.Rdata")
AMM <- communes_ident[communes_ident$CodeAU10 %in% "003", "CodeInsee"]

# CSP 8 positions (par sexe)

load("./data/RP2011_IRIS_CS1_P18ANS.Rdata")
cs1_sex_iris_rp2011 <- cs1_sex_iris_rp2011 %>%
  mutate_each(funs(. / total * 100), F_Agriculteurs:H_Retraités)

# niveau de diplôme

load("./data/RP2011_IRIS_DIP_P18ANS.Rdata")
dipl_sex_iris_rp2011 <- dipl_sex_iris_rp2011 %>%
  mutate_each(funs = funs(. / total * 100), F_Bac_général:H_Sans_dipl)

# niveau et composition des revenus
# on utilise les données 2011 car Filosofi n'est disponible pour l'instant qu'à l'échelle des communes

load("./data/RFDUiris20012011.Rdata")
load("./data/RFDUcomm20012011.Rdata")
load("./data/RFSTiris20012011.Rdata")
load("./data/RFSTcom20012011.Rdata")

RFDUcomm <- RFDUcomm %>%
  mutate(iris = paste0(CodeInsee, "0000"))
RFST2011com <- RFST2011com %>%
  mutate(iris = paste0(COM, "0000"))

RFDU2011iris <- bind_rows(RFDUcomm %>% mutate(CodeIris = iris) %>% select(-(CodeInsee:RFUCGI10), -iris),
                          RFDU2011iris %>% mutate(CodeIris = as.character(CodeIris)))


RFST2011iris <- bind_rows(RFST2011com %>% mutate(CodeIris = iris) %>% select(-(COM:LIBGEO), -X, -iris),
                          RFST2011iris %>% select(CodeIris, PMIMP11:PAUT11))

# précarité sociale 

## activité pour les majeurs

load("./data/RP2011_IRIS_ACT_P18ANS.Rdata")
act_sex_iris_rp2011 <- act_sex_iris_rp2011 %>%
  mutate_each(funs(. / total * 100), F_Actifs_employés:H_Retraités)

## temps partiel

load("./data/RP2011_IRIS_ACT_TP_P18ANS.Rdata")
act_tp_sex_iris_rp2011 <- act_tp_sex_iris_rp2011 %>%
  mutate_each(funs(. / total * 100), F_Sans_objet:`H_Temps partiel`)

## type de contrat
load("./data/RP2011_IRIS_ACT_EMPL_P18ANS.Rdata")
act_empl_sex_iris_rp2011 <- act_empl_sex_iris_rp2011 %>%
  mutate_each(funs(. / total * 100), F_Aides_familiaux:H_Stagiaire)

## CAF
load("./data/RP2011_IRIS_LOG.Rdata")
cnaf <- read.xls("./data/Prestas légales PACA.xls", pattern = "Code région")
cnaf <- cnaf %>%
  mutate(CodeIris = paste0(sprintf("%05.0f", `Code.département...commune`),
                           sprintf("%04.0f", `Code.IRIS`)))
cnaf_com <- read.csv("./data/Solidarité insertion_2013_communes_ss.csv", sep = ";", as.is = TRUE, check.names = FALSE, na.strings = "nc")

df_iris <- cnaf$Nombre.d.allocataires.percevant.le.Revenu.de.Solidarité.Active..Métropole. / RP_2011_IRIS_LOG[match(cnaf$CodeIris, RP_2011_IRIS_LOG$IRIS), "P11_RP"] * 100
pop <- RP_2011_IRIS_LOG %>%
  filter(COM %in% sprintf("%05.0f", as.integer(cnaf_com$`Code Commune`))) %>%
  group_by(COM) %>%
  summarise(pop = sum(P11_RP))
df_commune <- cnaf_com$`RSA p` / pop[match(sprintf("%05.0f", as.integer(cnaf_com$`Code Commune`)), pop$COM), "pop"] * 100

df_cnaf <- data.frame(ID = c(RP_2011_IRIS_LOG[match(cnaf$CodeIris, RP_2011_IRIS_LOG$IRIS), "IRIS"], paste0(magrittr::extract2(pop[match(sprintf("%05.0f", as.integer(cnaf_com$`Code Commune`)), pop$COM), "COM"], "COM"), "0000")), 
                      RSA = c(df_iris, df_commune$pop),
                      pop = c(RP_2011_IRIS_LOG[match(cnaf$CodeIris, RP_2011_IRIS_LOG$IRIS), "P11_RP"], magrittr::extract2(pop[match(sprintf("%05.0f", as.integer(cnaf_com$`Code Commune`)), pop$COM), "pop"], "pop")))

# dispatchage des données électorales sur les IRIS

load("./data/BV2012PACA.Rdata")
bvAMM <- PACA_BV12[substr(PACA_BV12@data$ID, 1, 5) %in% AMM, ]

load("./data/IRISCONTOURS-IRIS04.Rdata")
load("./data/IRISCONTOURS-IRIS05.Rdata")
load("./data/IRISCONTOURS-IRIS06.Rdata")
load("./data/IRISCONTOURS-IRIS13.Rdata")
load("./data/IRISCONTOURS-IRIS83.Rdata")
load("./data/IRISCONTOURS-IRIS84.Rdata")

`IRISCONTOURS-IRIS04` <- spChFIDs(`IRISCONTOURS-IRIS04`, `IRISCONTOURS-IRIS04`@data$DCOMIRIS)
`IRISCONTOURS-IRIS05` <- spChFIDs(`IRISCONTOURS-IRIS05`, `IRISCONTOURS-IRIS05`@data$DCOMIRIS)
`IRISCONTOURS-IRIS06` <- spChFIDs(`IRISCONTOURS-IRIS06`, `IRISCONTOURS-IRIS06`@data$DCOMIRIS)
`IRISCONTOURS-IRIS13` <- spChFIDs(`IRISCONTOURS-IRIS13`, `IRISCONTOURS-IRIS13`@data$DCOMIRIS)
`IRISCONTOURS-IRIS83` <- spChFIDs(`IRISCONTOURS-IRIS83`, `IRISCONTOURS-IRIS83`@data$DCOMIRIS)
`IRISCONTOURS-IRIS84` <- spChFIDs(`IRISCONTOURS-IRIS84`, `IRISCONTOURS-IRIS84`@data$DCOMIRIS)

irisPACA <- do.call(rbind, list(`IRISCONTOURS-IRIS04`, `IRISCONTOURS-IRIS05`, `IRISCONTOURS-IRIS06`, `IRISCONTOURS-IRIS13`, `IRISCONTOURS-IRIS83`, `IRISCONTOURS-IRIS84`))

irisAMM <- irisPACA[irisPACA@data$DEPCOM %in% AMM,]

# il ne faut dispatcher les données que dans les cas d'Aix et Marseille. Pour les autres communes divisées en IRIS, dispatcher simplement en fonction 

df2012$CodeBV <- str_replace(df2012$CodeBV, "_", "")

monoiris <- irisAMM@data %>%
  group_by(DEPCOM) %>%
  filter(n() == 1) %>% 
  distinct()
df1 <- df2012[match(monoiris$DEPCOM, df2012$CodeInsee), ]
df1$ID <- monoiris$DCOMIRIS
df1 <- df1 %>% select(ID, Inscrits:Exprimés2)

pluriiris <- irisAMM@data %>%
  group_by(DEPCOM) %>%
  filter(n() > 1, 
         !DEPCOM %in% c(13201:13216, 13001))
df2 <- df2012[match(pluriiris$DEPCOM, df2012$CodeInsee), ]
df2$ID <- pluriiris$DCOMIRIS
df2 <- df2 %>% select(ID, Inscrits:Exprimés2)

valeursMarseille <- ventiler(bvAMM[substr(bvAMM$ID, 1, 5) %in% 13201:13216, ], irisAMM[irisAMM@data$DEPCOM %in% 13201:13216, ], df2012, depart.ID = "ID", arrivee.ID = "DCOMIRIS", df.ID = "CodeBV", variables = names(df2012)[3:24])
valeursMarseille <- valeursMarseille %>%
  mutate(ID = arriveeID) %>%
  select(-arriveeID)

valeursAix <- ventiler(bvAMM[substr(bvAMM$ID, 1, 5) %in% 13001, ], irisAMM[irisAMM@data$DEPCOM %in% 13001, ], df2012, depart.ID = "ID", arrivee.ID = "DCOMIRIS", df.ID = "CodeBV", variables = names(df2012)[3:24])
valeursAix <- valeursAix %>%
  mutate(ID = arriveeID) %>%
  select(-arriveeID)

P2012AMM <- bind_rows(df1, df2, valeursMarseille, valeursAix)


