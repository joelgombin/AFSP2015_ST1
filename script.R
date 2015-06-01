library(dplyr)
library(tidyr)
library(tmap)
library(rgdal)
library(rgeos)
library(gdata)
library(stringr)
load("/media/Data/Dropbox/Thèse/données propres/présidentielle 2012/P2012BV.Rdata")
load("/media/Data/Dropbox/Thèse/données propres/identification/communes_ident.Rdata")
AMM <- communes_ident[communes_ident$CodeAU10 %in% "003", "CodeInsee"]

# CSP (par sexe)

load("/media/Data/Dropbox/Thèse/données propres/RP2011/IRIS/RP_2011_IRIS_POP.Rdata")
RP_2011_IRIS_POP <- RP_2011_IRIS_POP %>%
  mutate_each(funs(. / C11_POP15P * 100), matches("C11_[HF]{1}15P_CS[1-8]{1}"))

# niveau de diplôme

load("/media/Data/Dropbox/Thèse/données propres/RP2011/IRIS/RP_2011_IRIS_DIP_P18ANS.Rdata")
dipl_sex_iris_rp2011 <- dipl_sex_iris_rp2011 %>%
  mutate_each(funs = funs(. / total * 100), F_Bac_général:H_Sans_dipl)

# niveau et composition des revenus

load("/media/Data/Dropbox/Thèse/données propres/fisc/RFDUiris20012011.Rdata")
load("/media/Data/Dropbox/Thèse/données propres/fisc/RFDUcomm20012011.Rdata")
load("/media/Data/Dropbox/Thèse/données propres/fisc/RFSTiris20012011.Rdata")
load("/media/Data/Dropbox/Thèse/données propres/fisc/RFSTcom20012011.Rdata")

RFDUcomm <- RFDUcomm %>%
  mutate(iris = paste0(CodeInsee, "0000"))
RFST2011com <- RFST2011com %>%
  mutate(iris = paste0(COM, "0000"))

RFDU2011iris <- bind_rows(RFDUcomm %>% mutate(CodeIris = iris) %>% select(-(CodeInsee:RFUCGI10), -iris),
                          RFDU2011iris)
RFST2011iris <- bind_rows(RFST2011com %>% mutate(CodeIris = iris) %>% select(-(COM:LIBGEO), -X, -iris),
                          RFST2011iris %>% select(CodeIris, PMIMP11:PAUT11))

# précarité sociale 

## activité pour les majeurs

load("/media/Data/Dropbox/Thèse/données propres/RP2011/IRIS/RP_2011_IRIS_ACT_P18ANS.Rdata")
act_sex_iris_rp2011 <- act_sex_iris_rp2011 %>%
  mutate_each(funs(. / total * 100), F_Actifs_employés:H_Retraités)

## temps partiel

load("/media/Data/Dropbox/Thèse/données propres/RP2011/IRIS/RP_2011_IRIS_ACT_TP_P18ANS.Rdata")
act_tp_sex_iris_rp2011 <- act_tp_sex_iris_rp2011 %>%
  mutate_each(funs(. / total * 100), F_Sans_objet:`H_Temps partiel`)

## type de contrat
load("/media/Data/Dropbox/Thèse/données propres/RP2011/IRIS/RP_2011_IRIS_ACT_EMPL_P18ANS.Rdata")
act_empl_sex_iris_rp2011 <- act_empl_sex_iris_rp2011 %>%
  mutate_each(funs(. / total * 100), F_Aides_familiaux:H_Stagiaire)

## CAF
load("/media/Data/Dropbox/Thèse/données propres/RP2011/IRIS/RP_2011_IRIS_LOG.Rdata")
cnaf <- read.xls("/media/Data/Dropbox/région Paca/Données IRIS/Prestas légales PACA.xls", pattern = "Code région")
cnaf <- cnaf %>%
  mutate(CodeIris = paste0(sprintf("%05.0f", `Code.département...commune`),
                           sprintf("%04.0f", `Code.IRIS`)))
cnaf_com <- read.csv("/media/Data/Dropbox/Thèse/données CAF/Solidarité insertion_2013_communes_ss.csv", sep = ";", as.is = TRUE, check.names = FALSE, na.strings = "nc")

df_iris <- cnaf$Nombre.d.allocataires.percevant.le.Revenu.de.Solidarité.Active..Métropole. / RP_2011_IRIS_LOG[match(cnaf$CodeIris, RP_2011_IRIS_LOG$IRIS), "P11_RP"] * 100
pop <- RP_2011_IRIS_LOG %>%
  filter(COM %in% sprintf("%05.0f", as.integer(cnaf_com$`Code Commune`))) %>%
  group_by(COM) %>%
  summarise(pop = sum(P11_RP))
df_commune <- cnaf_com$`RSA f` / pop[match(sprintf("%05.0f", as.integer(cnaf_com$`Code Commune`)), pop$COM), "pop"] * 100

# dispatchage des données électorales sur les IRIS

load("/media/Data/Dropbox/Thèse/données propres/cartes/BV2012PACA.Rdata")
bvAMM <- PACA_BV12[substr(PACA_BV12@data$ID, 1, 5) %in% AMM, ]

load("/media/Data/Dropbox/DonneesIGN_ContoursIRIS/IRISCONTOURS-IRIS04.Rdata")
load("/media/Data/Dropbox/DonneesIGN_ContoursIRIS/IRISCONTOURS-IRIS05.Rdata")
load("/media/Data/Dropbox/DonneesIGN_ContoursIRIS/IRISCONTOURS-IRIS06.Rdata")
load("/media/Data/Dropbox/DonneesIGN_ContoursIRIS/IRISCONTOURS-IRIS13.Rdata")
load("/media/Data/Dropbox/DonneesIGN_ContoursIRIS/IRISCONTOURS-IRIS83.Rdata")
load("/media/Data/Dropbox/DonneesIGN_ContoursIRIS/IRISCONTOURS-IRIS84.Rdata")

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
