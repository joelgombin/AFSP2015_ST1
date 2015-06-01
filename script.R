library(dplyr)
library(tidyr)
library(tmap)
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

