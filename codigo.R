library(tidyr)
library(dplyr)
library(dplyr)

setwd("/Users/matheusdantas/Downloads/siconv")
filenames <- list.files("/Users/matheusdantas/Downloads/siconv", pattern = "*.csv", full.names = TRUE)

data <- lapply(filenames, read.csv, sep = ";")

historico_situacao <- read.csv("./siconv_historico_situacao.csv", sep = ";")
unique(historico_situacao$HISTORICO_SIT)

executado <- historico_situacao[historico_situacao$HISTORICO_SIT %in% c("PRESTACAO_CONTAS_CONCLUIDA",
                                                                        "PRESTACAO_CONTAS_APROVADA_COM_RESSALVAS"), ]

unique(executado$HISTORICO_SIT)

nao_executado <- historico_situacao[historico_situacao$HISTORICO_SIT %in% c("CONVENIO_ANULADO",
                                                                            "CONVENIO_RESCINDIDO",
                                                                            "PRESTACAO_CONTAS_REJEITADA"), ]
unique(nao_executado$HISTORICO_SIT)


proponentes <- read.csv("./siconv_proponentes.csv", sep = ";")

ids <- read.csv(file = "./siconv_proposta.csv", sep = ";", colClasses = c(NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL",
                                                                          "NULL", "NULL", "NULL", "NULL", "NULL", NA, "NULL", "NULL", "NULL", "NULL",
                                                                          "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL",
                                                                          "NULL", "NULL"))
names(ids)

uf_localidade <- read.csv(file = "./siconv_plano_aplicacao_detalhado.csv", sep = ";", colClasses = c(NA, NA, NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL",
                                                                                        "NULL", "NULL", "NULL", "NULL"))

prorroga_oficio <- read.csv(file = "./siconv_prorroga_oficio.csv", sep = ";")
info_convenio <- read.csv(file = "./siconv_convenio.csv", sep = ';')
info_convenio <- subset(info_convenio, ANO > 2014)

info_convenio <- inner_join(info_convenio, ids, by = "ID_PROPOSTA")
write.csv(info_convenio, "./info_convenio.csv")
#inseri as colunas Atrasou e Dias_de_atraso Manualmente no excel
info_convenio <- read.csv("./info_convenio.csv", sep = ";") #abri novamente o arquivo
unique(info_convenio$ANO)

convenios_atrasados <- subset(info_convenio, ATRASOU == "SIM" & ANO < 2020 
                              & DIAS_DE_ATRASO > 0) #selecionei os convenios atrasados anteriores a 2020
convenios_nao_atrasados <- subset(info_convenio, ATRASOU == "NÃO" & ANO < 2020) #selecionei os convenios nao atrasados anteriores a 2020
unique(convenios_atrasados$ANO)

convenios_atrasados <- convenios_atrasados %>%
    arrange(-DIAS_DE_ATRASO)
View(convenios_atrasados)

hist(convenios_atrasados$DIAS_DE_ATRASO)
names(convenios_atrasados)
convenios_atrasados <- left_join(convenios_atrasados, uf_localidade, by = "ID_PROPOSTA")
convenios_nao_atrasados <- left_join(convenios_nao_atrasados, uf_localidade, by = "ID_PROPOSTA")
View(convenios_nao_atrasados)

convenios_atrasados$UF_localidade <- convenios_atrasados$SIGLA
convenios_atrasados$Muni_localidade <- convenios_atrasados$MUNICIPIO
convenios_atrasados$SIGLA <- NULL
convenios_atrasados$MUNICIPIO <- NULL

convenios_nao_atrasados$UF_localidade <- convenios_atrasados$SIGLA
convenios_nao_atrasados$Muni_localidade <- convenios_atrasados$MUNICIPIO
convenios_nao_atrasados$SIGLA <- NULL
convenios_nao_atrasados$MUNICIPIO <- NULL

convenios_atrasados <- left_join(convenios_atrasados, ids, by = "ID_PROPOSTA")
convenios_nao_atrasados <- left_join(convenios_nao_atrasados, ids, by = "ID_PROPOSTA")

convenios_atrasados$IDENTIF_PROPONENTE <- convenios_atrasados$IDENTIF_PROPONENTE.y
convenios_atrasados$IDENTIF_PROPONENTE.x <- NULL
convenios_atrasados$IDENTIF_PROPONENTE.y <- NULL

convenios_atrasados$IDENTIF_PROPONENTE <- as.factor(convenios_atrasados$IDENTIF_PROPONENTE)
convenios_nao_atrasados$IDENTIF_PROPONENTE <- as.factor(convenios_nao_atrasados$IDENTIF_PROPONENTE)

convenios_atrasados <- inner_join(convenios_atrasados, proponentes, by = "IDENTIF_PROPONENTE")
convenios_nao_atrasados <- inner_join(convenios_nao_atrasados, proponentes, by = "IDENTIF_PROPONENTE")

write.csv(convenios_atrasados, "./convenios_atrasados.csv")
