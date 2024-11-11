library(dplyr)
library(ggplot2)
library(PNADcIBGE)
library(survey)
library(data.table)
library(DBI)
library(RSQLite)
library(stringr)

pnadc_um<-get_pnadc(year=2023,interview=1,design = FALSE, deflator=TRUE, labels=FALSE)
pnadc_dois<-get_pnadc(year=2023,interview=2,design = FALSE, deflator=TRUE, labels= FALSE)
                    
# Realizando coleta de lixo acumulada durante a obtenção dos microdados
gc(verbose=FALSE, reset=FALSE, full=TRUE)

# Criando variáveis auxiliares para obtenção da estimativa desejada
pnadc_anual_visita1 <- transform(pnadc_um, ID_DOMICILIO=paste0(UPA,V1008,V1014))
pnadc_anual_visita1 <- transform(pnadc_anual_visita1, Pais=as.factor("Brasil"))
pnadc_anual_visita1 <- transform(pnadc_anual_visita1, ID_PESSOA=paste0(UPA,V1008,V1014,V2003))
pnadc_anual_visita1$Pais <- factor(x=pnadc_anual_visita1$Pais, levels=c("Brasil"))
pnadc_anual_visita1 <- transform(pnadc_anual_visita1, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",ifelse(substr(UPA, start=1, stop=1)=="4","Sul",ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
pnadc_anual_visita1$GR <- factor(x=pnadc_anual_visita1$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))

pnadc_anual_visita2 <- transform(pnadc_dois, ID_DOMICILIO=paste0(UPA,V1008,V1014))
pnadc_anual_visita2 <- transform(pnadc_anual_visita2, Pais=as.factor("Brasil"))
pnadc_anual_visita2 <- transform(pnadc_anual_visita2, ID_PESSOA=paste0(UPA,V1008,V1014,V2003))
pnadc_anual_visita2$Pais <- factor(x=pnadc_anual_visita2$Pais, levels=c("Brasil"))
pnadc_anual_visita2 <- transform(pnadc_anual_visita2, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",ifelse(substr(UPA, start=1, stop=1)=="4","Sul",ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
pnadc_anual_visita2$GR <- factor(x=pnadc_anual_visita2$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))

# Seleciona as variáveis que devem ser replicadas da primeira visita
variaveis_replicar <- c("ID_PESSOA","V5001A", "V5001A2", "V5002A", "V5002A2", "V5003A", "V5003A2")

# Faz a junção para trazer as variáveis da visita 1 para a visita 2 com base na ID_PESSOA
pnadc_anual_visita2 <- pnadc_anual_visita2 %>%
  left_join(pnadc_anual_visita1[variaveis_replicar], by = "ID_PESSOA", suffix = c("", "_visita1"))

pnadc_anual_visita2 <- pnadc_anual_visita2 %>%
  rename(
    VD4046 = VDI4046,
    VD4047 = VDI4047,
    VD4048 = VDI4048,
    VD4052 = VDI4052,
    VD5007 = VDI5007,
    VD5008 = VDI5008,
    VD5009 = VDI5009,
    VD5010 = VDI5010,
    VD5011 = VDI5011,
    VD5012 = VDI5012
  )

# Encontrar colunas comuns entre as duas bases
colunas_comuns <- intersect(names(pnadc_anual_visita1), names(pnadc_anual_visita2))

# Selecionar as colunas comuns em ambas as bases
pnadc_anual_visita1 <- pnadc_anual_visita1[, colunas_comuns]
pnadc_anual_visita2 <- pnadc_anual_visita2[, colunas_comuns]

# Adiciona a coluna de visita em cada base
pnadc_anual_visita1$visita <- "1"
pnadc_anual_visita2$visita <- "2"

# Combina as duas bases de dados e intercalando pela variável ID_PESSOA
pnad_unificado <- rbind(pnadc_anual_visita1, pnadc_anual_visita2) %>%
  arrange(ID_PESSOA, visita)

# Filtra apenas IDs que aparecem nas duas visitas
pnad_unificado_filtrado <- pnad_unificado %>%
  group_by(ID_PESSOA) %>%
  filter(n_distinct(visita) == 2) %>%
  ungroup() %>%
  select(ID_PESSOA, visita, everything())

# Realizando processo de obtenção da estimativa do rendimento domiciliar real
pnad_unificado_filtrado <- transform(pnad_unificado_filtrado, V2001_rendimento=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,1))
pnad_unificado_filtrado <- transform(pnad_unificado_filtrado, VD4019real_proprioano=ifelse(is.na(VD4019) | is.na(V2001_rendimento),NA,VD4019*CO1))
pnad_unificado_filtrado <- transform(pnad_unificado_filtrado, VD4048real_proprioano=ifelse(is.na(VD4048) | is.na(V2001_rendimento),NA,VD4048*CO1e))
pnad_unificado_filtrado <- transform(pnad_unificado_filtrado, VD4019real_ultimoano=ifelse(is.na(VD4019) | is.na(V2001_rendimento),NA,VD4019*CO2))
pnad_unificado_filtrado <- transform(pnad_unificado_filtrado, VD4048real_ultimoano=ifelse(is.na(VD4048) | is.na(V2001_rendimento),NA,VD4048*CO2e))
pnadc_anual_visita_rendimento <- pnad_unificado_filtrado %>% dplyr::group_by(ID_DOMICILIO) %>% dplyr::summarise(moradores_rendimento=sum(V2001_rendimento, na.rm=TRUE),
                                                                                                           rendimento_todos_trabalhos_proprioano=sum(VD4019real_proprioano, na.rm=TRUE),
                                                                                                           rendimento_outras_fontes_proprioano=sum(VD4048real_proprioano, na.rm=TRUE),
                                                                                                           rendimento_todos_trabalhos_ultimoano=sum(VD4019real_ultimoano, na.rm=TRUE),
                                                                                                           rendimento_outras_fontes_ultimoano=sum(VD4048real_ultimoano, na.rm=TRUE))
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5007real_proprioano=rendimento_todos_trabalhos_proprioano+rendimento_outras_fontes_proprioano)
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5008real_proprioano=VD5007real_proprioano/moradores_rendimento)
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5007real_ultimoano=rendimento_todos_trabalhos_ultimoano+rendimento_outras_fontes_ultimoano)
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5008real_ultimoano=VD5007real_ultimoano/moradores_rendimento)
pnad_unificado_filtrado <- pnad_unificado_filtrado[,!(names(pnad_unificado_filtrado) %in% c("V2001_rendimento","VD4019real_proprioano","VD4048real_proprioano","VD4019real_ultimoano","VD4048real_ultimoano"))]
pnadc_anual_visita_rendimento <- pnadc_anual_visita_rendimento[,!(names(pnadc_anual_visita_rendimento) %in% c("moradores_rendimento","rendimento_todos_trabalhos_proprioano","rendimento_outras_fontes_proprioano","rendimento_todos_trabalhos_ultimoano","rendimento_outras_fontes_ultimoano"))]
pnad_unificado_filtrado <- merge(x=pnad_unificado_filtrado, y=pnadc_anual_visita_rendimento, by.x="ID_DOMICILIO", by.y="ID_DOMICILIO", all.x=TRUE, all.y=FALSE)
rm(pnadc_anual_visita_rendimento)
pnad_unificado_filtrado <- transform(pnad_unificado_filtrado, VD5007real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_proprioano))
pnad_unificado_filtrado <- transform(pnad_unificado_filtrado, VD5008real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_proprioano))
pnad_unificado_filtrado <- transform(pnad_unificado_filtrado, VD5007real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_ultimoano))
pnad_unificado_filtrado <- transform(pnad_unificado_filtrado, VD5008real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_ultimoano))

# Criando variável de faixa de rendimento domiciliar per capita em valores reais
salariominimo_proprioano <- 1320
salariominimo_ultimoano <- 1320
pnad_unificado_filtrado <- transform(pnad_unificado_filtrado, VD5009real_proprioano=as.factor(x=ifelse(VD5008real_proprioano>=0 & VD5008real_proprioano<=salariominimo_proprioano/4,"Até ¼ salário mínimo",
                                                                                             ifelse(VD5008real_proprioano>salariominimo_proprioano/4 & VD5008real_proprioano<=salariominimo_proprioano/2,"Mais de ¼ até ½ salário mínimo",
                                                                                                    ifelse(VD5008real_proprioano>salariominimo_proprioano/2 & VD5008real_proprioano<=salariominimo_proprioano,"Mais de ½ até 1 salário mínimo",
                                                                                                           ifelse(VD5008real_proprioano>salariominimo_proprioano & VD5008real_proprioano<=salariominimo_proprioano*2,"Mais de 1 até 2 salários mínimos",
                                                                                                                  ifelse(VD5008real_proprioano>salariominimo_proprioano*2 & VD5008real_proprioano<=salariominimo_proprioano*3,"Mais de 2 até 3 salários mínimos",
                                                                                                                         ifelse(VD5008real_proprioano>salariominimo_proprioano*3 & VD5008real_proprioano<=salariominimo_proprioano*5,"Mais de 3 até 5 salários mínimos",
                                                                                                                                ifelse(VD5008real_proprioano>salariominimo_proprioano*5,"Mais de 5 salários mínimos",NA)))))))))
pnad_unificado_filtrado <- transform(pnad_unificado_filtrado, VD5009real_ultimoano=as.factor(x=ifelse(VD5008real_ultimoano>=0 & VD5008real_ultimoano<=salariominimo_ultimoano/4,"Até ¼ salário mínimo",
                                                                                            ifelse(VD5008real_ultimoano>salariominimo_ultimoano/4 & VD5008real_ultimoano<=salariominimo_ultimoano/2,"Mais de ¼ até ½ salário mínimo",
                                                                                                   ifelse(VD5008real_ultimoano>salariominimo_ultimoano/2 & VD5008real_ultimoano<=salariominimo_ultimoano,"Mais de ½ até 1 salário mínimo",
                                                                                                          ifelse(VD5008real_ultimoano>salariominimo_ultimoano & VD5008real_ultimoano<=salariominimo_ultimoano*2,"Mais de 1 até 2 salários mínimos",
                                                                                                                 ifelse(VD5008real_ultimoano>salariominimo_ultimoano*2 & VD5008real_ultimoano<=salariominimo_ultimoano*3,"Mais de 2 até 3 salários mínimos",
                                                                                                                        ifelse(VD5008real_ultimoano>salariominimo_ultimoano*3 & VD5008real_ultimoano<=salariominimo_ultimoano*5,"Mais de 3 até 5 salários mínimos",
                                                                                                                               ifelse(VD5008real_ultimoano>salariominimo_ultimoano*5,"Mais de 5 salários mínimos",NA)))))))))
pnad_unificado_filtrado$VD5009real_proprioano <- factor(x=pnad_unificado_filtrado$VD5009real_proprioano, levels=c("Até ¼ salário mínimo","Mais de ¼ até ½ salário mínimo","Mais de ½ até 1 salário mínimo","Mais de 1 até 2 salários mínimos","Mais de 2 até 3 salários mínimos","Mais de 3 até 5 salários mínimos","Mais de 5 salários mínimos"))
pnad_unificado_filtrado$VD5009real_ultimoano <- factor(x=pnad_unificado_filtrado$VD5009real_ultimoano, levels=c("Até ¼ salário mínimo","Mais de ¼ até ½ salário mínimo","Mais de ½ até 1 salário mínimo","Mais de 1 até 2 salários mínimos","Mais de 2 até 3 salários mínimos","Mais de 3 até 5 salários mínimos","Mais de 5 salários mínimos"))

# Criar uma nova coluna indicando se a renda é menor que 1/2 salário mínimo
pnad_unificado_filtrado <- pnad_unificado_filtrado %>%
  mutate(renda_menor_meio_salario = if_else(VD5009real_ultimoano %in% c("Até ¼ salário mínimo", "Mais de ¼ até ½ salário mínimo"), 
                                         "Sim", 
                                         "Não"))

write.csv(pnad_unificado_filtrado, "C:/Users/julia/Downloads/pnad_unificado_filtrado.csv", row.names = FALSE)



    # Filtrar a base de dados conforme as condições especificadas usando OU
dados_filtrados_2 <- pnadc_anual_visita %>%
  filter(renda_menor_meio_salario == "Sim"| V5002A == "Sim" | V5001A == "Sim" | V5003A == "Sim")%>%
  filter(V2009 >= 14 & V2009 <= 24)

# Realizando processo de incorporação do desenho amostral nos microdados
dados_filtrados_2 <- tibble::as_tibble(x=dados_filtrados_2)
dados_filtrados_2 <- PNADcIBGE::pnadc_design(data_pnadc=dados_filtrados_2)
str(object=dados_filtrados_2)
                                                                                
# Transformando os resultados em data frame
df_totalsexo <- as.data.frame(t(totalsexo))
df_totalsexo$Sexo <- rownames(df_totalsexo)  # Adicionando coluna com os sexos
colnames(df_totalsexo)[1] <- "Total"  # Renomeando a coluna de totais

ggplot(df_totalsexo, aes(x = Sexo, y = Total, fill = Sexo)) +
  geom_bar(stat = "identity") +
  labs(title = "Total por Sexo", x = "Sexo", y = "Total") +
  theme_minimal()

#CADÚNICO

pes.design<- fread("D:\\Econ_II\\base_amostra_pessoa_201812.csv",sep=";")

pes.design <- pes.design %>%
  mutate(peso.pes = str_pad(peso.pes, 15, side = "right", pad = "0"), # Preenchendo a string
         peso.pes = as.numeric(peso.pes) * 1e-14)  # Convertendo para numérico e multiplicando

svy_design <- svydesign(
  id = ~id_familia,                # coluna para unidade primária de amostragem (PSU)
  strata = ~estrato,        # coluna para o estrato
  weights = ~peso.pes,          # coluna para os pesos amostrais
  data = pes.design,              # o dataframe de dados
  nest = TRUE               # se é um desenho aninhado (múltiplos estágios)
)

# Variable for Counting Observations
svy_design <- transform(svy_design, contagem=1)
svy_design <- subset(svy_design, ind_frequenta_escola_memb==1 & cod_curso_frequenta_memb == 7 &
                       idade >= 14 & idade <= 24)

# Population of Beneficiaries Estimation
print(survey::svytotal(x=~contagem, design=svy_design, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

names(pnadc_um)