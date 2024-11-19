#Calculando abandono escolar da PNAD Contínua fonte: https://github.com/manoelaferronato/Pe-de-Meia-/blob/main/abandono_escolar.R
# Carregando pacotes necessários
library(PNADcIBGE)
library(survey)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(purrr)


# Definindo diretório
setwd("D:\\Ajudinha\\pe-de-meia\\script")

# Anos que deseja processar
anos <- c(2019,2022,2023)

# Lista para armazenar dados de cada trimestre
dados_pnadc <- list()

for (ano in anos) {
  # Baixar e armazenar dados de cada trimestre uma vez
  for (tri in 1:4) {
    dados_pnadc[[paste(ano, tri, sep = "_")]] <- get_pnadc(year=ano, quarter=tri, defyear=ano, 
                                                           labels=TRUE, deflator=TRUE, 
                                                           design=FALSE)
  }
  
  # Realizar análise para os pares de trimestres (1 e 2, 2 e 3, 3 e 4)
  for (t in 1:3) {
    # Carregando dados do primeiro e segundo trimestre do par
    pnadc_1tri <- dados_pnadc[[paste(ano, t, sep = "_")]]
    pnadc_2tri <- dados_pnadc[[paste(ano, t+1, sep = "_")]]
    
    # Carregando PNADC 1 trimestre
    pnadc_1tri <- get_pnadc(year=2023, quarter = t, defyear=2023, 
                            labels=TRUE, deflator=TRUE, 
                            design=FALSE)
    
    # Carregando PNADC 2 trimestre
    pnadc_2tri <- get_pnadc(year=2023, quarter = t+1, defyear=2023, 
                            labels=TRUE, deflator=TRUE, 
                            design=FALSE)
    
    
    
    ########### Cálculo da estimativa RDPC somente para PNADc trime- Fonte: https://github.com/Gabriel-Assuncao/PNADcIBGE-RDPC ##########################
    pnadc_1tri <- transform(pnadc_1tri, ID_DOMICILIO=paste0(UPA,V1008,V1014))
    pnadc_1tri <- transform(pnadc_1tri, Pais=as.factor("Brasil"))
    pnadc_1tri$Pais <- factor(x=pnadc_1tri$Pais, levels=c("Brasil"))
    pnadc_1tri <- transform(pnadc_1tri, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",
                                                            ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",
                                                                   ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",
                                                                          ifelse(substr(UPA, start=1, stop=1)=="4","Sul",
                                                                                 ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",
                                                                                        NA)))))))
    
    pnadc_1tri$GR <- factor(x=pnadc_1tri$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))
    
    # Realizando processo de obtenção da estimativa do rendimento domiciliar real
    pnadc_1tri <- transform(pnadc_1tri, V2001_rendimento=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,1))
    pnadc_1tri <- transform(pnadc_1tri, VD4020real_proprioano=ifelse(is.na(VD4020) | is.na(V2001_rendimento),NA,VD4020*Efetivo))
    pnadc_1tri <- transform(pnadc_1tri, VD4020real_ultimoano=ifelse(is.na(VD4020) | is.na(V2001_rendimento),NA,VD4020*Efetivo))
    
    
    pnadc_1tri_rendimento <- pnadc_1tri %>% 
      dplyr::group_by(ID_DOMICILIO) %>% 
      dplyr::summarise(moradores_rendimento=sum(V2001_rendimento, na.rm=TRUE),
                       rendimento_todos_trabalhos_proprioano=sum(VD4020real_proprioano, na.rm=TRUE),
                       rendimento_todos_trabalhos_ultimoano=sum(VD4020real_ultimoano, na.rm=TRUE))
    
    # Rendimento domiciliar real
    pnadc_1tri_rendimento <- transform(pnadc_1tri_rendimento, VD5007real_proprioano=rendimento_todos_trabalhos_proprioano)
    pnadc_1tri_rendimento <- transform(pnadc_1tri_rendimento, VD5007real_ultimoano=rendimento_todos_trabalhos_ultimoano)
    
    # Rendimento domiciliar per capita real
    pnadc_1tri_rendimento <- transform(pnadc_1tri_rendimento, VD5008real_proprioano=VD5007real_proprioano/moradores_rendimento)
    pnadc_1tri_rendimento <- transform(pnadc_1tri_rendimento, VD5008real_ultimoano=VD5007real_ultimoano/moradores_rendimento)
    
    # Remvendo colunas da base principal que não vamos mais usar
    pnadc_1tri <- pnadc_1tri[,!(names(pnadc_1tri) %in% c("V2001_rendimento","VD4020real_proprioano","VD4020real_ultimoano"))]
    
    # Remvendo colunas da base de rendimento que não vamos mais usar
    pnadc_1tri_rendimento <- pnadc_1tri_rendimento[,!(names(pnadc_1tri_rendimento) %in% c("moradores_rendimento","rendimento_todos_trabalhos_proprioano","rendimento_todos_trabalhos_ultimoano"))]
    
    # Combinando as bases 
    pnadc_1tri <- merge(x=pnadc_1tri, y=pnadc_1tri_rendimento, by.x="ID_DOMICILIO", by.y="ID_DOMICILIO", all.x=TRUE, all.y=FALSE)
    
    rm(pnadc_1tri_rendimento)
    
    # Retirando variáveis de rendimento domiciliar para casos em que o morador não é da família
    pnadc_1tri <- transform(pnadc_1tri, VD5007real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_proprioano))
    pnadc_1tri <- transform(pnadc_1tri, VD5008real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_proprioano))
    pnadc_1tri <- transform(pnadc_1tri, VD5007real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_ultimoano))
    pnadc_1tri <- transform(pnadc_1tri, VD5008real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_ultimoano))
    
    salariominimo_ultimoano <- 1320
    
    ########### Educação dos Pais (Máximo entre pai e mãe)
    # Educação da mãe
    pnadc_1tri <- pnadc_1tri %>%
      # Passo 1: Identificar a mãe em cada família
      mutate(is_mae = (as.numeric(V2007) == 2 & (as.numeric(VD2002) == 1 | as.numeric(VD2002) == 2 | as.numeric(VD2002) == 6)
      )) %>%
      
      # Passo 2: Agrupar pela família e criar a coluna educacao_mae
      group_by(ID_DOMICILIO) %>%
      mutate(educacao_mae = ifelse(any(is_mae), VD3005[is_mae][1], NA)) %>%
      
      # Passo 3: Remover a coluna auxiliar is_mae e desagrupar
      select(-is_mae) %>%
      ungroup()
    
    # Educação do pai e máximo entre mãe e pai
    pnadc_1tri <- pnadc_1tri %>%
      # Passo 1: Identificar a mãe em cada família
      mutate(is_pai = (as.numeric(V2007) == 1 & (as.numeric(VD2002) == 1 | as.numeric(VD2002) == 2 | as.numeric(VD2002) == 6)
      )) %>%
      
      # Passo 2: Agrupar pela família e criar a coluna educacao_pai
      group_by(ID_DOMICILIO) %>%
      mutate(educacao_pai = ifelse(any(is_pai), VD3005[is_pai][1], NA)) %>%
      
      # Passo 3: Criar a coluna max_educacao_pais com o valor máximo entre educacao_mae e educacao_pai
      mutate(max_educacao_pais = pmax(educacao_mae, educacao_pai, na.rm = TRUE)) %>%
      
      # Passo 4: Remover a coluna auxiliar is_pai e desagrupar
      select(-is_pai) %>%
      ungroup()
    
    
    # Criando variável de id_individuo
    pnadc_1tri <- pnadc_1tri %>%
      mutate(id_individuo = paste0(UPA, "_", V1008, "_", V1014, "_", V2003, "_", V2008, "_", V20081, "_", V20082))
    
    pnadc_2tri <- pnadc_2tri %>%
      mutate(id_individuo = paste0(UPA, "_", V1008, "_", V1014, "_", V2003, "_", V2008, "_", V20081, "_", V20082))
    
    # Filtrando para pegar somente individuos que tem Visita = 1 no primeiro trimestre                  
    pnadc_1tri_1visita <- pnadc_1tri %>%
      filter(V1016 == "1")
    
    # Combinar bases dos dois trimestres   
    pnadc_merge <- inner_join(pnadc_1tri_1visita, pnadc_2tri, by = "id_individuo")
    
    # Filtrar base para quem frequenta escola no 1 trimestre e não frequenta no 2 trimestre
    abandono <- pnadc_merge %>%
      filter(V3002.x=="Sim" & V3002.y=="Não")
    
    # Criar lista de indivíduos que abandonaram
    abandono_list <- abandono %>%
      group_by(id_individuo) %>%
      summarise(id_individuo)
    
    # Fazer o merge para encontrar os indivíduos que abandonaram na PNADc do trimestre 1
    pnadc_abandono <- semi_join(pnadc_1tri, abandono_list, by = "id_individuo")
    
    # Realizando processo de incorporação do desenho amostral nos microdados
    pnadc_1tri_1visita <- tibble::as_tibble(x=pnadc_1tri_1visita)
    pnadc_1tri_1visita <- PNADcIBGE::pnadc_design(data_pnadc=pnadc_1tri_1visita)
    
    pnadc_1tri_1visita <- transform(pnadc_1tri_1visita, contagem=1)
    
    pnadc_abandono <- tibble::as_tibble(x=pnadc_abandono)
    pnadc_abandono <- PNADcIBGE::pnadc_design(data_pnadc=pnadc_abandono)
    
    pnadc_abandono <- transform(pnadc_abandono, contagem=1)
    
    # Definindo subset para o Público Alvo
    pnadc_abandono_pdm <- subset(pnadc_abandono, (V2009 >= 14 & V2009 <= 24) 
                                 & V3003A == "Regular do ensino médio"
                                 & V3002A == "Rede pública"
                                 & (!is.na(VD5008real_ultimoano) & VD5008real_ultimoano<=salariominimo_ultimoano/2)
                                 & VD2004 != "Unipessoal")
    
    
    # Definindo subset do público potencial do programa Pé de Meia
    pnadc_abandono_pdmpotencial <- subset(pnadc_abandono, (V2009 >= 14 & V2009 <= 24) 
                                          & ((!is.na(VD5008real_ultimoano) & VD5008real_ultimoano<=salariominimo_ultimoano/2))
                                          & (VD3004 == "Fundamental completo ou equivalente" | VD3004 == "Médio incompleto ou equivalente"))
    
    
    assign(paste0("total_1visita_", ano, "_", t), svytotal(~contagem, pnadc_1tri_1visita, na.rm = TRUE))
    assign(paste0("total_abandono_", ano, "_", t), svytotal(~contagem, pnadc_abandono, na.rm = TRUE))
    assign(paste0("total_abandono_serie_", ano, "_", t), svymean(~V3003A, pnadc_abandono, na.rm = TRUE))
    assign(paste0("total_abandono_pdm_", ano, "_", t), svytotal(~contagem, pnadc_abandono_pdm, na.rm = TRUE))
    assign(paste0("total_abandono_pdm_serie_", ano, "_", t), svymean(~V3006, pnadc_abandono_pdm, na.rm = TRUE))
    assign(paste0("total_abandono_pdmpotencial_", ano, "_", t), svytotal(~contagem, pnadc_abandono_pdmpotencial, na.rm = TRUE))
    assign(paste0("total_abandono_pdmpotencial_serie_", ano, "_", t), svymean(~V3006, pnadc_abandono_pdmpotencial, na.rm = TRUE))
    
  }
  
}

# Salvar os resultados no Excel
wb <- createWorkbook()

# Dicionário para abreviar os nomes das estatísticas
abreviacoes <- list(
  "total_1visita" = "1visita",
  "total_abandono" = "aband",
  "total_abandono_serie" = "aband_serie",
  "total_abandono_pdm" = "aband_pdm",
  "total_abandono_pdm_serie" = "aband_pdm_serie",
  "total_abandono_pdmpotencial_serie" = "aband_pdm_pot_serie"
)

# Loop para salvar cada estatística em uma aba
for (estat in names(abreviacoes)) {
  for (ano in anos) {
    df <- data.frame()
    
    # Loop para coletar valores de cada trimestre
    for (t in 1:3) {
      var_name <- paste0(estat, "_", ano, "_", t)
      valor <- get(var_name)
      df <- rbind(df, data.frame(Ano = ano, Trimestre = t, Valor = valor))
    }
    
    # Nome único e abreviado para cada aba
    sheet_name <- paste0(abreviacoes[[estat]], "_", ano)
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, df)
  }
}

# Salva o workbook
saveWorkbook(wb, "Resultados_PNAD_Abandono_Escolar.xlsx", overwrite = TRUE)


