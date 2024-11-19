library(PNADcIBGE)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

# Salário mínimo do último ano
salariominimo_ultimoano <- 1320

pnadtri1<-get_pnadc(year=2023,quarter=1,design = FALSE, deflator=TRUE, labels=FALSE)
pnadtri3<-get_pnadc(year=2023,quarter=2,design = FALSE, deflator=TRUE, labels=FALSE)
pnadtri3<-get_pnadc(year=2023,quarter=3,design = FALSE, deflator=TRUE, labels=FALSE)
pnadtri4<-get_pnadc(year=2023,quarter=4,design = FALSE, deflator=TRUE, labels=FALSE)

# Criando variáveis auxiliares para obtenção da estimativa desejada
pnadtri1 <- transform(pnadtri1, ID_DOMICILIO=paste0(UPA,V1008,V1014))
pnadtri1 <- transform(pnadtri1, Pais=as.factor("Brasil"))
pnadtri1 <- transform(pnadtri1, ID_PESSOA=paste0(UPA,V1008,V1014,V2003))
pnadtri1$Pais <- factor(x=pnadtri1$Pais, levels=c("Brasil"))
pnadtri1 <- transform(pnadtri1, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",ifelse(substr(UPA, start=1, stop=1)=="4","Sul",ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
pnadtri1$GR <- factor(x=pnadtri1$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))

pnadtri2 <- transform(pnadtri2, ID_DOMICILIO=paste0(UPA,V1008,V1014))
pnadtri2 <- transform(pnadtri2, Pais=as.factor("Brasil"))
pnadtri2 <- transform(pnadtri2, ID_PESSOA=paste0(UPA,V1008,V1014,V2003))
pnadtri2$Pais <- factor(x=pnadtri2$Pais, levels=c("Brasil"))
pnadtri2 <- transform(pnadtri2, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",ifelse(substr(UPA, start=1, stop=1)=="4","Sul",ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
pnadtri2$GR <- factor(x=pnadtri2$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))

pnadtri3 <- transform(pnadtri3, ID_DOMICILIO=paste0(UPA,V1008,V1014))
pnadtri3 <- transform(pnadtri3, Pais=as.factor("Brasil"))
pnadtri3 <- transform(pnadtri3, ID_PESSOA=paste0(UPA,V1008,V1014,V2003))
pnadtri3$Pais <- factor(x=pnadtri3$Pais, levels=c("Brasil"))
pnadtri3 <- transform(pnadtri3, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",ifelse(substr(UPA, start=1, stop=1)=="4","Sul",ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
pnadtri3$GR <- factor(x=pnadtri3$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))

pnadtri4 <- transform(pnadtri4, ID_DOMICILIO=paste0(UPA,V1008,V1014))
pnadtri4 <- transform(pnadtri4, Pais=as.factor("Brasil"))
pnadtri4 <- transform(pnadtri4, ID_PESSOA=paste0(UPA,V1008,V1014,V2003))
pnadtri4$Pais <- factor(x=pnadtri4$Pais, levels=c("Brasil"))
pnadtri4 <- transform(pnadtri4, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",ifelse(substr(UPA, start=1, stop=1)=="4","Sul",ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
pnadtri4$GR <- factor(x=pnadtri4$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))

# Função para calcular e adicionar renda domiciliar per capita (RDPC) à base
calcula_renda_domiciliar <- function(base) {
  base %>%
    mutate(
      # Criar o ID do indivíduo para unificação entre as visitas
      id_pessoa = paste0(UPA, "_", V1008, "_", V1014, "_", V2003, "_", V2008, "_", V20081, "_", V20082),
      ID_DOMICILIO = paste0(UPA, V1008, V1014),
      V2001_rendimento = ifelse(V2005 %in% c("Pensionista", "Empregado(a) doméstico(a)", 
                                             "Parente do(a) empregado(a) doméstico(a)"), NA, 1),
      VD5008real_proprioano = VD5008 * Efetivo,
      VD5008real_ultimoano = VD5008 * Efetivo
    ) %>%
    group_by(ID_DOMICILIO) %>%
    mutate(
      moradores_rendimento = sum(V2001_rendimento, na.rm = TRUE),
      rendimento_proprioano = sum(VD5008real_proprioano, na.rm = TRUE),
      rendimento_ultimoano = sum(VD5008real_ultimoano, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      RDPC_proprioano = rendimento_proprioano / moradores_rendimento,
      RDPC_ultimoano = rendimento_ultimoano / moradores_rendimento
    )
}

# Função para calcular a educação dos pais (máximo entre mãe e pai)
calcula_educacao_pais <- function(base) {
  base %>%
    # Educação da mãe
    mutate(is_mae = (as.numeric(V2007) == 2 & as.numeric(VD2002) %in% c(1, 2, 6))) %>%
    group_by(ID_DOMICILIO) %>%
    mutate(educacao_mae = ifelse(any(is_mae), VD3005[is_mae][1], NA)) %>%
    ungroup() %>%
    select(-is_mae) %>%
    # Educação do pai
    mutate(is_pai = (as.numeric(V2007) == 1 & as.numeric(VD2002) %in% c(1, 2, 6))) %>%
    group_by(ID_DOMICILIO) %>%
    mutate(educacao_pai = ifelse(any(is_pai), VD3005[is_pai][1], NA)) %>%
    mutate(max_educacao_pais = pmax(educacao_mae, educacao_pai, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-is_pai)
}

# Função para ajustar a base com o objeto de design da PNAD
criar_design <- function(base) {
  base_tibble <- tibble::as_tibble(base)
  design <- pnadc_design(data_pnadc = base_tibble)
  return(design)
}

# Ano de interesse (2023)
ano <- 2023

# Carregar as bases do ano 2023 (todos os trimestres)
bases_ano_2023 <- map(1:4, function(tri) {
  get_pnadc(year = ano, quarter = tri, defyear = ano, labels = TRUE, deflator = TRUE, design = FALSE)
})

# Processamento dos trimestres em pares (1-2, 2-3, 3-4)
pares_trimestres <- map(1:3, function(t) {
  base_1tri <- bases_ano_2023[[t]]
  base_2tri <- bases_ano_2023[[t + 1]]
  
  # Calcular renda domiciliar diretamente nas bases
  base_1tri <- calcula_renda_domiciliar(base_1tri)
  base_2tri <- calcula_renda_domiciliar(base_2tri)
  
  # Calcular educação dos pais
  base_1tri <- calcula_educacao_pais(base_1tri)
  base_2tri <- calcula_educacao_pais(base_2tri)
  
  # Criar o design de amostra
  pnadc_1tri_design <- criar_design(base_1tri)
  pnadc_2tri_design <- criar_design(base_2tri)
  
  # Unir os trimestres
  abandono <- inner_join(
    base_1tri %>% rename_with(~paste0(., ".x"), everything()),
    base_2tri %>% rename_with(~paste0(., ".y"), everything()),
    by = "id_pessoa"
  )
  
  # Subdefinir público alvo e público potencial
  abandono_pdm <- abandono %>%
    filter(
      V2009.x >= 14 & V2009.x <= 24,
      V3003A.x == "Regular do ensino médio",
      V3002A.x == "Rede pública",
      (!is.na(VD5008real_ultimoano.x) & VD5008real_ultimoano.x <= salariominimo_ultimoano / 2),
      VD2004.x != "Unipessoal"
    )
  
  abandono_pdmpotencial <- abandono %>%
    filter(
      V2009.x >= 14 & V2009.x <= 24,
      (!is.na(VD5008real_ultimoano.x) & VD5008real_ultimoano.x <= salariominimo_ultimoano / 2),
      VD3004.x %in% c("Fundamental completo ou equivalente", "Médio incompleto ou equivalente")
    )
  
  # Retornar resultados do par de trimestres
  list(
    base_1tri = base_1tri,
    base_2tri = base_2tri,
    abandono = abandono,
    abandono_pdm = abandono_pdm,
    abandono_pdmpotencial = abandono_pdmpotencial
  )
})

# Resultados finais por trimestre para 2023
pares_trimestres
