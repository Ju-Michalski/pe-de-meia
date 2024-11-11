library(dplyr)
library(PNADcIBGE)
library(survey)
library(haven)

# Definindo opção de exibição de números sem representação em exponencial
aviso <- getOption("warn")
options(warn=-1)
options(scipen=999)
options(warn=aviso)
rm(aviso)

# Carregar base de dados
pnad_data <- read.csv("pnad_unificado_filtrado.csv")

# Criar uma nova coluna indicando se a renda é menor que 1/2 salário mínimo
pnad_data <- pnad_data %>%
  mutate(renda_menor_meio_salario = if_else(VD5009real_ultimoano %in% c("Até ¼ salário mínimo", "Mais de ¼ até ½ salário mínimo"), 
                                            "Sim", 
                                            "Não"))

pnad_data <- pnad_data %>%
  mutate(
    V2007 = case_when(
      V2007 == "Mulher" ~ 2,  # Assumindo que o valor é "Mulher" para mulheres
      V2007 == "Homem" ~ 1,   # Assumindo que o valor é "Homem" para homens
      TRUE ~ as.numeric(V2007)  # Mantém outros valores, se houver
    ),
    VD2002 = case_when(
      VD2002 == 1  ~ "Pessoa responsável",
      VD2002 == 2  ~ "Cônjuge ou companheiro(a)",
      VD2002 == 3  ~ "Filho(a)",
      VD2002 == 4  ~ "Enteado(a)",
      VD2002 == 5  ~ "Genro ou nora",
      VD2002 == 6  ~ "Pai, mãe, padrasto ou madrasta",
      VD2002 == 7  ~ "Sogro(a)",
      VD2002 == 8  ~ "Neto(a)",
      VD2002 == 9  ~ "Bisneto(a)",
      VD2002 == 10 ~ "Irmão ou irmã",
      VD2002 == 11 ~ "Avô ou avó",
      VD2002 == 12 ~ "Outro parente",
      VD2002 == 13 ~ "Agregado(a)",
      VD2002 == 14 ~ "Convivente",
      VD2002 == 15 ~ "Pensionista",
      VD2002 == 16 ~ "Empregado(a) doméstico(a)",
      VD2002 == 17 ~ "Parente do(a) empregado(a) doméstico(a)",
      TRUE ~ as.character(VD2002)  # Mantém outros valores se houver
    ),
    VD3005 = case_when(
      VD3005 == 0  ~ "Sem instrução e menos de 1 ano de estudo",
      VD3005 == 1  ~ "1 ano de estudo",
      VD3005 == 2  ~ "2 anos de estudo",
      VD3005 == 3  ~ "3 anos de estudo",
      VD3005 == 4  ~ "4 anos de estudo",
      VD3005 == 5  ~ "5 anos de estudo",
      VD3005 == 6  ~ "6 anos de estudo",
      VD3005 == 7  ~ "7 anos de estudo",
      VD3005 == 8  ~ "8 anos de estudo",
      VD3005 == 9  ~ "9 anos de estudo",
      VD3005 == 10 ~ "10 anos de estudo",
      VD3005 == 11 ~ "11 anos de estudo",
      VD3005 == 12 ~ "12 anos de estudo",
      VD3005 == 13 ~ "13 anos de estudo",
      VD3005 == 14 ~ "14 anos de estudo",
      VD3005 == 15 ~ "15 anos de estudo",
      VD3005 == 16 ~ "16 anos ou mais de estudo",
      TRUE ~ "Não aplicável"  # Caso para valores que não se encaixam nos listados
    )
  )

# Variável de Ensino Médio, interação entre essas duas, se tem Fund. Completo
pnad_data <- pnad_data %>%
  transform(
    # Ensino Médio
    em = factor(
      case_when(
        V3002 == "Sim" & V3003A == "Regular do ensino médio" ~ "Estuda EM", 
        V3002 == "Não" ~ "Não Estuda EM",
        TRUE ~ NA_character_
      ),
      levels = c("Estuda EM","Não Estuda EM")
    ),
    # Unipessoal
    unip = factor(
      case_when(
        VD2004 == "Unipessoal" ~ "Unipessoal",
        VD2004 == "Nuclear" | VD2004 == "Estendida" | VD2004 == "Composta" ~ "Não é Unipessoal",
        TRUE ~ NA_character_
      ),
      levels = c("Unipessoal", "Não é Unipessoal")
    ),
    # Tem EF Completo?
    ef_comp = case_when(
      VD3004 == "Fundamental completo ou equivalente" ~ 1,
      .default = 0
    )
  )

# Passo 1: Criar a variável de educação da mãe
pnad_data <- pnad_data %>%
  mutate(
    is_mae = (as.numeric(V2007) == 2 & (as.numeric(VD2002) %in% c(1, 2, 6)))
  ) %>%
  group_by(ID_DOMICILIO) %>%
  mutate(
    educacao_mae = ifelse(any(is_mae), first(VD3005[is_mae]), NA)
  ) %>%
  ungroup() %>%
  select(-is_mae)

# Passo 2: Criar a variável de educação do pai e obter o máximo entre mãe e pai
pnad_data <- pnad_data %>%
  mutate(
    is_pai = (as.numeric(V2007) == 1 & (as.numeric(VD2002) %in% c(1, 2, 6)))
  ) %>%
  group_by(ID_DOMICILIO) %>%
  mutate(
    educacao_pai = ifelse(any(is_pai), first(VD3005[is_pai]), NA),
    max_educacao_pais = pmax(educacao_mae, educacao_pai, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(-is_pai)

# Realizando processo de incorporação do desenho amostral nos microdados
pnad_jovens <- tibble::as_tibble(x=pnad_jovens)
pnad_jovens <- PNADcIBGE::pnadc_design(data_pnadc=pnad_jovens)
str(object=pnad_jovens)

totalvisita <- svytotal(x=~visita, design=pnad_jovens, na.rm=TRUE)
totalvisita