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
pnad_data <- read.csv("D:\\Ajudinha\\pe-de-meia\\bases\\pnad_unificado_filtrado2.CSV")


# Variável de Ensino Médio, interação entre essas duas, se tem Fund. Completo
pnad_data <- pnad_data %>%
  transform(
    # Ensino Médio
    em = factor(
      case_when(
        V3002 == 1 & V3003A == 6 ~ "Estuda EM", 
        V3002 == 2 ~ "Não Estuda EM",
        TRUE ~ NA_character_
      ),
      levels = c("Estuda EM","Não Estuda EM")
    ),
    # Unipessoal
    unip = factor(
      case_when(
        VD2004 == 1 ~ "Unipessoal",
        VD2004 == 2 | VD2004 == 3 | VD2004 == 4 ~ "Não é Unipessoal",
        TRUE ~ NA_character_
      ),
      levels = c("Unipessoal", "Não é Unipessoal")
    ),
    # Tem EF Completo?
    ef_comp = case_when(
      VD3004 == 3 ~ 1,
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
pnad_data <- tibble::as_tibble(x=pnad_data)
pnad_data <- PNADcIBGE::pnadc_design(data_pnadc=pnad_data)
str(object=pnad_data)

# Definindo subset do público-alvo do programa Pé de Meia
# Cria uma variável indicadora temporária no objeto de pesquisa
pnad_data <- update(pnad_data,
                    atende_criterios = (V2009 >= 14 & V2009 <= 24 &
                                          em == "Estuda EM" &
                                          V3002A == 2 &
                                          (renda_menor_meio_salario == "Sim" | V5001A == 1 | V5002A == 1 | V5003A == 1) &
                                          unip == "Não é Unipessoal")
)

# Filtra o objeto svydesign para manter indivíduos que atendem ao critério em pelo menos um dos períodos
pnadc_pa <- subset(pnad_data, ave(atende_criterios, ID_PESSOA, FUN = any))


# Definindo subset do público-alvo potencial do programa Pé de Meia
# Adiciona a variável indicadora ao objeto de pesquisa para o subset do programa Pé de Meia
pnad_data <- update(pnad_data,
                    atende_criterios_papotencial = (V2009 >= 14 & V2009 <= 24 &
                                                      (renda_menor_meio_salario == "Sim" | V5001A == 1 | V5002A == 1 | V5003A == 1) &
                                                      (ef_comp == 1 | VD3004 == 4))
)

# Filtra o objeto svydesign para manter os indivíduos que atendem ao critério em pelo menos um dos períodos
pnadc_papotencial <- subset(pnad_data, ave(atende_criterios_papotencial, ID_PESSOA, FUN = any))

# Definindo subset de pessoas na rede pública de ensino 
pnadc_redepublica <- subset(pnad_data, V3002A == "Rede pública") #add ser EM



pnadc_redepublica <- transform(pnadc_redepublica, contagem=1)





totalvisita <- svytotal(x=~visita, design=pnad_data, na.rm=TRUE)
totalvisita