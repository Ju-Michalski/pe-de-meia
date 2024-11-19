library(dplyr)
library(PNADcIBGE)
library(survey)
library(haven)
library(knitr)
library(ggplot2)

# Definindo opção de exibição de números sem representação em exponencial
aviso <- getOption("warn")
options(warn=-1)
options(scipen=999)
options(warn=aviso)
rm(aviso)

# Carregar base de dados
pnad_data <- read.csv("D:\\Ajudinha\\pe-de-meia\\bases\\pnad_unificado_filtrado2.CSV")

pnad_data <- pnad_data %>%
  mutate(renda_menor_meio_salario = if_else(VD5009real_ultimoano %in% c("Até ¼ salário mínimo", "Mais de ¼ até ½ salário mínimo"), 
                                            "Sim", 
                                            "Não"))


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


##abandono
pnad_data <- pnad_data %>%
  arrange(ID_PESSOA, visita) %>% # Ordena os dados por ID_PESSOA e ordem das visitas
  group_by(ID_PESSOA) %>%
  mutate(
    abandono_escolar = case_when(
      visita == 2 & lag(V3002) == 1 & V3002 == 2 ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  ungroup()

# Realizando processo de incorporação do desenho amostral nos microdados
pnad_data <- tibble::as_tibble(x=pnad_data)
pnad_data <- PNADcIBGE::pnadc_design(data_pnadc=pnad_data)
str(object=pnad_data)

# Criando dummies diretamente no objeto survey
pnad_data <- update(
  pnad_data,
  mulher = ifelse(V2007 == 2, 1, 0), # 1 para mulher, 0 para outros
  branca = ifelse(V2010 == 1, 1, 0),
  preta = ifelse(V2010 == 2, 1, 0),
  amarela = ifelse(V2010 == 3, 1, 0),
  parda = ifelse(V2010 == 4, 1, 0),
  indigena = ifelse(V2010 == 5, 1, 0),
  norte = ifelse(GR == "Norte", 1, 0),
  nordeste = ifelse(GR == "Nordeste", 1, 0),
  sudeste = ifelse(GR == "Sudeste", 1, 0),
  sul = ifelse(GR == "Sul", 1, 0),
  centro_oeste = ifelse(GR == "Centro-Oeste", 1, 0),
  urbana = ifelse(V1022==1,1,0),
  rural = ifelse(V1022==2,1,0),
  matriculado = ifelse(V3002==1,1,0),
  abandono=ifelse(abandono_escolar==TRUE,1,0)
)

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
# Adiciona a variável indicadora ao objeto de pesquisa para o subset de pessoas na rede pública de ensino cursando EM
pnad_data <- update(pnad_data,
                    atende_criterios_redepublica = (V3002A == 2 & 
                                                      (ef_comp == 1 | VD3004 == 4))
)

# Filtra o objeto svydesign para manter os indivíduos que atendem ao critério em pelo menos um dos períodos
pnadc_redepublica <- subset(pnad_data, ave(atende_criterios_redepublica, ID_PESSOA, FUN = any))

#Calculo da taxa de abandono

proporcao_abandono1 <- svymean(~abandono_escolar, design = pnadc_pa, na.rm = TRUE)
proporcao_abandono1

proporcao_abandono2 <- svymean(~abandono_escolar, design = pnadc_papotencial, na.rm = TRUE)
proporcao_abandono2

proporcao_abandono3 <- svymean(~abandono_escolar, design = pnadc_redepublica, na.rm = TRUE)
proporcao_abandono3

# Calcular a proporção de abandono entre os matriculados
proporcao_abandono <- sum(pnadc_pa$abandono_escolar * pnadc_pa$matriculado, na.rm = TRUE) / sum(pnadc_pa$matriculado, na.rm = TRUE)
proporcao_abandono

# Ajustando o modelo probit
modelo_probit <- svyglm(
  abandono ~ mulher + branca + preta + parda + indigena +
    norte + nordeste + sudeste  + centro_oeste + V2009 + renda_menor_meio_salario+max_educacao_pais,
  design = pnadc_pa,
  family = binomial(link = "probit")
)

# Resumo do modelo
summary(modelo_probit)




#Distribuição por idade
# Filtrar a base "público alvo" para quando atende_criterios == TRUE e calcular a frequência ponderada
frequencia_cor_visita_criterio <- svytable(~V2009 + visita, subset(pnadc_redepublica, atende_criterios_redepublica == TRUE))

# Exibir a tabela formatada com kable
library(knitr)
kable(as.data.frame(frequencia_cor_visita_criterio), 
      col.names = c("Idade", "Visita", "Frequência Ponderada"),
      caption = "Frequência Ponderada de Pessoas por Cor e Visita (atende_criterios = TRUE) na Base 'Público Alvo'")

# Transformar a tabela de frequência ponderada em um data frame
frequencia_cor_visita_criterio_df <- as.data.frame(frequencia_cor_visita_criterio)

# Criar o gráfico pa e papotencial
ggplot(frequencia_cor_visita_criterio_df, aes(x = V2009, y = Freq, fill = visita)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("darkseagreen3", "skyblue3")) +  
  labs(
    x = "Idade",
    y = "Frequência Ponderada",
    fill = "Visita",
    title = "Frequência Ponderada de Pessoas por Idade e Visita para o Público Alvo Potencial",
    caption = "Base: 'Público Alvo Potencial' (atende_criterios = TRUE)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Criar o gráfico rede publica 
# Converter a variável V2009 para numérica, se ainda não estiver
frequencia_cor_visita_criterio_df$V2009 <- as.numeric(as.character(frequencia_cor_visita_criterio_df$V2009))

ggplot(frequencia_cor_visita_criterio_df, aes(x = V2009, y = Freq, color = visita, group = visita)) +
  geom_line(size = 1) +
  labs(
    x = "Idade",
    y = "Frequência Ponderada",
    color = "Visita",
    title = "Frequência Ponderada de Pessoas por Idade e Visita para Rede Pública",
    caption = "Base: 'Rede Pública'"
  ) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(13, 101, by =10)) +  # Define os intervalos de 5 em 5 anos
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calcula o total de pessoas únicas na base filtrada
total_pessoas_unicas1 <- svytotal(~contagem_unica, design = pnadc_pa)
total_pessoas_unicas1
total_pessoas_unicas2 <- svytotal(~contagem_unica, design = pnadc_papotencial)
total_pessoas_unicas2
total_pessoas_unicas3 <- svytotal(~contagem_unica, design = pnadc_redepublica)
total_pessoas_unicas3



