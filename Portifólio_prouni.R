library(tidyverse)
library(lubridate)
library(fs)

caminho <- "C:/Users/maria.alves/Downloads/Analise_Prouni"
arquivos <- dir_ls(caminho, glob = "*.csv")

# Função para ler e JÁ corrigir o nome na entrada
ler_e_corrigir <- function(arquivo) {
  df <- read_csv2(arquivo, col_types = cols(.default = "c"), locale = locale(encoding = "Latin1"))
  
  # SE encontrar a coluna "DATA_NASCIMENTO", renomeia para o padrão antigo
  if ("DATA_NASCIMENTO" %in% names(df)) {
    df <- df %>% rename(DT_NASCIMENTO_BENEFICIARIO = DATA_NASCIMENTO)
  }
  
  # SE encontrar "MODALIDADE_ENSINO", renomeia
  if ("MODALIDADE_ENSINO" %in% names(df)) {
    df <- df %>% rename(MODALIDADE_ENSINO_BOLSA = MODALIDADE_ENSINO)
  }
  
  return(df)
}

# 1. Carrega tudo já corrigido
dados_completos <- map_dfr(arquivos, ler_e_corrigir)

# 2. Seleciona e Calcula (Agora todos têm o mesmo nome de coluna!)
dados_grafico <- dados_completos %>%
  select(ANO_CONCESSAO_BOLSA, MODALIDADE_ENSINO_BOLSA, DT_NASCIMENTO_BENEFICIARIO) %>%
  mutate(
    # Padroniza Modalidade
    MODALIDADE = str_to_upper(MODALIDADE_ENSINO_BOLSA),
    MODALIDADE = ifelse(str_detect(MODALIDADE, "DIST"), "EAD", MODALIDADE),
    
    # Converte Data (Tenta separar por hífen ou barra)
    Data_Real = parse_date_time(DT_NASCIMENTO_BENEFICIARIO, orders = c("dmy", "dmY", "d-m-y", "d-m-Y")),
    
    # Calcula Idade
    Ano = parse_number(ANO_CONCESSAO_BOLSA),
    Idade = Ano - year(Data_Real)
  ) %>%
  filter(!is.na(Idade), Idade > 16, Idade < 80)

# 3. Gerar Gráfico
ggplot(dados_grafico, aes(x = MODALIDADE, y = Idade, fill = MODALIDADE)) +
  geom_boxplot() +
  labs(title = "Perfil Etário: Presencial vs EAD", y = "Idade") +
  theme_minimal()

--------------------------------------------------------------

# 1. Preparação Garantida (Padroniza antes de agrupar)
insight_geo <- dados_completos %>%
  # Tenta encontrar a coluna de UF (seja do jeito antigo ou novo)
  mutate(UF_FINAL = coalesce(
    if("SIGLA_UF_BENEFICIARIO_BOLSA" %in% names(.)) SIGLA_UF_BENEFICIARIO_BOLSA else NULL,
    if("UF_BENEFICIARIO" %in% names(.)) UF_BENEFICIARIO else NULL
  )) %>%
  
  # Garante que só existam as modalidades "EAD" e "PRESENCIAL" (sem nomes longos)
  mutate(MODALIDADE_CORRIGIDA = case_when(
    str_detect(MODALIDADE_ENSINO_BOLSA, "DIST") ~ "EAD",
    TRUE ~ "PRESENCIAL"
  )) %>%
  
  # Remove estados vazios ou nulos
  filter(!is.na(UF_FINAL)) %>%
  
  # 2. O Agrupamento
  group_by(UF_FINAL, MODALIDADE_CORRIGIDA) %>%
  count(name = "Total") %>%
  
  # 3. O Pivot (Transforma linhas em colunas)
  pivot_wider(names_from = MODALIDADE_CORRIGIDA, values_from = Total, values_fill = 0) %>%
  
  # 4. A Matemática (Agora segura, pois forçamos os nomes EAD e PRESENCIAL acima)
  mutate(
    Total_Geral = EAD + PRESENCIAL,
    Porcentagem_EAD = (EAD / Total_Geral) * 100
  ) %>%
  
  # Pega os Top 10
  arrange(desc(Porcentagem_EAD)) %>%
  head(10)

print(insight_geo)

# 5. O Gráfico Melhorado (Com rótulos de %)
ggplot(insight_geo, aes(x = reorder(UF_FINAL, Porcentagem_EAD), y = Porcentagem_EAD)) +
  geom_col(fill = "coral") +
  # Adiciona o número da porcentagem na ponta da barra (Fica lindo no portfólio!)
  geom_text(aes(label = paste0(round(Porcentagem_EAD, 1), "%")), 
            hjust = -0.1, size = 3.5) +
  coord_flip() + 
  labs(
    title = "Top 10 Estados que mais dependem do EAD",
    subtitle = "Proporção de bolsas EAD em relação ao total ofertado no estado",
    y = "% de Bolsas EAD",
    x = "Estado",
    caption = "Fonte: Dados Abertos MEC/Prouni"
  ) +
  theme_minimal() +
  # Aumenta um pouco o limite do gráfico para caber o texto da porcentagem
  scale_y_continuous(limits = c(0, 60))

------------------------------------------------------------
  #####Distribuição de Raça e Gênero (2005-2020)

library(tidyverse)
library(fs)

# --- 1. FUNÇÃO DE LEITURA (Padrão) ---
ler_e_corrigir <- function(arquivo) {
  df <- read_csv2(arquivo, col_types = cols(.default = "c"), 
                  locale = locale(encoding = "Latin1"), show_col_types = FALSE)
  
  if ("RACA_BENEFICIARIO" %in% names(df)) df <- df %>% rename(RACA_BENEFICIARIO_BOLSA = RACA_BENEFICIARIO)
  if ("SEXO_BENEFICIARIO" %in% names(df)) df <- df %>% rename(SEXO_BENEFICIARIO_BOLSA = SEXO_BENEFICIARIO)
  
  df %>% select(any_of(c("RACA_BENEFICIARIO_BOLSA", "SEXO_BENEFICIARIO_BOLSA")))
}

# --- 2. CARREGAR DADOS ---
caminho <- "C:/Users/maria.alves/Downloads/Analise_Prouni"
arquivos <- dir_ls(caminho, glob = "*.csv")
dados_brutos <- map_dfr(arquivos, ler_e_corrigir)

# --- 3. LIMPEZA E FILTRO DE EXCLUSÃO ---
dados_grafico <- dados_brutos %>%
  filter(!is.na(RACA_BENEFICIARIO_BOLSA), !is.na(SEXO_BENEFICIARIO_BOLSA)) %>%
  
  mutate(
    # Padroniza Raça
    RACA_TEMP = str_to_upper(RACA_BENEFICIARIO_BOLSA),
    RACA_TEMP = str_trim(RACA_TEMP), # Remove espaços em branco nas pontas
    
    # Corrige os Indígenas
    RACA = case_when(
      str_detect(RACA_TEMP, "IND") ~ "INDÍGENA",
      TRUE ~ RACA_TEMP
    ),
    
    # Padroniza Sexo
    SEXO_RAW = str_to_upper(SEXO_BENEFICIARIO_BOLSA),
    SEXO = case_when(
      str_detect(SEXO_RAW, "^F") ~ "FEMININO",
      str_detect(SEXO_RAW, "^M") ~ "MASCULINO",
      TRUE ~ "OUTROS"
    )
  ) %>%
  
  # --- FILTRO AGRESSIVO (AQUI ESTÁ A CORREÇÃO) ---
  filter(
    !str_detect(RACA, "INFORMADA"),  # Remove "Não Informada", "Nao Informada", etc.
    !str_detect(RACA, "DECLARADA"),  # Remove "Não Declarada"
    RACA != "",                      # Remove textos vazios
    !is.na(RACA)                     # Remove Nulos
  ) %>% 
  
  count(RACA, SEXO, name = "Total")

# --- 4. O GRÁFICO FINAL ---
ggplot(dados_grafico, aes(x = RACA, y = Total, fill = SEXO)) +
  geom_col(position = "dodge") + 
  labs(
    title = "Diversidade no Prouni (2005-2020)",
    subtitle = "Distribuição de bolsas por Raça/Cor e Gênero",
    x = "Raça / Cor Autodeclarada",
    y = "Total de Bolsas",
    fill = "Gênero",
    caption = "Fonte: Dados Abertos MEC"
  ) +
  scale_fill_manual(values = c("FEMININO" = "#E69F00", "MASCULINO" = "#56B4E9")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(scales) # Pacote necessário para formatar os números

# (Assumindo que você já rodou os passos anteriores de leitura e limpeza)
# Se precisar recarregar, use o bloco "ler_e_corrigir" anterior.

# --- O GRÁFICO CORRIGIDO ---
ggplot(dados_grafico, aes(x = RACA, y = Total, fill = SEXO)) +
  geom_col(position = "dodge") + 
  
  # AQUI ESTÁ A CORREÇÃO DO EIXO Y:
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  
  labs(
    title = "Diversidade no Prouni (2005-2020)",
    subtitle = "Distribuição de bolsas por Raça/Cor e Gênero",
    x = "Raça / Cor Autodeclarada",
    y = "Total de Bolsas",
    fill = "Gênero",
    caption = "Fonte: Dados Abertos MEC"
  ) +
  scale_fill_manual(values = c("FEMININO" = "#E69F00", "MASCULINO" = "#56B4E9")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
---------------------------------------------------------------------------------
  library(tidyverse)
library(fs)
library(scales) # Para formatar porcentagens bonitinhas (ex: 45,2%)

# --- Insight Geográfico: Onde estão as oportunidades? ---
ler_geo <- function(arquivo) {
  # Lê como texto para evitar erros
  df <- read_csv2(arquivo, col_types = cols(.default = "c"), 
                  locale = locale(encoding = "Latin1"), show_col_types = FALSE)
  
  # CORREÇÃO DE COLUNAS (O Segredo):
  # Se encontrar a coluna de UF de 2020, renomeia para o padrão antigo
  if ("UF_BENEFICIARIO" %in% names(df)) {
    df <- df %>% rename(SIGLA_UF_BENEFICIARIO_BOLSA = UF_BENEFICIARIO)
  }
  
  # Se encontrar a coluna de Modalidade de 2020
  if ("MODALIDADE_ENSINO" %in% names(df)) {
    df <- df %>% rename(MODALIDADE_ENSINO_BOLSA = MODALIDADE_ENSINO)
  }
  
  # Seleciona apenas o que importa
  df %>% select(any_of(c("SIGLA_UF_BENEFICIARIO_BOLSA", "MODALIDADE_ENSINO_BOLSA")))
}

# --- 2. CARREGAR DADOS ---
caminho <- "C:/Users/maria.alves/Downloads/Analise_Prouni"
arquivos <- dir_ls(caminho, glob = "*.csv")

print("Calculando geografia do EAD... (Aguarde)")
dados_geo <- map_dfr(arquivos, ler_geo)

# --- 3. CÁLCULO DA PORCENTAGEM POR ESTADO ---
insight_geo <- dados_geo %>%
  # Limpeza inicial
  filter(!is.na(SIGLA_UF_BENEFICIARIO_BOLSA), !is.na(MODALIDADE_ENSINO_BOLSA)) %>%
  
  mutate(
    # Padroniza Modalidade (EAD vs PRESENCIAL)
    MODALIDADE = str_to_upper(MODALIDADE_ENSINO_BOLSA),
    MODALIDADE = ifelse(str_detect(MODALIDADE, "DIST"), "EAD", "PRESENCIAL") # Agrupa tudo que não for EAD como Presencial
  ) %>%
  
  # Agrupa por Estado e Modalidade
  group_by(SIGLA_UF_BENEFICIARIO_BOLSA, MODALIDADE) %>%
  count(name = "Total") %>%
  
  # Transforma linhas em colunas para calcular a porcentagem
  pivot_wider(names_from = MODALIDADE, values_from = Total, values_fill = 0) %>%
  
  # A MATEMÁTICA: Calcula % de EAD
  mutate(
    Total_Geral = EAD + PRESENCIAL,
    Porcentagem_EAD = (EAD / Total_Geral)
  ) %>%
  
  # Ordena do maior para o menor (Importante para o gráfico!)
  arrange(desc(Porcentagem_EAD))

# Mostra os Top 5 no Console
print("Estados que mais dependem de EAD:")
print(head(insight_geo, 5))

# --- 4. O GRÁFICO (Top 15 Estados) ---
# Vamos pegar apenas os 15 primeiros para o gráfico não ficar gigante
top_estados <- head(insight_geo, 15)

ggplot(top_estados, aes(x = reorder(SIGLA_UF_BENEFICIARIO_BOLSA, Porcentagem_EAD), y = Porcentagem_EAD)) +
  geom_col(fill = "#FF6F61") + # Cor destaque (Coral)
  coord_flip() + # Deita o gráfico para ler as siglas
  
  # Adiciona o rótulo de % na ponta da barra
  geom_text(aes(label = percent(Porcentagem_EAD, accuracy = 0.1)), 
            hjust = -0.1, size = 3.5) +
  
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.6)) + # Eixo em % (ajuste o limite se precisar)
  
  labs(
    title = "Geografia do EAD: Onde o ensino a distância domina?",
    subtitle = "Top 15 estados com maior proporção de bolsas EAD (2005-2020)",
    x = "Estado (UF)",
    y = "Porcentagem de Bolsas EAD",
    caption = "Fonte: Dados Abertos MEC/Prouni"
  ) +
  theme_minimal()
---------------Top Cursos (Tendências de Mercado)---------------

  ler_cursos <- function(arquivo) {
    df <- read_csv2(arquivo, col_types = cols(.default = "c"), 
                    locale = locale(encoding = "Latin1"), show_col_types = FALSE)
    
    # Seleciona apenas Ano e Curso (Garante que não trave com colunas extras)
    df %>% select(any_of(c("ANO_CONCESSAO_BOLSA", "NOME_CURSO_BOLSA")))
  }

# --- 2. CARREGAR DADOS ---
caminho <- "C:/Users/maria.alves/Downloads/Analise_Prouni"
arquivos <- dir_ls(caminho, glob = "*.csv")

print("Analisando tendências de cursos... (Aguarde)")
dados_brutos <- map_dfr(arquivos, ler_cursos)

# --- 3. PROCESSAMENTO INTELIGENTE ---
dados_limpos <- dados_brutos %>%
  filter(!is.na(NOME_CURSO_BOLSA), !is.na(ANO_CONCESSAO_BOLSA)) %>%
  mutate(
    # 1. Padroniza o Ano (para número)
    Ano = parse_number(ANO_CONCESSAO_BOLSA),
    
    # 2. Padroniza o Nome do Curso (Maiúsculo e sem espaços extras)
    Curso = str_to_upper(NOME_CURSO_BOLSA),
    Curso = str_trim(Curso)
  )

# --- 4. DESCOBRIR OS TOP 5 CURSOS (Geral de todos os anos) ---
top_5_cursos <- dados_limpos %>%
  count(Curso, sort = TRUE) %>%
  head(5) %>%
  pull(Curso) # Extrai apenas os nomes numa lista

print("Os 5 cursos campeões são:")
print(top_5_cursos)

# --- 5. PREPARAR DADOS PARA O GRÁFICO (Filtrar só os Top 5) ---
evolucao_cursos <- dados_limpos %>%
  # Mantém apenas os cursos que estão na lista dos Top 5
  filter(Curso %in% top_5_cursos) %>%
  
  # Conta quantos alunos por Ano e Curso
  count(Ano, Curso, name = "Total_Bolsas")

# --- 6. O GRÁFICO DE TENDÊNCIAS ---
ggplot(evolucao_cursos, aes(x = Ano, y = Total_Bolsas, color = Curso)) +
  geom_line(linewidth = 1.2) + # Linha grossa
  geom_point(size = 3) +       # Pontos em cada ano
  
  # Escala do Eixo X (Todos os anos)
  scale_x_continuous(breaks = seq(2005, 2020, 2)) +
  
  # Formata o Eixo Y (Milhares com ponto)
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  
  labs(
    title = "Top 5 Cursos no Prouni (2005-2020)",
    subtitle = "Evolução do número de bolsas nos cursos mais procurados",
    x = "Ano",
    y = "Total de Bolsas",
    color = "Curso",
    caption = "Fonte: Dados Abertos MEC"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") # Legenda embaixo para não roubar espaço
-------------------------------------------------------------------------------
 #### Insight de Turno (Manhã/Noite vs Trabalho)
  
  # --- 1. FUNÇÃO DE LEITURA BLINDADA ---
  ler_turnos <- function(arquivo) {
    df <- read_csv2(arquivo, col_types = cols(.default = "c"), 
                    locale = locale(encoding = "Latin1"), show_col_types = FALSE)
    
    if ("NOME_TURNO_CURSO" %in% names(df)) {
      df <- df %>% rename(NOME_TURNO_CURSO_BOLSA = NOME_TURNO_CURSO)
    }
    
    df %>% select(any_of(c("NOME_TURNO_CURSO_BOLSA")))
  }

# --- 2. CARREGAR DADOS ---
caminho <- "C:/Users/maria.alves/Downloads/Analise_Prouni"
arquivos <- dir_ls(caminho, glob = "*.csv")
dados_turnos <- map_dfr(arquivos, ler_turnos)

# --- 3. LIMPEZA E AGRUPAMENTO (AQUI ESTÁ A CORREÇÃO) ---
analise_turno <- dados_turnos %>%
  filter(!is.na(NOME_TURNO_CURSO_BOLSA)) %>%
  
  mutate(
    # 1. Tudo Maiúsculo e sem espaços extras
    Turno_Temp = str_to_upper(NOME_TURNO_CURSO_BOLSA),
    Turno_Temp = str_trim(Turno_Temp),
    
    # 2. PADRONIZAÇÃO INTELIGENTE
    Turno = case_when(
      # Se tiver "DIST" no nome, vira "CURSO A DISTÂNCIA" (Resolve acentos e erros)
      str_detect(Turno_Temp, "DIST") ~ "CURSO A DISTÂNCIA",
      
      # Mantém os outros (Matutino, Vespertino, Noturno, Integral)
      TRUE ~ Turno_Temp
    )
  ) %>%
  
  # Conta os totais
  count(Turno, name = "Total") %>%
  
  # Calcula %
  mutate(Porcentagem = Total / sum(Total)) %>%
  arrange(desc(Total))

# --- 4. O GRÁFICO ---
ggplot(analise_turno, aes(x = reorder(Turno, Total), y = Total)) +
  geom_col(fill = "#6A5ACD") + 
  
  # Rótulo com número formatado e porcentagem
  geom_text(aes(label = paste0(number(Total, big.mark = "."), "\n(", percent(Porcentagem, accuracy = 0.1), ")")), 
            hjust = -0.1, size = 3.5) +
  
  coord_flip() + 
  scale_y_continuous(labels = number_format(big.mark = "."), expand = expansion(mult = c(0, 0.2))) +
  
  labs(
    title = "Turno dos Bolsistas Prouni (2005-2020)",
    subtitle = "Preferência de horário dos alunos beneficiados",
    x = "Turno",
    y = "Total de Bolsas",
    caption = "Fonte: Dados Abertos MEC"
  ) +
  theme_minimal()

---------------------------------------------------------------------------