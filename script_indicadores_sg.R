# ..........................................................................................
#                                       SOBRE ESSE SCRIPT
# ..........................................................................................
# Script: Indicadores da Vigilância Sentinela de Síndrome Gripal (SG)
# Escopo: 15ª Regional de Saúde - Maringá/PR
# Base legal:
#   - NOTA TÉCNICA Nº 9, de 27/11/2025 - Atualizada em 15/09/2026 (SESA-PR):
#     define os 14 indicadores e ATUALIZOU a definição de caso de SG.
#   - Caderno de Análise - Indicadores de Desempenho e Resultado das
#     Unidades Sentinelas da Vigilância da Síndrome Gripal no Brasil
#     (CGCOVID/DEDT/SVSA/MS, versão preliminar 2024): traz o "MÉTODO DE
#     CÁLCULO" oficial de cada indicador - é a fonte usada abaixo.
# Fonte dos dados: Ficha de Registro Individual - Casos de SG que
#     realizaram coleta de amostra (SIVEP-Gripe), export .dbf.
#
# [Não verificado] IMPORTANTE - conflito de datas entre as duas fontes:
# O Caderno de Análise é de 2024 (a própria capa diz "versão preliminar...
# 2024") e usa, no Indicador 5, a definição de caso ANTIGA de SG:
# "febre, mesmo que referida, acompanhada de tosse ou dor de garganta,
# com início dos sintomas nos últimos 7 dias" (a mesma definição que
# aparece no rodapé do Instrutivo de 25/05/2023). A NT9 (atualizada em
# 15/09/2026) MUDOU essa definição para: infecção respiratória com início
# nos últimos 10 dias + pelo menos 2 de 4 grupos de sintomas, sendo pelo
# menos 1 respiratório. Portanto, o Indicador 5 abaixo foi implementado
# com a definição NOVA (da NT9, vigente hoje), não com a fórmula literal
# e desatualizada do Caderno - ver comentário na função indicador_5().
#
# [Não verificado] Os Indicadores 1 e 10 dependem da FICHA DE AGREGADO
# SEMANAL (total de atendimentos gerais e por SG), OU dos relatórios já
# calculados pelo próprio SIVEP-Gripe (menu RELATÓRIOS > INDICADORES,
# exportáveis em Excel). O Caderno deixa isso explícito: o sistema já
# calcula esses dois indicadores automaticamente - não é preciso
# recalculá-los a partir do .dbf individual. Por isso NÃO estão
# implementados aqui. Ver observação ao final do script.
#
# Este script NÃO foi executado contra dados reais.
#
# ..........................................................................................
#                               CONVENÇÕES USADAS NESTE SCRIPT
# ..........................................................................................
# - Variáveis ORIGINAIS do banco (nomes do Dicionário de Dados) em MAIÚSCULAS.
# - Variáveis CRIADAS na análise em minúsculas.
# - Cada indicador tem cabeçalho dizendo a página do Caderno de onde a
#   fórmula foi tirada, e as suposições feitas quando a fórmula do Caderno
#   deixava algo em aberto.

# [Inferência] Este script assume que vai ser executado com o diretório
# de trabalho (working directory) na RAIZ do repositório do site
# (ex.: setwd() para a pasta do repo, ou abrir o projeto/.Rproj de lá),
# porque ele salva em "./dados" e "./img" - pastas na raiz do site
# Quarto, para as páginas .qmd lerem diretamente. Se rodar de outro
# lugar, ajuste esses dois caminhos ou rode getwd() para conferir antes.
#
# IMPORTANTE - PRIVACIDADE: a Ficha SG tem campos de identificação
# (CPF, CNS, nome, nome da mãe, endereço, telefone). Este script NUNCA
# escreve o data frame "sg" bruto (ou qualquer subconjunto linha-a-caso
# dele) em arquivo - só agrega por unidade/semana/faixa etária antes de
# salvar. Isso é proposital: o repositório do site é PÚBLICO no GitHub
# Pages. Se algum dia for adicionar uma nova exportação, cheque à mão
# que ela não carrega CPF/nome/endereço antes de fazer commit.

# ..........................................................................................
#                                        ---- PACOTES ----
# ..........................................................................................
# install.packages(c("foreign", "dplyr", "tidyr", "lubridate", "stringr",
#                     "MMWRweek", "readr"))

library(foreign)     # leitura de DBF
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(MMWRweek)    # cálculo de semana epidemiológica
library(readr)
library(ggplot2)     # gráficos por semana epidemiológica (seção 7)

# ..........................................................................................
#                          ---- 1. CARREGAMENTO E PREPARO DOS DADOS ----
# ..........................................................................................

# [Corrigido] Você já me deu dois caminhos diferentes pro mesmo arquivo
# (Windows: C:/Users/valentim.junior/OneDrive/.../SG2026.dbf; Mac:
# /Users/valentimsalajunior/Documents/DBF_Vig_Sentinela/SG2026.dbf) -
# porque você troca de computador. Em vez de eu editar esse caminho
# toda vez que isso mudar, ele agora vem do seu ~/.Renviron - que é
# LOCAL de cada máquina, então cada computador aponta pro seu próprio
# arquivo, e o script fica idêntico nos dois lugares.
#
# Adicione ao ~/.Renviron de CADA computador (uma linha, sem aspas):
#   Windows: CAMINHO_SG_DBF=C:/Users/valentim.junior/OneDrive/Área de Trabalho/Vírus Respiratórios/DBF_Vig_Sentinela/SG2026.dbf
#   Mac:     CAMINHO_SG_DBF=/Users/valentimsalajunior/Documents/DBF_Vig_Sentinela/SG2026.dbf
readRenviron("~/.Renviron")
arquivo_sg <- Sys.getenv("CAMINHO_SG_DBF")
if (identical(arquivo_sg, "")) {
  stop("Defina CAMINHO_SG_DBF no seu ~/.Renviron antes de rodar este ",
       "script (veja o comentario acima para o caminho de cada maquina).")
}

sg_raw <- foreign::read.dbf(arquivo_sg, as.is = TRUE)

# [Corrigido] O dicionário de dados da Ficha SG descreve os campos de data
# como "Date DD/MM/AAAA" (formato brasileiro), não o formato ISO
# "AAAA-MM-DD" que `as.Date()` assume por padrão. Se o campo já vier como
# Date (o `foreign::read.dbf` às vezes já converte campos de data do DBF),
# deixamos como está; se vier como texto, tentamos primeiro o formato
# brasileiro e depois o ISO, sem travar em erro se um dos formatos não
# bater (retorna NA para essas entradas, com aviso).
parse_data_dbf <- function(x) {
  if (inherits(x, "Date")) return(x)
  as.Date(as.character(x), tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d"))
}

# Filtra para as unidades sentinelas da 15ª RS (Maringá e Sarandi, conforme
# Tabela 1 da NT9: UPA Zona Sul - CNES 6986609; Unidade de Pronto
# Atendimento Sarandi - CNES 7023049).
# [Inferência] Ajustar a lista se houver outras unidades sentinelas na região.
unidades_15rs <- c("6986609", "7023049")

# Período de referência da análise - ajustar conforme necessidade do painel.
periodo_inicio <- as.Date("2026-01-01")
periodo_fim    <- as.Date("2026-09-13")

sg <- sg_raw %>%
  mutate(
    DT_PREENC  = parse_data_dbf(DT_PREENC),
    DT_COLETA  = parse_data_dbf(DT_COLETA),
    DT_PRISINT = parse_data_dbf(DT_PRISINT),
    DT_ENCERRA = parse_data_dbf(DT_ENCERRA),
    PCR_DATA   = parse_data_dbf(PCR_DATA),
    IFI_DATA   = parse_data_dbf(IFI_DATA)
  ) %>%
  filter(
    COD_UNID %in% unidades_15rs,
    DT_COLETA >= periodo_inicio & DT_COLETA <= periodo_fim
  )

# Gera a lista de semanas epidemiológicas esperadas no período (formato
# "AAAASS", ex. "202601"), usada como denominador em vários indicadores
# ("Número de SE no período analisado" / "Número de SE ativas no período").
# [Inferência] Confirmar se esse formato bate com o formato real do campo
# SE_COLETA/SE_PRISINT do seu export.
# [Corrigido] O campo real SE_COLETA/SE_PRISINT do seu export vem no
# formato "SSAAAA" (semana com 2 dígitos + ano com 4 dígitos, ex.
# "012026" = semana 1 de 2026) - a ordem INVERSA do que eu tinha
# implementado antes ("AAAASS"). Isso causava um bug real: o cálculo do
# Indicador 4 e o gráfico de amostras semanais fazem um cruzamento
# (join) desta lista gerada com o campo real SE_COLETA da base; como os
# formatos não batiam, o cruzamento não encontrava nada e tudo saía
# zerado - foi exatamente o que apareceu no gráfico "amostras_semanais.png"
# que você mandou (todas as barras em 0, mesmo havendo dados reais).
# [Não verificado] Inferi o formato "SSAAAA" comparando os eixos X dos
# seus próprios gráficos (Indicadores 5 a 14 saíram certos: "012026" a
# "372026"), não contra a documentação oficial do SIVEP-Gripe - se algum
# gráfico ainda sair estranho depois desta correção, pode ser um detalhe
# a mais no formato (ex. zero à esquerda faltando) que eu não peguei.
gerar_semanas_epi <- function(data_ini, data_fim) {
  datas   <- seq.Date(as.Date(data_ini), as.Date(data_fim), by = "day")
  semanas <- MMWRweek::MMWRweek(datas)
  unique(paste0(sprintf("%02d", semanas$MMWRweek), semanas$MMWRyear))
}
semanas_periodo <- gerar_semanas_epi(periodo_inicio, periodo_fim)

# Ordena rótulos de semana "SSAAAA" CRONOLOGICAMENTE (não alfabeticamente)
# nos gráficos. Isso importa se o período algum dia cruzar a virada do
# ano: em ordem de texto, "012026" viria antes de "532025", o que é
# cronologicamente errado (532025 é semana 53 de 2025, ou seja, ANTERIOR
# a 012026). Não afeta o período atual (só 2026), mas deixa o gráfico à
# prova de período futuro que cruze dezembro/janeiro.
ordenar_se <- function(vetor_se) {
  ano <- as.numeric(substr(vetor_se, 3, 6))
  sem <- as.numeric(substr(vetor_se, 1, 2))
  niveis <- vetor_se[order(ano, sem)]
  factor(vetor_se, levels = unique(niveis))
}

# "Preenchido" = não é NA, não é string vazia e não é "9-Ignorado".
preenchido <- function(x, ignorado = "9") {
  x_chr <- trimws(as.character(x))
  !(is.na(x) | x_chr == "" | x_chr == ignorado)
}

# Classificação de desempenho genérica (0% / 1-20% / 21-80% / 81-100%),
# usada no Caderno para a maioria dos indicadores em %.
classificar_desempenho <- function(pct) {
  case_when(
    is.na(pct)  ~ NA_character_,
    pct == 0    ~ "Silencioso",
    pct <= 20   ~ "Baixíssimo Desempenho",
    pct <= 80   ~ "Baixo Desempenho",
    TRUE        ~ "Meta Atingida"
  )
}

# ..........................................................................................
#         ---- 2. INDICADORES DE PROCESSO ----
# ..........................................................................................

# Indicador 1 - % de SE com envio de dados agregados
# [Não implementado] O Caderno (p.16-17) mostra que este indicador já é
# CALCULADO PELO PRÓPRIO SIVEP-Gripe: menu RELATÓRIOS > INDICADORES > "%
# DE SEMANAS COM INFORMAÇÃO DE AGREGADO SEMANAL DE ATENDIMENTOS POR SG",
# exportável direto em Excel. Requer a Ficha de Agregado Semanal, que não
# está no .dbf de registros individuais. Recomendo puxar esse relatório
# pronto do sistema em vez de recalcular aqui.

# Indicador 2 - % de SE com coleta de amostras de SG em US
# [Fórmula oficial - Caderno p.20] Número de SE com envio de amostras /
# Número de SE ativas no período, x 100.
indicador_2 <- function(df, semanas) {
  df %>%
    filter(!is.na(DT_COLETA)) %>%
    distinct(COD_UNID, SE_COLETA) %>%
    count(COD_UNID, name = "se_com_coleta") %>%
    mutate(
      se_periodo      = length(semanas),
      indicador_2_pct = round(100 * se_com_coleta / se_periodo, 1),
      classificacao   = classificar_desempenho(indicador_2_pct)
    )
}

# Indicador 3 - Média de amostras de casos de SG em US por SE
# [Fórmula oficial - Caderno p.26] Total de amostras enviadas / Número de
# SE ativas no período analisado (NÃO é percentual).
# Classificação oficial do Caderno (adaptada da NT nº 13/2023-CGVDI, que é
# NACIONAL): Silencioso=0 | Abaixo do recomendado=1-3 | Dentro do
# recomendado=4-20 | Acima do recomendado=21+.
# [Não verificado] Essa faixa nacional (4-20) é MAIS FROUXA que a meta da
# NT9/PR, que preconiza exatamente 5 amostras semanais por US (Tabela 2
# da NT9). Mantive a classificação oficial do Caderno abaixo, mas cuidado
# ao usar "Dentro do recomendado" como sinônimo de "cumprindo a meta do
# Paraná" - não são a mesma régua.
classificar_indicador3 <- function(media) {
  case_when(
    is.na(media)      ~ NA_character_,
    media == 0        ~ "Silencioso",
    media <= 3         ~ "Abaixo do recomendado",
    media <= 20        ~ "Dentro do recomendado",
    TRUE               ~ "Acima do recomendado"
  )
}
indicador_3 <- function(df, semanas) {
  df %>%
    filter(!is.na(DT_COLETA)) %>%
    count(COD_UNID, name = "total_amostras") %>%
    mutate(
      se_periodo        = length(semanas),
      indicador_3_media  = round(total_amostras / se_periodo, 2),
      classificacao      = classificar_indicador3(indicador_3_media)
    )
}

# Indicador 4 - Homogeneidade em % de envio de amostras de SG em US
# [Fórmula oficial - Caderno p.31] Média simples entre:
#   (a) Indicador 2 (% de SE com envio de amostras)
#   (b) % de SE com envio ENTRE 4 e 20 amostras naquela semana
# Indicador 4 = (a + b) / 2
indicador_4 <- function(df, semanas) {
  contagem <- df %>%
    filter(!is.na(DT_COLETA)) %>%
    count(COD_UNID, SE_COLETA, name = "amostras_semana")

  base_completa <- tidyr::expand_grid(
    COD_UNID  = unique(df$COD_UNID),
    SE_COLETA = semanas
  ) %>%
    left_join(contagem, by = c("COD_UNID", "SE_COLETA")) %>%
    mutate(amostras_semana = tidyr::replace_na(amostras_semana, 0))

  pct_4a20 <- base_completa %>%
    group_by(COD_UNID) %>%
    summarise(
      se_periodo         = n(),
      se_dentro_4a20      = sum(amostras_semana >= 4 & amostras_semana <= 20),
      pct_se_4a20         = round(100 * se_dentro_4a20 / se_periodo, 1),
      .groups = "drop"
    )

  ind2 <- indicador_2(df, semanas) %>% select(COD_UNID, indicador_2_pct)

  pct_4a20 %>%
    left_join(ind2, by = "COD_UNID") %>%
    mutate(
      indicador_4_pct = round((indicador_2_pct + pct_se_4a20) / 2, 1),
      classificacao   = classificar_desempenho(indicador_4_pct)
    )
}

# ..........................................................................................
#         ---- 3. INDICADORES DE QUALIDADE ----
# ..........................................................................................

# Indicador 5 - % de registros que atendem à definição de caso de SG em US
# [ADAPTADO - ver aviso no topo do script] O Caderno (p.37) calcula este
# indicador com a definição ANTIGA de caso (febre + tosse/dor de garganta,
# <=7 dias). Como a NT9 mudou a definição, implementei com a definição
# NOVA e vigente da NT9: infecção respiratória com início <= 10 dias E
# pelo menos 2 dos 4 campos de sintoma da ficha (Febre/Tosse/Dor de
# Garganta/Outros) E pelo menos 1 respiratório confirmado (Tosse ou Dor de
# Garganta - a ficha não tem campo próprio para coriza/congestão nasal).
# A "diferença de datas" (DT_COLETA - DT_PRISINT), citada no Caderno como
# base do critério temporal, foi mantida, só que com o limite de 10 dias
# em vez de 7.
indicador_5 <- function(df) {
  df %>%
    mutate(
      dias_desde_inicio = as.numeric(DT_COLETA - DT_PRISINT),
      n_sintomas         = rowSums(cbind(FEBRE == "1", TOSSE == "1",
                                          DOR_GARGAN == "1", OUT_SINT == "1"),
                                    na.rm = TRUE),
      tem_respiratorio   = (TOSSE == "1" | DOR_GARGAN == "1"),
      atende_definicao   = n_sintomas >= 2 & tem_respiratorio &
                            !is.na(dias_desde_inicio) & dias_desde_inicio <= 10
    ) %>%
    group_by(COD_UNID) %>%
    summarise(
      total_registros   = n(),
      atende_definicao  = sum(atende_definicao, na.rm = TRUE),
      indicador_5_pct   = round(100 * atende_definicao / total_registros, 1),
      classificacao     = classificar_desempenho(indicador_5_pct),
      .groups = "drop"
    )
}

# Indicador 6 - Média do % de registros com RAÇA/COR, ESCOLARIDADE e USO
# DE ANTIVIRAL preenchidos
# [Fórmula oficial - Caderno p.42] Percentual de preenchimento de cada uma
# das 3 variáveis, depois a média simples dos 3 percentuais.
# "Preenchido" = valor diferente de vazio/NA e diferente de "9-Ignorado"
# (mesmo critério usado no Indicador 14, oficial na NT9/Anexo 1).
indicador_6 <- function(df) {
  df %>%
    group_by(COD_UNID) %>%
    summarise(
      total_registros  = n(),
      pct_raca         = round(100 * sum(preenchido(RACA))      / total_registros, 1),
      pct_escolaridade = round(100 * sum(preenchido(ESCOLARID)) / total_registros, 1),
      pct_antiviral    = round(100 * sum(preenchido(ANTIVIRAL)) / total_registros, 1),
      indicador_6_media = round((pct_raca + pct_escolaridade + pct_antiviral) / 3, 1),
      classificacao     = classificar_desempenho(indicador_6_media),
      .groups = "drop"
    )
}

# Indicador 7 - % de amostras de SG enviadas que foram processadas por RT-PCR
# [Fórmula oficial - Caderno p.48] "Processada" = PCR_RESUL em
# {1-Detectável, 2-Não Detectável, 3-Inconclusivo, 5-Aguardando resultado}.
# NÃO processada = {4-Não realizado, 9-Ignorado, vazio}.
# [Não verificado] O Caderno inclui "5-Aguardando resultado" como
# "processada", o que é um pouco contraintuitivo (ainda não tem resultado),
# mas é a definição literal do documento - mantive assim.
# O Caderno também alerta: valores < 95% já indicam desperdício de amostra,
# mesmo estando acima da meta genérica de 80% - incluí essa checagem à parte.
indicador_7 <- function(df) {
  df %>%
    filter(!is.na(DT_COLETA)) %>%
    group_by(COD_UNID) %>%
    summarise(
      amostras_enviadas = n(),
      processadas_pcr   = sum(PCR_RESUL %in% c("1", "2", "3", "5"), na.rm = TRUE),
      indicador_7_pct   = round(100 * processadas_pcr / amostras_enviadas, 1),
      classificacao     = classificar_desempenho(indicador_7_pct),
      alerta_desperdicio_amostra = indicador_7_pct < 95,
      .groups = "drop"
    )
}

# Indicador 8 - % de amostras com resultado de RT-PCR em período <= 10 dias
# [Fórmula oficial - Caderno p.51] (DT resultado - DT coleta) <= 10 dias,
# sobre o total de amostras processadas (aqui: com resultado definitivo
# 1/2/3, já que só essas têm PCR_DATA preenchida).
indicador_8 <- function(df) {
  df %>%
    filter(PCR_RESUL %in% c("1", "2", "3"), !is.na(DT_COLETA), !is.na(PCR_DATA)) %>%
    mutate(dias_resultado = as.numeric(PCR_DATA - DT_COLETA)) %>%
    group_by(COD_UNID) %>%
    summarise(
      total_processadas = n(),
      ate_10_dias        = sum(dias_resultado <= 10, na.rm = TRUE),
      indicador_8_pct    = round(100 * ate_10_dias / total_processadas, 1),
      classificacao      = classificar_desempenho(indicador_8_pct),
      .groups = "drop"
    )
}

# Indicador 9 - % de casos encerrados até 60 dias
# [Fórmula oficial - Caderno p.57] (DT_ENCERRA - "data de notificação") <=
# 60 dias, sobre o total de casos registrados no período.
# [Inferência] "Data de notificação" não é literalmente nenhum nome de
# campo do dicionário; usei DT_PREENC (data de preenchimento da ficha),
# por ser a que mais se aproxima do conceito de "notificação do caso".
indicador_9 <- function(df) {
  df %>%
    filter(!is.na(DT_PREENC)) %>%
    mutate(
      dias_encerramento = as.numeric(DT_ENCERRA - DT_PREENC),
      encerrado_60d      = !is.na(DT_ENCERRA) & dias_encerramento <= 60
    ) %>%
    group_by(COD_UNID) %>%
    summarise(
      total_registros  = n(),
      encerrados_60d   = sum(encerrado_60d, na.rm = TRUE),
      indicador_9_pct  = round(100 * encerrados_60d / total_registros, 1),
      classificacao    = classificar_desempenho(indicador_9_pct),
      .groups = "drop"
    )
}

# ..........................................................................................
#         ---- 4. INDICADORES DE RESULTADO ----
# ..........................................................................................

# Indicador 10 - Proporção de casos de SG sobre consultas gerais no período
# [Não implementado] O Caderno confirma (p.69) que os Indicadores de
# Resultado (10 a 13) "nada mais são que uma tentativa de trazer parte das
# análises geradas automaticamente pelo próprio SIVEP-Gripe em seu
# módulo" - ou seja, o sistema já os calcula. O Indicador 10 em particular
# precisa do total de CONSULTAS GERAIS da unidade (não só os casos de SG),
# que vem da Ficha de Agregado Semanal - não está no .dbf de registros
# individuais.

# Indicador 11 - Proporção de positividade de vírus testados no período
# [Inferência] O Caderno não traz fórmula explícita (só conceito), então
# mantive minha reconstrução: "testado" = resultado de PCR ou IF diferente
# de "não realizado"/"ignorado"/vazio; "positivo" = PCR ou IF = Positivo/
# Detectável.
indicador_11 <- function(df) {
  df %>%
    filter(PCR_RESUL %in% c("1", "2", "3") | IFI_RESUL %in% c("1", "2", "3")) %>%
    mutate(positivo = (PCR_RESUL == "1") | (IFI_RESUL == "1")) %>%
    group_by(COD_UNID) %>%
    summarise(
      total_testados    = n(),
      positivos         = sum(positivo, na.rm = TRUE),
      indicador_11_pct  = round(100 * positivos / total_testados, 1),
      classificacao     = classificar_desempenho(indicador_11_pct),
      .groups = "drop"
    )
}

# Indicadores 12 e 13 - Distribuição da circulação viral por SE dos
# sintomas e por faixa etária
# [Inferência] Sem fórmula explícita no Caderno. Uso o campo CLASSI_FIN
# (classificação final do caso, já processada/encerrada pela própria
# unidade) como classificação viral principal, para não duplicar a lógica
# de prioridade IF x PCR que o SIVEP-Gripe já aplica no encerramento do
# caso (ver Instrutivo, campo 59). Isso significa que os Indicadores
# 12/13 só refletem casos já ENCERRADOS (CLASSI_FIN preenchido).
# As faixas etárias abaixo são escolha própria - ajustar se já existir um
# padrão nos outros painéis da 15ª RS.
classificar_faixa_etaria <- function(idade, tipo_idade) {
  case_when(
    tipo_idade %in% c("1", "2")                        ~ "Menor de 1 ano",
    tipo_idade == "3" & as.numeric(idade) < 5           ~ "1 a 4 anos",
    tipo_idade == "3" & as.numeric(idade) %in% 5:19     ~ "5 a 19 anos",
    tipo_idade == "3" & as.numeric(idade) %in% 20:39    ~ "20 a 39 anos",
    tipo_idade == "3" & as.numeric(idade) %in% 40:59    ~ "40 a 59 anos",
    tipo_idade == "3" & as.numeric(idade) >= 60         ~ "60 anos ou mais",
    TRUE                                                 ~ NA_character_
  )
}

# Ordem por IDADE (não alfabética) das faixas etárias acima - usada em
# todos os gráficos por faixa etária do script (Indicador 13 e a versão
# de vírus específicos da Seção 8b).
niveis_faixa_etaria <- c("Menor de 1 ano", "1 a 4 anos", "5 a 19 anos",
                          "20 a 39 anos", "40 a 59 anos", "60 anos ou mais")

indicadores_12_13 <- function(df) {
  base <- df %>%
    filter(!is.na(CLASSI_FIN)) %>%
    mutate(
      categoria_viral = case_when(
        CLASSI_FIN == "1" & FIN_FLU == "1" ~ "Influenza A",
        CLASSI_FIN == "1" & FIN_FLU == "2" ~ "Influenza B",
        CLASSI_FIN == "1"                  ~ "Influenza (tipo não especificado)",
        CLASSI_FIN == "2"                  ~ "Outro vírus respiratório",
        CLASSI_FIN == "3"                  ~ "Outro agente etiológico",
        CLASSI_FIN == "5"                  ~ "COVID-19",
        CLASSI_FIN == "4"                  ~ "SG não especificado",
        TRUE                                ~ NA_character_
      ),
      faixa_etaria = classificar_faixa_etaria(IDADE, TP_IDADE)
    )

  list(
    indicador_12_por_SE    = base %>% count(COD_UNID, SE_PRISINT, categoria_viral, name = "n_casos"),
    indicador_13_por_faixa = base %>% count(COD_UNID, faixa_etaria, categoria_viral, name = "n_casos")
  )
}

# ..........................................................................................
#         ---- 5. INDICADOR ESTADUAL (NT9, Anexo 1 - Indicador 14) ----
# ..........................................................................................

# Indicador 14 - % de registros com "contato direto com aves, suínos ou
# outro animal" preenchido (campo 31 / PAC_AVESU).
# [Fórmula oficial - Anexo 1 da NT9] Preenchido = valor em {1, 2, 3},
# excluindo 9-Ignorado e vazio.
indicador_14 <- function(df) {
  df %>%
    group_by(COD_UNID) %>%
    summarise(
      total_registros = n(),
      preenchidos     = sum(PAC_AVESU %in% c("1", "2", "3"), na.rm = TRUE),
      indicador_14_pct = round(100 * preenchidos / total_registros, 1),
      classificacao    = classificar_desempenho(indicador_14_pct),
      .groups = "drop"
    )
}

# ..........................................................................................
#         ---- 6. EXECUÇÃO E EXPORTAÇÃO ----
# ..........................................................................................

resultados <- list(
  indicador_2  = indicador_2(sg, semanas_periodo),
  indicador_3  = indicador_3(sg, semanas_periodo),
  indicador_4  = indicador_4(sg, semanas_periodo),
  indicador_5  = indicador_5(sg),
  indicador_6  = indicador_6(sg),
  indicador_7  = indicador_7(sg),
  indicador_8  = indicador_8(sg),
  indicador_9  = indicador_9(sg),
  indicador_11 = indicador_11(sg),
  indicadores_12_13 = indicadores_12_13(sg),
  indicador_14 = indicador_14(sg)
)

dir.create("./dados", recursive = TRUE, showWarnings = FALSE)

readr::write_csv2(resultados$indicador_2,  "./dados/indicador_02.csv")
readr::write_csv2(resultados$indicador_3,  "./dados/indicador_03.csv")
readr::write_csv2(resultados$indicador_4,  "./dados/indicador_04.csv")
readr::write_csv2(resultados$indicador_5,  "./dados/indicador_05.csv")
readr::write_csv2(resultados$indicador_6,  "./dados/indicador_06.csv")
readr::write_csv2(resultados$indicador_7,  "./dados/indicador_07.csv")
readr::write_csv2(resultados$indicador_8,  "./dados/indicador_08.csv")
readr::write_csv2(resultados$indicador_9,  "./dados/indicador_09.csv")
readr::write_csv2(resultados$indicador_11, "./dados/indicador_11.csv")
readr::write_csv2(resultados$indicadores_12_13$indicador_12_por_SE,    "./dados/indicador_12.csv")
readr::write_csv2(resultados$indicadores_12_13$indicador_13_por_faixa, "./dados/indicador_13.csv")
readr::write_csv2(resultados$indicador_14, "./dados/indicador_14.csv")

message("Indicadores calculados e exportados em ./dados/")
message("ATENÇÃO: Indicadores 1 e 10 NÃO foram calculados - dependem da Ficha de Agregado Semanal ou dos relatórios prontos do SIVEP-Gripe (menu RELATÓRIOS > INDICADORES).")
message("ATENÇÃO: Indicador 5 usa a definição de caso NOVA da NT9 (10 dias), diferente da fórmula literal (desatualizada) do Caderno de Análise (7 dias) - ver comentário no código.")

# ..........................................................................................
#         ---- 7. VERSÕES SEMANAIS DOS INDICADORES + GRÁFICOS ----
# ..........................................................................................
#
# [Inferência] A NT9 e o Caderno de Análise calculam os indicadores para o
# PERÍODO TODO (ex.: um semestre), não semana a semana. As versões
# semanais abaixo são uma extensão minha: aplico a mesma fórmula de cada
# indicador, mas separando por semana epidemiológica, para dar uma linha
# do tempo. Onde a fórmula original usa "%", o percentual semanal aqui
# pode ficar instável se o nº de registros por semana for pequeno (ex.:
# 1 amostra na semana = indicador semanal só pode dar 0% ou 100%) -
# olhar com cautela, principalmente semana a semana numa unidade só.
#
# Escolha de semana de agrupamento por indicador:
#   - Indicadores 2/3/4 (base: amostras coletadas) e 5/6/7/8/14: SE_COLETA
#     (semana em que a amostra foi coletada).
#   - Indicador 9 (encerramento): semana de DT_PREENC, derivada aqui via
#     MMWRweek, pois a ficha não tem um campo SE_PREENC pronto.
#   - Indicadores 11/12 (perfil viral): SE_PRISINT (semana de início dos
#     sintomas), padrão epidemiológico mais comum para curva de vírus.
#
# Indicadores 1 e 10: sem gráfico semanal - não implementados (ver seção 4).

# Deriva SE (formato "AAAASS") a partir de uma coluna de datas.
# Usa parse_data_dbf() (definida na Seção 1) para aceitar tanto Date já
# convertido quanto texto ainda não convertido, sem travar em erro de
# formato.
data_para_se <- function(datas) {
  datas <- parse_data_dbf(datas)
  se <- MMWRweek::MMWRweek(datas)
  ifelse(is.na(datas), NA_character_,
         paste0(sprintf("%02d", se$MMWRweek), se$MMWRyear))
}

# [Todas as funções abaixo aplicam ordenar_se() antes do arrange(), para
# o eixo do gráfico seguir a ordem cronológica real, não a ordem
# alfabética do texto "SSAAAA" - ver comentário em ordenar_se() (Seção 1).]

# --- Base semanal de amostras (usada nos gráficos dos Indicadores 2/3/4) ---
serie_semanal_amostras <- function(df, semanas) {
  contagem <- df %>%
    filter(!is.na(DT_COLETA)) %>%
    count(COD_UNID, SE_COLETA, name = "amostras_semana")

  tidyr::expand_grid(COD_UNID = unique(df$COD_UNID), SE_COLETA = semanas) %>%
    left_join(contagem, by = c("COD_UNID", "SE_COLETA")) %>%
    mutate(amostras_semana = tidyr::replace_na(amostras_semana, 0),
           SE_COLETA = ordenar_se(SE_COLETA)) %>%
    arrange(SE_COLETA)
}

# --- Indicador 5 semanal ---
indicador_5_semanal <- function(df) {
  df %>%
    filter(!is.na(DT_COLETA)) %>%
    mutate(
      dias_desde_inicio = as.numeric(DT_COLETA - DT_PRISINT),
      n_sintomas = rowSums(cbind(FEBRE == "1", TOSSE == "1",
                                  DOR_GARGAN == "1", OUT_SINT == "1"), na.rm = TRUE),
      tem_respiratorio = (TOSSE == "1" | DOR_GARGAN == "1"),
      atende_definicao = n_sintomas >= 2 & tem_respiratorio &
                          !is.na(dias_desde_inicio) & dias_desde_inicio <= 10
    ) %>%
    group_by(COD_UNID, SE_COLETA) %>%
    summarise(
      total_registros  = n(),
      atende_definicao = sum(atende_definicao, na.rm = TRUE),
      indicador_5_pct  = round(100 * atende_definicao / total_registros, 1),
      .groups = "drop"
    ) %>%
    mutate(SE_COLETA = ordenar_se(SE_COLETA)) %>%
    arrange(SE_COLETA)
}

# --- Indicador 6 semanal ---
indicador_6_semanal <- function(df) {
  df %>%
    group_by(COD_UNID, SE_COLETA) %>%
    summarise(
      total_registros  = n(),
      pct_raca         = round(100 * sum(preenchido(RACA))      / total_registros, 1),
      pct_escolaridade = round(100 * sum(preenchido(ESCOLARID)) / total_registros, 1),
      pct_antiviral    = round(100 * sum(preenchido(ANTIVIRAL)) / total_registros, 1),
      indicador_6_media = round((pct_raca + pct_escolaridade + pct_antiviral) / 3, 1),
      .groups = "drop"
    ) %>%
    mutate(SE_COLETA = ordenar_se(SE_COLETA)) %>%
    arrange(SE_COLETA)
}

# --- Indicador 7 semanal ---
indicador_7_semanal <- function(df) {
  df %>%
    filter(!is.na(DT_COLETA)) %>%
    group_by(COD_UNID, SE_COLETA) %>%
    summarise(
      amostras_enviadas = n(),
      processadas_pcr   = sum(PCR_RESUL %in% c("1", "2", "3", "5"), na.rm = TRUE),
      indicador_7_pct   = round(100 * processadas_pcr / amostras_enviadas, 1),
      .groups = "drop"
    ) %>%
    mutate(SE_COLETA = ordenar_se(SE_COLETA)) %>%
    arrange(SE_COLETA)
}

# --- Indicador 8 semanal ---
indicador_8_semanal <- function(df) {
  df %>%
    filter(PCR_RESUL %in% c("1", "2", "3"), !is.na(DT_COLETA), !is.na(PCR_DATA)) %>%
    mutate(dias_resultado = as.numeric(PCR_DATA - DT_COLETA)) %>%
    group_by(COD_UNID, SE_COLETA) %>%
    summarise(
      total_processadas = n(),
      ate_10_dias        = sum(dias_resultado <= 10, na.rm = TRUE),
      indicador_8_pct    = round(100 * ate_10_dias / total_processadas, 1),
      .groups = "drop"
    ) %>%
    mutate(SE_COLETA = ordenar_se(SE_COLETA)) %>%
    arrange(SE_COLETA)
}

# --- Indicador 9 semanal (semana derivada de DT_PREENC) ---
# [Não verificado - AVISO IMPORTANTE] Olhando o gráfico que você mandou
# (indicador_09_semanal.png), as últimas semanas do período despencam de
# 100% para 0%. Isso muito provavelmente NÃO é um problema real de
# desempenho da unidade - é um artefato de CENSURA À DIREITA: um caso
# notificado a poucos dias do fim do período analisado (periodo_fim)
# ainda não teve os 60 dias completos para poder ser encerrado dentro do
# prazo, então conta como "não encerrado em 60 dias" mesmo que a unidade
# feche o caso normalmente depois. Ou seja, as ~8-9 últimas semanas do
# gráfico deste indicador tendem a cair artificialmente só porque ainda
# não deu tempo. Considerar cortar as últimas ~9 semanas antes do
# periodo_fim ao interpretar/publicar este gráfico especificamente, ou
# marcar essa janela como "dados ainda incompletos" no painel.
indicador_9_semanal <- function(df) {
  df %>%
    filter(!is.na(DT_PREENC)) %>%
    mutate(
      SE_PREENC          = data_para_se(DT_PREENC),
      dias_encerramento  = as.numeric(DT_ENCERRA - DT_PREENC),
      encerrado_60d      = !is.na(DT_ENCERRA) & dias_encerramento <= 60
    ) %>%
    group_by(COD_UNID, SE_PREENC) %>%
    summarise(
      total_registros  = n(),
      encerrados_60d   = sum(encerrado_60d, na.rm = TRUE),
      indicador_9_pct  = round(100 * encerrados_60d / total_registros, 1),
      .groups = "drop"
    ) %>%
    mutate(SE_PREENC = ordenar_se(SE_PREENC)) %>%
    arrange(SE_PREENC)
}

# --- Indicador 11 semanal (SE_PRISINT) ---
indicador_11_semanal <- function(df) {
  df %>%
    filter(PCR_RESUL %in% c("1", "2", "3") | IFI_RESUL %in% c("1", "2", "3")) %>%
    mutate(positivo = (PCR_RESUL == "1") | (IFI_RESUL == "1")) %>%
    group_by(COD_UNID, SE_PRISINT) %>%
    summarise(
      total_testados   = n(),
      positivos        = sum(positivo, na.rm = TRUE),
      indicador_11_pct = round(100 * positivos / total_testados, 1),
      .groups = "drop"
    ) %>%
    mutate(SE_PRISINT = ordenar_se(SE_PRISINT)) %>%
    arrange(SE_PRISINT)
}

# --- Indicador 14 semanal ---
indicador_14_semanal <- function(df) {
  df %>%
    group_by(COD_UNID, SE_COLETA) %>%
    summarise(
      total_registros  = n(),
      preenchidos      = sum(PAC_AVESU %in% c("1", "2", "3"), na.rm = TRUE),
      indicador_14_pct = round(100 * preenchidos / total_registros, 1),
      .groups = "drop"
    ) %>%
    mutate(SE_COLETA = ordenar_se(SE_COLETA)) %>%
    arrange(SE_COLETA)
}

# --- Calcula todas as séries semanais ---
series_semanais <- list(
  amostras     = serie_semanal_amostras(sg, semanas_periodo),
  indicador_5  = indicador_5_semanal(sg),
  indicador_6  = indicador_6_semanal(sg),
  indicador_7  = indicador_7_semanal(sg),
  indicador_8  = indicador_8_semanal(sg),
  indicador_9  = indicador_9_semanal(sg),
  indicador_11 = indicador_11_semanal(sg),
  indicador_14 = indicador_14_semanal(sg),
  indicador_12 = resultados$indicadores_12_13$indicador_12_por_SE %>%
                   mutate(SE_PRISINT = ordenar_se(SE_PRISINT)) %>%
                   arrange(SE_PRISINT)
)

# --- Paleta e tema padrão, conforme convenções já usadas nos painéis da
#     15ª RS (barra em "#0057A3"; aqui uso a mesma cor como base para a
#     1ª unidade e um tom mais claro para a 2ª, já que agora são séries
#     por linha/tempo, não barras isoladas por município). Ajustar as
#     cores/paleta se você já tiver uma paleta oficial de outro painel.
cores_unidades <- c("#0057A3", "#7FB3E6")

tema_semanal <- theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
    plot.title = element_text(face = "bold")
  )

# --- Gráfico 1: amostras coletadas por semana (base dos Indicadores 2/3/4) ---
grafico_amostras_semanais <- ggplot(series_semanais$amostras,
                                     aes(x = SE_COLETA, y = amostras_semana, fill = COD_UNID)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "grey30") +
  geom_text(aes(label = amostras_semana), position = position_dodge(width = 0.8),
            vjust = -0.4, size = 2.5) +
  scale_fill_manual(values = cores_unidades, name = "Unidade Sentinela") +
  labs(
    title = "Amostras de SG coletadas por semana epidemiológica - 15ª RS",
    subtitle = "Linha tracejada = meta da NT9 (5 amostras/semana). Base dos Indicadores 2, 3 e 4.",
    x = "Semana epidemiológica (SE_COLETA)", y = "Nº de amostras"
  ) +
  tema_semanal

# --- Função genérica para gráfico de linha de um indicador em % por semana ---
grafico_pct_semanal <- function(dados, var_semana, var_pct, titulo, subtitulo = NULL, meta = 80) {
  ggplot(dados, aes(x = .data[[var_semana]], y = .data[[var_pct]],
                     group = COD_UNID, color = COD_UNID)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = meta, linetype = "dashed", color = "grey40") +
    scale_color_manual(values = cores_unidades, name = "Unidade Sentinela") +
    scale_y_continuous(limits = c(0, 100)) +
    labs(title = titulo, subtitle = subtitulo,
         x = "Semana epidemiológica", y = "%") +
    tema_semanal
}

grafico_indicador_5  <- grafico_pct_semanal(series_semanais$indicador_5, "SE_COLETA", "indicador_5_pct",
                          "Indicador 5 - % de registros que atendem à definição de caso de SG",
                          "Linha tracejada = meta OMS (80%). Definição nova (NT9).")

grafico_indicador_6  <- grafico_pct_semanal(series_semanais$indicador_6, "SE_COLETA", "indicador_6_media",
                          "Indicador 6 - Média do % de Raça/Cor, Escolaridade e Uso de Antiviral preenchidos")

grafico_indicador_7  <- grafico_pct_semanal(series_semanais$indicador_7, "SE_COLETA", "indicador_7_pct",
                          "Indicador 7 - % de amostras processadas por RT-PCR",
                          "Caderno recomenda olhar com atenção valores < 95%, não só < 80%.")

grafico_indicador_8  <- grafico_pct_semanal(series_semanais$indicador_8, "SE_COLETA", "indicador_8_pct",
                          "Indicador 8 - % de resultados de RT-PCR em até 10 dias da coleta")

grafico_indicador_9  <- grafico_pct_semanal(series_semanais$indicador_9, "SE_PREENC", "indicador_9_pct",
                          "Indicador 9 - % de casos encerrados em até 60 dias",
                          "Semana de referência = semana de preenchimento da ficha (DT_PREENC).")

grafico_indicador_11 <- grafico_pct_semanal(series_semanais$indicador_11, "SE_PRISINT", "indicador_11_pct",
                          "Indicador 11 - Proporção de positividade de vírus testados",
                          "Semana de referência = semana de início dos sintomas (SE_PRISINT).")

grafico_indicador_14 <- grafico_pct_semanal(series_semanais$indicador_14, "SE_COLETA", "indicador_14_pct",
                          "Indicador 14 - % de registros com contato com aves/suínos/outro animal preenchido")

# --- Indicador 12: circulação viral por semana de início dos sintomas
#     (gráfico de barras empilhadas por categoria viral, com % de cada
#     categoria dentro da coluna daquela semana) ---
# [Corrigido - dois bugs]
# 1) "series_semanais$indicador_12" tem uma linha POR UNIDADE SENTINELA
#    (2 unidades) - cada categoria virava 2 linhas dentro do mesmo
#    segmento colorido, cada uma com seu próprio rótulo de %,
#    sobrepostos. Resolvido somando as duas unidades antes de calcular
#    a %.
# 2) O `ifelse(sum(n_casos) > 0, ...)` usado antes tinha um bug: dentro
#    de um mutate() agrupado, `sum(n_casos)` é um valor único por grupo,
#    e o `ifelse()` do R base só devolve um resultado do tamanho da
#    CONDIÇÃO (não do tamanho do grupo) - ou seja, calculava certo só a
#    % da PRIMEIRA linha de cada semana e o dplyr espalhava esse mesmo
#    valor pra todas as outras categorias daquela semana. Confirmado com
#    os dados reais que você mandou para o Indicador 13 (mesmo padrão de
#    código). Corrigido calculando o total da semana como coluna
#    separada primeiro.
dados_indicador_12 <- series_semanais$indicador_12 %>%
  group_by(SE_PRISINT, categoria_viral) %>%
  summarise(n_casos = sum(n_casos), .groups = "drop") %>%
  group_by(SE_PRISINT) %>%
  mutate(
    total_semana = sum(n_casos),
    pct = ifelse(total_semana > 0, round(100 * n_casos / total_semana, 1), NA_real_)
  ) %>%
  ungroup()

# Checagem de sanidade: no máximo 1 linha por (semana, categoria) - o
# nº de categorias possíveis é 5 (COVID-19, Influenza A, Influenza B,
# Outro vírus respiratório, SG não especificado). Se aparecer mais que
# 5 linhas em alguma semana, ainda há duplicação de verdade no código
# (e não só um gráfico desatualizado no seu ambiente R).
max_linhas_indicador_12 <- dados_indicador_12 %>% count(SE_PRISINT) %>% pull(n) %>% max()
message("Indicador 12: ate ", max_linhas_indicador_12,
        " categorias por semana (deveria ser <= 5). Se for maior que 5, ",
        "ha duplicacao real no codigo - me avise com esse numero.")

grafico_indicador_12 <- ggplot(dados_indicador_12,
                                aes(x = SE_PRISINT, y = n_casos, fill = categoria_viral)) +
  geom_col() +
  geom_text(
    aes(label = ifelse(!is.na(pct) & pct >= 5, paste0(round(pct), "%"), "")),
    position = position_stack(vjust = 0.5), size = 2, color = "white"
  ) +
  labs(
    title = "Indicador 12 - Circulação viral por semana epidemiológica dos sintomas",
    x = "Semana epidemiológica (início dos sintomas)", y = "Nº de casos",
    fill = "Classificação final do caso"
  ) +
  tema_semanal +
  theme(legend.position = "bottom")

# --- Indicador 13: por faixa etária (não é série semanal - sem eixo de
#     tempo por definição do próprio indicador), com % de cada categoria
#     dentro da coluna daquela faixa etária, em ordem de IDADE (não
#     alfabética) ---
# [Corrigido - bug real de verdade] O `ifelse(sum(n_casos) > 0, ...)`
# usado antes tinha um bug clássico: `sum(n_casos)` vira um valor único
# por grupo (não um vetor do tamanho do grupo), e o `ifelse()` do R base
# só devolve um resultado do tamanho da CONDIÇÃO - ou seja, calculava a
# % certinha só da PRIMEIRA linha do grupo e o dplyr espalhava esse
# mesmo valor pra todas as outras linhas daquela semana/faixa etária.
# Foi confirmado com os dados reais que você mandou: grupo "Menor de 1
# ano" tinha 3 linhas com n_casos = 1, 4, 1 mas as 3 saíam com pct=16.7%
# (o valor certo só da linha de n_casos=1). A correção: calcular o total
# do grupo como uma COLUNA separada primeiro (isso o dplyr recicla
# corretamente para todas as linhas do grupo), e só depois dividir - o
# mesmo padrão que já estava certo nos gráficos da Seção 8.
dados_indicador_13 <- resultados$indicadores_12_13$indicador_13_por_faixa %>%
  mutate(faixa_etaria = factor(faixa_etaria, levels = niveis_faixa_etaria)) %>%
  group_by(faixa_etaria, categoria_viral) %>%
  summarise(n_casos = sum(n_casos), .groups = "drop") %>%
  group_by(faixa_etaria) %>%
  mutate(
    total_faixa = sum(n_casos),
    pct = ifelse(total_faixa > 0, round(100 * n_casos / total_faixa, 1), NA_real_)
  ) %>%
  ungroup()

# Mesma checagem de sanidade do Indicador 12: no máximo 5 linhas por
# faixa etária (uma por categoria). Se vier maior que 5, ainda há
# duplicação real no código.
max_linhas_indicador_13 <- dados_indicador_13 %>% count(faixa_etaria) %>% pull(n) %>% max()
message("Indicador 13: ate ", max_linhas_indicador_13,
        " categorias por faixa etaria (deveria ser <= 5). Se for maior que 5, ",
        "ha duplicacao real no codigo - me avise com esse numero.")

grafico_indicador_13 <- ggplot(dados_indicador_13,
                                aes(x = faixa_etaria, y = n_casos, fill = categoria_viral)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = ifelse(!is.na(pct) & pct >= 5, paste0(round(pct), "%"), "")),
    position = position_stack(vjust = 0.5), size = 2.5, color = "white"
  ) +
  labs(
    title = "Indicador 13 - Circulação viral por faixa etária",
    x = "Faixa etária", y = "Nº de casos", fill = "Classificação final do caso"
  ) +
  tema_semanal +
  theme(axis.text.x = element_text(angle = 0, size = 9), legend.position = "bottom")

# --- Salva todos os gráficos em PNG ---
dir.create("./img", recursive = TRUE, showWarnings = FALSE)

ggsave("./img/amostras_semanais.png",   grafico_amostras_semanais, width = 10, height = 5, dpi = 150)
ggsave("./img/indicador_05_semanal.png", grafico_indicador_5,      width = 10, height = 5, dpi = 150)
ggsave("./img/indicador_06_semanal.png", grafico_indicador_6,      width = 10, height = 5, dpi = 150)
ggsave("./img/indicador_07_semanal.png", grafico_indicador_7,      width = 10, height = 5, dpi = 150)
ggsave("./img/indicador_08_semanal.png", grafico_indicador_8,      width = 10, height = 5, dpi = 150)
ggsave("./img/indicador_09_semanal.png", grafico_indicador_9,      width = 10, height = 5, dpi = 150)
ggsave("./img/indicador_11_semanal.png", grafico_indicador_11,     width = 10, height = 5, dpi = 150)
ggsave("./img/indicador_12_semanal.png", grafico_indicador_12,     width = 10, height = 5, dpi = 150)
ggsave("./img/indicador_13_faixa.png",   grafico_indicador_13,     width = 10, height = 6, dpi = 150)
ggsave("./img/indicador_14_semanal.png", grafico_indicador_14,     width = 10, height = 5, dpi = 150)

message("Gráficos salvos em ./img/ (9 gráficos semanais + 1 por faixa etária).")
message("Indicadores 1, 2, 3, 4 e 10 sem gráfico semanal de %: 1 e 10 não implementados; 2/3/4 são medidas de período (não semanais por definição) - o gráfico 'amostras_semanais.png' mostra a série bruta que os alimenta.")

# ..........................................................................................
#         ---- 8. CIRCULAÇÃO VIRAL - SOMENTE VÍRUS EFETIVAMENTE ENCONTRADOS ----
# ..........................................................................................
#
# [Inferência] Pedido seu: um gráfico de circulação viral SEM "SG não
# especificado" (que domina o Indicador 12 original e esconde a
# variação dos vírus de verdade). Fui além do que o Indicador 12 faz:
# em vez de usar só CLASSI_FIN (que agrupa tudo que não é Influenza/
# COVID-19 num balaio único "Outro vírus respiratório"), abro esse
# balaio nos vírus específicos já registrados na ficha (VSR,
# Parainfluenza 1-4, Adenovírus, Metapneumovírus, Bocavírus, Rinovírus),
# usando os campos PCR_* e IFI_* diretamente - a mesma técnica de
# "flag por vírus" que vi no Script_1_LimpezaClassificacao.R do
# Ministério (só que adaptada aos campos da Ficha SG, que são
# diferentes dos da ficha SRAG).
#
# Regras aplicadas:
#   - Prioriza RT-PCR sobre Imunofluorescência (IF) quando os dois
#     foram feitos, conforme orientação do Instrutivo (campo 59: "se
#     houver resultados divergentes entre IF e RT-PCR, priorizar o
#     RT-PCR"). SARS-CoV-2, Metapneumovírus, Bocavírus e Rinovírus só
#     têm campo de PCR na ficha (a IF não testa esses 4).
#   - Um caso pode entrar em MAIS de uma linha/vírus se for uma
#     codetecção (positivo para dois vírus ao mesmo tempo) - por isso a
#     soma das barras/linhas pode passar do nº de casos.
#   - EXCLUÍDOS por não serem "vírus encontrados": "SG não especificado"
#     (CLASSI_FIN=4, sem identificação) e "Outro agente etiológico"
#     (CLASSI_FIN=3 - não é necessariamente viral, ex. poderia ser
#     bactéria).
#
# [Não verificado] O dicionário de dados lista o campo de Parainfluenza 4
# da Imunofluorescência como "IF_PARA4" (sem o segundo "I", diferente
# do padrão "IFI_..." dos outros vírus na IF) - pode ser erro de
# digitação do próprio dicionário oficial. O código já tenta os dois
# nomes ("IF_PARA4" e "IFI_PARA4") via campo_seguro() - se nenhum
# existir, essa detecção específica fica sempre como "não encontrado"
# (sem travar o script), e um aviso aparece no console avisando disso.

# Acesso seguro a colunas do DBF: campos com nome longo no dicionário
# de dados (>10 caracteres, ex. "POS_IFI_FLU") podem ter sido truncados
# de forma diferente no arquivo .dbf de verdade - o formato DBF clássico
# limita nomes de campo a 10 caracteres, e a documentação oficial nem
# sempre reflete isso. Esta função evita que o script inteiro trave por
# causa disso: se a coluna não existir com o nome esperado, avisa uma
# vez e trata como "sem informação" (NA) em vez de dar erro.
campo_seguro <- function(df, nomes) {
  for (nome in nomes) {
    if (nome %in% names(df)) return(df[[nome]])
  }
  warning("Nenhuma das colunas [", paste(nomes, collapse = ", "), "] foi ",
          "encontrada no .dbf (pode ter sido truncada pelo formato DBF - ",
          "limite classico de 10 caracteres). Tratando como NA - confira ",
          "com names(sg_raw) qual e o nome real da coluna.", call. = FALSE)
  rep(NA_character_, nrow(df))
}

# --- Marca, por caso, quais vírus específicos deram positivo ---
virus_positivos_longo <- function(df) {
  flag_virus <- function(pcr_var, ifi_var = NULL) {
    if (is.null(ifi_var)) return(!is.na(pcr_var) & pcr_var == "1")
    (!is.na(pcr_var) & pcr_var == "1") |
      (is.na(pcr_var) & !is.na(ifi_var) & ifi_var == "1")
  }

  # [Corrigido] "POS_IFI_FLU" tem 11 caracteres - acima do limite
  # clássico de nome de campo do DBF (10). Deu erro "objeto não
  # encontrado" porque o nome real da coluna no seu .dbf provavelmente
  # está truncado de outra forma. Uso campo_seguro() para não travar o
  # script; se o aviso aparecer, rode names(sg_raw) e me diga o nome
  # real para eu trocar aqui.
  pos_ifi_flu <- campo_seguro(df, c("POS_IFI_FLU"))
  if_para4    <- campo_seguro(df, c("IF_PARA4", "IFI_PARA4"))

  df %>%
    mutate(
      `Influenza A` = (!is.na(PCR_FLU) & PCR_FLU == "1") |
                       (is.na(PCR_FLU) & !is.na(IFI_FLU) & IFI_FLU == "1"),
      `Influenza B` = (!is.na(PCR_FLU) & PCR_FLU == "2") |
                       (is.na(PCR_FLU) & !is.na(IFI_FLU) & IFI_FLU == "2"),
      `Influenza (não subtipado)` =
        (!is.na(POS_PCRFLU) & POS_PCRFLU == "1" & is.na(PCR_FLU)) |
        (is.na(POS_PCRFLU) & !is.na(pos_ifi_flu) & pos_ifi_flu == "1" & is.na(IFI_FLU)),
      `SARS-CoV-2`               = flag_virus(PCR_SARS2),
      `VSR`                      = flag_virus(PCR_VRS,   IFI_VRS),
      `Parainfluenza 1`          = flag_virus(PCR_PARA1, IFI_PARA1),
      `Parainfluenza 2`          = flag_virus(PCR_PARA2, IFI_PARA2),
      `Parainfluenza 3`          = flag_virus(PCR_PARA3, IFI_PARA3),
      `Parainfluenza 4`          = flag_virus(PCR_PARA4, if_para4),
      `Adenovírus`               = flag_virus(PCR_ADENO, IFI_ADENO),
      `Metapneumovírus`          = flag_virus(PCR_METAP),
      `Bocavírus`                = flag_virus(PCR_BOCA),
      `Rinovírus`                = flag_virus(PCR_RINO),
      `Outro vírus respiratório` = flag_virus(PCR_OUTRO, IFI_OUTRO),
      faixa_etaria               = classificar_faixa_etaria(IDADE, TP_IDADE)
    ) %>%
    select(
      COD_UNID, SE_PRISINT, faixa_etaria,
      `Influenza A`, `Influenza B`, `Influenza (não subtipado)`, `SARS-CoV-2`,
      `VSR`, `Parainfluenza 1`, `Parainfluenza 2`, `Parainfluenza 3`, `Parainfluenza 4`,
      `Adenovírus`, `Metapneumovírus`, `Bocavírus`, `Rinovírus`,
      `Outro vírus respiratório`
    ) %>%
    tidyr::pivot_longer(
      cols = -c(COD_UNID, SE_PRISINT, faixa_etaria),
      names_to = "virus", values_to = "positivo"
    ) %>%
    filter(positivo) %>%
    count(COD_UNID, SE_PRISINT, faixa_etaria, virus, name = "n_casos")
}

virus_base       <- virus_positivos_longo(sg)
virus_encontrados <- unique(virus_base$virus)

# ..........................................................................................
#   8a. POR SEMANA - colunas empilhadas com o % de cada vírus na semana
#       (equivalente ao Indicador 12, só com vírus específicos)
# ..........................................................................................

virus_bruto_semana <- virus_base %>%
  filter(!is.na(SE_PRISINT)) %>%
  group_by(SE_PRISINT, virus) %>%
  summarise(n_casos = sum(n_casos), .groups = "drop")

# Preenche com 0 as combinações semana x vírus sem detecção, para o
# cálculo de % de cada semana ficar correto mesmo quando um vírus não
# apareceu naquela semana.
semanas_sintomas <- sort(unique(sg$SE_PRISINT[!is.na(sg$SE_PRISINT)]))

virus_semanal_15rs <- tidyr::expand_grid(SE_PRISINT = semanas_sintomas, virus = virus_encontrados) %>%
  left_join(virus_bruto_semana, by = c("SE_PRISINT", "virus")) %>%
  mutate(
    n_casos    = tidyr::replace_na(n_casos, 0),
    SE_PRISINT = ordenar_se(SE_PRISINT)
  ) %>%
  arrange(SE_PRISINT)

readr::write_csv2(virus_semanal_15rs, "./dados/circulacao_viral_especifica.csv")

# % de cada vírus DENTRO de cada semana (a coluna soma 100%). Semanas
# sem nenhuma detecção ficam sem barra (0/0 não tem proporção definida).
virus_semanal_pct <- virus_semanal_15rs %>%
  group_by(SE_PRISINT) %>%
  mutate(
    total_semana = sum(n_casos),
    pct = ifelse(total_semana > 0, round(100 * n_casos / total_semana, 1), NA_real_)
  ) %>%
  ungroup()

grafico_virus_especificos <- ggplot(virus_semanal_pct, aes(x = SE_PRISINT, y = pct, fill = virus)) +
  geom_col(position = "stack", width = 0.85) +
  geom_text(
    aes(label = ifelse(!is.na(pct) & pct >= 5, paste0(round(pct), "%"), "")),
    position = position_stack(vjust = 0.5), size = 2.2, color = "white"
  ) +
  labs(
    title = "Circulação viral por semana epidemiológica - % de cada vírus identificado",
    subtitle = "15ª RS (2 unidades sentinelas somadas). Cada coluna soma 100%. Exclui SG não especificado e outro agente etiológico não-viral.",
    x = "Semana epidemiológica (início dos sintomas)",
    y = "% das detecções na semana",
    fill = "Vírus"
  ) +
  tema_semanal +
  theme(legend.position = "right")

ggsave("./img/circulacao_viral_especifica.png", grafico_virus_especificos,
       width = 12, height = 6, dpi = 150)

message("Gráfico de vírus específicos por semana (% empilhado) salvo em ./img/circulacao_viral_especifica.png")
message("Vírus efetivamente encontrados no período: ", paste(virus_encontrados, collapse = ", "))

# ..........................................................................................
#   8b. POR FAIXA ETÁRIA - colunas empilhadas com o % de cada vírus
#       (equivalente ao Indicador 13, só com vírus específicos, sem
#       "SG não especificado")
# ..........................................................................................
#
# Usa a mesma ordem por idade (niveis_faixa_etaria) definida mais acima,
# já aplicada também no gráfico original do Indicador 13.
virus_faixa_15rs <- virus_base %>%
  filter(!is.na(faixa_etaria)) %>%
  group_by(faixa_etaria, virus) %>%
  summarise(n_casos = sum(n_casos), .groups = "drop") %>%
  mutate(faixa_etaria = factor(faixa_etaria, levels = niveis_faixa_etaria))

readr::write_csv2(virus_faixa_15rs, "./dados/circulacao_viral_especifica_faixa.csv")

virus_faixa_pct <- virus_faixa_15rs %>%
  group_by(faixa_etaria) %>%
  mutate(
    total_faixa = sum(n_casos),
    pct = ifelse(total_faixa > 0, round(100 * n_casos / total_faixa, 1), NA_real_)
  ) %>%
  ungroup()

grafico_virus_faixa_etaria <- ggplot(virus_faixa_pct, aes(x = faixa_etaria, y = pct, fill = virus)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(
    aes(label = ifelse(!is.na(pct) & pct >= 5, paste0(round(pct), "%"), "")),
    position = position_stack(vjust = 0.5), size = 2.5, color = "white"
  ) +
  labs(
    title = "Circulação viral por faixa etária - % de cada vírus identificado",
    subtitle = "15ª RS (2 unidades sentinelas somadas). Cada coluna soma 100%. Exclui SG não especificado e outro agente etiológico não-viral.",
    x = "Faixa etária", y = "% das detecções na faixa etária", fill = "Vírus"
  ) +
  tema_semanal +
  theme(axis.text.x = element_text(angle = 0, size = 9), legend.position = "right")

ggsave("./img/circulacao_viral_especifica_faixa.png", grafico_virus_faixa_etaria,
       width = 10, height = 6, dpi = 150)

message("Gráfico de vírus específicos por faixa etária (% empilhado) salvo em ./img/circulacao_viral_especifica_faixa.png")
