# ==========================================================================================
#  CLIMA E QUALIDADE DO AR x CASOS DE SG - UNIDADES SENTINELAS DA 15ª RS
# ==========================================================================================
#
# O que este script faz:
#   1. Lê os atendimentos semanais por unidade sentinela (Ficha de Agregado
#      Semanal do Sivep-Gripe, export .dbf): total de atendimentos (TC_TOTAL)
#      e atendimentos de SG (SG_TOTAL) por semana epidemiológica (SE).
#   2. Busca na API do Open-Meteo, para o município de cada unidade:
#        - clima diário (temperatura, umidade, precipitação) - Historical
#          Weather API (archive-api.open-meteo.com);
#        - qualidade do ar horária (MP2,5, MP10, O3, NO2, CO, IQA-EUA) -
#          Air Quality API (air-quality-api.open-meteo.com), resumida por dia.
#   3. Agrega clima e ar por SE (domingo a sábado, mesmo critério do
#      MMWRweek usado no script_indicadores_sg.R).
#   4. Cruza casos x clima por unidade e SE, e calcula a correlação de
#      Spearman entre a % de SG nos atendimentos e cada variável, com
#      defasagem de 0 a 3 semanas (clima da semana anterior x casos).
#
# Saídas (só dados agregados - nenhuma linha por paciente):
#   dados/clima_diario.csv           clima + ar por município e dia
#   dados/casos_clima_semanal.csv    casos + clima + ar por unidade e SE
#   dados/correlacao_casos_clima.csv correlação por unidade, variável e defasagem
#   img/casos_clima_semanal.png      séries semanais lado a lado
#
# Sobre a fonte (conferido na documentação do Open-Meteo em 28/10/2026):
#   - Uso gratuito: só não comercial (inclui "pesquisa pública em instituições
#     públicas"); limite de 10.000 chamadas/dia. Licença CC-BY 4.0 - citar
#     "Open-Meteo" ao divulgar. Este script faz 2 chamadas por município.
#   - Clima: dados de reanálise/modelo, não de estação meteorológica.
#   - Qualidade do ar fora da Europa: previsão global CAMS, grade 0,4°
#     (cerca de 45 km), disponível desde agosto de 2022. [Inferência]
#     Maringá e Sarandi ficam a cerca de 7 km uma da outra, então devem
#     cair na mesma célula da grade e ter valores de ar quase iguais.
#
# [Inferência] Correlação aqui é EXPLORATÓRIA: cerca de 40 semanas por
# unidade, sem controle de sazonalidade, circulação viral ou feriados.
# Não serve para afirmar causa.
# ==========================================================================================


# ---- 0. PACOTES -------------------------------------------------------------------------

pacman::p_load(
  foreign,    # leitura de DBF
  httr,       # requisições à API
  jsonlite,   # leitura do JSON da API
  dplyr,
  tidyr,
  purrr,
  MMWRweek,   # semana epidemiológica
  readr,
  ggplot2,
  here        # caminhos a partir da raiz do projeto
)


# ---- 1. CONFIGURAÇÃO --------------------------------------------------------------------

readRenviron("~/.Renviron")

# Ficha de Agregado Semanal. Mesma fonte do script_indicadores_sg.R:
#   1º ~/SG_dados/sg_agregado_<ano>.rds (gravado pelo anonimizar_sg.R; pasta
#      configurável por SG_DADOS) - o ano mais recente que existir;
#   2º se não houver, o DBF apontado por CAMINHO_SG_AGREGADO_DBF no
#      ~/.Renviron (computador sem o fluxo do ~/SG_entrada), ex.:
#      CAMINHO_SG_AGREGADO_DBF=/Users/valentimsalajunior/Documents/DBF_Vig_Sentinela/SG2026_agregado.dbf
dir_sg_dados <- path.expand(Sys.getenv("SG_DADOS", "~/SG_dados"))
bases_rds    <- sort(Sys.glob(file.path(dir_sg_dados, "sg_agregado_*.rds")),
                     decreasing = TRUE)
arquivo_agregado <- if (length(bases_rds) > 0) bases_rds[1] else Sys.getenv("CAMINHO_SG_AGREGADO_DBF")
if (identical(arquivo_agregado, "")) {
  stop("Não achei a Ficha de Agregado Semanal: nem ", dir_sg_dados,
       "/sg_agregado_<ano>.rds nem CAMINHO_SG_AGREGADO_DBF no ~/.Renviron.")
}
ler_agregado <- function(caminho) {
  if (grepl("\\.rds$", caminho)) readRDS(caminho)
  else foreign::read.dbf(caminho, as.is = TRUE)
}
message("Agregado semanal: ", arquivo_agregado)

# Chave do Open-Meteo: OPCIONAL. Vazia = API gratuita (uso não comercial).
# Só preencha se a SESA contratar um plano pago (OPEN_METEO_API_KEY=...).
# Com chave, o Open-Meteo usa o mesmo endereço com prefixo "customer-" e o
# parâmetro "apikey" (conferido na página de preços). [Não verificado] Não
# testei com chave, porque não temos uma.
chave_open_meteo <- Sys.getenv("OPEN_METEO_API_KEY")

# Unidades sentinelas da 15ª RS e o município onde ficam. Coordenadas da
# API de geocodificação do Open-Meteo (centro urbano, não centroide do
# polígono do município).
unidades <- tibble::tribble(
  ~COD_UNID, ~unidade,                ~cod_ibge, ~municipio, ~lat,      ~lon,
  "6986609", "UPA Zona Sul - Maringá", "4115200", "Maringá",  -23.42528, -51.93861,
  "7023049", "UPA Sarandi",            "4126256", "Sarandi",  -23.44361, -51.87389
)

# Até quantas semanas o clima pode "anteceder" os casos na correlação.
defasagem_max <- 3


# ---- 2. CASOS SEMANAIS (FICHA DE AGREGADO SEMANAL) --------------------------------------

# Só as colunas de contagem - OBSERVA (texto livre) fica de fora.
casos_semanais <- ler_agregado(arquivo_agregado) %>%
  select(COD_UNID, SE_ATEND, DT_INIC, DT_FIN, SG_TOTAL, TC_TOTAL) %>%
  filter(COD_UNID %in% unidades$COD_UNID) %>%
  mutate(
    DT_INIC = as.Date(DT_INIC),
    DT_FIN  = as.Date(DT_FIN)
  ) %>%
  # Se a mesma unidade/SE aparecer mais de uma vez no export, soma.
  group_by(COD_UNID, SE_ATEND, DT_INIC, DT_FIN) %>%
  summarise(
    SG_TOTAL = sum(SG_TOTAL, na.rm = TRUE),
    TC_TOTAL = sum(TC_TOTAL, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(pct_sg = round(100 * SG_TOTAL / TC_TOTAL, 2))

# Período da busca: da primeira à última SE com casos, com folga antes para
# as defasagens. O fim é limitado a ontem (a API de histórico não tem hoje).
periodo_inicio <- min(casos_semanais$DT_INIC) - 7 * defasagem_max
periodo_fim    <- min(max(casos_semanais$DT_FIN), Sys.Date() - 1)


# ---- 3. FUNÇÕES DE CONSULTA À API -------------------------------------------------------

# Monta o endereço: gratuito ou "customer-" quando há chave.
url_open_meteo <- function(servico, caminho) {
  prefixo <- if (identical(chave_open_meteo, "")) "" else "customer-"
  paste0("https://", prefixo, servico, ".open-meteo.com", caminho)
}

# Faz a requisição e devolve o JSON já convertido. Repete até 3 vezes se a
# API falhar (ex.: limite por minuto), esperando um pouco entre tentativas.
consultar_open_meteo <- function(url, parametros) {
  if (!identical(chave_open_meteo, "")) parametros$apikey <- chave_open_meteo
  resposta <- httr::RETRY("GET", url, query = parametros,
                          times = 3, pause_base = 5, quiet = TRUE)
  conteudo <- httr::content(resposta, as = "text", encoding = "UTF-8")
  if (httr::http_error(resposta)) {
    stop("Open-Meteo respondeu ", httr::status_code(resposta), ": ", conteudo)
  }
  jsonlite::fromJSON(conteudo)
}

# Clima diário de um ponto.
buscar_clima <- function(lat, lon, inicio, fim) {
  json <- consultar_open_meteo(
    url_open_meteo("archive-api", "/v1/archive"),
    list(
      latitude   = lat,
      longitude  = lon,
      start_date = format(inicio),
      end_date   = format(fim),
      daily      = paste(c("temperature_2m_mean", "temperature_2m_min",
                           "temperature_2m_max", "relative_humidity_2m_mean",
                           "relative_humidity_2m_min", "precipitation_sum"),
                         collapse = ","),
      timezone   = "America/Sao_Paulo"
    )
  )
  as_tibble(json$daily) %>%
    rename(data = time) %>%
    mutate(data = as.Date(data))
}

# Qualidade do ar: a API só entrega valores horários, então resumimos por
# dia (média do dia; para o IQA-EUA, o pior valor do dia).
buscar_ar <- function(lat, lon, inicio, fim) {
  json <- consultar_open_meteo(
    url_open_meteo("air-quality-api", "/v1/air-quality"),
    list(
      latitude   = lat,
      longitude  = lon,
      start_date = format(inicio),
      end_date   = format(fim),
      hourly     = paste(c("pm2_5", "pm10", "ozone", "nitrogen_dioxide",
                           "carbon_monoxide", "us_aqi"),
                         collapse = ","),
      timezone   = "America/Sao_Paulo"
    )
  )
  as_tibble(json$hourly) %>%
    mutate(data = as.Date(substr(time, 1, 10))) %>%
    group_by(data) %>%
    summarise(
      across(c(pm2_5, pm10, ozone, nitrogen_dioxide, carbon_monoxide),
             ~ mean(.x, na.rm = TRUE)),
      us_aqi_max = suppressWarnings(max(us_aqi, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(us_aqi_max = if_else(is.infinite(us_aqi_max), NA_real_, us_aqi_max))
}


# ---- 4. BUSCA POR MUNICÍPIO -------------------------------------------------------------

municipios <- unidades %>% distinct(cod_ibge, municipio, lat, lon)

clima_diario <- municipios %>%
  mutate(dados = purrr::pmap(
    list(lat, lon),
    function(lat, lon) {
      clima <- buscar_clima(lat, lon, periodo_inicio, periodo_fim)
      ar    <- buscar_ar(lat, lon, periodo_inicio, periodo_fim)
      Sys.sleep(1)   # gentileza com a API gratuita
      left_join(clima, ar, by = "data")
    }
  )) %>%
  select(cod_ibge, municipio, dados) %>%
  tidyr::unnest(dados) %>%
  mutate(amplitude_termica = temperature_2m_max - temperature_2m_min)

# Conferência simples: um dia por município, sem buracos no período.
dias_esperados <- as.integer(periodo_fim - periodo_inicio) + 1
conferencia <- count(clima_diario, municipio)
if (any(conferencia$n != dias_esperados)) {
  warning("Algum município veio com número de dias diferente do esperado (",
          dias_esperados, "):\n",
          paste(capture.output(print(conferencia)), collapse = "\n"))
}


# ---- 5. AGREGAÇÃO POR SEMANA EPIDEMIOLÓGICA ---------------------------------------------

# Rótulo "SSAAAA" (semana + ano), igual ao SE_ATEND do Sivep-Gripe.
rotulo_se <- function(datas) {
  se <- MMWRweek::MMWRweek(datas)
  paste0(sprintf("%02d", se$MMWRweek), se$MMWRyear)
}

clima_semanal <- clima_diario %>%
  mutate(SE = rotulo_se(data)) %>%
  group_by(cod_ibge, municipio, SE) %>%
  summarise(
    inicio_se        = min(data),
    dias_com_dado    = n(),
    temp_media       = mean(temperature_2m_mean, na.rm = TRUE),
    temp_min         = min(temperature_2m_min, na.rm = TRUE),
    temp_max         = max(temperature_2m_max, na.rm = TRUE),
    amplitude_media  = mean(amplitude_termica, na.rm = TRUE),
    umidade_media    = mean(relative_humidity_2m_mean, na.rm = TRUE),
    umidade_min      = min(relative_humidity_2m_min, na.rm = TRUE),
    precipitacao_mm  = sum(precipitation_sum, na.rm = TRUE),
    pm2_5            = mean(pm2_5, na.rm = TRUE),
    pm10             = mean(pm10, na.rm = TRUE),
    ozonio           = mean(ozone, na.rm = TRUE),
    no2              = mean(nitrogen_dioxide, na.rm = TRUE),
    co               = mean(carbon_monoxide, na.rm = TRUE),
    iqa_eua_max      = suppressWarnings(max(us_aqi_max, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    iqa_eua_max = if_else(is.infinite(iqa_eua_max), NA_real_, iqa_eua_max),
    across(where(is.double), ~ round(.x, 2))
  ) %>%
  arrange(municipio, inicio_se)

variaveis_clima <- c("temp_media", "temp_min", "temp_max", "amplitude_media",
                     "umidade_media", "umidade_min", "precipitacao_mm",
                     "pm2_5", "pm10", "ozonio", "no2", "co", "iqa_eua_max")


# ---- 6. CRUZAMENTO CASOS x CLIMA --------------------------------------------------------

casos_clima <- casos_semanais %>%
  left_join(select(unidades, COD_UNID, unidade, cod_ibge, municipio),
            by = "COD_UNID") %>%
  left_join(select(clima_semanal, -municipio, -inicio_se),
            by = c("cod_ibge", "SE_ATEND" = "SE")) %>%
  arrange(COD_UNID, DT_INIC)

# Correlação de Spearman (não supõe relação linear) entre a % de SG na
# semana e o clima da mesma semana (defasagem 0) e de 1 a 3 semanas antes.
# A defasagem é aplicada na série do município, então semanas sem caso
# registrado não deslocam o clima.
clima_defasado <- purrr::map_dfr(0:defasagem_max, function(k) {
  clima_semanal %>%
    group_by(cod_ibge) %>%
    arrange(inicio_se, .by_group = TRUE) %>%
    mutate(across(all_of(variaveis_clima), ~ dplyr::lag(.x, k))) %>%
    ungroup() %>%
    mutate(defasagem_semanas = k) %>%
    select(cod_ibge, SE, defasagem_semanas, all_of(variaveis_clima))
})

correlacao <- casos_semanais %>%
  left_join(select(unidades, COD_UNID, unidade, cod_ibge), by = "COD_UNID") %>%
  inner_join(clima_defasado, by = c("cod_ibge", "SE_ATEND" = "SE"),
             relationship = "many-to-many") %>%
  pivot_longer(all_of(variaveis_clima), names_to = "variavel",
               values_to = "valor") %>%
  group_by(COD_UNID, unidade, variavel, defasagem_semanas) %>%
  summarise(
    n_semanas = sum(!is.na(valor) & !is.na(pct_sg)),
    teste     = list(suppressWarnings(
      cor.test(pct_sg, valor, method = "spearman", exact = FALSE))),
    .groups   = "drop"
  ) %>%
  mutate(
    rho     = round(purrr::map_dbl(teste, "estimate"), 3),
    valor_p = signif(purrr::map_dbl(teste, "p.value"), 3)
  ) %>%
  select(-teste) %>%
  arrange(COD_UNID, variavel, defasagem_semanas)


# ---- 7. EXPORTAÇÃO ----------------------------------------------------------------------

dir.create(here::here("dados"), showWarnings = FALSE)
dir.create(here::here("img"), showWarnings = FALSE)

readr::write_csv2(clima_diario, here::here("dados", "clima_diario.csv"))
readr::write_csv2(casos_clima,  here::here("dados", "casos_clima_semanal.csv"))
readr::write_csv2(correlacao,   here::here("dados", "correlacao_casos_clima.csv"))

# Gráfico: % de SG e algumas variáveis por SE, uma linha por unidade.
grafico <- casos_clima %>%
  select(unidade, DT_INIC, pct_sg, temp_media, umidade_media,
         precipitacao_mm, pm2_5) %>%
  pivot_longer(-c(unidade, DT_INIC), names_to = "variavel",
               values_to = "valor") %>%
  mutate(variavel = factor(
    variavel,
    levels = c("pct_sg", "temp_media", "umidade_media", "precipitacao_mm", "pm2_5"),
    labels = c("% de SG nos atendimentos", "Temperatura média (°C)",
               "Umidade relativa média (%)", "Precipitação na semana (mm)",
               "MP2,5 médio (µg/m³)")
  )) %>%
  ggplot(aes(x = DT_INIC, y = valor, colour = unidade)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ variavel, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c("#0057A3", "#E07B00")) +
  labs(
    x = "Início da semana epidemiológica", y = NULL, colour = NULL,
    caption = paste0("Fontes: Ficha de Agregado Semanal (Sivep-Gripe); ",
                     "Open-Meteo (CC-BY 4.0). Gerado em ",
                     format(Sys.Date(), "%d/%m/%Y"), ".")
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")

ggsave(here::here("img", "casos_clima_semanal.png"), grafico,
       width = 9, height = 10, dpi = 150, bg = "white")

message("Pronto: ", nrow(clima_diario), " dias de clima, ",
        nrow(casos_clima), " linhas unidade x SE, período ",
        format(periodo_inicio, "%d/%m/%Y"), " a ",
        format(periodo_fim, "%d/%m/%Y"), ".")
