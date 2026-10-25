# ..........................................................................................
#         ANONIMIZAÇÃO DOS DBF DA VIGILÂNCIA SENTINELA DE SG (LGPD)
# ..........................................................................................
#
# Lê os DBF (ou .zip com DBF) baixados à mão do Sivep-Gripe em ~/SG_entrada,
# grava as bases tratadas em ~/SG_dados/ (fora do ~/Work, que vai para o
# OneDrive e para o Syncthing) e APAGA os arquivos brutos.
#
# Dois tipos de arquivo, reconhecidos pelas colunas (não pelo nome):
#   - Ficha SG individual (tem DT_COLETA/SE_COLETA): mantém só as colunas de
#     colunas_permitidas.txt e só os registros das unidades sentinelas da
#     15ª RS -> sg_ficha_anon_<ano>.rds
#   - Ficha de Agregado Semanal (tem SG_TOTAL/TC_TOTAL): já é contagem por
#     unidade/semana; tira só o texto livre (OBSERVA) e mantém as unidades
#     da 15ª RS -> sg_agregado_<ano>.rds
#
# Uso: Rscript anonimizar_sg.R [pasta_de_entrada]
# As bases guardam os tipos que o foreign::read.dbf() devolve (datas como
# Date), para o script_indicadores_sg.R ler igual lia o DBF. Atributos:
# origem, exportado_em (data do arquivo baixado), processado_em.
# ..........................................................................................

suppressMessages(library(foreign))

PASTA_ENTRADA <- path.expand(commandArgs(trailingOnly = TRUE)[1])
if (is.na(PASTA_ENTRADA)) PASTA_ENTRADA <- path.expand("~/SG_entrada")
PASTA_ERRO    <- file.path(PASTA_ENTRADA, "erro")
DIR_PROJETO   <- local({
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg))) else getwd()
})
DIR_SENSIVEIS <- path.expand(Sys.getenv("SG_DADOS", "~/SG_dados"))

# Mesma lista do script_indicadores_sg.R (unidades_15rs) - Tabela 1 da NT9.
UNIDADES_15RS <- c("6986609", "7023049")

# Identificadores diretos e texto livre: se algum aparecer na saída da ficha,
# é bug na lista de colunas permitidas - o script aborta sem gravar nada.
PROIBIDAS <- c("NOME_PAC", "NU_CPF", "NU_CNS", "DT_NASC", "NOME_MAE", "CEP",
               "COD_BAIRRO", "NOM_BAIRRO", "COD_LOGRAD", "NOM_LOGRAD",
               "NUM_LOGRAD", "COMPLEMENT", "TELEFONE", "NUM_REGIST", "OBSERVA",
               "NOME_PROF", "REG_PROF", "VG_PROF", "OUT_SINTD", "OUT_FRISCD")

permitidas <- readLines(file.path(DIR_PROJETO, "colunas_permitidas.txt"), encoding = "UTF-8")
permitidas <- toupper(trimws(sub("#.*$", "", permitidas)))
permitidas <- permitidas[permitidas != ""]
stopifnot(!any(PROIBIDAS %in% permitidas))

brutos <- list.files(PASTA_ENTRADA, pattern = "\\.(dbf|zip)$",
                     ignore.case = TRUE, full.names = TRUE)
if (length(brutos) == 0) stop("Nenhum .dbf ou .zip em ", PASTA_ENTRADA)

tmp <- tempfile("sg_")
dir.create(tmp, mode = "0700")
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

# Ano predominante de um campo de semana "SSAAAA"; exige >= 95% num só ano
ano_da_base <- function(se) {
  dist <- sort(table(substr(se, 3, 6), useNA = "ifany"), decreasing = TRUE)
  message("Ano das semanas: ", paste(names(dist), dist, sep = " = ", collapse = " | "))
  ano <- names(dist)[1]
  if (is.na(ano) || !grepl("^20[0-9]{2}$", ano)) stop("Não consegui identificar o ano da base")
  if (dist[1] / length(se) < 0.95) stop("Base mistura anos (", ano, " é só ",
                                        round(100 * dist[1] / length(se)), "%) - exporte um ano por arquivo")
  ano
}

gravar <- function(df, nome, bruto) {
  attr(df, "origem")        <- basename(bruto)
  attr(df, "exportado_em")  <- as.Date(file.mtime(bruto))
  attr(df, "processado_em") <- Sys.time()
  dir.create(DIR_SENSIVEIS, showWarnings = FALSE, recursive = TRUE, mode = "0700")
  saida <- file.path(DIR_SENSIVEIS, nome)
  tmp_saida <- paste0(saida, ".tmp")
  saveRDS(df, tmp_saida)
  stopifnot(identical(dim(readRDS(tmp_saida)), dim(df)))
  file.rename(tmp_saida, saida)
  Sys.chmod(saida, "0600")
  message("Base gravada: ", saida, " (", nrow(df), " x ", ncol(df), ")")
}

processar <- function(bruto) {
  message("\nArquivo bruto: ", basename(bruto))
  caminho_dbf <- bruto
  if (grepl("\\.zip$", bruto, ignore.case = TRUE)) {
    destino <- tempfile("zip_", tmpdir = tmp)
    utils::unzip(bruto, exdir = destino)
    dbfs <- list.files(destino, pattern = "\\.dbf$", ignore.case = TRUE,
                       full.names = TRUE, recursive = TRUE)
    if (length(dbfs) != 1) stop("O .zip deve conter exatamente 1 DBF (tem ", length(dbfs), ")")
    caminho_dbf <- dbfs
  }

  df <- foreign::read.dbf(caminho_dbf, as.is = TRUE)
  names(df) <- toupper(names(df))
  message("Registros: ", nrow(df), " | colunas no DBF: ", ncol(df))

  if (all(c("SG_TOTAL", "TC_TOTAL", "SE_ATEND") %in% names(df))) {
    # --- Agregado semanal: contagens por unidade/semana, sem paciente ---
    df <- df[df$COD_UNID %in% UNIDADES_15RS, setdiff(names(df), "OBSERVA"), drop = FALSE]
    message("Tipo: agregado semanal | registros das unidades da 15ª RS: ", nrow(df))
    if (nrow(df) == 0) stop("Nenhuma unidade sentinela da 15ª RS no agregado")
    gravar(df, paste0("sg_agregado_", ano_da_base(df$SE_ATEND), ".rds"), bruto)

  } else if (all(c("DT_COLETA", "SE_COLETA", "COD_UNID") %in% names(df))) {
    # --- Ficha individual: só colunas permitidas (log mostra só nomes) ---
    descartadas <- setdiff(names(df), permitidas)
    message("Tipo: ficha individual | colunas descartadas (", length(descartadas), "): ",
            paste(sort(descartadas), collapse = ", "))
    ausentes <- setdiff(permitidas, names(df))
    if (length(ausentes)) message("Colunas permitidas que não vieram no DBF: ",
                                  paste(sort(ausentes), collapse = ", "))
    df <- df[df$COD_UNID %in% UNIDADES_15RS, intersect(names(df), permitidas), drop = FALSE]
    stopifnot(!any(PROIBIDAS %in% names(df)))
    message("Registros das unidades da 15ª RS: ", nrow(df))
    if (nrow(df) == 0) stop("Nenhuma unidade sentinela da 15ª RS na ficha")
    gravar(df, paste0("sg_ficha_anon_", ano_da_base(df$SE_COLETA), ".rds"), bruto)

  } else {
    stop("Não reconheci o arquivo (nem Ficha SG individual nem Agregado Semanal)")
  }
}

falhas <- 0
for (bruto in brutos) {
  ok <- tryCatch({ processar(bruto); TRUE }, error = function(e) {
    message("[erro] ", conditionMessage(e))
    FALSE
  })
  if (ok) {
    # O bruto da ficha (com nome, CPF, endereço) não fica no disco.
    unlink(bruto)
    message("Bruto apagado: ", basename(bruto))
  } else {
    # Tira da pasta vigiada para o systemd não disparar em loop.
    falhas <- falhas + 1
    dir.create(PASTA_ERRO, showWarnings = FALSE, mode = "0700")
    file.rename(bruto, file.path(PASTA_ERRO, basename(bruto)))
    message("Bruto movido para ", PASTA_ERRO, " - corrija e mova de volta.")
  }
}
if (falhas > 0) quit(status = 1)
