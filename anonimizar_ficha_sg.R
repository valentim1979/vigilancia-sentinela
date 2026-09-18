# ..........................................................................................
#         PRÉ-TRATAMENTO LGPD - Ficha SG (antes de subir para qualquer
#         nuvem/API fora do ambiente institucional: Google Drive, Nextcloud etc.)
# ..........................................................................................
#
# [Não verificado] Este script NÃO é aconselhamento jurídico. Baseei o
# que ele faz nos seguintes artigos da LGPD (Lei 13.709/2018):
#   - Art. 5º, XI: define ANONIMIZAÇÃO como uso de meios técnicos
#     razoáveis para que um dado perca a possibilidade de associação,
#     direta ou indireta, a um indivíduo.
#   - Art. 12: dado anonimizado só deixa de ser "dado pessoal" (e sai
#     do escopo da LGPD) se a reidentificação NÃO for possível com
#     esforços razoáveis - não basta tirar nome e CPF.
#   - Art. 13, §4º: define PSEUDONIMIZAÇÃO como tratamento em que o
#     dado só pode ser reassociado à pessoa por meio de informação
#     adicional mantida separadamente, em ambiente controlado. Dado
#     pseudonimizado AINDA é dado pessoal pela LGPD.
#
# O QUE ESTE SCRIPT FAZ: remove por completo os campos de identificação
# direta (CPF, CNS, nome, endereço, telefone) e troca CPF/CNS por um
# hash com sal (pseudonimização, art. 13 §4º) - não uma anonimização
# irreversível.
#
# O QUE ESTE SCRIPT NÃO FAZ (fica sob sua responsabilidade avaliar):
# - Não trata os QUASI-IDENTIFICADORES que sobram (município de
#   residência, data de nascimento/idade, sexo, datas de sintoma e
#   coleta). Com poucos casos por semana/unidade, essa combinação pode
#   ainda reidentificar alguém - é um risco de reidentificação por
#   ligação de dados (linkage), não coberto por só tirar nome/CPF.
#   Se o destino final for público ou fora do controle institucional,
#   considere agregar/generalizar esses campos também (ex.: idade em
#   faixas, já tem classificar_faixa_etaria() no script de indicadores;
#   município a nível de RS, não de bairro).
# - Não decide a base legal de tratamento (LGPD art. 7º/11º) nem
#   confirma se sua instituição já tem um fluxo homologado para isso -
#   pergunte ao encarregado de proteção de dados (DPO) da SESA-PR/15ª RS.
#
# [Não verificado] O dicionário de dados que você me passou lista o
# campo DBF do "Nome" do paciente (campo 9) com o MESMO nome do CNS
# (campo 8) - "NU_CNS" para os dois - o que é quase certamente um erro
# de digitação no próprio dicionário oficial. Não tenho como confirmar
# o nome real da coluna de nome sem ver o seu .dbf. Rode names(sg_raw)
# e confira antes de usar - deixei uma lista de candidatos prováveis
# abaixo (ajuste se nenhum bater).

library(dplyr)
library(digest)  # install.packages("digest") - para o hash com sal

# ..........................................................................................
#   1. SEGREDO DO HASH (nunca no repositório, só no seu .Renviron local)
# ..........................................................................................
#
# CPF/CNS têm poucos dígitos válidos - um hash SEM sal pode ser
# quebrado testando todas as combinações possíveis contra o hash
# (ataque de força bruta/rainbow table). O "sal" é uma string secreta
# que você mistura antes de gerar o hash, pra isso não ser viável.
#
# No terminal (uma vez só): openssl rand -hex 16
# Depois adicione ao seu ~/.Renviron:
#   SALT_PSEUDONIMIZACAO_SG=<o valor gerado>
readRenviron("~/.Renviron")
sal_pseudonimizacao <- Sys.getenv("SALT_PSEUDONIMIZACAO_SG")
if (identical(sal_pseudonimizacao, "")) {
  stop("Defina SALT_PSEUDONIMIZACAO_SG no seu ~/.Renviron antes de rodar ",
       "este script (veja o comentario acima para gerar um valor).")
}

# ..........................................................................................
#   2. CAMPOS REMOVIDOS POR COMPLETO (identificação direta, sem valor
#      analítico - remover é sempre certo, não precisa pesar custo x
#      benefício aqui)
# ..........................................................................................

campos_identificacao_direta <- c(
  "NU_CPF", "NU_CNS", "NOME_MAE", "TELEFONE",
  "CEP", "COD_LOGRAD", "NOM_LOGRAD", "NUM_LOGRAD", "COMPLEMENT",
  # Estes dois identificam o PROFISSIONAL DE SAÚDE, não o paciente -
  # mas ainda são dado pessoal de um terceiro, então saem também.
  "NOME_PROF", "REG_PROF"
)

# [Não verificado] Candidatos para o campo de nome do paciente - ajuste
# conforme o que aparecer em names(sg_raw) no seu .dbf real.
candidatos_nome_paciente <- c("NM_PACIENT", "NOME_PAC", "NOME", "NM_PAC")

# ..........................................................................................
#   3. FUNÇÃO PRINCIPAL
# ..........................................................................................

anonimizar_ficha_sg <- function(df, sal = sal_pseudonimizacao) {

  total_linhas <- nrow(df)

  # --- 3.1 Nome do paciente ---
  nome_encontrado <- intersect(candidatos_nome_paciente, names(df))
  if (length(nome_encontrado) > 0) {
    df <- df %>% select(-all_of(nome_encontrado))
    message("Campo(s) de nome removido(s): ", paste(nome_encontrado, collapse = ", "))
  } else {
    warning("Nenhum campo de nome do paciente encontrado entre os candidatos [",
            paste(candidatos_nome_paciente, collapse = ", "),
            "]. Rode names(df) e remova manualmente a coluna de nome antes ",
            "de subir este arquivo para qualquer lugar - NÃO prossiga sem isso.")
  }

  # --- 3.2 ID pseudonimizado a partir de CPF/CNS, para permitir
  #     rastrear o MESMO caso ao longo do tempo (ex.: ficha inicial +
  #     atualização de resultado laboratorial) sem guardar CPF/CNS real.
  #     Uso um loop simples (não vetorizado) de propósito: digest() só
  #     aceita 1 valor por vez, e tentar vetorizar isso com ifelse()
  #     dentro de mutate() é um erro fácil de cometer (já vi esse
  #     padrão dar problema antes neste mesmo projeto).
  if (all(c("NU_CPF", "NU_CNS") %in% names(df))) {
    id_pseudonimizado <- character(total_linhas)
    for (i in seq_len(total_linhas)) {
      cpf_i <- df$NU_CPF[i]
      cns_i <- df$NU_CNS[i]
      valor_base <- if (!is.na(cpf_i) && trimws(cpf_i) != "") cpf_i else cns_i
      id_pseudonimizado[i] <- if (is.na(valor_base) || trimws(valor_base) == "") {
        NA_character_
      } else {
        digest::digest(paste0(sal, valor_base), algo = "sha256")
      }
    }
    df$id_pseudonimizado <- id_pseudonimizado
  } else {
    warning("Colunas NU_CPF/NU_CNS não encontradas - id_pseudonimizado não foi gerado.")
  }

  # --- 3.3 Remove os campos de identificação direta que existirem ---
  campos_presentes <- intersect(campos_identificacao_direta, names(df))
  df <- df %>% select(-all_of(campos_presentes))
  message(length(campos_presentes), " campo(s) de identificação direta removido(s): ",
          paste(campos_presentes, collapse = ", "))

  # --- 3.4 Data de nascimento: remove e mantém só IDADE/TP_IDADE (já
  #     existem na ficha, e são menos precisos que a data exata - data
  #     de nascimento exata + município pequeno é um risco real de
  #     reidentificação).
  if ("DT_NASC" %in% names(df)) {
    df <- df %>% select(-DT_NASC)
    message("DT_NASC removida (mantido IDADE/TP_IDADE, já derivados na ficha).")
  }

  # --- 3.5 Aviso final sobre o que NÃO foi tratado ---
  message(
    "\nATENÇÃO: restam quasi-identificadores nesta base (município de ",
    "residência, idade, sexo, datas de sintoma/coleta). Com poucos casos ",
    "por semana/unidade, isso ainda pode reidentificar alguém combinado ",
    "com conhecimento externo. Avalie se o destino deste arquivo precisa ",
    "de tratamento adicional (generalizar idade em faixas, agregar ",
    "município) antes de subir para fora do ambiente institucional."
  )

  df
}

# ..........................................................................................
#   4. EXEMPLO DE USO
# ..........................................................................................
# sg_raw <- foreign::read.dbf("SG2026.dbf", as.is = TRUE)
# sg_pseudonimizado <- anonimizar_ficha_sg(sg_raw)
# write.csv(sg_pseudonimizado, "sg_pseudonimizado.csv", row.names = FALSE)
#
# [Não verificado] Não testei este script contra o seu .dbf real - em
# especial a parte 3.1 (nome do paciente) depende de eu ter acertado o
# nome do campo, o que não pude confirmar. Rode e me diga o que
# aparecer nos avisos (warning()) antes de considerar o arquivo pronto
# para sair do seu computador.
