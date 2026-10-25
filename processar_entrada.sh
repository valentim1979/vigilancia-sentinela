#!/bin/bash
# ==============================================================================
# processar_entrada.sh — Vigilância Sentinela de SG / 15ª RS Maringá
# Disparado pelo systemd (sg-entrada.path) quando um DBF/ZIP do Sivep-Gripe
# (Ficha SG individual ou Ficha de Agregado Semanal) chega em ~/SG_entrada:
# anonimiza, recalcula os indicadores, renderiza e publica.
# Uso manual: ./processar_entrada.sh
# ==============================================================================

set -eo pipefail

PROJETO="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$PROJETO"
LOG="$PROJETO/processar_entrada.log"

avisar() { notify-send -a "Sentinela SG" "$@" 2>/dev/null || true; }
trap 'avisar -u critical "Publicação do painel sentinela falhou" "Veja $LOG"' ERR

# Uma execução por vez (o systemd pode disparar de novo durante o processamento)
exec 9>"$PROJETO/.processar_entrada.lock"
flock -n 9 || { echo "Já existe um processamento em andamento."; exit 0; }

exec > >(tee -a "$LOG") 2>&1
echo ""
echo "=================================================="
echo "  PROCESSAR ENTRADA — $(date '+%d/%m/%Y %H:%M')"
echo "=================================================="

avisar "Arquivo do Sivep-Gripe (sentinela) recebido" "Anonimizando e recalculando os indicadores…"

# 1º a anonimização: o bruto sai da pasta vigiada em qualquer caso (sucesso
# apaga, erro move para erro/), então o systemd não dispara em loop.
Rscript "$PROJETO/anonimizar_sg.R" "$HOME/SG_entrada"

# publicar.sh faz "git add ." — não publica com alterações locais pendentes
if [ -n "$(git status --porcelain)" ]; then
  echo "Alterações locais não commitadas — base atualizada, publicação adiada."
  avisar -u normal "Base sentinela atualizada, publicação adiada" \
    "Há alterações locais não commitadas em $PROJETO. Faça o commit e rode ./publicar.sh."
  exit 0
fi

git pull --rebase
"$PROJETO/publicar.sh"

avisar "Painel sentinela publicado" "https://valentim1979.github.io/vigilancia-sentinela/"
