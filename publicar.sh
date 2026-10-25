#!/bin/bash
# Publica o painel de indicadores SG no GitHub Pages.
# Normalmente roda sozinho via processar_entrada.sh, quando um DBF novo
# chega em ~/SG_entrada. Rodar à mão: ./publicar.sh (usa as bases já
# anonimizadas em ~/SG_dados).

cd "$(dirname "${BASH_SOURCE[0]}")"
set -e  # para o script se algum passo falhar, em vez de seguir com dado incompleto

echo ">>> 1/3 - Calculando indicadores (script_indicadores_sg.R)..."
Rscript script_indicadores_sg.R

echo ">>> 2/3 - Renderizando o site Quarto..."
quarto render

echo ">>> 3/3 - Publicando no GitHub..."
git add .
git commit -m "Atualiza indicadores SG - $(date +'%Y-%m-%d %H:%M')" || echo "Nada novo para commitar."
git push origin main

echo ">>> Pronto. Se for a primeira publicação, confira em Settings > Pages"
echo "    do repositório no GitHub que a fonte está configurada como"
echo "    'Deploy from a branch' -> branch 'main', pasta '/docs'."
