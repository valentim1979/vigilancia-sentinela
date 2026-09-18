#!/bin/bash
# Publica o painel de indicadores SG no GitHub Pages.
# Uso: rode este script a partir da raiz do repositório (mesma pasta
# deste arquivo), depois de já ter atualizado o SG2026.dbf localmente.
#
# [Inferência] Assume Git Bash (vem com o Git para Windows) ou um
# terminal Unix-like. Se preferir rodar no PowerShell/cmd, os mesmos 3
# passos (Rscript, quarto render, git push) podem ser feitos à mão.

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
