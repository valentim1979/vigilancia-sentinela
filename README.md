# Painel de Indicadores da Vigilância Sentinela de SG — 15ª RS

Painel estático (Quarto + GitHub Pages) com os indicadores de desempenho da Vigilância Sentinela de Síndrome Gripal (SG), conforme a **Nota Técnica nº 9, de 27/11/2025** (atualizada 15/09/2026, SESA-PR), para as unidades sentinelas da 15ª Regional de Saúde de Maringá/PR.

## ⚠️ Privacidade — leia antes de usar

A base de origem (Ficha SG, export `.dbf` do Sivep-Gripe) contém dados identificáveis de paciente (CPF, CNS, nome, nome da mãe, endereço, telefone). Este repositório é **público**. Regras que o script (`script_indicadores_sg.R`) já segue e que **não devem ser quebradas**:

- O `.dbf` bruto nunca é commitado (está no `.gitignore`).
- Só saem para `dados/` e `img/` tabelas **agregadas** (contagens/percentuais por unidade, semana ou faixa etária) e gráficos — nunca uma linha por caso.
- Antes de adicionar qualquer exportação nova, confira à mão que ela não carrega CPF/nome/endereço.

## Estrutura

```
.
├── _quarto.yml              # configuração do site
├── styles.css                # cor padrão dos painéis da 15ª RS (#0057A3)
├── index.qmd                  # página inicial
├── indicadores.qmd            # indicadores 2-9, 11, 14 (NT9)
├── circulacao-viral.qmd       # indicadores 12 e 13 + versão só com vírus específicos
├── script_indicadores_sg.R    # calcula os indicadores a partir do .dbf e gera dados/ e img/
├── dados/                     # CSVs agregados (gerados pelo script, versionados)
├── img/                       # gráficos PNG (gerados pelo script, versionados)
├── docs/                      # HTML final do site (gerado pelo `quarto render`, versionado - é o que o GitHub Pages serve)
└── publicar.sh                # roda o script R + quarto render + git push
```

## Como atualizar o painel

1. Atualize o `SG2026.dbf` localmente (export mais recente do Sivep-Gripe).
2. Confira o caminho do arquivo e o período (`periodo_inicio`/`periodo_fim`) no topo do `script_indicadores_sg.R`.
3. Rode `./publicar.sh` (Git Bash) — ou, à mão:
   ```
   Rscript script_indicadores_sg.R
   quarto render
   git add . && git commit -m "Atualiza indicadores" && git push
   ```

## Configuração inicial do repositório (só na primeira vez)

1. Crie o repositório **`vigilancia-sentinela`** no GitHub (pode ser público, já que nenhum dado identificável é versionado).
2. Clone: `git clone https://github.com/valentim1979/vigilancia-sentinela.git`
3. Copie os arquivos deste pacote (`_quarto.yml`, `styles.css`, os `.qmd`, `script_indicadores_sg.R`, `publicar.sh`, `.gitignore`, `README.md`) para dentro da pasta clonada.
4. Rode `script_indicadores_sg.R` uma vez para gerar `dados/` e `img/`.
5. Rode `quarto render` para gerar `docs/`.
6. `git add . && git commit -m "Primeira versão" && git push`.
7. No GitHub: **Settings > Pages** → Source: **Deploy from a branch** → Branch: **main**, pasta **/docs** → Save.
8. O site fica em `https://valentim1979.github.io/vigilancia-sentinela/`.

## O que este painel NÃO calcula

Os **Indicadores 1 e 10** (NT9) dependem da Ficha de Agregado Semanal do Sivep-Gripe (total de atendimentos gerais por unidade/semana), que é uma fonte de dados diferente da Ficha individual usada aqui. O próprio Sivep-Gripe já calcula esses dois automaticamente (menu **Relatórios > Indicadores**, exportável em Excel) — não estão neste painel.

## Referências

- Nota Técnica nº 9/2025-2026 (SESA-PR) — define os 14 indicadores e a definição de caso vigente.
- Caderno de Análise - Indicadores de Desempenho e Resultado das Unidades Sentinelas da Vigilância da Síndrome Gripal no Brasil (CGCOVID/DEDT/SVSA/MS) — fórmulas de cálculo dos Indicadores 1 a 9.
