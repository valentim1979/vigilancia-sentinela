# Painel de Indicadores da Vigilância Sentinela de SG — 15ª RS

Painel estático (Quarto + GitHub Pages) com os indicadores de desempenho da Vigilância Sentinela de Síndrome Gripal (SG), conforme a **Nota Técnica nº 9, de 27/11/2025** (atualizada 15/09/2026, SESA-PR), para as unidades sentinelas da 15ª Regional de Saúde de Maringá/PR.

## ⚠️ Privacidade — leia antes de usar

A base de origem (Ficha SG, export `.dbf` do Sivep-Gripe) contém dados identificáveis de paciente (CPF, CNS, nome, nome da mãe, endereço, telefone). Este repositório é **público**. Regras que o script (`script_indicadores_sg.R`) já segue e que **não devem ser quebradas**:

- O `.dbf` bruto **nunca fica no `~/Work`** (que vai para o OneDrive e para o Syncthing) nem é commitado. Ele entra por `~/SG_entrada/`; o `anonimizar_sg.R` mantém só as colunas de `colunas_permitidas.txt` (sem nome, CPF, CNS, nome da mãe, endereço, telefone, data de nascimento, nº do registro e campos de texto livre) e só as unidades sentinelas da 15ª RS, grava a base em `~/SG_dados/` (pasta 0700, fora do `~/Work`) e **apaga o bruto**. Em erro, o bruto vai para `~/SG_entrada/erro/`.
- Só saem para `dados/` e `img/` tabelas **agregadas** (contagens/percentuais por unidade, semana ou faixa etária) e gráficos — nunca uma linha por caso.
- Antes de adicionar qualquer exportação nova, confira à mão que ela não carrega CPF/nome/endereço.

## Estrutura

```
.
├── _quarto.yml              # configuração do site
├── styles.css                # cor padrão dos painéis da 15ª RS (#0057A3)
├── index.qmd                  # página inicial
├── indicadores.qmd            # indicadores 1-11 e 14 (NT9)
├── circulacao-viral.qmd       # indicadores 12 e 13 + versão só com vírus específicos
├── anonimizar_sg.R            # DBF bruto de ~/SG_entrada -> base tratada em ~/SG_dados (apaga o bruto)
├── colunas_permitidas.txt     # colunas da Ficha SG que saem do bruto
├── script_indicadores_sg.R    # calcula os indicadores a partir de ~/SG_dados e gera dados/ e img/
├── processar_entrada.sh       # disparado pelo systemd: anonimiza + publica
├── dados/                     # CSVs agregados (gerados pelo script, versionados)
├── img/                       # gráficos PNG (gerados pelo script, versionados)
├── docs/                      # HTML final do site (gerado pelo `quarto render`, versionado - é o que o GitHub Pages serve)
└── publicar.sh                # roda o script R + quarto render + git push
```

## Como atualizar o painel

1. No Sivep-Gripe, exporte o DBF do ano da **Ficha SG individual** e/ou da **Ficha de Agregado Semanal** (pode ser `.dbf` ou `.zip`) e salve em `~/SG_entrada/`. O script reconhece o tipo pelas colunas, não pelo nome do arquivo.
2. A unit systemd de usuário `sg-entrada.path` dispara `processar_entrada.sh`: anonimiza, roda o `script_indicadores_sg.R`, `quarto render`, commit e push, com notificação no desktop. Log em `processar_entrada.log`.
3. Se a árvore do git estiver suja, a base é atualizada mas a publicação é adiada: faça o commit e rode `./publicar.sh`.

O período analisado vai da SE 1 do ano da base até o último sábado antes da exportação da Ficha SG — exporte as duas fichas juntas para os Indicadores 1 e 10 cobrirem o mesmo período dos demais.

## Configuração inicial do repositório (só na primeira vez)

1. Crie o repositório **`vigilancia-sentinela`** no GitHub (pode ser público, já que nenhum dado identificável é versionado).
2. Clone: `git clone https://github.com/valentim1979/vigilancia-sentinela.git`
3. Copie os arquivos deste pacote (`_quarto.yml`, `styles.css`, os `.qmd`, `script_indicadores_sg.R`, `publicar.sh`, `.gitignore`, `README.md`) para dentro da pasta clonada.
4. Rode `script_indicadores_sg.R` uma vez para gerar `dados/` e `img/`.
5. Rode `quarto render` para gerar `docs/`.
6. `git add . && git commit -m "Primeira versão" && git push`.
7. No GitHub: **Settings > Pages** → Source: **Deploy from a branch** → Branch: **main**, pasta **/docs** → Save.
8. O site fica em `https://valentim1979.github.io/vigilancia-sentinela/`.

## Indicadores 1 e 10

Calculados da Ficha de Agregado Semanal (`sg_agregado_<ano>.rds`): Indicador 1 = SE com registro de agregado / SE no período; Indicador 10 = atendimentos por SG (`SG_TOTAL`) / consultas gerais (`TC_TOTAL`). O próprio Sivep-Gripe também calcula os dois (menu **Relatórios > Indicadores**) — use para conferir. Sem a base do agregado, os dois ficam de fora e o resto do painel sai normalmente.

## Referências

- Nota Técnica nº 9/2025-2026 (SESA-PR) — define os 14 indicadores e a definição de caso vigente.
- Caderno de Análise - Indicadores de Desempenho e Resultado das Unidades Sentinelas da Vigilância da Síndrome Gripal no Brasil (CGCOVID/DEDT/SVSA/MS) — fórmulas de cálculo dos Indicadores 1 a 9.
