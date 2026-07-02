# Internacoes, ICSAP e Auditoria Hospitalar SUS

## Motivacao

A producao hospitalar do SUS concentra volume, valores e eventos clinico-administrativos que precisam de triagem clara antes da auditoria documental.

## Objetivo da pesquisa

Organizar indicadores do SIH/SUS, internacoes sensiveis a atencao primaria e alertas de AIH para priorizar municipios e registros.

## Resultados

A pesquisa combina volume, valor aprovado, mortalidade bruta, permanencia, ICSAP, prestadores e alertas administrativos em paineis de auditoria exploratoria.

## Dashboard executivo

Abra a apresentacao em [docs/index.html](docs/index.html). Ela resume a pesquisa, lista os entregaveis e aponta para os dashboards originais de cada estudo.

## Projetos incluidos

| Estudo | Dashboard | Relatorios | Mapa |
|---|---|---|---|
| 01 - internacoes-hospitalares-sus-ms | [Dashboard](projects/01 - internacoes-hospitalares-sus-ms/docs/index.html) | [01-internacoes-hospitalares-sus-ms.html](projects/01 - internacoes-hospitalares-sus-ms/reports/01-internacoes-hospitalares-sus-ms.html)<br>[01-internacoes-hospitalares-sus-ms.pdf](projects/01 - internacoes-hospitalares-sus-ms/reports/01-internacoes-hospitalares-sus-ms.pdf) | Sim |
| 05 - internacoes-condicoes-sensiveis-atencao-primaria | [Dashboard](projects/05 - internacoes-condicoes-sensiveis-atencao-primaria/docs/index.html) | [05-ambulatory-care-sensitive-hospitalizations.html](projects/05 - internacoes-condicoes-sensiveis-atencao-primaria/reports/05-ambulatory-care-sensitive-hospitalizations.html)<br>[05-ambulatory-care-sensitive-hospitalizations.pdf](projects/05 - internacoes-condicoes-sensiveis-atencao-primaria/reports/05-ambulatory-care-sensitive-hospitalizations.pdf) | Sim |
| 06 - auditoria-analitica-aih | [Dashboard](projects/06 - auditoria-analitica-aih/docs/index.html) | [06-aih-analytical-audit.html](projects/06 - auditoria-analitica-aih/reports/06-aih-analytical-audit.html)<br>[06-aih-analytical-audit.pdf](projects/06 - auditoria-analitica-aih/reports/06-aih-analytical-audit.pdf) | Sim |

## Estrutura

- `docs/index.html`: apresentacao executiva do repositorio.
- `projects/`: estudos originais preservados para auditoria.
- `projects/*/src/R`: scripts analiticos.
- `projects/*/reports`: relatorios tecnicos e figuras.
- `projects/*/data/processed`: bases finais usadas nos dashboards.

## Reprodutibilidade

Os dados brutos pesados, caches publicos e bases processadas record-level acima de 50 MB nao entram no Git para manter o repositorio publicavel. Quando necessario, consulte `data/raw/README.md`, scripts em `src/R` e instrucoes de download dentro de cada estudo.

## Limites

As analises sao exploratorias e tecnicas. Mapas, rankings e modelos apoiam triagem e hipotese; nao substituem validacao institucional, auditoria documental ou decisao clinica.
