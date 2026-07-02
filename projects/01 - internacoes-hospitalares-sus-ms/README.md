# Painel de internações SUS em Mato Grosso do Sul

**Tema:** Saúde pública, auditoria SUS e produção hospitalar  
**Pasta:** `01 - internacoes-hospitalares-sus-ms`  
**Autor:** Matheus Assis de Oliveira  

## Para que serve este projeto

Este projeto organiza as internações financiadas pelo SUS em Mato Grosso do Sul em 2024. Ele foi refeito para servir a **auditores e técnicos de saúde**, inclusive leitores que não são especialistas em estatística.

O objetivo não é ranquear hospitais nem afirmar qualidade assistencial. O objetivo é criar uma base clara para triagem: onde há maior produção, maior valor aprovado, maior mortalidade bruta, maior permanência média e quais municípios merecem revisão documental mais cuidadosa.

## Entregáveis principais

| Entregável | Link |
|---|---|
| Painel interativo com mapa municipal | [abrir painel](<docs/index.html>) |
| Relatório técnico em PDF | [abrir PDF](<reports/01-internacoes-hospitalares-sus-ms.pdf>) |
| Relatório HTML | [abrir HTML](<reports/01-internacoes-hospitalares-sus-ms.html>) |
| Mapa municipal GeoJSON | [abrir GeoJSON](<docs/mapa_painel.geojson>) |
| Instruções de commit | [abrir instruções](<COMMIT_INSTRUCTIONS.txt>) |

## O que foi analisado

- 214.811 internações SUS registradas no SIH/SUS RD em 2024.
- R$ 329.824.342,03 de valor aprovado.
- 8.900 óbitos hospitalares registrados.
- Mortalidade bruta estadual de 4,14%.
- Permanência média de 4,5 dias.
- Mapa dos 79 municípios de Mato Grosso do Sul.

## Como interpretar os indicadores

**Internações** medem produção hospitalar registrada em AIH.  
**Valor aprovado** é valor administrativo do SIH/SUS, não custo total.  
**Mortalidade bruta** é a proporção de internações com óbito registrado. Ela não é ajustada por idade, diagnóstico, gravidade ou complexidade.  
**Permanência média** mostra dias médios de internação e pode indicar complexidade, fluxo assistencial ou permanência prolongada.  
**Prioridade de auditoria** é um escore exploratório que combina volume, valor, mortalidade e permanência para orientar triagem.

## Mapa e análise espacial

O painel agora possui mapa municipal real, construído com malha do IBGE e indicadores do SIH/SUS. Ele permite alternar entre:

- internações;
- valor aprovado;
- mortalidade bruta;
- permanência média;
- prioridade exploratória de auditoria.

O mapa usa **município de internação**, não município de residência. Portanto, ele mostra concentração da produção hospitalar e papel regional dos serviços, não risco populacional direto.

## Arquivos de dados

- `data/processed/sih_rd_ms_2024_internacoes.csv`
- `data/processed/sih_rd_ms_2024_resumo_mensal.csv`
- `data/processed/sih_rd_ms_2024_resumo_municipal.csv`
- `data/processed/s01_prioridade_auditoria_municipios.csv`
- `docs/mapa_painel.geojson`

## Reprodutibilidade

O script abaixo reconstrói o mapa, o painel e os relatórios:

```r
source("scripts/reconstruir_s01_portugues.R")
```

## Limitações

Este projeto é descritivo. Ele não ajusta risco, não mede qualidade hospitalar isoladamente e não substitui auditoria documental. Para avaliação de desempenho, é necessário cruzar diagnóstico, procedimento, complexidade, idade, origem do paciente, CNES, perfil assistencial e regras de faturamento.

