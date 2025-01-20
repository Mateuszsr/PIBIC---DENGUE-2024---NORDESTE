# 📊 Análise de Regressão Espacial da Dengue no Nordeste

A dengue representa um grande desafio para a saúde pública, com impactos significativos tanto na saúde das populações quanto na economia. Este projeto visa realizar uma análise espacial da distribuição da dengue no estado de Sergipe, correlacionando a incidência da doença com determinantes sociais e fatores ambientais, como o abastecimento de água, o saneamento básico, o IDHM e o Índice de Gini. O objetivo é entender os padrões de transmissão e identificar áreas de maior risco e vulnerabilidade.

## 🎯 Objetivos
### Objetivo Geral
- Realizar uma análise espacial da distribuição da dengue em Sergipe, correlacionando-a com determinantes sociais e fatores ambientais.

### Objetivos Específicos
- Caracterizar os pacientes acometidos com dengue no Nordeste.
- Verificar a existência de autocorrelação espacial da incidência da dengue.
- Identificar padrões na distribuição espacial da dengue.
- Calcular o Risco Relativo da infecção por dengue.
- Relacionar determinantes sociais, fatores ambientais e climáticos com a incidência da dengue.

## 🔬 Metodologia
1. **Coleta de Dados**:
   - Dados sobre notificações de casos de dengue no ano de 2024 serão obtidos através da plataforma DATASUS, com filtro para os casos confirmados na região Nordeste do Brasil.
   - Informações sobre características dos domicílios (abastecimento de água, esgotamento sanitário, coleta de lixo) e indicadores municipais (IDHM, Índice de Gini, Índice de Vulnerabilidade Social) serão extraídas do SIDRA (IBGE), referentes ao Censo 2022.

2. **Análise de Dados**:
   - A análise será realizada utilizando o software R (versão 4.3.2).
   - Pacotes específicos de análise espacial serão utilizados para verificar a autocorrelação espacial e identificar padrões de distribuição.
   - A base cartográfica será obtida do IBGE, atualizada em 2022, através do pacote `geobr`.

3. **Nível de Significância**:
   - O nível de significância adotado será de 5%, com um nível de confiança de 95%.

## 🗺️ Resultados Esperados
- Identificação de áreas de maior risco e vulnerabilidade à dengue em Sergipe.
- Entendimento da relação entre fatores sociais, ambientais e climáticos com a incidência da doença.
- Contribuição para o planejamento de políticas públicas de prevenção e controle da dengue.

## 🛠️ Tecnologias Utilizadas
- **R** (versão 4.3.2)
- **Pacotes**: `geobr`, `spdep`, `rgdal`, entre outros.
- **Fontes de Dados**: DATASUS, SIDRA (IBGE).

## 📑 Referências
- DONALISIO, M. R.; FREITAS, A. R.; ZUBEN, F. M. A. O Impacto Econômico da Dengue. *Revista de Saúde Pública*, 2017.
- DA SILVA, G. D.; MACHADO, E. A. Impacto Climático e de Saneamento na Proliferação da Dengue. *Revista Brasileira de Epidemiologia*, 2019.
- DO CARMO, E. T.; et al. A Relação entre Temperatura e Incidência de Arbovírus. *Revista Brasileira de Saúde Pública*, 2020.

## 📫 Contato
- Email: [Mateusramos2001@gmail.com](mailto:Mateusramos2001@gmail.com)
- LinkedIn: [Seu LinkedIn](https://www.linkedin.com/)
- GitHub: [Seu GitHub](https://github.com/)

---

Obrigado por visitar o projeto! 🚀
