# Gentrificació a Barcelona

Anàlisi de la gentrificació als barris de Barcelona (2015–2025) a partir d'indicadors socioeconòmics, demogràfics i del mercat de l'habitatge.

**Treball Final de Màster — Ciència de Dades — UOC (2025)**
Autor: David Piñol Navas

---


## Aplicació Shiny

L'app visualitza cinc panells:

| Panell | Descripció |
|--------|------------|
| Mapa de variables | Evolució temporal de cada indicador per barri |
| Desplaçament de barcelonins | Canvi absolut vs. relatiu respecte al 2015 |
| Mapa de clusters | Classificació k-means dels 73 barris |
| Comparació de barris | Evolució d'un indicador per dos barris seleccionats |
| Perfil de clusters | Evolució o distribució per cluster |

### Execució local

```r
source("install.R")          # primera vegada
shiny::runApp("shiny/App.R")
```

## Dependències R

`shiny`, `plotly`, `dplyr`, `ggplot2`, `tidyr`, `scales`, `jsonlite`

## Dades

Les dades processades es troben a `dades/net/`. Les dades en brut no s'inclouen al repositori per la seva mida.

**Fonts:**
- Renda per persona: Ajuntament de Barcelona (2015–2023)
- Padró municipal: Ajuntament de Barcelona (2015–2025)
- Habitatges d'ús turístic (HUT): Ajuntament de Barcelona
- Preus de lloguer: Secretaria d'Habitatge, Generalitat de Catalunya
