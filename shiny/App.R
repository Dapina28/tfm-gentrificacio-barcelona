clamp <- function(x, lo, hi) pmax(lo, pmin(hi, x))

library(shiny)
library(plotly)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

df_renda_barri  <- readRDS("../dades/net/df_renda_barri.rds")
df_padro_barri  <- readRDS("../dades/net/df_padro_barri.rds")
df_desplagament <- readRDS("../dades/net/df_desplagament.rds")
df_clusters     <- readRDS("../dades/net/df_clusters.rds")
df_temporal     <- readRDS("../dades/net/df_temporal.rds")
barris_geo_list <- jsonlite::read_json("../dades/net/barris_geo.geojson")

extract_coords <- function(feature) {
  coords <- feature$geometry$coordinates
  nom    <- feature$properties$nom_barri
  ring   <- if (feature$geometry$type == "Polygon") coords[[1]] else coords[[1]][[1]]
  data.frame(
    lon       = vapply(ring, `[[`, numeric(1), 1),
    lat       = vapply(ring, `[[`, numeric(1), 2),
    nom_barri = nom,
    stringsAsFactors = FALSE
  )
}
barris_poly <- bind_rows(lapply(barris_geo_list$features, extract_coords))

lat_ratio <- 1 / cos(41.4 * pi / 180)

year_ranges <- df_temporal |>
  pivot_longer(cols = -c(Any, Nom_Barri), names_to = "var", values_to = "val") |>
  filter(!is.na(val)) |>
  group_by(var) |>
  summarise(min_any = min(Any), max_any = max(Any), .groups = "drop")

variables <- c(
  "Preu lloguer (€/m²)"      = "preu_m2",
  "Renda mitjana (€)"        = "renda_mitjana",
  "% Barcelonins"            = "pct_barcelona",
  "% Pisos turístics"        = "pct_hut",
  "% Estrangers reg. riques" = "pct_regio_rica",
  "% Propietaris jurídics"   = "pct_juridica",
  "% Educació superior"      = "pct_edu_alta",
  "Pèrdua de barcelonins"    = "canvi_bcn"
)

indicadors_choices <- c(
  "Preu lloguer (€/m²)"               = "preu_m2",
  "Renda mitjana (€)"                  = "renda_mitjana",
  "% Barcelonins"                      = "pct_barcelona",
  "% Joves adults (25-39)"             = "pct_joves_adults",
  "% Pisos turístics"                  = "pct_hut",
  "% Estrangers regions rendes altes"  = "pct_regio_rica",
  "% Propietaris jurídics"             = "pct_juridica",
  "% Educació superior"                = "pct_edu_alta",
  "Pèrdua de barcelonins"              = "canvi_bcn"
)

cluster_labels <- c("1" = "Perifèric", "2" = "Acomodat", "3" = "Gentrificat", "4" = "Transició")
cluster_colors <- c("1" = "#66C2A5",   "2" = "#FC8D62",  "3" = "#8DA0CB",     "4" = "#E78AC3")
cl_df <- df_clusters |> mutate(cluster = as.character(cluster))

ui <- navbarPage(
  title = "Gentrificació a Barcelona",

  tabPanel("Mapa de variables",
    sidebarLayout(
      sidebarPanel(
        selectInput("variable", "Variable:", choices = variables),
        sliderInput("any", "Any:", min = 2015, max = 2025, value = 2025, step = 1, sep = "")
      ),
      mainPanel(plotlyOutput("mapa", height = "600px"))
    )
  ),

  tabPanel("Desplaçament de barcelonins",
    sidebarLayout(
      sidebarPanel(
        sliderInput("any_scatter", "Any (vs 2015):",
                    min = 2016, max = 2025, value = 2025, step = 1, sep = "")
      ),
      mainPanel(plotOutput("scatter", height = 600))
    )
  ),

  tabPanel("Mapa de clusters",
    fluidRow(
      column(8, plotlyOutput("mapa_clusters", height = "640px")),
      column(4,
        wellPanel(
          h4(textOutput("titol_barri")),
          uiOutput("info_barri")
        )
      )
    )
  ),

  tabPanel("Comparació de barris",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("barri1", "Barri 1:",
                    choices  = sort(unique(df_temporal$Nom_Barri)),
                    selected = "el Barri Gòtic"),
        selectInput("barri2", "Barri 2:",
                    choices  = sort(unique(df_temporal$Nom_Barri)),
                    selected = "la Vila de Gràcia"),
        selectInput("ind_comp", "Indicador:", choices = indicadors_choices)
      ),
      mainPanel(width = 9, plotOutput("grafic_comp", height = "500px"))
    )
  ),

  tabPanel("Perfil de clusters",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("ind_cluster", "Indicador:", choices = indicadors_choices),
        radioButtons("tipus_cluster", "Visualització:",
                     choices = c("Evolució per cluster" = "linies",
                                 "Distribució (boxplot)" = "boxplot"))
      ),
      mainPanel(width = 9, plotOutput("grafic_cluster", height = "500px"))
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$variable, {
    r <- year_ranges |> filter(var == input$variable)
    updateSliderInput(session, "any",
                      min   = r$min_any,
                      max   = r$max_any,
                      value = clamp(input$any, r$min_any, r$max_any))
  })

  output$mapa <- renderPlotly({
    var     <- input$variable
    var_lbl <- names(variables)[variables == var]

    fmt <- switch(var,
      renda_mitjana = function(x) paste0(round(x), " €"),
      preu_m2       = function(x) paste0(round(x, 1), " €/m²"),
      function(x) paste0(round(x * 100, 1), "%")
    )

    dades <- df_temporal |>
      filter(Any == input$any) |>
      select(Nom_Barri, valor = all_of(var))

    poly_data <- barris_poly |>
      left_join(dades, by = c("nom_barri" = "Nom_Barri")) |>
      mutate(tooltip = if_else(
        is.na(valor), nom_barri,
        paste0(nom_barri, ": ", fmt(valor))
      ))

    p <- ggplot(poly_data,
                aes(x = lon, y = lat, group = nom_barri, fill = valor, text = tooltip)) +
      geom_polygon(color = "white", linewidth = 0.3) +
      coord_fixed(ratio = lat_ratio) +
      scale_fill_distiller(palette = "YlOrRd", direction = 1,
                           name = var_lbl, na.value = "grey80") +
      theme_void()

    ggplotly(p, tooltip = "text") |>
      layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
  })

  output$scatter <- renderPlot({
    df_desplagament |>
      filter(Any == input$any_scatter) |>
      ggplot(aes(x = canvi_absolut, y = canvi_relatiu,
                 label = Nom_Barri, color = Nom_Districte)) +
      geom_point(size = 2.5, alpha = 0.8) +
      geom_text(size = 2.5, vjust = -0.5, color = "black") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::percent) +
      annotate("text", x = -Inf, y = -Inf, hjust = -0.1, vjust = -0.5,
               label = "Desplaçament real", color = "darkred", size = 4) +
      labs(title = paste0("Desplaçament de barcelonins respecte 2015 (", input$any_scatter, ")"),
           x = "Canvi absolut (persones)", y = "Canvi relatiu (%)", color = NULL) +
      theme_minimal()
  })

  output$mapa_clusters <- renderPlotly({
    color_map <- setNames(unname(cluster_colors), unname(cluster_labels))

    poly_data <- barris_poly |>
      left_join(cl_df |> select(nom_barri = Nom_Barri, cluster), by = "nom_barri") |>
      mutate(
        cluster_lbl = cluster_labels[cluster],
        tooltip     = paste0(nom_barri, " — ", cluster_labels[cluster]),
        key         = nom_barri
      )

    p <- ggplot(poly_data,
                aes(x = lon, y = lat, group = nom_barri,
                    fill = cluster_lbl, text = tooltip, key = key)) +
      geom_polygon(color = "white", linewidth = 0.3) +
      coord_fixed(ratio = lat_ratio) +
      scale_fill_manual(values = color_map, name = "Clúster", na.value = "grey80") +
      theme_void()

    ggplotly(p, tooltip = "text", source = "cluster_map") |>
      layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)) |>
      event_register("plotly_click")
  })

  barri_sel <- reactiveVal(NULL)
  observeEvent(event_data("plotly_click", source = "cluster_map"), {
    click <- event_data("plotly_click", source = "cluster_map")
    if (!is.null(click$key)) barri_sel(click$key)
  })

  output$titol_barri <- renderText({
    if (is.null(barri_sel())) "Clica un barri al mapa" else barri_sel()
  })

  output$info_barri <- renderUI({
    req(barri_sel())
    row_cl <- df_clusters |> filter(Nom_Barri == barri_sel())
    if (nrow(row_cl) == 0) return(p("Sense dades per a aquest barri"))

    hist_barri <- df_temporal |> filter(Nom_Barri == barri_sel())

    get_last <- function(col) {
      vals <- hist_barri |> filter(!is.na(.data[[col]])) |> arrange(desc(Any))
      if (nrow(vals) == 0) return(list(val = NA, any = NA))
      list(val = vals[[col]][1], any = vals$Any[1])
    }

    fmt_eur   <- function(l) if (is.na(l$val)) "—" else paste0(round(l$val),      " € (", l$any, ")")
    fmt_eurm2 <- function(l) if (is.na(l$val)) "—" else paste0(round(l$val, 1), " €/m² (", l$any, ")")
    fmt_pct   <- function(l) if (is.na(l$val)) "—" else paste0(round(l$val * 100, 1), "% (", l$any, ")")

    col <- cluster_colors[as.character(row_cl$cluster)]

    tagList(
      p("Cluster: ", tags$strong(
        style = paste0("color:", col, "; font-size:1.1em"),
        cluster_labels[as.character(row_cl$cluster)]
      )),
      tags$table(
        class = "table table-sm",
        tags$thead(tags$tr(tags$th("Indicador"), tags$th("Valor (any)"))),
        tags$tbody(
          tags$tr(tags$td("Preu lloguer"),     tags$td(fmt_eurm2(get_last("preu_m2")))),
          tags$tr(tags$td("Renda mitjana"),    tags$td(fmt_eur(  get_last("renda_mitjana")))),
          tags$tr(tags$td("% Barcelonins"),    tags$td(fmt_pct(  get_last("pct_barcelona")))),
          tags$tr(tags$td("% Joves adults"),   tags$td(fmt_pct(  get_last("pct_joves_adults")))),
          tags$tr(tags$td("% HUTs"),           tags$td(fmt_pct(  get_last("pct_hut")))),
          tags$tr(tags$td("% Prop. jurídics"), tags$td(fmt_pct(  get_last("pct_juridica")))),
          tags$tr(tags$td("% Edu. alta"),      tags$td(fmt_pct(  get_last("pct_edu_alta"))))
        )
      )
    )
  })

  output$grafic_comp <- renderPlot({
    req(input$barri1, input$barri2, input$ind_comp)
    ind     <- input$ind_comp
    ind_lbl <- names(indicadors_choices)[indicadors_choices == ind]
    df_temporal |>
      filter(Nom_Barri %in% c(input$barri1, input$barri2)) |>
      select(Any, Nom_Barri, valor = all_of(ind)) |>
      filter(!is.na(valor)) |>
      ggplot(aes(x = Any, y = valor, color = Nom_Barri)) +
      geom_line(linewidth = 1.3) + geom_point(size = 2.5) +
      scale_color_manual(values = c("#E63946", "#457B9D")) +
      labs(title = paste("Evolució de", ind_lbl), x = "Any", y = ind_lbl, color = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")
  })

  df_cl <- reactive({
    df_temporal |>
      inner_join(df_clusters |> select(Nom_Barri, cluster), by = "Nom_Barri") |>
      select(Any, Nom_Barri, cluster, valor = all_of(input$ind_cluster)) |>
      filter(!is.na(valor)) |>
      mutate(cluster = as.factor(cluster))
  })

  output$grafic_cluster <- renderPlot({
    req(input$ind_cluster, input$tipus_cluster)
    ind_lbl <- names(indicadors_choices)[indicadors_choices == input$ind_cluster]
    df      <- df_cl()
    if (input$tipus_cluster == "linies") {
      df |>
        group_by(Any, cluster) |>
        summarise(valor_mitja = mean(valor, na.rm = TRUE), .groups = "drop") |>
        ggplot(aes(x = Any, y = valor_mitja, color = cluster)) +
        geom_line(linewidth = 1.3) + geom_point(size = 2.5) +
        scale_color_manual(values = cluster_colors, labels = cluster_labels) +
        labs(title = paste("Evolució de", ind_lbl, "per cluster"),
             x = "Any", y = ind_lbl, color = NULL) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "top")
    } else {
      df |>
        filter(Any == max(Any)) |>
        ggplot(aes(x = cluster, y = valor, fill = cluster)) +
        geom_boxplot(alpha = 0.7, outlier.colour = "grey40", outlier.size = 2) +
        scale_fill_manual(values = cluster_colors) +
        labs(
          title = paste("Distribució de", ind_lbl, "per cluster (", max(df$Any), ")"),
          x = "Cluster", y = ind_lbl
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none")
    }
  })
}

shinyApp(ui, server)
