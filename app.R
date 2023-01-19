library(shiny)
library(DT)
library(data.table)
library(networkD3)
library(igraph)
library(jsonlite)
library(shinythemes)
library(curl)
library(RColorBrewer)
library(heatmaply)

about <- paste(readLines("changable_about.txt", warn = FALSE), collapse = "\n")

url <- "https://dracor.org/api/corpora/rus"
urlshort <- "https://dracor.org/api/corpora/"
urlcorpora <- 'https://dracor.org/api/corpora'

downloadcorpus <- function(url){
  fromJSON(url, flatten = TRUE)$dramas
}

selectcorpus <- function(urlcorpora){
  corpora <- fromJSON(urlcorpora)
  cornames <- corpora$name
  names(cornames) <- corpora$title
  as.list(cornames)
}

selectauthors <- function(corp){
  authors <- unique(corp$author.name)
  names(authors) <- unique(corp$author.name)
  authors
}

selectplays <- function(corp, input = input){
  links <- corp[corp$author.name == input, "networkdataCsvUrl"]
  names(links) <- corp[corp$author.name == input, "title"]
  links[order(names(links))]
}

options(shiny.sanitize.errors = FALSE)

csv2d <- function(file){
  d <- fread(file, encoding = "UTF-8")
  colnames(d) <- tolower(colnames(d))
  d <- d[,.(source, target, weight)]
  d <- d[weight>0,]
  d}

d2ig <- function(d){
  x <- graph_from_data_frame(d, directed = FALSE)
  V(x)$betweenness <- betweenness(x, v = V(x), directed = F, weights = NA)
  V(x)$closeness <- closeness(x, weights = NA, normalized = T)
  V(x)$strength <- strength(x)
  V(x)$degree <- degree(x)
  V(x)$average_distance <- 1/closeness(x, weights = NA, normalized = T)
  x
}

d2istats <- function(d){
  x <- graph_from_data_frame(d, directed = FALSE)
  df <- data.frame(`Mean Distance` = mean_distance(x, directed = FALSE),
                  `Graph Density` = edge_density(x),
                  Clustering =  transitivity(x),
                  Diameter = diameter(x),
                  check.names = FALSE)
  df
}

add_labels <- function(d3, link){
  shortlink <- gsub("/networkdata/csv", "",link, fixed = TRUE)
  bib <- fromJSON(shortlink)$cast
  d3$nodes$label <- as.character(d3$nodes$name)
  index <- match(bib$id, d3$nodes$label, nomatch = 0)
  d3$nodes$label[index] <- bib[bib$id %in% d3$nodes$label, "name"]
  d3
}

ig2d3 <- function(x,
                  cluster = cluster_label_prop,
                  nodemetric = strength,
                  edgesize = 0.1, 
                  nodesize = 0.1){
  members <- membership(cluster(x))
  x_d3 <- igraph_to_networkD3(x, group = members)
  x_d3 <- add_labels(x_d3, isolate(values$url))
  x_d3$links$value = E(x)$weight * edgesize
  x_d3$nodes$nodesize = nodemetric(x) * nodesize
  x_d3
}

plotnet <- function(x_d3, charge = -1000, size = 400, fontsize = 20){
  forceNetwork(Links = x_d3$links, Nodes = x_d3$nodes, charge = charge,
               Source = 'source', Target = 'target', NodeID = 'label', 
               colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
               Group = "group", Value = "value",
               width = size, height = size,
               Nodesize = "nodesize", opacity = 0.8, opacityNoHover = 0.85,
               fontSize = fontsize)}

ig2csv <- function (x){
  as.data.table(as_data_frame(x, "vertices"))
}

formatRainbow <- function(data, met, name, pall){
  formatStyle(data, columns = name,
            background = styleInterval(
              cuts = seq(min(met), max(met) * 2, length.out = 8),
              brewer.pal(9, pall)))}

heat <- function(m, type = "plotly", maxn = 40) {
  switch(type,
        ggplot =  heatmaply(m,
                            grid_gap = 0,
                            colors = colorRampPalette(brewer.pal(9, "YlGnBu")),
                            Colv = FALSE,
                            Rowv = FALSE,
                            margins = c(100, 100, 0, 0),
                            showticklabels = length(colnames(m)) < maxn,
                            label_names = c("Column", "Row", "Weight"),
                            plot_method = "ggplot",
                            colorbar_len = 1,
                            node_type = "scatter",
                            point_size_mat = m),
        plotly = colorbar(heatmaply(m,
                                    colors = colorRampPalette(
                                      brewer.pal(9, "YlGnBu")),
                                    grid_gap = 0,
                                    Colv = FALSE,
                                    Rowv = FALSE,
                                    margins = c(100, 100, 0, 0),
                                    showticklabels = length(colnames(m)) < maxn,
                                    label_names = c("Column", "Row", "Weight"),
                                    plot_method = "plotly",
                                    colorbar_len = 0.7),
                          nticks = max(m) + 1)
  )}

########
ui <- fluidPage(theme = shinytheme("united"),
                #shinythemes::themeSelector(),
  headerPanel("Shiny Dracor"),
  sidebarLayout(
  sidebarPanel(
    verticalLayout(
      uiOutput("corpora"),
      uiOutput("authors"),
      uiOutput("base"),
      tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              ")))),
    wellPanel(
      selectInput("nodemetric", "Choose a metric for nodes size:",
                  choices = list("Degree" = 'degree',
                                 "Strength" = 'strength',
                                 "Betweeness Centrality" = 'betweenness',
                                 "Closeness Centrality" = 'closeness')),
      selectInput("cluster", "Choose clusterization algorithm:",
                  choices = list('cluster_optimal',
                                 'cluster_edge_betweenness',
                                 'cluster_fast_greedy',
                                 'cluster_label_prop',
                                 'cluster_leading_eigen',
                                 'cluster_louvain',
                                 'cluster_spinglass',
                                 'cluster_walktrap')),
      splitLayout(verticalLayout(
        sliderInput("charge", "Select charge:",
                    min = 0, max = 12, value = 4, step = 0.05),
        sliderInput("nodesize", "Nodes size:",
                    min = 0, max = 4, value = 1, step = 0.05)),
        verticalLayout(
      sliderInput("fontsize", "Select font size:",
                  min = 0, max = 50, value = 20),
      sliderInput("edgesize", "Edges size:",
                  min = 0, max = 4, value = 1, step = 0.05)))
    )
  ),
  mainPanel(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    tabsetPanel(
      tabPanel("Graph", forceNetworkOutput("force")),
      tabPanel("Edges", dataTableOutput(outputId = "table")),
      tabPanel("Vertices", dataTableOutput(outputId = "vertices")),
#      tabPanel("Weights matrix", tableOutput(output = "matrix")),
      tabPanel("Play Info", verticalLayout(plotlyOutput("heatmap",
                                                        width = 550,
                                                        height = 400),
                                           tableOutput(output = "info"))),
      tabPanel("About", HTML(about))
      #tabPanel("About", includeHTML(knitr::knit2html("about.Rmd", force_v1 = T, fragment.only = T)))
    )
  )
  )
)
server <- function(input, output){

  corp <- reactive({
    lang <- input$corpus
    if(is.null(lang)) return(NULL)
    downloadcorpus(paste0(urlshort, lang))
  })

  output$corpora <- renderUI({
    selectInput("corpus",
                "Choose a corpus",
                selectcorpus(urlcorpora)
    )
})

  output$authors <- renderUI({
    if (is.null(corp())) return(NULL)
    selectInput("selectedauthor",
                "Choose a writer from a list:",
                selectauthors(corp())
    )
  })

  output$base <- renderUI({
    if (is.null(corp())) return(NULL)
    selectInput("file2download",
                "Choose his/her play to visualize:",
                selectplays(corp(), input$selectedauthor)
    )
  })


  d <- reactive({inFile <- input$file2download
    if (is.null(inFile)) return(NULL)
    values <<- reactiveValues(url = inFile)
    csv2d(inFile)})
  ig <- reactive({
    if (is.null(d())) return(NULL)
    d2ig(d())
    })
  df <- reactive({
    if (is.null(ig())) return(NULL)
    as_data_frame(ig(), "vertices")
    })
  d3 <- reactive({
    if (is.null(ig())) return(NULL)
    ig2d3(ig(),
          cluster = eval(parse(text = input$cluster)),
          nodemetric = eval(parse(text = input$nodemetric)),
          edgesize = exp(input$edgesize) - 1,
          nodesize = exp(input$nodesize) - 1)
    })

  output$table <- DT::renderDataTable({
    datatable(d(),
              options = list(digits = 2,
                             lengthMenu = c(10, 15, 20, 50)),
                             filter = 'top',
                             rownames = FALSE) %>%
    formatStyle(columns = c('weight'),
                background = styleInterval(cuts = round(seq(0, 
                                                            max(d()$weight) * 2,
                                                            length.out = 8)),
                                           brewer.pal(9, "GnBu")))
    })
  output$vertices <- DT::renderDataTable({
      datatable(df(),
                options = list(digits = 2,
                               lengthMenu = c(10, 15, 20, 50)),
                filter = 'top',
                rownames = FALSE) %>%
      formatRound(columns = c('closeness'), digits=4) %>%
      formatRainbow(df()$closeness, "closeness", "BuGn") %>%
      formatRainbow(df()$betweenness, "betweenness", "GnBu") %>%
      formatRound(columns=c('betweenness'), digits=2) %>%
      formatRainbow(df()$strength, "strength", "PuRd") %>%
      formatRainbow(df()$degree, "degree", "Reds") %>%
      formatRainbow(df()$average_distance, "average_distance", "Purples")
  })
#  output$matrix <- renderTable({
#    as.matrix(ig()[])
#  }, rownames = T, bordered = T, striped = T, spacing = "xs", digits = 0, align = "c")

  output$heatmap <- renderPlotly(
    heat(as.matrix(ig()[]))
  )
  output$force <- renderForceNetwork({
    if (is.null(d3())) return(NULL)
    #plotnet(d3(), charge = -(input$charge+1)^2.2, fontsize = input$fontsize)
    plotnet(d3(), charge = -2 ^ (input$charge + 1), fontsize = input$fontsize)
    })

  output$info <- renderTable({
                              d() %>% d2istats()
    },
                             rownames = FALSE,
                             bordered = FALSE,
                             striped = FALSE,
                             digits = 2
  )

}
shinyApp(ui = ui, server = server)
