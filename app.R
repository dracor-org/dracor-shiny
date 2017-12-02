
library(shiny)
library(DT)
library(data.table)
library(networkD3)
library(igraph)
library(RColorBrewer)
library(jsonlite)
library(curl)
#library(RJSONIO)

about <- readLines(con <- file("changable_about.txt"))
close(con)
paste(about, collapse = "")

url <- "https://api.github.com/repositories/86837035/contents/csv/"
titles <- fromJSON(url, flatten = T)
dow <- titles$download_url
names(dow) <- titles$name
dow <- dow[grep('.*\\.csv', dow)]
names(dow) <- gsub('(.*)\\.csv','\\1', names(dow))

csv2d <- function(file){
  d <- fread(file, encoding = "UTF-8")
  #d <- d[,c(1,3,2,4)] #changing columns order
  colnames(d) <- tolower(colnames(d))
  d <- d[,.(source, target, weight)]
  d <- d[weight>0,]
  d}

d2ig <- function(d){
  x <- graph_from_data_frame(d, directed = F)
  V(x)$betweenness <- betweenness(x, v = V(x), directed = F, weights = NA)
  V(x)$closeness <- closeness(x, weights = NA)
  V(x)$strength <- strength(x)
  V(x)$degree <- degree(x)
  V(x)$average_distance <- 1/closeness(x, weights = NA)
  V(x)$graph_density <- edge_density(x)
  x
}


ig2d3 <- function(x, cluster = cluster_label_prop, nodemetric = strength, edgesize = 0.1, nodesize = 0.1){
  members <- membership(cluster(x))
  x_d3 <- igraph_to_networkD3(x, group = members)
  x_d3$links$value = E(x)$weight*edgesize
  x_d3$nodes$nodesize = nodemetric(x)*nodesize
  x_d3
}

plotnet <- function(x_d3, charge = -1000, size = 400, fontsize = 20){
  forceNetwork(Links = x_d3$links, Nodes = x_d3$nodes, charge = charge, 
               Source = 'source', Target = 'target', 
               NodeID = 'name', colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
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
              cuts = seq(min(met), max(met)*2, length.out = 8), 
              brewer.pal(9, pall)))}

########
ui <- fluidPage(theme = "bootstrap.css",
  headerPanel("Russian Drama Corpus (RusDraCor): Showcase"),
  sidebarLayout( 
  sidebarPanel(  
    #fileInput("file1", "Choose CSV edges file"),
    selectInput("file2download", "Choose a play to visualize from a list:", dow),
    wellPanel(
      sliderInput("charge", "Select charge:", min = 0, max = 12, value = 4, step = 0.05),
      selectInput("nodemetric", "Choose a metric for nodes size:", 
                  choices = list("Degree" = 'degree',
                                 "Strength" = 'strength', 
                                 "Betweeness Centrality" = 'betweenness',
                                 "Closeness Centrality" = 'closeness')),
      sliderInput("nodesize", "Nodes size:", min = 0, max = 4, value = 1, step = 0.05),
      selectInput("cluster", "Choose clusterization algorithm:", 
              choices = list('cluster_optimal','cluster_edge_betweenness', 'cluster_fast_greedy', 
                             'cluster_label_prop', 'cluster_leading_eigen', 
                             'cluster_louvain', 'cluster_spinglass', 'cluster_walktrap')),
      sliderInput("fontsize", "Select font size:", min = 0, max = 50, value = 20),
      sliderInput("edgesize", "Edges size:", min = 0, max = 4, value = 1, step = 0.05))
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Graph", forceNetworkOutput("force")),
      tabPanel("Edges", dataTableOutput(outputId = "table")),
      tabPanel("Vertices", dataTableOutput(outputId = "vertices")),
      tabPanel("Weights matrix", tableOutput(output = "matrix")),
      tabPanel("About", HTML(about))
      #tabPanel("About", includeHTML(knitr::knit2html("about.Rmd", force_v1 = T, fragment.only = T)))
    )
  )
  )
)
server <- function(input, output){

  #d <-  reactive({inFile <- input$file1
  #  if (is.null(inFile)) return(NULL)
  #  csv2d(inFile$datapath)})
  d <- reactive({inFile <- input$file2download
    if (is.null(inFile)) return(NULL)
    csv2d(inFile)})
  ig <- reactive({if (is.null(d())) return(NULL)
    d2ig(d())})
  df <- reactive({if (is.null(ig())) return(NULL)
    as_data_frame(ig(), "vertices")})
  d3 <- reactive({if (is.null(ig())) return(NULL)
    ig2d3(ig(), cluster = eval(parse(text = input$cluster)),
              nodemetric = eval(parse(text = input$nodemetric)), 
              edgesize = exp(input$edgesize)-1, nodesize = exp(input$nodesize)-1)})
  
  output$table <- DT::renderDataTable({
    datatable(d(), options = list(digits = 2, lengthMenu = c(10, 15, 20, 50)), 
              filter = 'top', rownames = F) %>%
      formatStyle(columns = c('weight'),
                  background = styleInterval(
                    cuts = round(seq(0, max(d()$weight)*2, length.out = 8)), 
                    brewer.pal(9, "GnBu")))
    })
  output$vertices <- DT::renderDataTable({
    datatable(df(), options = list(digits = 2, 
                                   lengthMenu = c(10, 15, 20, 50)), 
              filter = 'top', rownames = F) %>% 
      formatRound(columns=c('closeness'), digits=4) %>%
      formatRainbow(df()$closeness, "closeness", "BuGn") %>%
      formatRound(columns=c('graph_density'), digits=2) %>%
      formatRainbow(df()$betweenness, "betweenness", "GnBu") %>%
      formatRound(columns=c('betweenness'), digits=2) %>%
      formatRainbow(df()$strength, "strength", "PuRd") %>%
      formatRainbow(df()$degree, "degree", "Reds") %>%
      formatRainbow(df()$average_distance, "average_distance", "Purples")  
  })
  output$matrix <- renderTable({
    as.matrix(ig()[])
  }, rownames = T, bordered = T, striped = T, spacing = "xs", digits = 0, align = "c")
  
  output$force <- renderForceNetwork({
    if (is.null(d3())) return(NULL)
    #plotnet(d3(), charge = -(input$charge+1)^2.2, fontsize = input$fontsize)
    plotnet(d3(), charge = -2^(input$charge+1), fontsize = input$fontsize)
    })
}
shinyApp(ui = ui, server = server)
