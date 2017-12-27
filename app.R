
library(shiny)
library(DT)
library(data.table)
library(networkD3)
library(igraph)
library(RColorBrewer)
library(jsonlite)
library(curl)


about <- '<h1>About RusDraCor</h1>
  <p>Our project <b>Russian Drama Corpus (RusDraCor)</b> aims at presenting a
collection of Russian plays from the 1740s to the 1930s. The entire
corpus is encoded in <a href = "https://en.wikipedia.org/wiki/Text_Encoding_Initiative">TEI</a>, an XML vocabulary, which makes it easy to
extract structural information.</p>  

<p>RusDraCor comprises <i>67 plays</i> at present (early July, 2017), but will
be growing throughout the year. Featured authors so far are Blok,
Bulgakov, Chechov, Fonvizin, Gogol, Gorkij, Gumilyov, Krylov,
Majakovskij, Ostrovskij, Plavilschikov, Prutkov ☺, Pushkin, Sumarokov, Leo Tolstoy
and Turgenev.</p>  

<p>Our main purpose is the social network analysis of these dramatic texts:
we extract social situations by collecting information on who is talking
along with others in a given segment of a play (acts, scenes, other
constellations). Our extraction scripts generate CSV files, which we can
analyse with a variety of tools. This website gives you the opportunity
to explore the social structure of individual plays.</p>

<p>The analytical part for this page was done by Ivan Pozdniakov in R and
is presented by help of the web application framework Shiny. Ultimately,
we are interested in structural changes over time, the evolution of
Russian drama, so to speak, for which we will provide other tools.</p>  

<h2>Links</h2>

<p><a href = "https://hum.hse.ru/digital/rusdracor/">Website of our research group</a></p>
<p><a href = "https://github.com/lehkost/RusDraCor">GitHub repository</a></p>

<h2>Group members (in alphabetical order)</h2>  
Veronika Faynberg · Frank Fischer · Matvey Kolbasov ·  Svetlana Laschuk
· Kirill Mazur · Tatyana Orlova · German Palchikov · Irina Pavlova ·
Ivan Pozdniakov · Evgeniya Shlosman · Danil Skorinkin · Nataliya Tyshkevich  

<h2>Acknowledgements</h2>
This page was prepared within the framework of the Academic Fund Program
at the National Research University Higher School of Economics (HSE) in
2017–2018 (grant No 17-05-0054) and by the Russian Academic Excellence
Project "5-100".  ' 

url <- "https://dracor.org/api/corpus/rus"
urlshort <- "https://dracor.org/api/corpus/"

downloadbase <- function(url){
base <- fromJSON(url, flatten = T)
base <- base$dramas
base$titlename <- paste(base$author.name, "-", base$title)
dow <- base$networkdataCsvUrl
names(dow) <- base$titlename
dow
}
#dow <- downloadbase(url)

options(shiny.sanitize.errors = F)

csv2d <- function(file){
  d <- fread(file, encoding = "UTF-8")
  colnames(d) <- tolower(colnames(d))
  d <- d[,.(source, target, weight)]
  d <- d[weight>0,]
  d}

d2ig <- function(d){
  x <- graph_from_data_frame(d, directed = F)
  V(x)$betweenness <- betweenness(x, v = V(x), directed = F, weights = NA)
  V(x)$closeness <- closeness(x, weights = NA, normalized = T)
  V(x)$strength <- strength(x)
  V(x)$degree <- degree(x)
  V(x)$average_distance <- 1/closeness(x, weights = NA, normalized = T)
  x
}

d2istats <- function(d){
  x <- graph_from_data_frame(d, directed = F)
  df <- data.frame(Variable = c("Mean Distance", "Graph Density", "Clustering", "Diameter"),
                  Value = c(mean_distance(x, directed = FALSE),
                       edge_density(x), transitivity(x), diameter(x))
                  )
  df
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
    splitLayout(
      cellWidths = c("30%", "70%"),
      radioButtons("corpus", "Drama Corpus", choices = list(Russian = "rus",
                                                            German = "ger")),
#fileInput("file1", "Choose CSV edges file"),
      uiOutput("base"),
      tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              ")))),
#      selectInput("file2download", "Choose a play to visualize from a list:", downloadbase(url))),
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
      tabPanel("Play Info", tableOutput(output = "info")),
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
  output$base <- renderUI({
    selectInput("file2download",
                "Choose a play to visualize from a list:",
                downloadbase(
                  paste0(
                    urlshort, input$corpus)
                  )
                )
  })
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

  output$info <- renderTable({
    d() %>% d2istats()},
    rownames = F, bordered = F, striped = F, digits = 2
  )

}
shinyApp(ui = ui, server = server)
