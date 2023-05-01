library(shiny)
library(shinythemes)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggbiplot)
library(gridExtra)
library(GGally)

data(iris)
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("曾祐展的 PCA 作業"),
  sidebarLayout(
    sidebarPanel(
      h4("PCA"),
      selectInput("xaxis_pca", "X axis", choices = c("PCA1", "PCA2", "PCA3", "PCA4"),
                  selected = "PCA1"),
      selectInput("yaxis_pca", "Y axis", choices = c("PCA1", "PCA2", "PCA3", "PCA4"),
                  selected = "PCA2"),
      br(),
      h4("CA"),
      sliderInput("ca", "Number of contribs", min = 1, max = 150, value = 150),
      br(),
      h4("Features"),
      sliderInput("kmeans_centers", "K-Means Centers", min = 1, max = 10, value = 3),
      selectInput("xaxis_Features", "X axis", choices = colnames(log.ir),
                  selected = "Sepal.Length"),
      selectInput("yaxis_Features", "Y axis", choices = colnames(log.ir),
                  selected = "Petal.Length"),
      br(),
      h4("Pairs"),
      sliderInput("Pairs", "Number of pairs", min = 1, max = 150, value = 150),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("PCA",
                 plotOutput("pca_plot", height = 500, width = "100%")
        ),
        tabPanel("CA",
                 plotOutput("ca_plot", height = 500, width = "100%")
        ),
        tabPanel("Data",
                 dataTableOutput("iris_table")
        ),
        tabPanel("Data (log)",
                 dataTableOutput("iris_table_log")
        ),
        tabPanel("Features",
                 plotOutput("Features", height = 500, width = "100%")
        ),
        tabPanel("Pairs",
                 plotOutput("Pairs", height = 500, width = "100%")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$pca_plot <- renderPlot({
    xcol <- match(input$xaxis_pca, c("PCA1", "PCA2", "PCA3", "PCA4"))
    ycol <- match(input$yaxis_pca, c("PCA1", "PCA2", "PCA3", "PCA4"))
    ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
    g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species, choices = c(xcol, ycol))
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    print(g)
  })
  
  output$ca_plot <- renderPlot({
    ca <- CA(iris[,1:4], graph = FALSE)
    fviz_ca_biplot(ca, select.row = list(contrib = input$ca))
    
  })
  
  output$iris_table <- renderDataTable({
    iris
  })
  
  output$iris_table_log <- renderDataTable({
    logd <- data.frame(log(iris[, 1:4]), iris[, 5])
    colnames(logd)[5] <- "Species"
    logd
  })
  
  output$Features <- renderPlot({
    xcol <- input$xaxis_Features
    ycol <- input$yaxis_Features
    centers <- input$kmeans_centers
    km <- kmeans(log.ir, centers)
    iris$cluster <- as.factor(km$cluster)
    g <- ggplot(data = iris, aes_string(x = xcol, y = ycol, color = "cluster")) +
      geom_point() +
      scale_color_discrete(name = 'Cluster') +
      labs(x = xcol, y = ycol) +
      theme(legend.direction = 'horizontal', legend.position = 'top')
    print(g)
  })
  
  output$Pairs <- renderPlot({
    ggpairs(iris[1:input$Pairs, ], aes(colour = Species, alpha = 0.4), columns = 1:4, upper=NULL)
  })
  
}

shinyApp(ui, server)