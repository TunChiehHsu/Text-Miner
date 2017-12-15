# load packages
library(shiny)
require(devtools)
install_version("tm", version = "0.6-2", repos = "http://cran.r-project.org")

# load packages
library(SnowballC)
library(ggplot2)
library(purrr)
library(magrittr)
library(stringr)
library(dplyr)
library(tm)
library(SnowballC)
library(knitr)
library(igraph)
library(shiny)
library(cluster)   


# clean data 
clean_text = function(val)
{
  # delete punctuations and numbers
  val = gsub("[[:punct:]]", " ", val)
  val = gsub("[[:digit:]]+", " ", val)
  # clean it up
  clean_a =  val %>%
    tolower() %>%
    str_split(" ") 
  
  delete_single = function(x){
    if(length(x) != 1){
      x = x[-which(sapply(x,nchar) == 1)]
    }
    return(x)
  }
  
  clean_a = lapply(clean_a,function(x) x[x  != "" & ! x %in% stopwords("en")])
  clean_a = lapply(clean_a,delete_single)
  clean_a = lapply(clean_a, function(x) paste(x,collapse = " "))
  clean_a = clean_a[clean_a != ""]
  
  return(clean_a)
}


# stemming 
stem = function(val){
  text = Corpus(VectorSource(val))
  text = tm_map(text, removeWords, stopwords("english"))
  text = tm_map(text,stemDocument)  
  text = tm_map(text, stripWhitespace) 
  text = tm_map(text, PlainTextDocument) 
  tm::TermDocumentMatrix(text, control = list(minWordLength = 1))
}


# compute frequency and output data frame
freq_table = function(val){
  # find frequency:
  freq = colSums(t(as.matrix(val)))
  # order   
  ord = rev(order(freq))
  
  # output df
  data_frame(word=names(freq), freq=freq) %>%
    group_by(word) %>% 
    arrange(desc(freq))
  
}


# call shiny app
shinyApp(
  ui = fluidPage(
    titlePanel("Text Miner"),
    
    hr(),
    
    sidebarPanel(
      # select a file 
      fileInput("file", label = h3("Source"), multiple = FALSE),
      
      # add a reset button 
      actionButton("reset", "Reset File"),
      
      # reset fileInput 
      tags$script('
                  Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {      
                  
                  var id = "#" + x + "_progress";
                  
                  var idBar = id + " .bar";
                  
                  $(id).css("visibility", "hidden");
                  
                  $(idBar).css("width", "0%");
                  
                  });
                  
                  '),
      
      # parameters for each plot
      conditionalPanel(condition="input.conditionedPanels==2",
                       hr(),
                       h3("Parameters"),
                       helpText("Number of Most Frequent Words"),
                       sliderInput("n1", label = "", min = 1, max = 100, 
                                   value = 20, step = 1)),
      
      conditionalPanel(condition="input.conditionedPanels==3",
                       hr(),
                       h3("Parameters"),
                       helpText("Number of Most Frequent Words"),
                       sliderInput("n2", label = "", min = 1, max = 100, 
                                   value = 50, step = 1)),
      
      conditionalPanel(condition="input.conditionedPanels == 4 ||
                       input.conditionedPanels == 5||input.conditionedPanels == 6",
                       hr(),
                       h3("Parameters"),
                       helpText("Sparsity"),
                       sliderInput("sparsity", label = "", min = 0, max = 1, 
                                   value = 0.87, step = 0.01)),
      
      conditionalPanel(condition="input.conditionedPanels==4 || input.conditionedPanels==5",
                       hr(),
                       helpText("Number of clusters"),
                       sliderInput("k", label = "", min = 1, max = 10, 
                                   value = 5, step = 1))     
      
      ),
    
    mainPanel(
      
      # show plots 
      tabsetPanel(
        tabPanel("Original Text", value = 1, verbatimTextOutput("value")),
        tabPanel("Frequency Table", dataTableOutput("table1")),
        tabPanel("Histogram", value = 2, plotOutput("plot1")), 
        tabPanel("Word Cloud", value = 3, plotOutput("plot2")), 
        tabPanel("Hierarchical Clustering", value = 4, plotOutput("plot3")),
        tabPanel("KMeans", value = 5, plotOutput("plot4")),
        tabPanel("Network", value = 6, plotOutput("plot5")),
        id = "conditionedPanels"
      )
    )
  ),
  
  server = function(input, output, session) {
    file = reactive(input$file)
    
    data = reactive({
      if (is.null(file())){
        NULL
      } else {
        readLines(file()$datapath) %>% as.list()
      }
    })
    output$value = renderPrint(
      data()
    )
    
    data = reactive({readLines(file()$datapath) %>% as.list})
    
    myDtm = reactive({
      clean_data = clean_text(data())
      stem(clean_data)
    })
    
    df = reactive({
      freq_table(myDtm())
    })
    
    output$table1 = renderDataTable(
      df()
    )
    
    output$plot1 = renderPlot({
      hist_data = df()[1:input$n1,]
      ggplot(hist_data, aes(reorder(word, -freq), freq))+
        geom_bar(stat="identity") +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=45, hjust=1)) +
        ylab("Frequency") + xlab("")
    })
    
    output$plot2 = renderPlot({
      wordcloud(df()$word, df()$freq, 
                max.words = input$n2, scale = c(4,0.5),
                colors = brewer.pal(8, "Dark2") )
    })
    
    dtmss = reactive({
      removeSparseTerms(myDtm(), input$sparsity)       
    })
    
    d = reactive(dist((dtmss()), method = "euclidian"))
    
    fit = reactive(hclust(d = d(), method = "ward.D"))
    
    output$plot3 = renderPlot({
      # remove sparse terms
      plot(fit(), hang=-1, xlab = "", sub ="")
      rect.hclust(fit(), input$k, border="red") # draw dendogram with red borders around the 5 clusters   
      groups = cutree(fit(), input$k)   # "k=" defines the number of clusters you are using 
    })
    
    output$plot4 = renderPlot({
      kfit = kmeans(d(), input$k)   
      clusplot(as.matrix(d()), kfit$cluster, main = "",
               color=T, shade=T, labels=2, lines=0)
    })
    
    output$plot5  = renderPlot({
      termDocMatrix = as.matrix(dtmss())
      termDocMatrix[termDocMatrix>=1] = 1
      termMatrix = termDocMatrix %*% t(termDocMatrix)
      g = graph.adjacency(termMatrix, weighted=T, mode = "undirected")
      V(g)$label = V(g)$name
      V(g)$degree = degree(g)
      # remove loops
      g = simplify(g)
      V(g)$label.cex = log(rank(V(g)$degree)) + 1
      V(g)$label.color = rgb(0, 0, .2, .8)
      V(g)$frame.color = NA
      egam = (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
      E(g)$color = rgb(.5, .5, 0, egam)
      E(g)$width = egam*2
      plot(g)
    })
    
    observe({
      input$reset
      session$sendCustomMessage(type = "resetFileInputHandler", "file")   
    })
  }
)

