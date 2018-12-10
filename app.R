

library(shiny)
library(bio3d)

#sharable env to store blast hits
share.env <- new.env()

# Define UI for application 
ui <- fluidPage(
  
  titlePanel("Shiny Blast App"),
  sidebarLayout(
    sidebarPanel(
      p("A web tool for blasting a provided PDB structure against the PDB database"),br()
    ),
    mainPanel(
      textInput("pdb_id", "PDB ID:", "4IJG"),
      actionButton("blast","blast for models")
      
    )
  ),
  
  fluidRow(
    column(12,
           dataTableOutput('table')
    )
  ),
  downloadButton("download", "Download best models"),
  downloadButton("downloadTable", "Download blast result")
)

best_hits <- NULL
blast_result <- NULL


# Define server logic required to process the input
server <- function(input, output) {
  # process button click
  observeEvent(input$blast, {
    id <- showNotification(paste("blasting against pdb ", input$pdb_id,"..."), duration = 10)
    
    ##### process pdb entry
    pdb <- read.pdb(input$pdb_id)
    #print(pdb)
    aa <- pdbseq(pdb)
    #print(aa)
    
    
    ##### blast against pdb database
    blast <- blast.pdb(aa)
    b_hits=head(blast$hit.tbl)
    cod_pdbs = b_hits$pdb.id
    
    ##### save best hits on a sharable environment
    share.env$best_hits <- b_hits
    share.env$blast_result <- blast$hit.tbl
    
    ##### send hits to table
    output$table <- renderDataTable(blast$hit.tbl)
  })
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    input$table
  })
  
  observeEvent(input$download, {
    id2 <- showNotification(paste("downloading best PDB's from blast result..."), duration = 5)
  })
  
  observeEvent(input$downloadTable, {
    id3 <- showNotification(paste("downloading blast results..."), duration = 2)
  })
  
  # Download best pdbs from blast ----
  output$download <- downloadHandler(
    filename = function() {
      temp=get('best_hits', envir=share.env)
      cod_pdbs = temp$pdb.id
      paste("best_results", "zip", sep=".")
    },
    content = function(file) {
      temp=get('best_hits', envir=share.env)
      pdb_files=get.pdb(temp$pdb.id)
      print(pdb_files)
      #print(pdb_file)
      zip(zipfile=file, files=pdb_files)
    },
    contentType = "application/zip"
  )
  
  
  
  # Download blast result on CSV format ----
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("blast_result", ".csv", sep = "")
    },
    content = function(file) {
      temp=get('blast_result', envir=share.env)
      write.csv(temp, file, row.names = TRUE)
    }
  )
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

