

library(shiny)
library(bio3d)

# Define UI for application 
ui <- fluidPage(
  
  titlePanel("PDB Blast App"),
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
  downloadButton("download", "Download best models")
  #actionButton("download","download best models")
)
best_hits <- NULL
cod_pdbs = NULL
pdb = NULL
# Define server logic required to process the input
server <- function(input, output) {
  # process button click
  observeEvent(input$blast, {
    id <- showNotification(paste("blasting against pdb ", input$pdb_id,"..."), duration = 10)
    
    #process pdb entry
    pdb <- read.pdb(input$pdb_id)
    #print(pdb)
    aa <- pdbseq(pdb)
    #print(aa)
    #blast against pdb database
    blast <- blast.pdb(aa)
    b_hits=head(blast$hit.tbl)
    cod_pdbs = b_hits$pdb.id
    assign("best_hits", b_hits, envir = .GlobalEnv)
    #print(bets_hits)
    #send hits to table
    output$table <- renderDataTable(blast$hit.tbl)
  })
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    input$table
  })
  
  observeEvent(input$download, {
    #cod_pdbs = best_hits$pdb.id
    id2 <- showNotification(paste("downloading results..."), duration = 2)
    print(best_hits)  
  })
  
  # Downloadable csv of selected dataset ----
  output$download <- downloadHandler(
    filename = function() {
      paste("blast_result", ".csv", sep = ",")
    },
    content = function(file) {
      write.csv(best_hits, file, row.names = TRUE)
    }
  )
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

