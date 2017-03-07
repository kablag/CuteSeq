
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("CuteSeq"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("gbFile", "Upload GenBank File"),
      uiOutput("seqNameSelectUI"),
      uiOutput("limitSeqSliderUI"),
      uiOutput("colorByUI"),
      uiOutput("labelByUI"),
      checkboxInput("considerStrand", "Consider Strand", TRUE),
      checkboxInput("geneious", "Parse Geneious Types", TRUE),
      numericInput("linesWidth", "Lines Width", 60, min = 0, step = 1),
      actionButton("processCuteSeq", "Process")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput("cuteSeqHtml")
    )
  )
))
