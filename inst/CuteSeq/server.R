
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(genbankr)
library(dplyr)

shinyServer(function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })

  gbFile <- reactive({
    req(input$gbFile)
    readGenBank(input$gbFile$datapath)
  })

  output$seqNameSelectUI <- renderUI({
    req(gbFile())
    selectInput("seqNameSelect",
                "Select Sequence",
                names(gbFile()@sequence))
  })

  gbSequence <- reactive({
    req(input$seqNameSelect)
    gbFile()@sequence[[input$seqNameSelect]]
  })

  gbFeatures <- reactive({
    req(input$seqNameSelect)
    gbFile()@other_features %>>%
      as.data.frame() %>>%
      filter(seqnames == input$seqNameSelect)
    # ,
    #          (start < first(flatMap$seqI) &&
    #             end < first(flatMap$seqI)) ||
    #            (start < first(flatMap$seqI) &&
    #               end < first(flatMap$seqI)))
  })

  output$colorByUI <- renderUI({
    req(gbFeatures())
    selectInput("colorBy",
                "Color by",
                colnames(gbFeatures()))
  })

  output$labelByUI <- renderUI({
    req(gbFeatures())
    selectInput("labelBy",
                "Label by",
                colnames(gbFeatures()))
  })

  output$limitSeqSliderUI <- renderUI({
    req(gbSequence())
    sliderInput("limitSeqSlider",
                "Limit Sequence",
                min = 1 + gbSequence()@offset,
                max = gbSequence()@length + gbSequence()@offset,
                value = c(1 + gbSequence()@offset,
                          gbSequence()@length + gbSequence()@offset),
                step = 1)
  })

  output$cuteSeqHtml <- renderUI({
    input$processCuteSeq
    isolate({
      req(gbSequence(), gbFeatures())
      HTML(
        paste0(
          "<div style='font-family: monospace;'>",
          cuteSeq(gbSequence(), gbFeatures(),
                  colorBy = input$colorBy,
                  labelBy = input$labelBy,
                  considerStrand = input$considerStrand,
                  geneious = input$geneious,
                  linesWidth = input$linesWidth),
          "</div>"
        ))
    })
  })

})
