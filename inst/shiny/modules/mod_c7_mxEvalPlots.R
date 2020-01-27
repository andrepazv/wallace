
mxEvalPlots_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns('mxEvalSel'), label = "Select evaluation statistic",
                choices = list("Select Stat..." = '',
                               "average AUC test" = 'auc.test',
                               "average AUC diff" = 'auc.diff',
                               "average OR mtp" = 'or.mtp',
                               "average OR 10%" = 'or.10p',
                               "delta AICc" = 'delta.AICc'), 
                selected = 'auc.test'),
    HTML('<hr>'),
    strong("Download Maxent evaluation plot (.png)"), br(), br(),
    downloadButton(ns('dlMxEvalPlot'), "Download")
  )
}

mxEvalPlots_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$mods)) {
      rvs %>% writeLog(type = 'error', "Models must first be run in component 6.")
      return()
    }
    if (is.null(input$mxEvalSel)) {
      rvs %>% writeLog(type = 'error', "No statistic selected for plotting.")
      return()
    }
    
    # record for RMD
    rvs$mxEvalSel <- input$mxEvalSel
    rvs$comp7 <- isolate(c(rvs$comp7, 'mxEval'))

    # handle downloads for Maxent Evaluation Plots png
    output$dlMxEvalPlot <- downloadHandler(
      filename = function() {paste0(spName(), "_",rvs$mxEvalSel,"_EvalPlot.png")},
      content = function(file) {
        png(file)
        evalPlot(rvs$e, input$mxEvalSel)
        dev.off()
      }
    )
    evalPlot(rvs$e, input$mxEvalSel)
  })
}
