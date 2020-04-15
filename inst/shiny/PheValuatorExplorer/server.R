library(shiny)
library(shinydashboard)
library(DT)

truncateStringDef <- function(columns, maxChars) {
  list(
    targets = columns,
    render = JS(sprintf("function(data, type, row, meta) {\n
      return type === 'display' && data != null && data.length > %s ?\n
        '<span title=\"' + data + '\">' + data.substr(0, %s) + '...</span>' : data;\n
     }", maxChars, maxChars))
  )
}

minCellCountDef <- function(columns) {
  list(
    targets = columns,
    render = JS("function(data, type) {
    if (type !== 'display' || isNaN(parseFloat(data))) return data;
    if (data >= 0) return data.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
    return '<' + Math.abs(data).toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
  }")
  )
}

minCellPercentDef <- function(columns, showSign = TRUE) {
  if (showSign) {
    list(
      targets = columns,
      render = JS("function(data, type) {
    if (type !== 'display' || isNaN(parseFloat(data))) return data;
    if (data >= 0) return (100 * data).toFixed(1).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,') + '%';
    return '<' + Math.abs(100 * data).toFixed(1).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,') + '%';
  }")
    ) 
  } else {
    list(
      targets = columns,
      render = JS("function(data, type) {
    if (type !== 'display' || isNaN(parseFloat(data))) return data;
    if (data >= 0) return (100 * data).toFixed(1).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
    return '<' + Math.abs(100 * data).toFixed(1).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
  }")
    ) 
  }
}

minCellRealDef <- function(columns, digits = 1) {
  list(
    targets = columns,
    render = JS(sprintf("function(data, type) {
    if (type !== 'display' || isNaN(parseFloat(data))) return data;
    if (data >= 0) return data.toFixed(%s).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
    return '<' + Math.abs(data).toFixed(%s).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
  }", digits, digits))
  )
}

styleAbsColorBar <- function(maxValue, colorPositive, colorNegative, angle = 90) {
  JS(sprintf("isNaN(parseFloat(value))? '' : 'linear-gradient(%fdeg, transparent ' + (%f - Math.abs(value))/%f * 100 + '%%, ' + (value > 0 ? '%s ' : '%s ') + (%f - Math.abs(value))/%f * 100 + '%%)'", 
             angle, maxValue, maxValue, colorPositive, colorNegative, maxValue, maxValue))
}

shinyServer(function(input, output, session) {
  
  subsetToView <- reactive({
    data <- phevaluatorResult[phevaluatorResult$databaseId %in% input$databases, ]
    data <- merge(data, phevaluatorAnalysis)
    return(data)
  })
  
  output$pheValuatorResultsTable <- renderDataTable({
    data <- subsetToView()
    if (nrow(data) == 0) {
      return(NULL)
    }
    data$sensCi <- sprintf("(%0.1f - %0.1f)", 100*abs(data$sensCi95Lb), 100*abs(data$sensCi95Ub))
    data$ppvCi <- sprintf("(%0.1f - %0.1f)", 100*abs(data$ppvCi95Lb), 100*abs(data$ppvCi95Ub))
    data$specCi <- sprintf("(%0.1f - %0.1f)", 100*abs(data$specCi95Lb), 100*abs(data$specCi95Ub))
    data$npvCi <- sprintf("(%0.1f - %0.1f)", 100*abs(data$npvCi95Lb), 100*abs(data$npvCi95Ub))
    data$cohortName <- cohort$cohortFullName[match(data$phenotypeCohortId, cohort$cohortId)]
    table <- data[, c("databaseId", "description", "cohortName", "cutPoint","sens", "sensCi", "ppv", "ppvCi", "spec", "specCi", "npv", "npvCi", "f1Score")]
    options = list(pageLength = 15,
                   searching = TRUE,
                   lengthChange = TRUE,
                   ordering = TRUE,
                   paging = TRUE,
                   info = TRUE,
                   columnDefs = list(truncateStringDef(0, 10),
                                     truncateStringDef(1:2, 50),
                                     minCellPercentDef(c(4, 6, 8, 10), showSign = FALSE),
                                     minCellRealDef(12, 3)))
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th(rowspan = 2, "Database"),
          th(rowspan = 2, "Analysis"),
          th(rowspan = 2, "Evaluated cohort"),
          th(rowspan = 2, "Cutpoint"),
          th(colspan = 2, "Sensitivity", class = "dt-center"),
          th(colspan = 2, "PPV", class = "dt-center"),
          th(colspan = 2, "Specificity", class = "dt-center"),
          th(colspan = 2, "NPV", class = "dt-center"),
          th(rowspan = 2, "F1 Score")
        ),
        tr(
          lapply(rep(c("%", "95% CI"), 4), th)
        )
      )
    ))
    selection = list(mode = "single", target = "row")
    dataTable <- datatable(table,
                           options = options,
                           rownames = FALSE,
                           escape = FALSE,
                           container = sketch,
                           selection = selection,
                           class = "stripe nowrap compact")
    
    dataTable <- formatStyle(table = dataTable,
                             columns = c(5, 7, 9, 11, 13),
                             background = styleColorBar(c(0, 1), "lightblue"),
                             backgroundSize = "98% 88%",
                             backgroundRepeat = "no-repeat",
                             backgroundPosition = "center")
    return(dataTable)
  })
  
  selectedRow <- reactive({
    idx <- input$pheValuatorResultsTable_rows_selected
    if (is.null(idx)) {
      return(NULL)
    } else {
      row <- subsetToView()[idx, ]
      return(row)
    }
  })
  
  output$pheValuatorDetailsUi <- renderUI({
    row <- selectedRow()
    if (is.null(row)) {
      return(HTML("<p>Select an analysis to see details</p>"))
    } else {
      formatInteger <- function(x) {
        string <- formatC(x, big.mark = ",", format = "d")
        string <- gsub("^-", "<", string)
        return(string)
      }
      formatPercent <- function(x) {
        string <- sprintf("%0.1f%%", 100*x)
        string <- gsub("^-", "<", string)
        return(string)
      }
      getCohortName <- function(cohortId) {
        return(sprintf("<strong>%s</strong> (cohort ID %s)", cohort$cohortFullName[cohort$cohortId == cohortId], cohortId))
      }
      
      lines <- list("<table>",
                    sprintf("<tr><td>Database</td><td>&nbsp;&nbsp;</td><td><strong>%s</strong></td></tr>", row$databaseId),
                    sprintf("<tr><td>Analysis</td><td>&nbsp;&nbsp;</td><td><strong>%s</strong></td></tr>", row$description),
                    sprintf("<tr><td>Evaluated cohort</td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", getCohortName(row$phenotypeCohortId)),
                    sprintf("<tr><td>Cut point</td><td>&nbsp;&nbsp;</td><td><strong>%s</strong></td></tr>", row$cutPoint),
                    sprintf("<tr><td>xSpec cohort</td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", getCohortName(row$xSpecCohortId)),
                    sprintf("<tr><td>xSens cohort</td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", getCohortName(row$xSensCohortId)),
                    sprintf("<tr><td>Estimated prevalence</td><td>&nbsp;&nbsp;</td><td><strong>%s</strong></td></tr>", formatPercent(row$estimatedPrevalence)),
                    "</table>",
                    "<br/>",
                    "<table>",
                    sprintf("<tr><td>True positives</td><td>&nbsp;&nbsp;</td><td align = 'right'><strong>%s</strong></td>", formatInteger(row$truePositives)),
                    "<td>&nbsp;&nbsp;&nbsp;</td>",
                    sprintf("<td>False negatives</td><td>&nbsp;&nbsp;</td><td align = 'right'><strong>%s</strong></td></tr>", formatInteger(row$falseNegatives)),
                    sprintf("<tr><td>False positives</td><td>&nbsp;&nbsp;</td><td align = 'right'><strong>%s</strong></td>", formatInteger(row$falsePositives)),
                    "<td>&nbsp;&nbsp;&nbsp;</td>",
                    sprintf("<td>True negatives</td><td>&nbsp;&nbsp;</td><td align = 'right'><strong>%s</strong></td></tr>", formatInteger(row$trueNegatives)),
                    "</table>")
      
      
      
      return(HTML(paste(lines, collapse = "\n")))
      
    }
  })
  
  output$databaseInformationTable <- renderDataTable({
    
    table <- database[, c("databaseId", "databaseName", "description")]
    options = list(pageLength = 25,
                   searching = TRUE,
                   lengthChange = FALSE,
                   ordering = TRUE,
                   paging = FALSE,
                   columnDefs = list(list(width = '30%', targets = 1),
                                     list(width = '60%', targets = 2))
    )
    table <- datatable(table,
                       options = options,
                       colnames = c("ID", "Name", "Description"),
                       rownames = FALSE,
                       class = "stripe compact")
    return(table)
  })
  
  showInfoBox <- function(title, htmlFileName) {
    showModal(modalDialog(
      title = title,
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(readChar(htmlFileName, file.info(htmlFileName)$size) )
    ))
  }
  
  observeEvent(input$pheValuatorResultsInfo, {
    showInfoBox("PheValuator Results", "html/phevaluatorResults.html")
  })
  
})
