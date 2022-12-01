library(shiny)
library(shinydashboard)
library(DT)

addInfo <- function(item, infoId) {
  infoTag <- tags$small(class = "badge pull-right action-button",
                        style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
                        type = "button", 
                        id = infoId,
                        "i")
  item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag))
  return(item)
}

dashboardPage(
  dashboardHeader(title = "PheValuator Results"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                addInfo(menuItem("PheValuator Results", tabName = "pheValuatorResults"), "pheValuatorResultsInfo"),
                menuItem("Database information", tabName = "databaseInformation"), 
                conditionalPanel(condition = "input.tabs=='pheValuatorResults'",
                                 checkboxGroupInput("databases", "Database", database$databaseId, selected = database$databaseId[1])
                )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "pheValuatorResults",
              dataTableOutput("pheValuatorResultsTable"),
              uiOutput("pheValuatorDetailsUi")
      ),
      tabItem(tabName = "databaseInformation",
              dataTableOutput("databaseInformationTable")
      )
    )
  )
)
