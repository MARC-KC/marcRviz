# library(shiny)
# library(shinyjs)

golem::document_and_reload()


#Define the UI
ui <- fluidPage(
    
    #to allow shinyjs scripts
    shinyjs::useShinyjs(),
    
    
    #Buttons for Demo
    actionButton('btn_demoRefresh', label = 'Refresh', icon('refresh'),
                 style="margin-bottom: 100px"), 
    actionButton('btn_demoAdd', label = 'Add Record', icon('plus'),
                 style="margin-bottom: 100px"), 
    
    
    
    #Call module UI
    editDT_ui("editableDTiris")
)


#Define the Server
server <- function(input, output, session) {

    #Define main dataset somewhere in the server
    mainData <- reactiveValues(iris = iris)
    
    
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Module Specific Details (Pieces optional for the module to function and likely worked into other processes/button presses)
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #When a refresh button is pressed
    observeEvent(input[['btn_demoRefresh']], {
        print("refresh")
        
        #refresh main data (extra rows added to verify its working)
        mainData[['iris']] <- dplyr::bind_rows(iris, iris[sample(1:nrow(iris), sample(1:10, 1)),])
        # mainData[['iris']] <- dplyr::bind_rows(iris, iris[sample(1:nrow(iris), 1),])

        #Edit app values
        iris_editDT_appValues[['tableFull']] <-  editDT_prepareNewData(newData = mainData[['iris']], modifyPlaceID = 0)
        iris_editDT_appValues[['modifyPlaceID']] <- 0

    })
    
    #When a refresh button is pressed
    observeEvent(input[['btn_demoAdd']], {
        print("add")

        # Check for placement of modifyPlaceID and rebase table if needed
        temp <- editDT_rebaseModifyPoint(iris_editDT_appValues[['tableFull']], iris_editDT_appValues[['modifyPlaceID']])
        iris_editDT_appValues[['tableFull']] <- temp[['tableFull']]
        iris_editDT_appValues[['modifyPlaceID']] <- temp[['modifyPlaceID']]

        #Add to modifyID counter
        iris_editDT_appValues[['modifyPlaceID']] <- iris_editDT_appValues[['modifyPlaceID']] + 1
        print(iris_editDT_appValues[['modifyPlaceID']])
        #Prepare New Data
        newData <- iris[sample(1:nrow(iris), 1),]
        newData <- editDT_prepareNewData(newData,
                                         modifyPlaceID = iris_editDT_appValues[['modifyPlaceID']],
                                         fullData = iris_editDT_appValues[['tableFull']],
                                         newRecord = TRUE)
        #Update return table
        iris_editDT_appValues[['tableFull']] <- dplyr::bind_rows(iris_editDT_appValues[['tableFull']], newData)

    })
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    
    
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Module Specific Details (Pieces required for the module to function)
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    #Initialize appValues
    iris_editDT_appValues <- reactiveValues(
        tableFull = editDT_prepareNewData(iris, modifyPlaceID = 0),
        modifyPlaceID = 0
    )
    
    
    
    #Call the module server. It returns a reactiveValues list with the updated table data
    iris_editDT_modValues <-
        editDT_server(
            id = "editableDTiris",
            inputTableFull = reactive(iris_editDT_appValues[['tableFull']]),
            inputModifyPlaceID = reactive(iris_editDT_appValues[['modifyPlaceID']]), 
            colToDisplay = c('Sepal.Length', 'Sepal.Width', 'Species'),
            colToEdit = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'),
            colWidth = c("Sepal.Length" = "100px")
        )
    
    
    #Update the table data outside the module
    observe({
        iris_editDT_appValues[['tableFull']] <- iris_editDT_modValues[['tableFull']]
        iris_editDT_appValues[['modifyPlaceID']] <- iris_editDT_modValues[['modifyPlaceID']]
    })
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
}


#Run the example application
shinyApp(ui, server)
