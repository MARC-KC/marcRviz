
# devtools::document()
reactlog::reactlog_enable()


mod_ui <- function(id) {
    ns <- NS(id)

    tagsOut <- tagList(
        shinyjs::useShinyjs(),

        span(
            tagList(
                actionButton(ns('btn_deleteSelected'), label = 'Delete Selected', icon('trash'), class = glue::glue('{id} inline btn_delete')),
                actionButton(ns('btn_undo'), label = 'Undo', icon('undo'), class = glue::glue('{id} inline')),
                actionButton(ns('btn_redo'), label = 'Redo', icon('redo'), class = glue::glue('{id} inline')),
                tagAppendAttributes(textOutput(ns('txt_deletedCount')), class = glue::glue('{id} inline'))
            )
        ),

        DT::dataTableOutput(ns('dt_editableTable'))
    )

}



mod_server <- function(id, inputTableFull, inputModifyPlaceID) {
    moduleServer(
        id,
        ## Below is the module function
        function(input, output, session) {


            #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            # Define Return Variables ####
            # This allows our main input reactive 'tableFull' to be updated in  module and returned back to the outer server/module
            #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            toReturn <- reactiveValues(
                tableFull = tibble::tibble(),
                modifyPlaceID = NULL
            )
            #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


            #+++++++++++++++++++++++++++++++++++++++++++++
            # Input/Output Observations ####
            #+++++++++++++++++++++++++++++++++++++++++++++
            #When the input inputTableFull is updated, update the in module variables
            observeEvent(inputTableFull(), {
                toReturn[['tableFull']] <- inputTableFull()
                toReturn[['modifyPlaceID']] <- inputModifyPlaceID()
            })
            # +++++++++++++++++++++++++++++++++++++++++++++



            #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            # Render Outputs ####
            #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            # output[['dt_editableTable']] <- DT::renderDataTable({
            #     purrr::map_dfr(1:1, ~tibble::tibble(iris))
            # })
            output[['dt_editableTable']] <- DT::renderDataTable({
                toReturn[['tableFull']]
            })
            #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


            return(toReturn)

        }
    )
}





#Define the UI
ui <- fluidPage(

    #to allow shinyjs scripts
    shinyjs::useShinyjs(),

    #Call module UI
    mod_ui("editableDTiris")
)



server <- function(input, output, session) {

    #Define main dataset somewhere in the server
    mainData <- reactiveValues(iris = iris)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Module Specific Details (Pieces required for the module to function)
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    #Initialize appValues
    iris_editDT_appValues <- reactiveValues(
        # tableFull = iris
        tableFull = purrr::map_dfr(1:500, ~tibble::tibble(iris)),
        modifyPlaceID = 0
    )



    #Call the module server. It returns a reactiveValues list with the updated table data
    iris_editDT_modValues <-
        mod_server(
            id = "editableDTiris",
            inputTableFull = reactive(iris_editDT_appValues[['tableFull']]),
            inputModifyPlaceID = reactive(iris_editDT_appValues[['modifyPlaceID']])
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


# shiny::reactlogShow()







