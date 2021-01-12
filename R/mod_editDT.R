#' @title Editable DataTable {shiny} Module
#'
#' @description A shiny Module to create an editable DataTable. It adds buttons
#'   at the top of the table for deleting selected rows, undoing, and redoing 
#'   modifications to the table including deletes, additions, and edits,
#'   deletions. Additionally it adds a new row to the front of the dataset that
#'   holds a row specific delete and edit buttons.
#'
#' @param id id for module. The id should match in both the
#'   editDT_ui and editDT_server function calls.
#'   
#' @return editDT_server retruns a reactiveValues list containing
#'   the modified input table named 'tableFull' and the modifyID named 'modifyPlaceID'.
#'   These returns are used to allow the main inputs to be adapted and changed 
#'   inside and outside the module.
#'
#' @section Creation notes: This module was first created 2020-12-11 when 
#'   designing the CovidDataEntry Shiny application.
#'
#'
#' @examples 
#' \dontrun{
#' 
#' library(shiny)
#' library(shinyjs)
#' 
#' 
#' #Define the UI
#' ui <- fluidPage(
#'   
#'   #to allow shinyjs scripts
#'   shinyjs::useShinyjs(),
#'   
#'   
#'   #Buttons for Demo
#'   actionButton('btn_demoRefresh', label = 'Refresh', icon('refresh'),
#'                style="margin-bottom: 100px"), 
#'   actionButton('btn_demoAdd', label = 'Add Record', icon('plus'),
#'                style="margin-bottom: 100px"), 
#'   
#'   
#'   
#'   #Call module UI
#'   editDT_ui("editableDTiris")
#' )
#' 
#' 
#' #Define the Server
#' server <- function(input, output, session) {
#'   
#'   #Define main dataset somewhere in the server
#'   mainData <- reactiveValues(iris = iris)
#'   
#'   
#'   #When a refresh button is pressed
#'   observeEvent(input[['btn_demoRefresh']], {
#'     print("refresh")
#'     
#'     #refresh main data (extra rows added to verify its working)
#'     mainData[['iris']] <- dplyr::bind_rows(iris, iris[sample(1:nrow(iris), sample(1:10, 1)),])
#'     # mainData[['iris']] <- dplyr::bind_rows(iris, iris[sample(1:nrow(iris), 1),])
#'     
#'     #Edit app values
#'     iris_editDT_appValues[['tableFull']] <-  editDT_prepareNewData(newData = mainData[['iris']], modifyPlaceID = 0)
#'     iris_editDT_appValues[['modifyPlaceID']] <- 0
#'     
#'   })
#'   
#'   #When a refresh button is pressed
#'   observeEvent(input[['btn_demoAdd']], {
#'     print("add")
#'     
#'     # Check for placement of modifyPlaceID and rebase table if needed
#'     temp <- editDT_rebaseModifyPoint(iris_editDT_appValues[['tableFull']], iris_editDT_appValues[['modifyPlaceID']])
#'     iris_editDT_appValues[['tableFull']] <- temp[['tableFull']]
#'     iris_editDT_appValues[['modifyPlaceID']] <- temp[['modifyPlaceID']]
#'     
#'     #Add to modifyID counter
#'     iris_editDT_appValues[['modifyPlaceID']] <- iris_editDT_appValues[['modifyPlaceID']] + 1
#'     print(iris_editDT_appValues[['modifyPlaceID']])
#'     #Prepare New Data
#'     newData <- iris[sample(1:nrow(iris), 1),]
#'     newData <- editDT_prepareNewData(newData,
#'                                                     modifyPlaceID = iris_editDT_appValues[['modifyPlaceID']],
#'                                                     fullData = iris_editDT_appValues[['tableFull']],
#'                                                     newRecord = TRUE)
#'     #Update return table
#'     iris_editDT_appValues[['tableFull']] <- dplyr::bind_rows(iris_editDT_appValues[['tableFull']], newData)
#'     
#'   })
#'   
#'   
#'   #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'   # Module Specific Details
#'   #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'   
#'   #Initialize appValues
#'   iris_editDT_appValues <- reactiveValues(
#'     tableFull = editDT_prepareNewData(iris, modifyPlaceID = 0),
#'     modifyPlaceID = 0
#'   )
#'   
#'   
#'   
#'   #Call the module server. It returns a reactiveValues list with the updated table data
#'   iris_editDT_modValues <-
#'     editDT_server(
#'       id = "editableDTiris",
#'       inputTableFull = reactive(iris_editDT_appValues[['tableFull']]),
#'       inputModifyPlaceID = reactive(iris_editDT_appValues[['modifyPlaceID']]),
#'       colToEdit = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width')
#'     )
#'   
#'   
#'   #Update the table data outside the module
#'   observeEvent(iris_editDT_modValues[['tableFull']], {
#'     iris_editDT_appValues[['tableFull']] <- iris_editDT_modValues[['tableFull']]
#'     iris_editDT_appValues[['modifyPlaceID']] <- iris_editDT_modValues[['modifyPlaceID']]
#'   })
#'   #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'   
#' }
#' 
#' 
#' #Run the example application
#' shinyApp(ui, server)
#' 
#' }
#' @rdname editDT
#' @export
#'
#' @importFrom shiny NS tagList

# editableDataTable UI Function
editDT_ui <- function(id){
  ns <- NS(id)
  
  
  
  #To be Ran on initialization
  jsInit <- glue::glue("
//initialize isInEditing
$( document ).on('shiny:sessioninitialized', function(event) {
  Shiny.onInputChange('%{%id%}%-isInEditing', false);           
});
", .open = "%{%", .close = "%}%")
  
  #General javascript pieces for use in module
  jsCustom <- glue::glue("
//are any disabled?
function anyDisabled(jQobject) {
    //search each jQobject index and look for existance of the disabled property
    var obj = jQobject.map(function() { return($(this).prop('disabled'));});
    //make the map return an array (needed for some function)
    var arr = $.makeArray(obj);
    //are any true?
    return arr.some(function(e) {return e == true});
}
//example:  anyDisabled($('div#editableDTiris-dt_editableTable button'));
//initailize isInEditing
//Shiny.onInputChange('%{%id%}%-isInEditing', true);
", .open = "%{%", .close = "%}%")
  
  
  
  tagsOut <- tagList(
    shinyjs::useShinyjs(), 
    div(tags$head(tags$script(HTML(jsInit)))),
    div(tags$head(tags$script(HTML(jsCustom)))),
    shinyjs::inlineCSS(glue::glue("
            .%{%id%}%.btn_deleteRow, .%{%id%}%.btn_delete {transition-duration: 0.4s;}
            .%{%id%}%.btn_deleteRow:hover, .%{%id%}%.btn_delete:hover {background-color: red; color: white;}
            .%{%id%}%.btn_deleteRow:hover:disabled, .%{%id%}%.btn_delete:hover:disabled {background-color: white; color: black;}
            .%{%id%}%.btn_editRow {transition-duration: 0.4s;}
            .%{%id%}%.btn_editRow:hover {background-color: green; color: white;}
            .%{%id%}%.btn_editRow:hover:disabled {background-color: white; color: black;}
            .%{%id%}%.btn_editRow_clicked {background-color: green; color: white;}
            .%{%id%}%.btn_editRow_clicked {transition-duration: 0.4s;}
            .%{%id%}%.btn_editRow_clicked:hover {background-color: white; color: black;}
            .%{%id%}%.inline {display: inline-block;}
            ", .open = "%{%", .close = "%}%")),
    span(
      tagList(
        actionButton(ns('btn_deleteSelected'), label = 'Delete Selected', icon('trash'), class = glue::glue('{id} inline btn_delete')), 
        actionButton(ns('btn_undo'), label = 'Undo', icon('undo'), class = glue::glue('{id} inline')), 
        actionButton(ns('btn_redo'), label = 'Redo', icon('redo'), class = glue::glue('{id} inline')), 
        tagAppendAttributes(textOutput(ns('txt_deletedCount')), class = glue::glue('{id} inline'))
      )
    ),
    
    DT::dataTableOutput(ns('dt_editableTable')) #class easier to add in server fxn
  )
  
  
  return(tagsOut)
}

#' editableDataTable Server Function
#' @param input,output,session Internal parameters for {shiny}. These don't have
#'   to be specified.
#' @param inputTableFull Main input table.Must be initialized with
#'   \code{editDT_prepareNewData()}. Must be passed in a
#'   \code{reactive()} statement
#' @param inputModifyPlaceID Modify ID. Initialized at 0. Must be passed in a
#'   \code{reactive()} statement
#' @param colToDisplay Character vector of column names to display in the main
#'   table.
#' @param colToEdit Character vector of column names that will be editable when
#'   an edit button is pressed.
#' @param colWidth Named numeric vector for the width of displayed columns.
#' @param colSort Named logical vector for whether sorting is enabled on a
#'   columns. (Currently Unimplemented)
#' @param indexStart Whether the index should start at 0 or 1. If the table 
#'   contains all new data (i.e. all from a form entry in a current session)
#'   set the index to 1, else set it to 0 (the default)
#' @rdname editDT
#' @export
editDT_server <- function(id, inputTableFull, inputModifyPlaceID, colToDisplay=NULL, colToEdit=NULL, colWidth=NULL, colSort=NULL, indexStart = 0) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session){
      # ns <- session$ns
      
      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      # Define Return Variables ####
      # This allows our main input reactive 'tableFull' to be updated in  module and returned back to the outer server/module
      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      toReturn <- reactiveValues(
        tableFull = tibble::tibble(),
        modifyPlaceID = NULL
      )
      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      
      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      # Define Module Variables ####
      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      #To Hold Deleted Data
      deletedTable <- reactiveVal(tibble::tibble())
      
      #To Hold Displayed Data
      displayedTable <- reactiveVal(tibble::tibble())
      
      #To Hold count of number of rows that have been deleted
      deletedTableNumRows <- reactiveVal(0)
      
      #TRUE/FALSE on if any rows are selected
      anyRowsSelected <- reactive({!is.null(input[['dt_editableTable_rows_selected']])})
      
      
      #TRUE/FALSE on if any deleteSelected button should be available
      deleteSelectedEnabled <- reactive({
        # jsFns$isInEditing()
        if (is.null(input[['isInEditing']])) {
          FALSE
        } else {
          anyRowsSelected() & !input[['isInEditing']]
        }
      })
      
      #TRUE/FALSE on if  undo button should be available
      undoEnabled <- reactive({
        if (is.null(input[['isInEditing']])) {
          FALSE
        } else {
          # print(paste0("Is Editing?: ", input[['isInEditing']]))
          !is.null(toReturn[['tableFull']]) & toReturn[['modifyPlaceID']] != 0 & !input[['isInEditing']]
        }
      })
      
      #TRUE/FALSE on if redo button should be available
      redoEnabled <- reactive({
        if (is.null(input[['isInEditing']]) | length(toReturn[['tableFull']][['modifyID']]) == 0) {
          FALSE
        } else {
          !is.null(toReturn[['tableFull']]) & max(toReturn[['tableFull']][['modifyID']]) > toReturn[['modifyPlaceID']] & !input[['isInEditing']]
        }
      })
      
      
      
      displayTableNames <- reactiveVal('')
      editIDs <- reactiveVal('')
      colWidths <- reactiveValues(IDs = c('0,1'), Widths = c("c('70px', '60px')"))
      
      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      
      
      
      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      # Simple Input Error Checks ####
      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      inputTableFullNames <- isolate(names(inputTableFull()))
      
      
      
      
      
      if (!is.null(colToDisplay)) {
        
        #Make sure all colToDisplay are in names(inputTableFull)
        if (!all(colToDisplay %in% inputTableFullNames)) {
          warning("Subsetting 'colToDisplay' argument to align with columns in 'inputTableFull'")
          colToDisplay <- colToDisplay[colToDisplay %in% inputTableFullNames]
          if (length(colToDisplay) == 0) {
            stop("No columns specified in 'colToDisplay' are in 'inputTableFull'")
          }
        }
        
        #Make sure all colToEdit are in colToDisplay
        if (!is.null(colToEdit)) {
          if (!all(colToEdit %in% colToDisplay)) {
            warning("Subsetting 'colToEdit' argument to align with 'colToDisplay'")
            colToEdit <- colToEdit[colToEdit %in% colToDisplay]
            if (length(colToEdit) == 0) {
              stop("No columns specified in 'colToEdit' are in 'colToDisplay'")
            }
          }
        }
        
        #Make sure all names(colWidth) are in colToDisplay
        if (!is.null(colWidth)) {
          if (!all(names(colWidth) %in% colToDisplay)) {
            warning("Subsetting 'colToEdit' argument to align with 'colToDisplay'")
            colWidth <- colWidth[names(colWidth) %in% colToDisplay]
            if (length(colWidth) == 0) {
              stop("No columns specified in 'colWidth' are in 'colToDisplay'")
            }
          }
        }
        
      } else {
        colToDisplay <- inputTableFullNames
      }
      
      
      colToDisplay <- c("Modify", "rowID", colToDisplay)
      
      
      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      
      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      # Define Module Observations ####
      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      
      
      #+++++++++++++++++++++++++++++++++++++++++++++
      # Input/Output Observations ####
      #+++++++++++++++++++++++++++++++++++++++++++++
      #When the input inputTableFull is updated, update the in module variables
      observeEvent(inputTableFull(), {
        toReturn[['tableFull']] <- inputTableFull()
        toReturn[['modifyPlaceID']] <- inputModifyPlaceID()
        
        
        #Update displayTableNames
        displayTableNames(names(cleanForDisplay(toReturn[['tableFull']], toReturn[['modifyPlaceID']], id, colToDisplay = colToDisplay, colToEditID = editIDs())))
        
        
        
      })
      # +++++++++++++++++++++++++++++++++++++++++++++
      
      
      #+++++++++++++++++++++++++++++++++++++++++++++
      # Table Formatting Observations ####
      #+++++++++++++++++++++++++++++++++++++++++++++
      observeEvent(displayTableNames(), {

        # Change specified column widths (used in DT::datatable callback js)
        basicWidths <- c("Modify" = '70px', "RecordID" = "60px")
        colWidth <- c(basicWidths, colWidth)

        colWidths[['IDs']] <- purrr::map_int(names(colWidth), ~as.integer(which(displayTableNames() %in% .x) - 1))  %>% glue::glue_collapse(sep=',')
        
        colWidths[['widths']] <- glue::glue("'{`names<-`(colWidth, NULL)}'") %>% glue::glue_collapse(sep=',')


        #Column ID's to Edit (used in addModifyToDF function)
        if (is.null(colToEdit)) {
          # Default to all columns except Modify and RecordID
          ids <- (seq_along(displayTableNames())[-c(1,2)] - 1)
          editIDs(paste0(ids, collapse = ","))
        } else {
          ids <- (which(displayTableNames() %in% colToEdit) - 1)
          editIDs(paste0(ids, collapse = ","))
        }
      })
      
      #+++++++++++++++++++++++++++++++++++++++++++++
      
      
      
      
      #+++++++++++++++++++++++++++++++++++++++++++++
      # Update on change to the return Values ####
      #+++++++++++++++++++++++++++++++++++++++++++++
      observe({
        #Update the number of deleted table rows based on current data
        deletedTable(editDT_deletedData(toReturn[['tableFull']], toReturn[['modifyPlaceID']]))
        deletedTableNumRows(nrow(deletedTable()))
        
        #Update the displayed Data
        displayedTable(editDT_displayData(toReturn[['tableFull']], toReturn[['modifyPlaceID']]))
        
        print(paste0('modifyPlaceID: ', toReturn[['modifyPlaceID']]))
        print(tail(toReturn[['tableFull']]))
        
      })
      #+++++++++++++++++++++++++++++++++++++++++++++
      
      
      
      
      #+++++++++++++++++++++++++++++++++++++++++++++
      # btn_deleteSelected ####
      #+++++++++++++++++++++++++++++++++++++++++++++
      
      # Disable btn_deleteSelected if no rows are selected and not editing any rows
      observeEvent(deleteSelectedEnabled(), {
        if(deleteSelectedEnabled()) {
          shinyjs::enable('btn_deleteSelected')
        } else {
          shinyjs::disable('btn_deleteSelected')
        }
      })
      
      
      #When the btn_deleteSelected button is pressed
      observeEvent(input[['btn_deleteSelected']], {
        
        #Get Row ID's of rows that are selected
        selectedRowIDs <- input[['dt_editableTable_rows_selected']]
        
        
        #Check for placement of modifyPlaceID and rebase table if needed
        temp <- editDT_rebaseModifyPoint(toReturn[['tableFull']], toReturn[['modifyPlaceID']], indexStart = indexStart)
        toReturn[['tableFull']] <- temp[['tableFull']]
        toReturn[['modifyPlaceID']] <- temp[['modifyPlaceID']]
        
        #Add to modifyID counter
        toReturn[['modifyPlaceID']] <- toReturn[['modifyPlaceID']] + 1
        
        #Update return table
        toReturn[['tableFull']] <- deleteRecords(toReturn[['tableFull']], displayedTable(), deleteRowIDs = selectedRowIDs, toReturn[['modifyPlaceID']])
        
        
      })
      #+++++++++++++++++++++++++++++++++++++++++++++
      
      
      #+++++++++++++++++++++++++++++++++++++++++++++
      # btn_undo ####
      #+++++++++++++++++++++++++++++++++++++++++++++
      
      # Disable the undo button if we have not deleted anything and not editing any rows
      observeEvent(undoEnabled(), {
        if(undoEnabled()) {
          shinyjs::enable('btn_undo')
        } else {
          shinyjs::disable('btn_undo')
        }
      })
      
      
      
      #When the undo button is pressed
      observeEvent(input[['btn_undo']], {
        toReturn[['modifyPlaceID']] <- toReturn[['modifyPlaceID']] - 1
      })
      #+++++++++++++++++++++++++++++++++++++++++++++
      
      
      
      
      #+++++++++++++++++++++++++++++++++++++++++++++
      # btn_redo ####
      #+++++++++++++++++++++++++++++++++++++++++++++
      
      # Disable/Enable the redo button
      observeEvent(redoEnabled(), {
        if(redoEnabled()) {
          shinyjs::enable('btn_redo')
        } else {
          shinyjs::disable('btn_redo')
        }
      })
      
      
      
      #When the redo button is pressed
      observeEvent(input[['btn_redo']], {
        
        toReturn[['modifyPlaceID']] <- toReturn[['modifyPlaceID']] + 1
        
      })
      #+++++++++++++++++++++++++++++++++++++++++++++
      
      
      
      
      
      #+++++++++++++++++++++++++++++++++++++++++++++
      # Rowwise btn 'id'_deletePressed ####
      #+++++++++++++++++++++++++++++++++++++++++++++
      #When a delete row button is pressed
      observeEvent(input[['deletePressed']], {
        
        #Finds deleted row number
        rowNum <- parseDeleteEvent(input[['deletePressed']])
        
        
        #Check for placement of modifyPlaceID and rebase table if needed
        temp <- editDT_rebaseModifyPoint(toReturn[['tableFull']], toReturn[['modifyPlaceID']], indexStart = indexStart)
        toReturn[['tableFull']] <- temp[['tableFull']]
        toReturn[['modifyPlaceID']] <- temp[['modifyPlaceID']]
        
        
        #Add to modifyID counter
        toReturn[['modifyPlaceID']] <- toReturn[['modifyPlaceID']] + 1
        
        #Update return table
        toReturn[['tableFull']] <- deleteRecords(toReturn[['tableFull']], displayedTable(), deleteRowIDs = rowNum, toReturn[['modifyPlaceID']])
        
      })
      #+++++++++++++++++++++++++++++++++++++++++++++
      
      
      #+++++++++++++++++++++++++++++++++++++++++++++
      # Rowwise btn 'id'_editPressed ####
      #+++++++++++++++++++++++++++++++++++++++++++++
            observeEvent(input[['editedData']], {
        
        #Finds editing row number
        rowNum <- parseDeleteEvent(input[['editPressed']])
        
        # if row being edited (check for btn_editRow_clicked)
        # make copy of row
        # print(editDT_displayData(toReturn[['tableFull']] , toReturn[['modifyPlaceID']])[rowNum,])
        
        
        
        oldData <<- editDT_displayData(toReturn[['tableFull']] , toReturn[['modifyPlaceID']])[rowNum,]
        
        
        editedData <<- input$editedData
        
        
        
        
        #Set Variables
        newData <- oldData
        headings <- unlist(editedData[['headings']])
        updatedValues <- editedData[['updatedValues']]
        
        #get types
        typeClasses <- purrr::map_chr(headings, ~class(newData[[.x]])[1])
        
        #cast list (needs a function)
        for (i in seq_along(updatedValues)) {
          if (typeClasses[i] == 'Date') {
            updatedValues[[i]] <- as.Date(updatedValues[[i]])
          } else if (typeClasses[i] == 'character') {
            updatedValues[[i]] <- as.character(updatedValues[[i]])
            if (nchar(updatedValues[[i]]) == 0) {
              updatedValues[[i]] <- NA_character_
            }
          } else if (typeClasses[i] == 'integer') {
            updatedValues[[i]] <- as.integer(updatedValues[[i]])
          } else if (typeClasses[i] == 'logical') {
            updatedValues[[i]] <- as.logical(updatedValues[[i]])
          } else if (typeClasses[i] %in% c('double', 'numeric')) {
            updatedValues[[i]] <- as.double(updatedValues[[i]])
          } else if (typeClasses[i] == 'POSIXct') {
            updatedValues[[i]] <- as.POSIXct(updatedValues[[i]])
          } else {
            stop(paste0("Type '", typeClasses[i], "' not recognized."))
          }
          
        } 
        
        
        
        #insert data
        for (i in seq_along(headings)) {
          newData[1,names(newData) %in% headings[i]] <- updatedValues[[i]]
        }
        
        newData <<- newData
        
        print("Old Data")
        print(oldData)
        print("New Data")
        print(newData)
        
        
        #compare for changes
        if ((dplyr::bind_rows(oldData, newData) %>% dplyr::distinct() %>% nrow()) == 2) {
          
          #if there are changes add record to main data
          
          # Check for placement of modifyPlaceID and rebase table if needed
          temp <- editDT_rebaseModifyPoint(toReturn[['tableFull']], toReturn[['modifyPlaceID']], indexStart = indexStart)
          # toReturn[['tableFull']] <- temp[['tableFull']]
          # toReturn[['modifyPlaceID']] <- temp[['modifyPlaceID']]
          
          
          #Add to modifyID counter
          toReturn[['modifyPlaceID']] <- temp[['modifyPlaceID']] + 1
          
          #modify newData
          newData <- newData %>% dplyr::mutate(editState = TRUE, modifyID = toReturn[['modifyPlaceID']])
          
          #Update return table
          toReturn[['tableFull']] <- dplyr::bind_rows(temp[['tableFull']], newData)
          
        }
        # dplyr::bind_rows(oldData, oldData) %>% dplyr::distinct() %>% nrow()
        
        
        # }
        
        
        
      })
      #+++++++++++++++++++++++++++++++++++++++++++++
      
      
      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      
      
      
      
      
      
      
      # #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      # # Render Outputs ####
      # #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      # 
      # #Render Count of Deleted Text
      # output[['txt_deletedCount']] <- renderText({
      #   if (deletedTableNumRows() == 0) {
      #     NULL
      #   } else {
      #     paste0(deletedTableNumRows(), " rows deleted.")
      #   }
      # })
      # 
      #Render Table
      output[['dt_editableTable']] <- DT::renderDataTable(
        
        # Add the delete button column
        DT::datatable(
          data = cleanForDisplay(toReturn[['tableFull']], toReturn[['modifyPlaceID']], id, colToDisplay = colToDisplay, colToEditID = editIDs()),
          callback = DT::JS(glue::glue("
          var colIDs = [%{%colWidths[['IDs']]%}%];
          var colWidths = [%{%colWidths[['widths']]%}%];
          //console.log(colIDs);
          //console.log(colWidths);
          var colWidthHeadings = $('div#%{%id%}%-dt_editableTable thead th').filter(function(index) {return colIDs.indexOf(index) > -1;});
          //console.log(colWidthHeadings);
          $.each(colWidthHeadings, function(index, el) {
            //console.log($(this));
            $(this).css('width', colWidths[index]).css('max-width', colWidths[index]).css('min-width', colWidths[index]);
            //console.log($(this));
        
          });
          
          ", .open = '%{%', .close = '%}%')),

          escape = FALSE, # Need to disable escaping for html as string to work
          options = list(
            # scrollX=TRUE,# scrollCollapse=TRUE,
            # autoWidth=TRUE,
            
            # Disable sorting for the delete column
            columnDefs = list(
              list(targets = 0, sortable = FALSE))
          ), 
          rownames= FALSE#,

        )
      )
      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      
      return(toReturn)
    }
  )
}




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Exported Module Helper Functions ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#' @title editDT Helpers
#'
#' @description Functions that can be used inside or outside the module to add,
#'   modify, or filter and display the dataset that gets passed inside and
#'   outside the module.
#'
#' @param fullData A data.frame/tibble previously initialized by a
#'   \code{editDT_prepareNewData()} call and stored in a \code{reactiveValues()}
#'   list.
#' @param modifyPlaceID The id for the current place in the modify timeline
#'   being tracked using the modifyID column of fullData. This is an integer
#'   value from 0 to infinity (i.e. zero indexed). Default 0 used for
#'   initializing a dataset for use in the module.
#' @param newData A data.frame/tibble that holds a new dataset for being
#'   initialized for or added into a \code{reactiveValues()} object for use in
#'   the module.
#' @param newRecord TRUE/FALSE of whether or not the record is new. When
#'   initializing data from your main source (i.e., a database or file) the
#'   default is FALSE. When adding a new record to an already initialized module
#'   dataset, set to TRUE.
#' @param indexStart Whether the index should start at 0 or 1. If the table 
#'   contains all new data (i.e. all from a form entry in a current session)
#'   set the index to 1, else set it to 0 (the default)

#' @section Function Specific Details: 
#' \describe{
#'   \item{editDT_currentData}{
#'     \itemize{
#'       \item Filters out the current data based on your modifyPlaceID. This 
#'       does not filter out deleted data (see \code{editDT_displayData()})
#'       \item For use inside and outside the module.}}
#'   \item{editDT_displayData}{
#'     \itemize{
#'       \item Filters out the current data based on your modifyPlaceID and 
#'       filters out deleted data.
#'       \item This should mimic the data being used to fill the module output 
#'       DataTable.
#'       \item For use inside and outside the module.}}
#'   \item{editDT_deletedData}{
#'     \itemize{
#'       \item Filters out the current data based on your modifyPlaceID and 
#'       returns only the currently deleted rows.
#'       \item This is used in the module to determine if there are any deleted 
#'       rows.
#'       \item For use inside and outside the module.}}
#'   \item{editDT_prepareNewData}{
#'    \itemize{ 
#'       \item Prepares data for input into module 
#'       by adding the columns: 'uuid', 'deleteState', 'editState', 
#'       'newRecord', 'rowID', 'modifyID'. Can be used to initialize a dataset 
#'       for the module or prepare a new record to be added to an already 
#'       initialized dataset for the module.
#'       \item For use outside the module.
#'       \item The argument newData and modifyPlaceID are used to initialize 
#'       the input to the main 'tableFull' reactive value. 
#'       \item When initializing a new dataset set the modifyPlaceID to 0. 
#'       \item When using the function to prepare a new record for input into 
#'       the table, all arguments will be used and the newRecord argument 
#'       should be set to TRUE. The fullData argument is used to input the in 
#'       already initialized reactive values dataset}} 
#'   \item{editDT_rebaseModifyPoint}{
#'     \itemize{
#'       \item Determines where you are in the modify timeline.If your
#'       modifyPlaceID is not equal to \code{max(fullData[['modifyID']])} it
#'       will rebase the fullData. This involves removing all records that have 
#'       a modifyID larger the modifyPlaceID and redefining the modifyID 
#'       timeline to rebase to 0.
#'       \item WARNING: This is a destructive action and the module is not 
#'       equipped to undo this modification.
#'       \item This function is called before every action that required a 
#'       record to be appended to the fullData. This includes record adds, 
#'       deletes, and edits.
#'       \item For use inside and outside the module.}}
#'}





#' @rdname editDThelpers
#' @export
editDT_currentData <- function(fullData, modifyPlaceID) {
  out <- fullData %>% 
    dplyr::filter(modifyID <= modifyPlaceID) %>%
    marcR::groupby_rank(uuid, rankby = "modifyID", filterIDs = 1)
  return(out)
}

#' @rdname editDThelpers
#' @export
editDT_displayData <- function(fullData, modifyPlaceID) {
  out <- editDT_currentData(fullData, modifyPlaceID) %>% 
    dplyr::filter(!deleteState)
  return(out)
}

#' @rdname editDThelpers
#' @export
editDT_deletedData <- function(fullData, modifyPlaceID) {
  out <- editDT_currentData(fullData, modifyPlaceID) %>% 
    dplyr::filter(deleteState)
  return(out)
}


#' @rdname editDThelpers
#' @export
editDT_prepareNewData <- function(newData, modifyPlaceID = 0, fullData = NULL, newRecord = FALSE) {
  
  if (is.null(fullData) | nrow(newData) == 0) {
    rowIDs <- seq_along(newData[[1]])
  } else if (!is.null(fullData)) {
    if (nrow(fullData) == 0) {
      maxRowID <- 0
    } else {
      maxRowID <- max(fullData[['rowID']])
    }
    rowIDs <- seq_along(newData[[1]]) + maxRowID
  }
  
  newData %>% 
    dplyr::mutate(uuid = uuid::UUIDgenerate(n=dplyr::n()),
                  deleteState = FALSE,
                  editState = FALSE,
                  newRecord = newRecord,
                  rowID = rowIDs,
                  modifyID = modifyPlaceID) %>% 
    dplyr::mutate(dplyr::across(where(is.factor), as.character))
  
}



#' @rdname editDThelpers
#' @export
editDT_rebaseModifyPoint <- function(fullData, modifyPlaceID, indexStart = 0) {
  
  #minus one so the rank is 0 indexed (default) or minus 0 if the table is all new data so it starts at 1
  indexCalc <- dplyr::if_else(indexStart == 0, 1, 0)
  
  #if there are no records return the blank table and set modifyPlaceID to 0
  if (length(fullData[['modifyID']]) == 0) {
    out <- list("tableFull"  = fullData, "modifyPlaceID" = 0)
  } else {
    
    #if there are records in the full data but the modifyPlaceID == 0
    if (modifyPlaceID == 0) {
      out <- list("tableFull"  = editDT_displayData(fullData, modifyPlaceID), 
                  "modifyPlaceID" = modifyPlaceID)
      
      # else if there are some records in full dataset and modifyPlaceID < max(modifyID)
    } else if (modifyPlaceID == max(fullData[['modifyID']])) {
      out <- list("tableFull"  = fullData, "modifyPlaceID" = modifyPlaceID)
      
      #else do full rebase
    } else {
      # outTable <- editDT_displayData(fullData, modifyPlaceID)
      outTable <- editDT_currentData(fullData, modifyPlaceID)
      
      outTable[['modifyID']] <- dplyr::dense_rank(outTable$modifyID) - indexCalc 
      
      outModifyID <- max(outTable$modifyID)
      
      out <- list("tableFull"  = outTable, "modifyPlaceID" = outModifyID)
    }
    
    
  }
  
  
  return(out)
  
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++














#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Internal Module Helper Functions ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


deleteRecords <- function(fullData, displayData, deleteRowIDs, modifyPlaceID) {
  deletedRows <- displayData[deleteRowIDs,] %>% 
    dplyr::mutate(deleteState = TRUE,
                  modifyID = modifyPlaceID)
  out <- dplyr::bind_rows(fullData, deletedRows)
  return(out)
}


cleanForDisplay <- function(fullData, modifyPlaceID, id, colToDisplay, colToEditID) {
  out <- editDT_displayData(fullData, modifyPlaceID) %>% 
    addModifyToDF(id = id, colToEditID = colToEditID) %>% 
    dplyr::select(dplyr::any_of(colToDisplay)) %>% 
    dplyr::arrange(rowID) %>% dplyr::relocate("rowID", .after = 1) %>% dplyr::rename("RecordID" = "rowID") %>% 
    dplyr::select(-dplyr::any_of(c('uuid', 'deleteState', 'editState', 'newRecord', 'modifyID'))) %>% 
    dplyr::mutate(dplyr::across(where(lubridate::is.POSIXct), as.character))
  return(out)
}



#' @description  A column of rowise delete and edit buttons for each row in the data frame for the first column
#'   Derived from https://stefanengineering.com/2019/07/06/delete-rows-from-shiny-dt-datatable/
#'
#' @param df data frame
#' @param id id prefix to add to each actionButton. The buttons will be id'd as id_INDEX.
#' @param colToEditID id's of the columns to allow editing
#' 
#' @return A data.frame with the new 'Modify' column for use within DT
#' @noRd
addModifyToDF <- function(df, id, colToEditID) {

    # function to create one action button as string
  f <- function(i) {
    deleteInputID <- paste(id, 'delete', i, sep="_")
    editInputID <- paste(id, 'edit', i, sep="_")
    
    as.character(
      tagList(
        #add delete button
        actionButton(
          # The id prefix with index
          inputId = deleteInputID,
          class = id, 
          label = NULL,
          icon = icon('trash'),
          class = "btn_deleteRow",
          # to access Shiny.setInputValue as input[['deletePressed']] inside a module you prefix the module id and a '-' to the value (https://community.rstudio.com/t/shiny-setinputvalue-in-modular-app-with-namespaces/23263)
          onclick = glue::glue('
Shiny.setInputValue(\"%{%id%}%-deletePressed\", this.id, {priority: "event"});',
.open = '%{%', .close = '%}%'
          )
        ),
#add edit button (lots of onclick javascript here)
actionButton(
  # The id prefix with index
  inputId = editInputID,
  class = id, 
  label = NULL,
  icon = icon('edit'),
  class = "btn_editRow",
  # to access Shiny.setInputValue as input[['deletePressed']] inside a module you prefix the module id and a '-' to the value (https://community.rstudio.com/t/shiny-setinputvalue-in-modular-app-with-namespaces/23263)
  onclick = glue::glue('
//set up input value for when a rowise edit button is pressed
Shiny.setInputValue(\"%{%id%}%-editPressed\", this.id, {priority: "event"});

//toggle formatting class of button
document.getElementById("%{%editInputID%}%").classList.toggle("btn_editRow");
document.getElementById("%{%editInputID%}%").classList.toggle("btn_editRow_clicked");

//Is editing?
Shiny.onInputChange("%{%id%}%-isInEditing", $("#%{%editInputID%}%")[0].matches(".btn_editRow_clicked"));


//if an edit button is clicked
if ($("#%{%editInputID%}%")[0].matches(".btn_editRow_clicked")) {

  //disable all buttons in table
  $("div#%{%id%}%-dt_editableTable button").each(function() {$(this).attr("disabled", "true");});
  
  //disable paginate links (seems this isnt possible)
  //$("div#%{%id%}%-dt_editableTable div.dataTables_paginate a").each(function() {$(this).attr("disabled", "true");});

  //enable single edit button 
  $("div#%{%id%}%-dt_editableTable button#%{%editInputID%}%").each(function() {$(this).removeAttr("disabled");});
  
  //start editing
  var editIndicies = [%{%colToEditID%}%];
  //var headings = $("div#%{%id%}%-dt_editableTable thead th").filter(function(index) {return editIndicies.indexOf(index) > -1;});
  var trObjs = $("div#%{%id%}%-dt_editableTable button#%{%editInputID%}%").closest("tr").children("td").filter(function(index) {return editIndicies.indexOf(index) > -1;});
  //console.log(trObjs);
  //Add input fields (could program type here too)
  $.each(trObjs, function(i, el) {
    var txt = $(this).text();
    //had to add some special escaping here on the R side to escape the single quotes and slashes
    $(this).html("").append("<input type=\'text\' value=\\""+txt+"\\">");
  });

} else {

  //save data for use in R
  var editIndicies = [%{%colToEditID%}%];
  var headings = $("div#%{%id%}%-dt_editableTable thead th").filter(function(index) {return editIndicies.indexOf(index) > -1;});
  var trObjs = $("div#%{%id%}%-dt_editableTable button#%{%editInputID%}%").closest("tr").children("td").filter(function(index) {return editIndicies.indexOf(index) > -1;});
  var headArr = $.makeArray(headings.map(function() { return($(this).text());}));;
  var inputArr = $.makeArray(trObjs.map(function() { return($(this).find("input").val());}));
  var editOutput = {headings: headArr,
                    updatedValues: inputArr};
  Shiny.onInputChange("%{%id%}%-editedData", editOutput);

  //save data in displayed table and close inputs
  $.each(trObjs, function(i, el) {
    var txt = $(this).find("input").val()
    $(this).html(txt);
  });
  
  //enable all buttons in table
  $("div#%{%id%}%-dt_editableTable button").each(function() {$(this).removeAttr("disabled");});
  
  //enable all buttons in paginate (seemse this isnt possible)
  //$("div#%{%id%}%-dt_editableTable div.dataTables_paginate a").each(function() {$(this).removeAttr("disabled");});


}


            ', .open = '%{%', .close = '%}%'
  )
)
      )
    )
  }

if (nrow(df) == 0) {
  modifyCol <- character(0)
} else {
  modifyCol <- unlist(lapply(seq_len(nrow(df)), f))
}


outDF <- cbind(Modify = modifyCol, df)

return(outDF)
}


#' @description Extracts the row id number from the id string
#' 
#' Taken from https://stefanengineering.com/2019/07/06/delete-rows-from-shiny-dt-datatable/
#' 
#' @param idstr the id string formatted as id_INDEX
#' @return INDEX from the id string id_INDEX
#' @noRd
parseDeleteEvent <- function(idstr) {
  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
