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
#' # See the example file in the repo at /dev/02_Example_editDT_shinyModule.R for an example of how this module works
#'
#' }
#'
#' @rdname editDT
#' @export
#'
#' @import shiny
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
    div(tags$head(marcRviz::editDT_js())),
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
#'   \code{editDT_prepareNewData()}. Must be passed in a \code{reactive()}
#'   statement
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
#'   contains all new data (i.e. all from a form entry in a current session) set
#'   the index to 1, else set it to 0 (the default)
#' @param allowDeletes,allowEdits TRUE/FALSE on whether or not to include the
#'   edit or delete buttons. Both TRUE by default.
#' @rdname editDT
#' @export
editDT_server <- function(id, inputTableFull, inputModifyPlaceID, colToDisplay=NULL, colToEdit=NULL, colWidth=NULL, colSort=NULL, indexStart = 0, allowDeletes = TRUE, allowEdits = TRUE) {
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
        if (is.null(input[['isInEditing']]) | length(toReturn[['tableFull']][['editDT_modifyID']]) == 0) {
          FALSE
        } else {
          !is.null(toReturn[['tableFull']]) & max(toReturn[['tableFull']][['editDT_modifyID']]) > toReturn[['modifyPlaceID']] & !input[['isInEditing']]
        }
      })



      displayTableNames <- reactiveVal('')
      editIDs <- reactiveVal('')
      colWidths <- reactiveValues(IDs = c('0,1'), Widths = c("c('70px', '60px')"))
      deletedSelectedWasPressed <- reactiveVal(FALSE)

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


      colToDisplay <- c("editDT_modifyCol", "editDT_rowID", colToDisplay)


      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      # Define Module Observations ####
      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



      #+++++++++++++++++++++++++++++++++++++++++++++
      # Input/Output Observations ####
      #+++++++++++++++++++++++++++++++++++++++++++++
      #When the input inputTableFull is updated, update the in module variables
      observeEvent(inputTableFull(), {

        if (any(is.na(inputTableFull()[['editDT_modifyCol']]))) {

          tableFullHasModify <- dplyr::filter(inputTableFull(), !is.na(editDT_modifyCol))
          tableFullAddedModify <- dplyr::filter(inputTableFull(), is.na(editDT_modifyCol))

          tableFullAddedModify <- addModifyToDF(tableFullAddedModify, id = id, colToEditID = editIDs(), allowDeletes = allowDeletes)

          toReturn[['tableFull']] <- dplyr::bind_rows(tableFullHasModify, tableFullAddedModify) %>%
            dplyr::arrange(editDT_modifyID, editDT_rowID)

        } else {

          toReturn[['tableFull']] <- inputTableFull()

        }


        toReturn[['modifyPlaceID']] <- inputModifyPlaceID()

      })


      observeEvent(inputTableFull(), {

        #Update displayTableNames
        displayTableNames(getDisplayNames(inputTableFull(), colToDisplay = colToDisplay))

      }, priority = 100)
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
      }, priority = 90)

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

      if (!allowDeletes) {
        shinyjs::runjs(glue::glue("$('#{id}-btn_deleteSelected').remove()"))
      }


      # Disable btn_deleteSelected if no rows are selected and not editing any rows
      observeEvent(deleteSelectedEnabled(), {
        if(deleteSelectedEnabled() & allowDeletes) {
          shinyjs::enable('btn_deleteSelected')
        } else {
          shinyjs::disable('btn_deleteSelected')
        }
      })





      #When the btn_deleteSelected button is pressed (works in tandem with observeEvent(input[['JSselectedRows']], {}))
      observeEvent(input[['btn_deleteSelected']], {

        #Begin delete Selected Process and jump to observe change in JSselectedRows
        deletedSelectedWasPressed(TRUE)
        shinyjs::runjs(glue::glue('getSelectedRecordIDs("{id}", "RecordID")'))

      })

      observeEvent(input[['JSselectedRows']], {

        if (deletedSelectedWasPressed()) {
          print('Deleting Selected Rows')

          #Reset deleteSelectedWasPressed Trigger
          deletedSelectedWasPressed(FALSE)

          selectedRowIDs <- input[['JSselectedRows']]
          print(selectedRowIDs)

          #Check for placement of modifyPlaceID and rebase table if needed
          temp <- editDT_rebaseModifyPoint(toReturn[['tableFull']], toReturn[['modifyPlaceID']], indexStart = indexStart)
          toReturn[['tableFull']] <- temp[['tableFull']]
          toReturn[['modifyPlaceID']] <- temp[['modifyPlaceID']]

          #Add to modifyID counter
          toReturn[['modifyPlaceID']] <- toReturn[['modifyPlaceID']] + 1

          #Update return table
          toReturn[['tableFull']] <- deleteRecords(toReturn[['tableFull']], displayedTable(), deleteRowIDs = selectedRowIDs, toReturn[['modifyPlaceID']])

          #reset JSseelctedRows
          shinyjs::runjs(glue::glue('Shiny.onInputChange("{id}-JSselectedRows", []);)'))

        } else {
          print('Skip Deleting Selected Rows')
        }

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


        #make copy of old data
        oldData <- dplyr::filter(editDT_displayData(toReturn[['tableFull']] , toReturn[['modifyPlaceID']]), editDT_rowID %in% rowNum)

        #pull edited data for comparison
        editedData <- input$editedData




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

        newData <- newData

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
          newData <- newData %>% dplyr::mutate(editDT_editState = TRUE, editDT_modifyID = toReturn[['modifyPlaceID']])

          #Update return table
          toReturn[['tableFull']] <- dplyr::bind_rows(temp[['tableFull']], newData)

        }
        # dplyr::bind_rows(oldData, oldData) %>% dplyr::distinct() %>% nrow()


        # }



      })
      #+++++++++++++++++++++++++++++++++++++++++++++


      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++








      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      # Render Outputs ####
      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      # #Render Count of Deleted Text
      # output[['txt_deletedCount']] <- renderText({
      #   if (deletedTableNumRows() == 0) {
      #     NULL
      #   } else {
      #     paste0(deletedTableNumRows(), " rows deleted.")
      #   }
      # })




      #Render Table
      output[['dt_editableTable']] <- DT::renderDataTable({
        # outDF <- toReturn[['tableFull']]
        outDF <- cleanForDisplay(toReturn[['tableFull']], toReturn[['modifyPlaceID']], id, colToDisplay = colToDisplay, colToEditID = editIDs(), allowDeletes = allowDeletes)
        callbackJS <- glue::glue("
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
            ", .open = '%{%', .close = '%}%')
        rowCallbackJS <- glue::glue("
           function( row, data, index ) {

              //remove deleteRow Buttons
              if (%{%tolower(as.character(!allowDeletes))%}%) {
                $(row).find('button.btn_deleteRow').remove();
                //disable only the delete buttons in table
                //$(row).find('button.btn_deleteRow').each(function() {$(this).attr('disabled', 'true');});
              }

              //remove editRow Buttons
              if (%{%tolower(as.character(!allowEdits))%}%) {
                $(row).find('button.btn_editRow').remove();
                //disable only the edit buttons in table
                //$(row).find('button.btn_editRow').each(function() {$(this).attr('disabled', 'true');});
              }
            }
            ", .open = '%{%', .close = '%}%')


        DT::datatable(data = outDF,
                      escape = FALSE,
                      rownames= FALSE,
                      callback = DT::JS(callbackJS),
                      options = list(
                            # scrollX=TRUE,# scrollCollapse=TRUE,
                            # autoWidth=TRUE,

                            # Disable sorting for the delete column
                            columnDefs = list(
                              list(targets = 0, sortable = FALSE)),

                            # Row Callbacks
                            rowCallback = DT::JS(rowCallbackJS)
                          )
        )

      })
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
#'   being tracked using the editDT_modifyID column of fullData. This is an integer
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
#'       by adding the columns: 'editDT_uuid', 'editDT_deleteState',
#'       'editDT_editState', 'editDT_newRecord', 'editDT_rowID',
#'       'editDT_modifyID'. Can be used to initialize a dataset for the module
#'       or prepare a new record to be added to an already initialized dataset
#'       for the module.
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
#'       modifyPlaceID is not equal to \code{max(fullData[['editDT_modifyID']])} it
#'       will rebase the fullData. This involves removing all records that have
#'       a editDT_modifyID larger the modifyPlaceID and redefining the editDT_modifyID
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
    dplyr::filter(editDT_modifyID <= modifyPlaceID) %>%
    marcR::groupby_rank(editDT_uuid, rankby = "editDT_modifyID", filterIDs = 1)
  return(out)
}

#' @rdname editDThelpers
#' @export
editDT_displayData <- function(fullData, modifyPlaceID) {
  out <- editDT_currentData(fullData, modifyPlaceID) %>%
    dplyr::filter(!editDT_deleteState)
  return(out)
}

#' @rdname editDThelpers
#' @export
editDT_deletedData <- function(fullData, modifyPlaceID) {
  out <- editDT_currentData(fullData, modifyPlaceID) %>%
    dplyr::filter(editDT_deleteState)
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
      maxRowID <- max(fullData[['editDT_rowID']])
    }
    rowIDs <- seq_along(newData[[1]]) + maxRowID
  }

  newData %>%
    dplyr::mutate(editDT_uuid = uuid::UUIDgenerate(n=dplyr::n()),
                  editDT_deleteState = FALSE,
                  editDT_editState = FALSE,
                  editDT_newRecord = newRecord,
                  editDT_rowID = rowIDs,
                  editDT_modifyID = modifyPlaceID,
                  editDT_modifyCol = NA_character_) %>%
    dplyr::mutate(dplyr::across(where(is.factor), as.character))

}



#' @rdname editDThelpers
#' @export
editDT_rebaseModifyPoint <- function(fullData, modifyPlaceID, indexStart = 0) {

  #minus one so the rank is 0 indexed (default) or minus 0 if the table is all new data so it starts at 1
  indexCalc <- dplyr::if_else(indexStart == 0, 1, 0)

  #if there are no records return the blank table and set modifyPlaceID to 0
  if (length(fullData[['editDT_modifyID']]) == 0) {
    out <- list("tableFull"  = fullData, "modifyPlaceID" = 0)
  } else {

    #if there are records in the full data but the modifyPlaceID == 0
    if (modifyPlaceID == 0) {
      out <- list("tableFull"  = editDT_displayData(fullData, modifyPlaceID),
                  "modifyPlaceID" = modifyPlaceID)

      # else if there are some records in full dataset and modifyPlaceID < max(editDT_modifyID)
    } else if (modifyPlaceID == max(fullData[['editDT_modifyID']])) {
      out <- list("tableFull"  = fullData, "modifyPlaceID" = modifyPlaceID)

      #else do full rebase
    } else {
      # outTable <- editDT_displayData(fullData, modifyPlaceID)
      outTable <- editDT_currentData(fullData, modifyPlaceID)

      outTable[['editDT_modifyID']] <- dplyr::dense_rank(outTable$editDT_modifyID) - indexCalc

      outModifyID <- max(outTable$editDT_modifyID)

      out <- list("tableFull"  = outTable, "modifyPlaceID" = outModifyID)
    }


  }


  return(out)

}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++














#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Internal Module Helper Functions ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

editDT_js <- function() {
  includeScript(path = system.file("mod_editDT.js", package = "marcRviz"))
}

getDisplayNames <- function(tableFull, colToDisplay) {
  return(c("Modify", "RecordID", names(tableFull)[names(tableFull) %in% colToDisplay]))
}


deleteRecords <- function(fullData, displayData, deleteRowIDs, modifyPlaceID) {
  # deletedRows <- displayData[deleteRowIDs,] %>%
  deletedRows <- dplyr::filter(displayData, editDT_rowID %in% deleteRowIDs) %>%
    dplyr::mutate(editDT_deleteState = TRUE,
                  editDT_modifyID = modifyPlaceID)
  out <- dplyr::bind_rows(fullData, deletedRows)
  return(out)
}


cleanForDisplay <- function(fullData, modifyPlaceID, id, colToDisplay, colToEditID, allowDeletes) {
  out <- editDT_displayData(fullData, modifyPlaceID) %>%
    # addModifyToDF(id = id, colToEditID = colToEditID, allowDeletes = allowDeletes) %>%
    dplyr::select(dplyr::any_of(colToDisplay)) %>%
    dplyr::arrange(editDT_rowID) %>% dplyr::relocate("editDT_modifyCol", "editDT_rowID", .before = 1) %>%
    dplyr::rename("Modify" = "editDT_modifyCol", "RecordID" = "editDT_rowID") %>%
    dplyr::select(-dplyr::any_of(c('editDT_uuid', 'editDT_deleteState', 'editDT_editState', 'editDT_newRecord', 'editDT_modifyID'))) %>%
    dplyr::mutate(dplyr::across(where(lubridate::is.POSIXct), as.character))
  return(out)
}



#' @description  A column of rowise delete and edit buttons for each row in the data frame for the first column
#'   Derived from https://stefanengineering.com/2019/07/06/delete-rows-from-shiny-dt-datatable/
#'
#' @param df data frame
#' @param id id prefix to add to each actionButton. The buttons will be id'd as id_INDEX.
#' @param colToEditID id's of the columns to allow editing
#' @param allowDeletes TRUE/FALSE on whether or not to include the
#'   edit or delete buttons. Both TRUE by default.
#'
#' @return A data.frame with the new 'Modify' column for use within DT
#' @noRd
addModifyToDF <- function(df, id, colToEditID, allowDeletes) {


  if (nrow(df) == 0) {
    modifyCol <- character(0)
  } else {


    i = '___i___'

    deleteInputID <- paste(id, 'delete', i, sep="_")
    editInputID <- paste(id, 'edit', i, sep="_")
    modifyButton <- as.character(
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
          onclick = glue::glue('Shiny.setInputValue(\"%{%id%}%-deletePressed\", this.id, {priority: "event"});',.open = '%{%', .close = '%}%')
        ),

        #add edit button (lots of onclick javascript here)
        actionButton(
          # The id prefix with index
          inputId = editInputID,
          class = id,
          label = NULL,
          icon = icon('edit'),
          class = "btn_editRow",
          onclick = glue::glue('rowiseEditOnclick("{id}", "{i}", [{colToEditID}], {tolower(as.character(allowDeletes))})')
        )
      )
    )

    modifyCol <- rep(modifyButton, nrow(df))
    modifyCol <- modifyCol %>% stringr::str_replace_all('___i___', as.character(df[['editDT_rowID']]))


  }


  outDF <- dplyr::mutate(df, editDT_modifyCol = modifyCol)

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
  print(idstr)
  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  print(res)
  if (! is.na(res)) res
}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
