rowiseEditOnclick = function(id, row, editIndicies, allowDeletes) {


    var shinyInputEditPressed = (id + "-editPressed");
    var editInputID = (id + "_" + "edit" + "_" + row);
    //set up input value for when a rowise edit button is pressed
    //Shiny.setInputValue(shinyInputEditPressed, this.id, {priority: "event"});
    Shiny.setInputValue(shinyInputEditPressed, editInputID, {priority: "event"});

    //toggle formatting class of button
    document.getElementById(editInputID).classList.toggle("btn_editRow");
    document.getElementById(editInputID).classList.toggle("btn_editRow_clicked");

    //Is editing?
    Shiny.onInputChange(id + "-isInEditing", $("#" + editInputID)[0].matches(".btn_editRow_clicked"));


    //if an edit button is clicked
    if ($("#" + editInputID)[0].matches(".btn_editRow_clicked")) {

        //disable all buttons in table
        $("div#" + id + "-dt_editableTable button").each(function() {$(this).attr("disabled", "true");});

        //enable single edit button
        $("div#" + id + "-dt_editableTable button#" + editInputID).each(function() {$(this).removeAttr("disabled");});

        //start editing
        var trObjs = $("div#" + id + "-dt_editableTable button#" + editInputID).closest("tr").children("td").filter(function(index) {return editIndicies.indexOf(index) > -1;});
        //console.log(trObjs);

        //Add input fields (could program type here too)
        $.each(trObjs, function(i, el) {
            var txt = $(this).text();
            $(this).html("").append("<input type='text' value='"+ txt + "'>");
        });

    } else {

        //save data for use in R
        var headings = $("div#" + id + "-dt_editableTable thead th").filter(function(index) {return editIndicies.indexOf(index) > -1;});
        var trObjs = $("div#" + id + "-dt_editableTable button#" + editInputID).closest("tr").children("td").filter(function(index) {return editIndicies.indexOf(index) > -1;});
        var headArr = $.makeArray(headings.map(function() { return($(this).text());}));;
        var inputArr = $.makeArray(trObjs.map(function() { return($(this).find("input").val());}));
        var editOutput = {headings: headArr, updatedValues: inputArr};

        Shiny.onInputChange(id + "-editedData", editOutput);

        //save data in displayed table and close inputs
        $.each(trObjs, function(i, el) {
            var txt = $(this).find("input").val()
            $(this).html(txt);
        });

        if (allowDeletes) {
            //enable all buttons in table
            $("div#" + id + "-dt_editableTable button").each(function() {$(this).removeAttr("disabled");});
        } else {
            //enable only the edit buttons in table
            $("div#" + id + "-dt_editableTable button.btn_editRow").each(function() {$(this).removeAttr("disabled");});
        }

    }
}



getSelectedRecordIDs = function(id, RecordIDname) {
    console.log(id)
    console.log(RecordIDname)
    //Find the correct column with RecordID
    var colNames = $('div#' + id + '-dt_editableTable table.dataTable thead th').map(function() {return($(this).text());});
    // var recordIDIndex = colNames.toArray().indexOf("RecordID");
    var recordIDIndex = colNames.toArray().indexOf(RecordIDname);

    //Pull the ID's for the selected Rows
    var selectedIDs = $('div#' + id + '-dt_editableTable table.dataTable tbody tr.selected').map(function() {return($(this).children('td')[recordIDIndex].innerText);}).toArray();
    selectedIDs = selectedIDs.map( function(x) {return(parseInt(x));});
    console.log(selectedIDs)

    //Export to a shiny variable
    Shiny.onInputChange(id + "-JSselectedRows", selectedIDs);

}
