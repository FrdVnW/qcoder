## This is a Shiny web application. You can run the application by clicking
## the 'Run App' button above. Alternitively in the console run qcode().
## This assumes that you have set up a standard QCoder project.
                                        #
if (interactive()) {
    library(qcoder)
    library(shiny)
    library(dplyr)
    library(magrittr)
    library(shinyAce)
    library(rlang)
    library(shinyFiles)
    library(shinythemes)
    library(DT)

    library(shinyjs)
    library(here)
    ## hard coded for now
    editor_name <- "aceeditor"

    ## Define UI for application
    ui <- navbarPage("Qcoder",
                     theme = shinytheme("flatly"),
                     header = list(
                         verbatimTextOutput("project_directory"),
                         shinyDirButton('select_project',
                                        label="Select your project folder",
                                        title="Select your project folder",
                                        buttonType = "default", class = NULL),
                         actionButton("update", "Reload project for data updating",
                                      icon = icon("refresh")
                                      ),
                         tags$br(), tags$br()
                     ), ## close header
                     tabPanel("Coding documents",
                              sidebarLayout(
                                  sidebarPanel(
                                      uiOutput('choices'),
                                      uiOutput('saveButton'),
                                      uiOutput('docpart'),
                                      uiOutput('fromclass'),
                                      uiOutput('from'),
                                      uiOutput('toclass'),
                                      uiOutput('to'),
                                      uiOutput('weight'),
                                      uiOutput('sign'),
                                      uiOutput('class'),
                                      actionButton("replace", "Add selected code")
                                  ),## close add data tab
                                  mainPanel(
                                      tabsetPanel(id = "subTabPanel1",
                                                  tabPanel("Edit",
                                                           uiOutput('mydocA')),
                                                  tabPanel("Existing file",
                                                           htmlOutput("this_doc" )
                                                           ),
                                                  tabPanel("Unit to Document Links" ,
                                                           uiOutput('checkbox_save_links'),
                                                           uiOutput('checkbox_links')
                                                           )
                                                  )## close document sub-tabset
                                  )## close main panel
                              )## close sidebarLayout
                              ),# close editor tab panel
                     navbarMenu("Data",
                                tabPanel("Documents",
                                         sidebarLayout(
                                             sidebarPanel(
                                                 tags$h2("Add new document"),
                                                 actionButton("add_new_document",
                                                              "Add a new document",
                                                              icon = icon("plus")),
                                                 uiOutput("selectsend_new_document")
                                             ),## close add data tab
                                             mainPanel(
                                                 'The documents?'
                                             )
                                         )
                                         ),
                                tabPanel("Concpets",
                                         sidebarLayout(
                                             sidebarPanel(
                                                 tags$h2("Add new concept"),
                                                 uiOutput("addsubmit_new_concept")
                                             ),## close add data tab
                                             mainPanel(
                                                 dataTableOutput('concept_table')
                                             )
                                         )
                                         ),## close codes tab panel
                                tabPanel("Codes",
                                         sidebarLayout(
                                             sidebarPanel(
                                                 tags$h2("Add new code"),
                                                 uiOutput("addsubmit_new_code")
                                             ),## close add data tab
                                             mainPanel(
                                                 dataTableOutput('code_table')
                                             )
                                         )
                                         ),## close codes tab panel
                                tabPanel("Codings",
                                         dataTableOutput('coding_table')
                                         ),## close codes tab panel
                                tabPanel("Coded data",
                                         ## Button
                                         downloadButton("download_coded",
                                                        "Download the table ('.csv')"),
                                         dataTableOutput('coded')
                                         ),## close coded tab panel
                                tabPanel("Units",
                                         sidebarLayout(
                                             sidebarPanel(
                                                 tags$h2("Add new unit"),
                                                 textInput("new_unit",  "Unit name"),
                                                 uiOutput('add_new_unit')
                                             ),## close add data tab
                                             mainPanel(
                                                 dataTableOutput('units_table')
                                             )
                                         )
                                         ),## close units panel
                                tabPanel("Summary",
                                         dataTableOutput('code_freq')
                                         )
                                )
                     )
}

## Define server logic
server <- function(input, output, session) {

    ## Select the project directory
    user_folder <- c('Select Volume' = Sys.getenv("HOME"))
    if (user_folder != ""){
        shinyDirChoose(input, 'select_project',  roots = user_folder)
    }
    observeEvent(c(input$select_project, input$file, input$update),{
        req(input$select_project)
        if (input$select_project[1] == ""){return()}
        output$project_directory <- renderPrint({parseDirPath(user_folder,
                                                              input$select_project)
        })

        if (as.character(input$select_project[1]) == "1" |
            input$select_project[1] == "" ) {return()}
        project_path <<- parseDirPath(user_folder, input$select_project)
        docs_df_path <<- paste0(project_path,
                                "/data_frames/qcoder_documents_",
                                basename(project_path), ".rds")
        concepts_df_path <<- paste0(project_path,
                                    "/data_frames/qcoder_concepts_",
                                    basename(project_path), ".rds")
        codes_df_path <<- paste0(project_path,
                                 "/data_frames/qcoder_codes_",
                                 basename(project_path), ".rds")
        codings_df_path <<- paste0(project_path,
                                   "/data_frames/qcoder_codings_",
                                   basename(project_path), ".rds")
        units_df_path <<- paste0(project_path,
                                 "/data_frames/qcoder_units_",
                                 basename(project_path), ".rds")
        units_docs_path <<- paste0(project_path,
                                   "/data_frames/qcoder_unit_document_map_",
                                   basename(project_path), ".rds")

        project_status <- reactiveValues(saved = TRUE,
                                         addingcode = FALSE,
                                         addingconcept = FALSE
                                         )
        
        ## coding_status <- reactiveValues(select_code="none")
        
        my_choices <- reactive({
            req(input$select_project)
            if (input$select_project[1] == ""){return()}
            text_df <- readRDS(file = docs_df_path)
            options <- text_df["doc_path"]
            options <- c(" ", options)
            options
        })

        output$choices <- renderUI({
            req(input$select_project)
            if (input$select_project[1] == ""){return()}

            if (docs_df_path == "") {return()}
                selectInput('this_doc_path', 'Document', my_choices())
        })

        ## observeEvent(input$this_doc_path,{
        ##     project_status$working_doc_id <- readRDS(file = docs_df_path) %>%
        ##         dplyr::filter(doc_path == input$this_doc_path) %>%
        ##         dplyr::select(doc_id) %>%
        ##         pull()
        ## })

        output$saveButton <- renderUI({
            if (project_status$saved) {
                saving.alert <- "check-circle"
            } else {
                saving.alert <- "exclamation-triangle"
            }        
            actionButton("submit", "Save changes (docs) & Backup codings",icon= icon(saving.alert))
        })

        observeEvent(input$submit,{
            project_status$saved=TRUE
        })

        ## Functions related to rendering an individual text document in an editor and
        ## verbatim
        ## Consider making a backup each time you load this.
        doc <- reactive ({
            if (is.null(input$this_doc_path)) {return()}
            if (docs_df_path == "") {return()}
            ## move to utils
            text_df <- readRDS(docs_df_path)
            if (length(text_df) == 0){return()}

            this_doc <- text_df %>%
                filter(doc_path == as.character(input$this_doc_path)) %>%
                select(document_text)
            ## Sanitize this
            return(as.character(this_doc[1, "document_text"]))
        })

        comps <- list()
        if (codes_df_path == "" | is.null(codes_df_path)) {return()}
        code_df <- readRDS(codes_df_path)
        comps[["codes"]] <- code_df["code"]
        comps[["tags"]] <- c("QCODE",  "{#")

        concepts_df <- reactive({
            input$update
            input$submit_new_concept
            if (concepts_df_path == "") {return()}
            concept_df <- readRDS(concepts_df_path)
            return(concept_df)
        })

        concepts <- reactive({
            input$update
            input$submit_new_concept
            if (concepts_df_path == "") {return()}
            concept_df <- concepts_df()
            concept_l <- as.list(as.character(concept_df[["concept_id"]]))
            names(concept_l)  <- as.character(concept_df[["concept"]])
            return(concept_l)
        })
        
        codes <- reactive({
            if (codes_df_path == "") {return()}
            code_df <- readRDS(codes_df_path)
            return(code_df["code"])
        })
        
        ## Adding a new concept
        output$addsubmit_new_concept <- renderUI({
            tagList(
                actionButton(
                    "add_new_concept",
                    "Add a new concept",
                    icon = icon("plus"))
            )
        })
        
        observeEvent(input$add_new_concept, {
            project_status$addingconcept=TRUE
            output$addsubmit_new_concept <- renderUI({
                tagList(
                    textInput("new_concept",
                              label = "New concept"
                              ),
                    textInput("new_concept_description",
                              label = "Description",
                              "NA"
                              ),
                    selectInput(inputId = "new_concept_class",
                             label = "Class of the concept",
                             choices = project_concept_class
                             ),
                    actionButton("submit_new_concept", "Submit new concept",
                                 icon = icon("share-square"))
                )
            })
        })
                
        observeEvent(input$submit_new_concept, {
            req(input$new_concept,
                input$new_concept_description,
                input$new_concept_class)
            project_status$addingconcept = FALSE
            x <- readRDS(concepts_df_path)
            qcoder::add_new_concept(input$new_concept,
                                    input$new_concept_description,
                                    input$new_concept_class,
                                    x, concepts_df_path)
        })
        
        ## Adding a new code
        output$addsubmit_new_code <- renderUI({
            tagList(
                actionButton(
                    "add_new_code",
                    "Add a new code",
                    icon = icon("plus"))
            )
        })
        
        observeEvent(input$add_new_code,{
            project_status$addingcode=TRUE
            output$addsubmit_new_code <- renderUI({
                tagList(
                    textInput("new_code",
                              label = "New code"
                              ),
                    textInput("new_code_description",
                              label = "Description"
                              ),
                    actionButton("submit_new_code", "Submit new code",
                                 icon = icon("share-square"))
                )
            })
            
        })
        
        observeEvent(input$submit_new_code, {
            req(input$new_code,input$new_code_description)
            project_status$addingcode=FALSE
            x <- readRDS(codes_df_path)
            qcoder::add_new_code(input$new_code,
                                 input$new_code_description,
                                 x, codes_df_path)
        })

        concepts_classes <- reactive({
            input$update
            input$submit_new_concept
            if (concepts_df_path == "") {return()}
            concepts_df <- concepts_df()
            return(concepts_df$concept.class)
        })
        
        concepts_from <- reactive({
            input$update
            input$submit_new_concept
            ## input$select_concept_from_class
            if (concepts_df_path == "") {return()}
            concept_df <- concepts_df() %>%
                dplyr::filter(concept.class %in% input$select_concept_from_class)
            concept_l <- as.list(as.character(concept_df[["concept_id"]]))
            names(concept_l)  <- as.character(concept_df[["concept"]])
            return(concept_l)
        })
        
        concepts_to <- reactive({
            input$update
            input$submit_new_concept
            ## input$select_concept_to_class
            if (concepts_df_path == "") {return()}
            concept_df <- concepts_df() %>%
                dplyr::filter(concept.class %in% input$select_concept_to_class)
            concept_l <- as.list(as.character(concept_df[["concept_id"]]))
            names(concept_l)  <- as.character(concept_df[["concept"]])
            return(concept_l)
        })
        
        
        ## Elements for coding a document
        output$docpart <- renderUI({
            selectInput(inputId = "document_part",
                        label = "Part of the document",
                        choices = project_document_part)
        })
        output$fromclass <- renderUI({
            selectInput(inputId = "select_concept_from_class",
                        label = "Add a relationship from (class) ",
                        choices = concepts_classes()
                        )
        })
        output$from <- renderUI({
            selectInput(inputId = "select_concept_from",
                        label = "Add a relationship from",
                        choices = concepts_from()
                        )
        })
        output$toclass <- renderUI({
            selectInput(inputId = "select_concept_to_class",
                        label = "Add a relationship to (class) ",
                        choices = concepts_classes()
                        )
        })
        output$to <- renderUI({
            selectInput(inputId = "select_concept_to",
                        label = "to ",
                        choices = concepts_to()
                        )
        })
        output$weight <- renderUI({
            sliderInput(inputId = "coding_weight",
                        label = "Weight of the coding",
                        min = 1, max = 7, value = 1, step = 1)
            })
        output$sign <- renderUI({
            radioButtons(inputId = "coding_sign",
                         label = "Sign of the coding",
                         choices = c("-", "+"),
                         selected = "+",
                         inline = TRUE)
            })
        output$class <- renderUI({
            selectInput(inputId = "coding_class",
                        label = "Classe(s) of the coding",
                        choices = project_coding_class)
        })
        
        ## Create the text editor
        output$mydocA <- renderUI({
            list(useShinyjs(),               
                 aceEditor(
                     editor_name,
                     value = doc(),
                     mode = "markdown",
                     height = "500",
                     wordWrap = TRUE,
                     autoComplete = "live",
                     autoCompleters = "static",
                     selectionId = "selected",
                     cursorId = "cursorpos",
                     autoCompleteList = comps
                 )
                 )
        })

        observeEvent(input$replace,{
            project_status$saved=FALSE
        })

        output$this_doc <-{renderText(qcoder::txt2html(doc()))}


        ## Get the code data for display
        output$coding_table <- DT::renderDataTable({
            if (codings_df_path == "") {return()}
            coding_df <- readRDS(codings_df_path)
            DT::datatable(coding_df,options = list(paging = FALSE))
        })

        ## Get the code data for display
        output$concept_table <- DT::renderDataTable({
            if (concepts_df_path == "") {return()}
            concept_df <- readRDS(concepts_df_path)
            DT::datatable(concept_df,options = list(paging = FALSE))
        })

        ## Get the code data for display
        output$code_table <- DT::renderDataTable({
            if (codes_df_path == "") {return()}
            code_df <- readRDS(codes_df_path)
            DT::datatable(code_df,options = list(paging = FALSE))
        })

        ## Get the units data for display
        p("Units are units of analysis which might be individuals, organizations,
        events, locations or any other entity relevant to the project.")
        output$units_table <- DT::renderDataTable({
            if (units_df_path == "") {return()}
            units_df <- readRDS(units_df_path)
            DT::datatable(units_df,options = list(paging = FALSE))
        })

        ## Get the parsed values with codes.
        output$coded <- DT::renderDataTable({
            if (docs_df_path == "" | codes_df_path == "" ) {return()}
            text_df <- readRDS(docs_df_path)
            code_df <- readRDS(codes_df_path)
            parsed <- qcoder::parse_qcodes(text_df, save_path = codes_df_path, code_data_frame = code_df)

            DT::datatable(parsed,options = list(paging = FALSE))
        })

        ## Export the coding table (parsed values with codes ?)
        output$download_coded <- downloadHandler(
            filename = paste("coded-data-", Sys.Date(), ".csv", sep=""),
            content = function(file) {
                if (docs_df_path == "" | codes_df_path == "" ) {return()}
                text_df <- readRDS(docs_df_path)
                code_df <- readRDS(codes_df_path)
                parsed <- qcoder::parse_qcodes(text_df, save_path = codes_df_path, code_data_frame = code_df)          
                write.csv(parsed, file, row.names = FALSE)
            }
        )

        output$code_freq <- DT::renderDataTable({
            if (docs_df_path == "" | codes_df_path == "" ) {return()}
            text_df <- readRDS(docs_df_path)
            code_df <- readRDS(codes_df_path)
            parsed <- qcoder::parse_qcodes(text_df)
            parsed %>% dplyr::group_by(as.factor(qcode)) %>%
                dplyr::summarise(n = n()) %>%
                    rename('code'='as.factor(qcode)') %>%
                    DT::datatable(options = list(paging = FALSE))
        })
    }) #close observer


    ## Functions related to updating the text.
    new_text <- reactive({
        input$aceeditor
    })

    observeEvent(input$replace, {          
        req(input$select_concept_from,
            input$select_concept_to,
            input$coding_sign,
            input$coding_weight,
            input$coding_class,
            input$document_part,
            input$selected)
        x <- readRDS(codings_df_path)
        doc <- readRDS(docs_df_path)
        qcoder::add_new_coding(
                    doc$doc_id[doc$doc_path == input$this_doc_path],
                    input$select_concept_from,
                    input$select_concept_to,
                    input$coding_sign,
                    input$coding_weight,
                    input$coding_class,
                    input$document_part,
                    input$selected,
                    x, codings_df_path)
    })

    
    update_editor <- observeEvent(input$replace, {
        select_code <- paste(input$select_concept_from,
                             input$select_concept_to,
                             sep=">")
        
        x <- readRDS(codes_df_path)
        if (!(select_code %in% x$code)) {
            qcoder::add_new_code(select_code,
                                 '', ## to be amended or to be define in another function
                                 x, codes_df_path)
        }
        
        text_old <- new_text()
        codes <- select_code
        selected <- input$selected
        if (length(selected) == 0) {return(message("No text selected"))}

        updated_selection <- qcoder:::add_codes_to_selection(selection = selected, codes = codes)

        updated_text <- qcoder:::replace_selection(text_old, selected, updated_selection)

        updateAceEditor(session=session, editorId=editor_name, value=updated_text)
        ## put js code to move cursor here
        jump_to <- input$cursorpos
        ## print(jump_to)
        row_num <- jump_to$row + 1
        col_num <- jump_to$column + (nchar(updated_selection) - nchar(selected))

        ## print(jump_to)

        js_statement <- paste0("editor__",editor_name,".focus(); editor__",editor_name,".gotoLine(", row_num, ",", col_num, ");")
        ## print(js_statement)

        shinyjs::runjs(js_statement)
    })


    update_document <- observeEvent(input$submit,
    {
        qcoder::do_update_document(new_text(), docs_df_path = docs_df_path,
                                   this_doc_path = input$this_doc_path)
        codes <- get_codes(new_text())
        if (length(codes) > 0){
            x <- readRDS(codes_df_path)
            qcoder::add_discovered_code(codes, x, codes_df_path)
        }
    }
    )

    backup_codings <- observeEvent(input$submit,
    {
        qcoder::do_update_codings()
    }
    )

    ## Adding a new document
    observeEvent(c(input$add_new_document,input$update),{
        req(input$select_project)
        doc_folder <- c(paste0(project_path, "/documents/"))
        text_df <- readRDS(docs_df_path)
        old_docs <- text_df[["doc_path"]]
        files.series <- list.files(doc_folder)
        files.series <- setdiff(files.series,old_docs)
        
        output$selectsend_new_document <- renderUI({
            tagList(
                selectInput("file",
                            label = "Select a new file in the document folder of the project",
                            choices = files.series
                            ),
                actionButton("send_new_document", "Send new document",
                             icon = icon("share-square"))
            )
        })
    })
    
    
    observeEvent(input$send_new_document, {
        doc_folder <- c(paste0(project_path, "/documents/"))
        files <- list(name = input$file)
        if (files != "") {
            qcoder::add_new_documents(files, docs_df_path, doc_folder)
        }  else {
            warning("no new file selected")
        }
    })

    ## Set up for associating units and documents
    observeEvent(c(input$select_project,input$update), {
        output$checkbox_links <- renderUI({
            units_df <- as.data.frame(readRDS(units_df_path))
            units_docs_df <- as.data.frame(readRDS(units_docs_path))

            checknames <- units_df$name
            checkvalues <- units_df$unit_id
            this_selected_df <- units_docs_df %>% filter(doc_path == input$this_doc_path)
            this_selected <- as.character(this_selected_df$unit_id)

            checkboxGroupInput(inputId =  "unit_doc_links", label = "Units connected to this document:",
                               choiceNames = checknames,
                               choiceValues = checkvalues,
                               selected = this_selected
                               )
        })

        output$checkbox_save_links <- renderUI({
            actionButton("save_links", "Save links")
        })
    })

    observeEvent(input$save_links, {
        checks <- input$unit_doc_links
        qcoder::update_links(checked = checks, docs_df_path = docs_df_path,
                             this_doc_path = input$this_doc_path,
                             units_docs_path = units_docs_path)
    })

    output$add_new_unit <- renderUI({
        actionButton("add_new_unit", "Add unit")
    })

    observeEvent(   input$add_new_unit, {
        units_df <- readRDS(units_df_path)
        qcoder::add_unit(units_df, input$new_unit, units_df_path)
    })
}## close server

## Run the application
shinyApp(ui = ui, server = server)

