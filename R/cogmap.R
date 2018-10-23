#'  Create a file of concepts from csv file
#'  Use this is you have a spreadsheet of concepts already created.
#'
#' @param file_path Path to a file containing  concept data in csv.
#' @param data_frame_name The name of the RDS file that the data frame will be stored in.
#' @param project_name Name of the project, which matches folder name
#' @param project_path Full path to the project folder
#' @param concepts_df_path Full path to the concepts data frame.
#'
#' @examples
#'  \dontrun{
#' fp <-"inst/example_concepts/"
#' dfn <- "test_concepts"
#' read_concept_data(fp, dfn)
#' }
#' @export
read_concept_data <- function(file_path = "concepts/concepts.csv", concepts_df_path = "",
                           data_frame_name = "qcoder_concepts", project_name, project_path = ""){
    if (project_path == ""){
        project_path <- paste0(getwd(), "/", project_name)
    }

    if (!is.null(project_name)){
      file_path <- paste0(project_path, "/", file_path)
      concepts_df_path <- paste0(project_path, "/data_frames/",
                                data_frame_name, "_", project_name, ".rds" )
    }

  if (file.exists(file_path)){
      concept_data <- readr::read_csv(file = file_path,
                                  col_types = readr::cols(concept_id = "i",
                                                   concept = "c",
                                                   concept.description = "c"))
      # validate column names etc here
      concept_data$concept <- as.factor(concept_data$concept)
   } else {
      concept_data <- create_empty_concept_file(project_name = project_path , concepts_df_path = concepts_df_path)
   }

  # try catch this save
  saveRDS(concept_data, file = concepts_df_path)
  invisible(TRUE)
}

#' Create an empty concepts data set
#'
#' Used to create a concepts data frame with no data but that can
#' have data added.
#'
#' @param data_frame_name Name of the data frame to be created
#' @param project_name Name of the project that the concepts are associated with
#' @param file_path Path to the file location to be used (under project root).
#'                  defaults to "data_frames".
#' @param concepts_df_path The full path to the concepts data frame
#' @param project_path  The full path to the project folder
#'
#' @export
create_empty_concept_file <- function( data_frame_name = "qcoder_concepts",
                                   concepts_df_path = "",
                                   project_path = "",
                                   file_path = "data_frames",
                                   project_name = ""){
  if (project_path == "" & project_name != ""){
    project_path <- paste0(getwd(), '/',project_name)
  }
  if (project_name != ""){
    file_path <- paste0(project_path, "/", file_path)
    concepts_df_path <- paste0(file_path, "/",
                              data_frame_name, "_", project_name, ".rds" )
  }

  cn <- c("concept_id", "concept", "concept.description")
  concept_data <- as.data.frame(matrix(data = NA, 0, length(cn)))
  colnames(concept_data) <- cn
  concept_data$concept_description <- as.character(concept_data$concept.description)
  concept_data$concept_id <- as.numeric(concept_data$concept_id)
  concept_data$concept <-as.factor(concept_data$concept)
  saveRDS(concept_data, file = concepts_df_path)

}

#' Update concepts data frame with description
#' Add concept to the concepts data frame
#'
#' @param new_concept The name of a concept (usually from a add concept form)
#' @param concept_data_frame Existing data frame of QCODER concepts
#' @param concepts_df_path The path where the updated concept data frame should be saved
#'
#' @export
add_new_concept <- function(new_concept = "" , description = "" , concept_data_frame = NULL , concepts_df_path = "" ){
    concept_data_frame <- as.data.frame(concept_data_frame)
    old_concepts <- as.character(concept_data_frame[,"concept"])
    new_concept <- unique(new_concept)
    concept <- setdiff(new_concept, old_concepts)
    if (length(concept) > 0){
      concept_id <- integer(length(concept))
      concept.description <- description
      new_rows <- data.frame(concept_id, concept, concept.description)

      concept_data_frame <- rbind(concept_data_frame, new_rows)
      row_n <- row.names(concept_data_frame)
      concept_data_frame$concept_id[length(old_concepts):
                      (length(old_concepts) + length(concept))] <-
        row_n[length(old_concepts):(length(old_concepts) + length(concept))]

      saveRDS(concept_data_frame, file = concepts_df_path )
    }
}
