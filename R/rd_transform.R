#' Transformation of the raw data
#'
#' Function that transforms the raw data from REDCap read by the function `redcap_data`. It returns the transformed data and dictionary along with the summary of the results of each step.
#' @param ... List containing the data and the dictionary and the event_form if it's needed. Can be the output of the function `redcap_data`.
#' @param data Data frame containing data from REDCap. If the list is specified this argument is not needed.
#' @param dic Data frame  containing the dictionary read from REDCap. If the list is specified this argument is not needed.
#' @param event_form Data frame  containing the correspondence of each event with each form. If the list is specified this argument is not needed.
#' @param event_path Character string with the pathname of the file with the correspondence between each event and each form (it can be downloaded in `Designate Instruments for My Events` inside the `Project Setup` section of REDCap).
#' @param checkbox_labels Character vector with the names that will have the two options of every checkbox variable. Default is `c('No', 'Yes')`.
#' @param checkbox_na Logical indicating if values of checkboxes that have a branching logic have to set to missing only when the branching logic is missing (meaning that some of the variables specified in it are missing) or also when the branching logic isn't satisfied (true).
#' @param exclude_to_factor Character vector with the names of the variables that do not have to be transformed to factors.
#' @param keep_labels Logical indicating if the labels have to be kept or not.
#' @param delete_vars Character vector specifying the pattern that will contain variables to exclude. By default, variables ending up with `_complete` will be removed.
#' @param final_format Character string indicating the final arrangement format of the data that the function will return. Choose one of `raw`, `by_event` or `by_form`. `raw` (default) will return the transformed data with the original structure. `by_event` will return the transformed data as a nested data frame by event. `by_form` will return the transformed data as a nested data frame by form.
#' @param which_event Character string indicating if only one event has to be returned if the final format selected is `by_event`.
#' @param which_form Character string indicating if only one form has to be returned if the final format selected is `by_form`.
#' @param wide Logical indicating if the data split by form (if selected) has to be in a wide format or in a long one.
#' @return List with the transformed dataset, dictionary and the results
#'
#' @examples
#' rd_transform(covican)
#'
#' # For customization of checkbox labels
#' rd_transform(covican,
#'              checkbox_labels = c("Not present", "Present"))
#'
#' @export

rd_transform <- function(..., data = NULL, dic = NULL, event_form = NULL, event_path = NULL, checkbox_labels = c("No", "Yes"), checkbox_na = FALSE, exclude_to_factor = NULL, keep_labels = FALSE, delete_vars = c("_complete", "_timestamp"), final_format = "raw", which_event = NULL, which_form = NULL, wide = NULL){

  results <- NULL
  ind <- 1

  project <- c(...)
  if(!is.null(project)){
    if(!is.null(data)){
      warning("Data has been specified twice so the function will not use the information in the data argument.")
    }

    if(!is.null(dic)){
      warning("Dictionary has been specified twice so the function will not use the information in the dic argument.")
    }

    data <- project$data
    dic <- project$dictionary

    if("event_form" %in% names(project)){
      if(!is.null(event_form)){
        warning("The event-form has been specified twice so the function will not use the information in the event_form argument.")
      }
      event_form <- project$event_form
    }
  }

  if(is.null(data) | is.null(dic)){
    stop("No data/dictionary was provided")
  }


  #Check if the project is longitudinal (has more than one event) or not:
  longitudinal <- ifelse("redcap_event_name" %in% names(data), TRUE, FALSE)

  if(final_format == "by_event" & is.null(event_form)){
    stop("To split the data by event the event_form has to be provided", call. = FALSE)
  }

  #If the project is not longitudinal 'by_form' can be used without event_form:
  if(final_format == "by_form" & is.null(event_form) & longitudinal){
    stop("To split the data by form the event_form has to be provided in a longitudinal project", call. = FALSE)
  }

  if(!is.null(which_event) & final_format != "by_event"){
    stop("Which event has been specified but the final format is not to split the data by event", call. = FALSE)
  }

  if(!is.null(which_form) & final_format != "by_form"){
    stop("Which form has been specified but the final format is not to split the data by form", call. = FALSE)
  }

  if(!is.null(wide) & final_format != "by_form"){
    stop("The argument wide has been specified but the final format is not to split the data by form", call. = FALSE)
  }

  if(!final_format %in% c("raw", "by_event", "by_form")){
    stop("final_format argument has to be one of the following: 'raw', 'by_event', 'by_form'", call. = FALSE)
  }

  #If an event_path is provided, read the event file
  if(!is.null(event_path)){
    if(!is.null(event_form)){
      warning("The event-form has been specified twice so the function will not read the file in the event_path argument.")
    }else{
      event_form <- utils::read.csv(paste(event_path), encoding="UTF-8")
    }
  }

  #If the project is longitudinal and the event hasn't been specified:
  if(longitudinal & is.null(event_form)){
    warning("The project contains more than one event. For a complete transformation is recommended to include the event-form correspondence")
  }


  labels <- Hmisc::label(data)

  #Remove all variable labels:
  data <- data %>%
    dplyr::mutate_all(function(x){
      class(x) <- setdiff(class(x), "labelled")
      attr(x,"label") <- NULL
      x
    })

  #Recalculate calculated fields (previous to transforming factors and other preprocessing)
  #It wil create duplicate variables of each calculated field with "_recalc" in the end and the recalculated value

  results <- c(results, stringr::str_glue("{ind}. Recalculating calculated fields and saving them as '[field_name]_recalc'\n\n"))
  ind <- ind + 1

  #If the project is longitudinal and the event hasn't been specified no recalculation was possible
  if(longitudinal & is.null(event_form)){

    results <- c(results, "\nNo recalculation was possible as the project has more than one event and the event-form correspondence has not been specified\n")

  }else{

    recalc <- recalculate(data, dic, event_form)

    data <- recalc$data
    dic <- recalc$dic

    results <- c(results, recalc$results)

  }


  if(checkbox_na){
    results <- c(results, stringr::str_glue("\n\n{ind}. Transforming checkboxes: changing their values to No/Yes and changing their names to the names of its options. For checkboxes that have a branching logic, when the logic isn't satisfied their values will be set to missing\n\n"))
  }else{
    results <- c(results, stringr::str_glue("\n\n{ind}. Transforming checkboxes: changing their values to No/Yes and changing their names to the names of its options. For checkboxes that have a branching logic, when the logic is missing their values will be set to missing\n\n"))
  }

  ind <- ind + 1

  #Identify checkbox variables:
  var_check<-names(data)[grep("___",names(data))]
  #Remove .factor:
  var_check_factors <- var_check[grep(".factor$",var_check)]

  data <- data %>%
    dplyr::select(-tidyselect::all_of(var_check_factors))

  var_check <- var_check[!grepl(".factor$",var_check)]

  #If there is some checkbox:
  if(length(var_check) > 0){

    #Transform missings of checkboxes with branching logic:

    trans <- transform_checkboxes(data, dic, event_form, checkbox_labels, checkbox_na)

    data <- trans$data

    results <- c(results, trans$results)

    #Transform them to No/Yes:

    data <- data %>%
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(var_check),
        ~ factor(.x, levels = 0:1, labels = checkbox_labels)
      ))

    #Change the names:

    data_dic <- checkbox_names(data, dic, labels, checkbox_labels)

    data <- data_dic$data
    dic <- data_dic$dic

    #If the project is longitudinal and the event_form hasn't been specified no branching logic evaluation was possible
    if(longitudinal & is.null(event_form)){
      results <- c(results, "\nBranching logic evaluation was not possible as the project has more than one event and the event-form correspondence has not been specified\n")
    }

  }else{
    results <- c(results, "\nNo checkboxes have been found in the data\n")
  }

  #Replace original variables with their factor version except for redcap_event_name and redcap_data_access_group
  #If we dont want to convert another additional variable to factor we can specify it with the exclude argument: to_factor(data, exclude = "var")

  results <- c(results, stringr::str_glue("\n\n{ind}. Replacing original variables for their factor version"))
  ind <- ind + 1

  data <- to_factor(data, exclude = exclude_to_factor)

  #Fix variables that instead of missing have an empty field (text variables, etc.):
  data <- data %>%
    #Fix characters:
    dplyr::mutate_if(is.character,  ~ gsub("^$", NA, .x)) %>%
    #Fix factors:
    dplyr::mutate_if(is.factor,function(x){levels(x)[levels(x)==""] <- NA; x})

  #Convert dates (dates to as.date and dates & hours to posicxt):

  names_d <- names(data)[grepl("^d_", names(data))]
  names_posicxt <- names_d[purrr::map_lgl(names_d, function(x){
    if(any(grepl(":00", data[,x]))){
      TRUE
    }else{
      FALSE
    }
  })]
  names_d <- names_d[!names_d %in% names_posicxt]

  data <- data %>%
    dplyr::mutate_at(names_d, as.Date) %>%
    dplyr::mutate_at(names_posicxt, as.POSIXct)

  #Only if we would want a labelled dataset:
  if(keep_labels){
    #Restore labels:
    change_lab<-function(i){
     labels[i]
    }
    Hmisc::label(data)<-purrr::map(1:ncol(data),change_lab)
  }


  results <- c(results, stringr::str_glue("\n\n{ind}. Deleting variables that contain some patterns"))
  ind <- ind + 1

  if(!is.null(delete_vars)){

    for(i in 1:length(delete_vars)){

      if(delete_vars[i] == "_complete"){
        data <- data %>%
          dplyr::select(!tidyselect::ends_with("_complete"))
      }else if(delete_vars[i] == "_timestamp"){
        data <- data %>%
          dplyr::select(!tidyselect::ends_with("_timestamp"))
      }else{
        data <- data %>%
          dplyr::select(!tidyselect::contains(delete_vars[i]))
      }

    }

  }

  #Arrange our dataset by record_id and event (will keep the same order of events as in redcap)
  if(longitudinal){
    data <- data %>%
      dplyr::arrange(.data$record_id, .data$redcap_event_name.factor)
  }


  #If an event_form is specified or if the project has only one event and by_form has been specified
  if(!is.null(event_form) | (final_format == "by_form" & !longitudinal)){

    if(!is.null(event_form)){
      var_noevent <- dic$field_name[! dic$form_name %in% event_form$form]

      if(length(var_noevent) > 0){

        results <- c(results, stringr::str_glue("\n\n{ind}. Erasing variables from forms that are not linked to any event"))
        ind <- ind + 1

        var_noevent <- var_noevent[var_noevent %in% names(data)]
        data <- data %>%
          dplyr::select(-var_noevent)
        dic <- dic %>%
          dplyr::filter(! .data$field_name %in% var_noevent)
      }

    }

    #Final arrangment

    if(final_format == "by_event"){

      results <- c(results,stringr::str_glue("\n\n{ind}. Final arrangment of the data by event"))
      ind <- ind + 1

      if(is.null(which_event)){

        data <- split_event(data,dic,event_form)

      }else{

        data <- split_event(data,dic,event_form,which=which_event)

      }

    }else if(final_format == "by_form"){

      results <- c(results, stringr::str_glue("{ind}. Final arrangment of the data by form"))
      ind <- ind + 1

      if(is.null(wide)){
        wide <- FALSE
      }

      if(is.null(which_form)){

        if(longitudinal){
          data <- split_form(data, dic, event_form, which = NULL, wide)
        }else{
          data <- split_form(data, dic, which = NULL, wide)
        }

      }else{
        if(longitudinal){
          data <- split_form(data, dic, event_form, which=which_form, wide)
        }else{
          data <- split_form(data, dic, which=which_form, wide)
        }
      }

    }

    list(
      data = data,
      dictionary = dic,
      results = stringr::str_glue("{results}")
    )

  }else{
    list(
      data = data,
      dictionary = dic,
      results = stringr::str_glue("{results}")
    )
  }


}
