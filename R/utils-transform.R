# Internal functions used for the pre-processing of 'rd_transform' function:
#'
#' Recalculate REDCap calculated fields
#' @description
#' Function that recalculates every calculated field if the logic can be transcribed to R. Recall that calculated fields with smart-variables in the logic or variables in other events cannot be transcribed.
#'
#' The function will return the dataset and dictionary with the added recalculated variables (the name of the calculated field + `_recalc`) along with a table that shows the summary of the results.
#' @param data Data frame containing data from REDCap.
#' @param dic Data frame  containing the dictionary read from REDCap.
#' @param event_form Data frame  containing the correspondence of each event with each form.
#' @importFrom rlang :=
#'
############Calculated functions############
recalculate <- function(data, dic, event_form = NULL){

  #Calculate for each calculated field the transcribed logic and if possible to transcribe recalculate it

  calc <- tibble::tibble(dic) %>%
    dplyr::filter(.data$field_type == "calc") %>%
    dplyr::mutate(
      calc = purrr::map(.data$field_name, function(x) {
          val <- data[, x]
          if(is.numeric(val)){
            as.numeric(val)
          }else{
            val
          }
      }),
      rlogic = purrr::map2(.data$choices_calculations_or_slider_labels, .data$field_name, function(x, y) {
        rlogic <- try(rd_rlogic(data = data, dic = dic, event_form = event_form, logic = x, var = y), silent = TRUE)
        if (!inherits(rlogic, "try-error")) {
          rlogic
        } else{
          NULL
        }
      }),
      trans = purrr::map_chr(.data$rlogic, function(x){
        if(!is.null(x)){
          x$rlogic
        }else{
          NA
        }
        }),
      recalc = purrr::map(.data$rlogic, function(x){
        if(!is.null(x)){
          x$eval
        }else{
          NA
        }
      }),
      calc = purrr::map2(.data$trans, calc, function(x, y){
        if(!is.na(x)){
          y
        }else{
          NA
        }
      }),
      is_equal = purrr::map2_lgl(.data$calc, .data$recalc, function(x, y){
        if(is.numeric(x) & is.numeric(y)){
          identical(round(x, 3), round(y, 3))
        }else{
          identical(x, y)
        }
      }),
      is_equal = ifelse(is.na(.data$trans), NA, .data$is_equal)
    ) %>%
    dplyr::select(-"rlogic")


  #Add this recalculated variables to data and dictionary and return both datasets

  calc_change <- calc %>%
    dplyr::filter(!is.na(.data$trans))

  if(nrow(calc_change) > 0){

    for(i in 1:nrow(calc_change)){

      name <- stringr::str_glue("{calc_change$field_name[i]}_recalc")

      data <- data %>%
        tibble::add_column("{name}" := calc_change$recalc[[i]], .after = calc_change$field_name[i])

      add_row <- dic %>%
        dplyr::filter(.data$field_name == calc_change$field_name[i]) %>%
        dplyr::mutate(
               field_name = stringr::str_glue("{field_name}_recalc"),
               field_label = stringr::str_glue("{field_label} (Recalculate)")
        )

      dic <- rbind(dic, add_row)

    }

  }

  #Summary of the results

  report1 <- calc %>%
    dplyr::mutate(n = 1) %>%
    dplyr::summarise(trans = sum(!is.na(.data$trans)),
              N = sum(.data$n),
              no_trans = .data$N - .data$trans,
              no_equal = sum(!.data$is_equal, na.rm = TRUE),
    ) %>%
    dplyr::mutate(text1 = stringr::str_glue("{no_trans} ({round(no_trans*100/N, 2)}%)"),
           text2 = stringr::str_glue("{no_equal} ({round(no_equal*100/trans, 2)}%)")
    ) %>%
    dplyr::select("Total calculated fields" = "N", "Non-transcribed fields" = "text1", "Recalculated different fields" = "text2")

  results <- knitr::kable(report1, "pipe", align = "ccc")

  report2 <- calc %>%
    dplyr::mutate(trans2 = ifelse(!is.na(.data$trans), "Yes", "No")) %>%
    dplyr::arrange(.data$trans2, .data$is_equal) %>%
    dplyr::select("field_name", "Transcribed?" = "trans2", "Is equal?" = "is_equal")

  results <- c(results, "\n", knitr::kable(report2, "pipe", align = "ccc"))

  list(
    data = data,
    dic = dic,
    results = results
  )

}


############Checkbox functions############

#' Transformation of checkboxes in case of having a branching logic
#'
#' @description
#' Inspects all the checkboxes of the study and looks if there is a branching logic. If there is one, when the logic of the branching logic is missing it directly inputs a missing to the checkbox. If checkbox_na is TRUE additionally it will put a missing when the branching logic isn't satisfied and not only when the logic is missing. If a branching logic cannot be found or the logic cannot be transcribed because of the presence of some smart variables, the variable is added in the list of the reviewable ones that will be printed.
#'
#' The function will return the dataset with the transformed checkboxes along with a table that shows a summary of the results.
#' @param data Data frame containing data from REDCap.
#' @param dic Data frame  containing the dictionary read from REDCap.
#' @param event_form Data frame  containing the correspondence of each event with each form.
#' @param checkbox_labels Character vector with the names that will have the two options of every checkbox variable. Default is `c('No', 'Yes')`.
#' @param checkbox_na Logical indicating if values of checkboxes that have a branching logic have to set to missing only when the branching logic is missing (meaning that some of the variables specified in it are missing) or also when the branching logic isn't satisfied (true).

transform_checkboxes <- function(data, dic, event_form, checkbox_labels = c("No", "Yes"), checkbox_na = FALSE){

  vars <- dic$field_name[dic$field_type=="checkbox"]
  results <- results1 <- results2 <- NULL
  caption <- "Checkbox variables advisable to be reviewed"

  if(length(vars) > 0){

    review <- NULL
    review2 <- NULL
    for(i in 1:length(vars)){

      #Identify all the variables in the database that belong to this checkbox (one for each one of the options)
      vars_data <- names(data)[grep(stringr::str_glue("{vars[i]}___"),names(data))]

      #Get the branching logic
      logic <- dic$branching_logic_show_field_only_if[dic$field_name==vars[i]]

      #If there is one
      if(logic!=""){

        #Translate the REDCap logic to r language using rd_rlogic function

        rlogic <- try(rd_rlogic(data = data, dic = dic, event_form = event_form, logic = logic, var = vars[i]), silent = TRUE)

        if(!inherits(rlogic, "try-error")){

          #Evaluate the logic
          rlogic_eval <- rlogic$eval

          #It will be missing when the logic isn't satisfied
          if(checkbox_na){
            for(j in 1:length(vars_data)){
              data[,vars_data[j]] <- ifelse(rlogic_eval,as.character(data[,vars_data[j]]),NA)
            }
          #It will be missing when the logic is missing
          }else{
            for(j in 1:length(vars_data)){
              data[,vars_data[j]] <- ifelse(!is.na(rlogic_eval),as.character(data[,vars_data[j]]),NA)
            }
          }


        }else{
          review2 <- c(review2, vars[i])
        }

      }else{

        #If there isn't a branching logic we put the variable to be reviewed
        review <- c(review,vars[i])

      }

    }

    #Summary with the results
    if(!is.null(review)){
      results1 <- tibble::tibble("Variables without any branching logic" = review)
      results <- knitr::kable(results1, "pipe", align = c("ccc"), caption = caption)
      if(!is.null(review2)){
        results <- c(results, "\n")
        caption <- NULL
      }
    }

    if(!is.null(review2)){
      results2 <- tibble::tibble("Variables with a logic that can't be transcribed" = review2)
      results <- c(results, knitr::kable(results2, "pipe", align = c("ccc"), caption = caption))
    }

  }else{
    warning("There isn't any checkbox variable in the dataset", call. = FALSE)
  }

  list(data = data,
       results = results)
}

#' Change checkboxes names into the name of their options
#' @description
#' Function that returns both data and dictionary with the name of the checkboxes transformed by the name of their options.
#' @param data Dataset containing the REDCap data.
#' @param dic Dataset containing the REDCap dictionary.
#' @param labels Named character vector with the name of the variables in the data and the REDCap label in its name.
#' @param checkbox_labels Character vector with the names that will have the two options of every checkbox variable. Default is c('No', 'Yes').

checkbox_names <- function(data,dic,labels, checkbox_labels = c("No", "Yes")){

  #Identify checkbox variables:
  var_check<-names(data)[grep("___",names(data))]

  #Add to dictionary all variables from checkbox found in the data and remove the original one (dic_checkboxes is an auxiliary function defined afterwards)
  dic <- dic_checkboxes(var_check,dic,labels, checkbox_labels)

  #Transform name in the bbdd and in the dictionary (transform_name is another auxiliary function defined afterwards)
  names(data) <- make.unique(purrr::map_chr(names(data),~transform_name(var_check,.x,labels)))
  dic$field_name <- make.unique(purrr::map_chr(dic$field_name,~transform_name(var_check,.x,labels)))

  out <- list(
    data=data,
    dic=dic
  )

  out

}

#' Change the names of checkboxes variables in the REDCap dictionary
#' @description
#' Auxiliary function to checkbox_names. Adds to the dictionary all variables that correspond to all the options of checkbox (with the name as it is in the data) and remove the original general checkbox variable.
#' @param var_check Character vector containing the names of those variables that are checkboxes.
#' @param dic  Dataset containing the REDCap dictionary.
#' @param labels Named character vector with the name of the variables in the data and the REDCap label in its name.
#' @param checkbox_labels Character vector with the names that will have the two options of every checkbox variable. Default is c('No', 'Yes').

dic_checkboxes <- function(var_check,dic,labels, checkbox_labels = c("No", "Yes")){

  names_trim <- NULL

  for(i in 1:length(var_check)){

    label <- labels[var_check[i]]
    label <- gsub(".*choice=","",label)
    label <- gsub("\\)","",label)

    names_trim[i] <- gsub("___.*$","",var_check[i])

    #Add rows with the name of all the variables for all the options
    new_row <- dic %>%
      dplyr::filter(.data$field_name==names_trim[i]) %>%
      dplyr::mutate(
             field_name=var_check[i],
             field_label=label,
             choices_calculations_or_slider_labels=stringr::str_glue("0, {checkbox_labels[1]} | 1, {checkbox_labels[2]}"))

    dic <- dic %>%
      tibble::add_row(new_row, .before = which(dic$field_name==names_trim[i]))


  }

  dic %>%
    #Remove the original checkbox variable that was present in the beginning
    dplyr::filter(!.data$field_name%in%names_trim)

}

#' Auxiliary function to `checkbox_names`
#' @description
#' Auxiliary function to checkbox_names. It changes the name of the checkbox variable to the name of the option it corresponds
#' @param var_check a character vector containing the names of those variables that are checkboxes
#' @param name a character element with the original name of the checkbox variable that has to be changed
#' @param labels a named character vector with the name of the variables in the data and the REDCap label in its name
transform_name <- function(var_check,name,labels){

  #It only transforms the name if the variable is a checkbox
  if(name%in%var_check){
    #Look the variable label and obtain the name of the corresponding option
    label <- labels[name]
    label <- gsub(".*choice=","",label)
    label <- janitor::make_clean_names(label)
    label <- gsub("^x(\\d)","\\1",label)

    #Replace the original number corresponding on the option to the name of the choice
    out <-  gsub("___.*","",name)
    out <- stringr::str_glue("{out}_{label}")

    #Trim name if it has more than 60 characters (if the option name is very large)
    strtrim(out, 60)
  }else{
    name
  }

}

############Structural functions##################

#' Creation of a data frame with variables of all the forms of a specified event
#' @description
#' Function that given the data, the dictionary and the mapping between forms and events it creates a dataset with the variables of all the forms that are in this event. It can be chosen to return only the data from the specified event.
#' @param data Data frame containing data from REDCap.
#' @param dic Data frame  containing the dictionary read from REDCap.
#' @param event_form Data frame  containing the correspondence of each event with each form.
#' @param which Specify an event if only data for the desired event is wanted.
split_event <- function(data,dic,event_form,which=NULL){

  #We create event-variable correspondence::
  var_event <- event_form %>%
    dplyr::select("form_name"="form","redcap_event_name"="unique_event_name") %>%
    dplyr::right_join(dic[,c("form_name","field_name","field_type","branching_logic_show_field_only_if")],by="form_name") %>%
    #Remove variables that we don't need (type descriptive and borrar/complete)
    dplyr::filter(.data$field_type!="descriptive",!(grepl("borrar_",.data$field_name) | grepl("_complete$",.data$field_name))) %>%
    tibble::as_tibble() %>%
    #Remove variables with event filters
    dplyr::mutate(
      # To be included:
      filtre_events = purrr::map(.data$branching_logic_show_field_only_if, ~unlist(stringr::str_extract_all(.x, "\\[event-name\\]='[\\w,\\-]+"))),
      filtre_events = purrr::map(.data$filtre_events, ~gsub("\\[event-name\\]='", "", .x)),
      # To be excluded:
      no_filtre_events = purrr::map(.data$branching_logic_show_field_only_if, ~unlist(stringr::str_extract_all(.x, "\\[event-name\\]<>'[\\w,\\-]+"))),
      no_filtre_events = purrr::map(.data$no_filtre_events, ~gsub("\\[event-name\\]<>'", "", .x)),
      #create variable for filtering
      logic = purrr::pmap_lgl(list(.data$redcap_event_name,.data$branching_logic_show_field_only_if, .data$filtre_events, .data$no_filtre_events), ~((length(..3) == 0 & length(..4) == 0) | (..1 %in% ..3) | (length(..4) > 0 & !..1 %in% ..4))),
      #extra logic to look for variables that is not event-name and has an or
      vars_clau = purrr::map(.data$branching_logic_show_field_only_if, ~unlist(stringr::str_extract_all(.x, "\\[[\\w,\\-]+\\]"))),
      logic = ifelse(grepl(" or ", .data$branching_logic_show_field_only_if) & !all(.data$vars_clau == "[event-name]"), TRUE, .data$logic)
    ) %>%
    #If the variable only is found in one event we keep it
    dplyr::group_by(.data$field_name) %>%
    dplyr::mutate(l = length(.data$field_name)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      logic = ifelse(!.data$logic & .data$l == 1, TRUE, .data$logic)
    ) %>%
    #Apply the filter generated before
    dplyr::filter(.data$logic) %>%
    dplyr::select("redcap_event_name", "field_name")

  var_event_add <- data.frame(redcap_event_name=unique(var_event$redcap_event_name)[-1],field_name="record_id")
  var_event <- rbind(var_event, var_event_add)%>%
    dplyr::mutate(redcap_event_name=factor(.data$redcap_event_name, levels=unique(var_event$redcap_event_name)))%>%
    dplyr::arrange(.data$redcap_event_name)

  #Add basic REDCap variables that by default aren't in the dictionary:

  basic_redcap_vars <- c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance","redcap_data_access_group","redcap_event_name.factor", "redcap_data_access_group.factor")

  #It can happen that one of these variables are not in the database for some projects
  basic_redcap_vars <- basic_redcap_vars[basic_redcap_vars%in%names(data)]
  var_event_redcap <- data.frame(redcap_event_name=NA,field_name=basic_redcap_vars)

  var_event <- rbind(var_event_redcap,var_event)

  vars_more <- var_event$field_name[!var_event$field_name%in%names(data)]

  if(length(vars_more)>0){
    stop("There're more variables in the dictionary than in the data base. Transformation stops", call. = FALSE)
  }

  vars_less <- names(data)[!names(data)%in%var_event$field_name]

  if(length(vars_less)>0){
    stop("There're more variables in the data base than in the dictionary. Transformation stops", call. = FALSE)
  }

  #Let's create a database for every event filtering variables found in every event

  list_events <- stats::na.exclude(unique(var_event$redcap_event_name))

  ndata <- tibble::tibble("events"=list_events) %>%
    dplyr::mutate(vars=purrr::map(.data$events,~var_event %>%
                      dplyr::filter(.data$redcap_event_name==.x | is.na(.data$redcap_event_name)) %>%
                      dplyr::pull("field_name")),
           df=purrr::map2(.data$events, .data$vars, ~data %>%
                     dplyr::filter(.data$redcap_event_name==.x) %>%
                     dplyr::select(tidyselect::all_of(.y))%>% dplyr::relocate(.data$record_id)))


  if(!is.null(which)){
    ndata$df[[which(ndata$events==which)]]
  }else{
    ndata
  }


}

#' Creation of a data frame with variables of a specified form
#' @description
#' Function that given the data, the dictionary and the mapping between forms and events it creates a dataset with the variables that are in this form for all events. It can be chosen to return only the data from the specified form.
#' @param data Data frame containing data from REDCap.
#' @param dic Data frame  containing the dictionary read from REDCap.
#' @param event_form Data frame  containing the correspondence of each event with each form.
#' @param which Specify a form if only data for the desired form is wanted.
#' @param wide If the dataset needs to be in a wide format or not (long).

split_form <- function(data, dic, event_form = NULL, which = NULL, wide=FALSE){

  #Check if the project is longitudinal or not:
  longitudinal <- ifelse("redcap_event_name" %in% names(data), TRUE, FALSE)

  if(longitudinal & is.null(event_form)){
    stop("To split the data by form the event_form has to be provided in a longitudinal project", call. = FALSE)
  }

  #Find basic REDCap variables found in the database
  basic_redcap_vars <- c("record_id","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance","redcap_data_access_group","redcap_event_name.factor", "redcap_data_access_group.factor", "redcap_survey_identifier")

  basic_redcap_vars <- basic_redcap_vars[basic_redcap_vars%in%names(data)]

  #Previous to begin with the transformation let's find if there're the same variables in the data base than in the dictionary
  #But first we have to remove from the dictionary the descriptive variables that are found in the dictionary but not in the data
  dic <- dic %>%
    dplyr::filter(.data$field_type!="descriptive",!(grepl("borrar_",.data$field_name) | grepl("_complete$",.data$field_name)))

  vars_more <- dic$field_name[!dic$field_name%in%names(data)]

  if(length(vars_more)>0){
    stop("There're more variables in the dictionary than in the data base. Transformation stops", call. = FALSE)
  }

  vars_less <- names(data)[!names(data)%in%dic$field_name]
  #Remove the REDCap basic variables that are found in the database but not in the dictionary
  vars_less <- vars_less[!vars_less %in% basic_redcap_vars]

  if(length(vars_less)>0){
    stop("There're more variables in the data base than in the dictionary. Transformation stops", call. = FALSE)
  }

  form <- unique(dic$form_name)

  if(longitudinal){
    ndata <- tibble::tibble("form"=form) %>%
      dplyr::mutate(events = purrr::map(.data$form, ~event_form$unique_event_name[event_form$form == .x]),
                    vars = purrr::map(.data$form, ~dic$field_name[dic$form_name == .x]),
                    #Add to vars the basic REDCap variables not found in the dictionary:
                    vars = purrr::map(.data$vars, ~unique(c(basic_redcap_vars, .x)))
      ) %>%
      #Collect variables from every event
      dplyr::mutate(df = purrr::map2(.data$events, .data$vars,~data %>%
                                       dplyr::filter(redcap_event_name==.x) %>%
                                       dplyr::select(tidyselect::all_of(.y))%>% dplyr::relocate(.data$record_id)))
  }else{
    ndata <- tibble::tibble("form"=form) %>%
      dplyr::mutate(vars = purrr::map(.data$form, ~dic$field_name[dic$form_name == .x]),
                    #Add to vars the basic REDCap variables not found in the dictionary:
                    vars = purrr::map(.data$vars, ~unique(c(basic_redcap_vars, .x)))
      ) %>%
      #Collect variables from every event
      dplyr::mutate(df = purrr::map(.data$vars,~data %>%
                                       dplyr::select(tidyselect::all_of(.x))%>% dplyr::relocate(.data$record_id)))
  }


  if(wide){

    #We will add it with the order that repeated measures happen for every patient

    ndata <- ndata %>%
      dplyr::mutate(vars_esp = purrr::map(.data$vars, ~.x[!.x %in% basic_redcap_vars]),
             df = purrr::map2(.data$vars_esp, .data$df, ~ .y %>%
                         dplyr::select("record_id", .x) %>%
                         dplyr::group_by(.data$record_id) %>%
                         dplyr::mutate(id = 1:length(.data$record_id)) %>%
                         dplyr::ungroup() %>%
                         tidyr::pivot_wider(names_from = .data$id, values_from = -c(.data$record_id, .data$id))
             )
      )
  }

  if(!is.null(which)){

    ndata$df[[which(ndata$form==which)]]

  }else{

    ndata

  }


}

###############Other functions###############

#' Convert variables to factors
#' @description
#' Function that converts every variable except those specified to factor.
#' @param data Dataset containing the REDCap data.
#' @param exclude Character vector containing the names of those variables that will not be converted to factors. If `NULL`, all variables will be converted.
to_factor <- function(data, exclude = NULL){

  #We need redcap_event_name to have the original values so we exclude of the conversion the variable redcap_event_name.factor. Also for redcap_data_access_group if present

  keep <- c("redcap_event_name.factor", "redcap_data_access_group.factor")
  keep_factors <- data %>%
    dplyr::select(keep[keep %in% names(data)])

  data$redcap_event_name.factor <- NULL
  data$redcap_data_access_group.factor <- NULL

  factors <- names(data)[grep("\\.factor$",names(data))]
  factors <- gsub("\\.factor$","",factors)

  #Exclude those variables that we don't want to convert to factors
  factors <- factors[!factors %in% exclude]

  data <- data %>%
    #Assign to the non factor variable the factor one and remove the later
    dplyr::mutate(dplyr::across(tidyselect::all_of(factors), ~ get(stringr::str_glue("{dplyr::cur_column()}.factor")))) %>%
    dplyr::select(-tidyselect::ends_with(".factor")) %>%
    tibble::add_column("redcap_event_name.factor" = keep_factors$redcap_event_name.factor, .after = "redcap_event_name")

  if("redcap_data_access_group" %in% names(data)){
    data %>%
      tibble::add_column("redcap_data_access_group.factor" = keep_factors$redcap_data_access_group.factor, .after = "redcap_data_access_group")
  }else{
    data
  }

}

#' Fill rows with the values in one event
#' @description
#' Function that with one particular variable and event it fills all the rows in the data with the value in that particular event
#' @param which_event String with the name of the event
#' @param which_var String with the name of the variable
#' @param data Dataset containing the REDCap data.

fill_data <- function(which_event, which_var, data){

  if(which_event %in% data$redcap_event_name){

    fill_values <- data %>%
      dplyr::select("record_id", "redcap_event_name", "var" = "which_var") %>%
      dplyr::group_by(.data$record_id) %>%
      dplyr::mutate(
             var = ifelse(.data$redcap_event_name != which_event, NA, .data$var),
             #Only the first value if the event is repeated
             var = stats::na.exclude(unique(.data$var))[1]
      ) %>%
      tidyr::fill(.data$var, .direction = "downup") %>%
      dplyr::pull("var")

    data[,which_var] <- fill_values

    data

  }else{

    stop("The logic can't be evaluated after the translation")

  }

}

