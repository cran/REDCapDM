## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(REDCapDM) 
library(kableExtra)
library(knitr)
library(dplyr)
library(purrr)

## ----message=FALSE, warning=FALSE, eval = FALSE, comment="#>", collapse = TRUE----
# dataset |>
#     rd_delete_vars(pattern = c("_complete", "_timestamp")) |>
#     rd_dates() |>
#     rd_recalculate() |>
#     rd_checkbox() |>
#     rd_factor() |>
#     rd_dictionary() |>
#     rd_split(by = "event") # use "form" if not longitudinal

## -----------------------------------------------------------------------------
# Option A: delete by variable name
covican_deleted <- covican |> 
  rd_delete_vars(vars = c("potassium", "leuk_lymph"))

# Option B: delete by regex pattern (here, the inclusion and exclusion criteria)
covican_deleted <- covican |> 
  rd_delete_vars(pattern = c("^inc_", "^exc_"))

# Option C: delete every variable of a form
covican_deleted <- covican |> 
  rd_delete_vars(forms = "laboratory_findings")

## ----message=FALSE, warning=FALSE, comment="#>", collapse = TRUE--------------
covican_dates <- covican |> 
  rd_dates()

## ----message=FALSE, warning=FALSE, comment="#>", collapse = TRUE--------------
# Simulate a character date since covican already has the dates in the correct format
covican_dates <- covican
covican_dates$data <- covican_dates$data |> 
  dplyr::mutate(d_birth = as.character(d_birth))
# Check class before conversion

class(covican_dates$data$d_birth)

# Check class after conversion
covican_dates <- covican_dates |> 
  rd_dates()
class(covican_dates$data$d_birth)

## -----------------------------------------------------------------------------
covican_recalc <- covican |> 
  rd_recalculate()

# Print recalculation results
covican_recalc$results

## -----------------------------------------------------------------------------
# Exclude specific variables from recalculation
covican_recalc <- covican |> 
  rd_recalculate(exclude = "screening_fail_crit")

covican_recalc$results

## -----------------------------------------------------------------------------
# Default transformation: "No"/"Yes" labels & renamed variables
cb <- covican |> 
  rd_checkbox()

str(cb$data$underlying_disease_hemato_acute_myeloid_leukemia.factor)

## -----------------------------------------------------------------------------
# use the argument checkbox_names to choose the final format of the variable names
cb <- covican |> 
  rd_checkbox(checkbox_names = FALSE)

str(cb$data$underlying_disease_hemato___1)

## -----------------------------------------------------------------------------
cb <- covican |> 
  rd_checkbox(na_logic = "eval")

## -----------------------------------------------------------------------------
cb <- covican |> 
  rd_checkbox(checkbox_labels = c("Absent", "Present"))

str(cb$data$underlying_disease_hemato_acute_myeloid_leukemia.factor)

## -----------------------------------------------------------------------------
factored <- covican |> 
  rd_factor()

# Checking class of the variable
str(factored$data$available_analytics)

## -----------------------------------------------------------------------------
factored <- covican |> 
  rd_factor(exclude = c("available_analytics", "urine_culture"))

# The excluded variables keep both versions
str(factored$data$available_analytics)
str(factored$data$available_analytics.factor)

## -----------------------------------------------------------------------------
# Update dictionary after cleaning
dict_result <- covican |>
  rd_checkbox() |>
  rd_factor() |>
  rd_dictionary()

## -----------------------------------------------------------------------------
forms_data <- covican |>
  rd_split(by = "form")

forms_data$data

## -----------------------------------------------------------------------------
head(forms_data$data$df[[1]])

## -----------------------------------------------------------------------------
forms_data <- covican |>
  rd_split(by = "form", wide = TRUE)

## -----------------------------------------------------------------------------
events_data <- covican |>
  rd_split(by = "event")

events_data$data

## -----------------------------------------------------------------------------
# Example by form
baseline_data <- covican |>
  rd_split(by = "form", which = "demographics")

head(baseline_data$data)

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
cb <- covican |> 
  rd_checkbox()

#Before inserting missing values
table(cb$data$type_underlying_disease_haematological_cancer)

#Run with this function
cb2 <- covican |> 
  rd_checkbox() |> 
  rd_insert_na(vars = "type_underlying_disease_haematological_cancer",
               filter = "age < 65")

#After inserting missing values
table(cb2$data$type_underlying_disease_haematological_cancer)

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
logic_trans <- covican |> 
  rd_rlogic(logic = "if([exc_1]='1' or [inc_1]='0' or [inc_2]='0' or [inc_3]='0',1,0)",
            var = "screening_fail_crit")

logic_trans$rlogic

head(logic_trans$eval_data)

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
covican_transformed <- rd_transform(covican)

#Print the results of the transformation
covican_transformed$results

