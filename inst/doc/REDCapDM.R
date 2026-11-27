## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(REDCapDM) 
library(kableExtra)
library(knitr)
library(dplyr)
library(purrr)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("REDCapDM")

## ----eval=FALSE---------------------------------------------------------------
# install.packages("remotes") # Run this line if the 'remotes' package isn't installed already.
# remotes::install_github("bruigtp/REDCapDM")

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
library(REDCapDM)

data(covican)

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA---------------------
str(covican, max.level = 1)

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA---------------------
descr <- c("Identifier of each record", "",
           "Auto-generated name of the events", "",
           "Auto-generated name of each center", "",
           "Patients older than 18 years", "No ; Yes",
           "Cancer patients", "No ; Yes",
           "Diagnosed of COVID-19", "No ; Yes",
           "Solid tumour remission >1 year", "No ; Yes",
           "Indicator of non-compliance with inclusion and exclusion criteria", "Compliance ; Non-compliance",
           "Date of birth (y-m-d)", "",
           "Date of first visit (y-m-d)", "",
           "Age", "",
           "Indicator of diabetes", "No ; Yes",
           "Type of diabetes", "No complications ; End-organ diabetes-related disease",
           "Indicator of chronic pulmonary disease", "No ; Yes",
           "Fraction of inspired oxygen (%)", "",
           "Indicator of blood test available", "No ; Yes",
           "Potassium (mmol/L)", "",
           "Respiratory rate (bpm)", "",
           "Indicator of leukemia or lymphoma", "No ; Yes",
           "Indicator of acute leukemia", "No ; Yes")

vars <- data.frame("Name" = names(covican$data |> dplyr::select(record_id:acute_leuk)),
                   "Description" = descr[seq(1, length(descr), 2)],
                   "Categories" = descr[seq(2, length(descr), 2)])

kable(vars) |> 
  kableExtra::row_spec(0, bold=TRUE) |> 
  kableExtra::kable_styling(full_width = F)

## ----message=FALSE, warning=FALSE, comment=NA, eval=FALSE---------------------
# dataset <- redcap_data(data_path = "C:/Users/username/example.r",
#                        dic_path = "C:/Users/username/example_dictionary.csv")

## ----message=FALSE, warning=FALSE, comment=NA, eval=FALSE---------------------
# dataset <- redcap_data(data_path = "C:/Users/username/example.r",
#                        dic_path = "C:/Users/username/example_dictionary.csv",
#                        event_path = "C:/Users/username/events.csv")

## ----eval=FALSE, message=FALSE, warning=FALSE, comment=NA---------------------
# dataset_api <- redcap_data(uri = "https://redcap.example.org/api/",
#                            token = Sys.getenv("REDCAP_TOKEN"))

## ----message=FALSE, warning=FALSE, comment=NA, eval=FALSE---------------------
# dataset <- redcap_data(data_path = "C:/Users/username/example.r",
#                        dic_path = "C:/Users/username/example_dictionary.csv",
#                        rm_mdc = FALSE)

## ----message=FALSE, warning=FALSE, comment=NA, eval=FALSE---------------------
# dataset <- redcap_data(data_path = "C:/Users/username/example.r",
#                        dic_path = "C:/Users/username/example_dictionary.csv",
#                        mdc = c("-999" = "Not applicable", "-888" = "Unknown"))

## ----message=FALSE, warning=FALSE, eval = FALSE, comment="#>", collapse = TRUE----
# dataset |>
#     rd_delete_vars(pattern = c("_complete", "_timestamp")) |>
#     rd_dates() |>
#     rd_recalculate() |>
#     rd_checkbox() |>
#     rd_factor() |>
#     rd_dictionary() |>
#     rd_split(by = "event") # use "form" if not longitudinal

## ----message=FALSE, warning=FALSE, comment="#>", collapse = TRUE--------------
covican_transformed <- covican |> 
    rd_recalculate() |> 
    rd_checkbox() |> 
    rd_factor() |> 
    rd_dictionary() |> 
    rd_split(by = "event") 

covican_transformed$results

