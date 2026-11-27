## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(REDCapDM) 
library(kableExtra)
library(knitr)
library(dplyr)
library(purrr)

## ----message=FALSE, warning=FALSE, fig.width=8, fig.height=5, eval=requireNamespace("ggplot2", quietly = TRUE)----
rd_missplot(covican)

## ----message=FALSE, warning=FALSE, fig.width=8, fig.height=7, eval=requireNamespace("ggplot2", quietly = TRUE)----
covican_30 <- covican
covican_30$data <- covican$data |> 
  dplyr::filter(record_id %in% unique(record_id)[1:30])

rd_missplot(covican_30, by = "record", events = "all", forms = c("vital_signs", "laboratory_findings"))

