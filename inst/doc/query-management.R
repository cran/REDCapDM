## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(REDCapDM) 
library(kableExtra)
library(knitr)
library(dplyr)
library(purrr)

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
covican_transformed <- rd_transform(covican)

## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
example <- rd_query(covican_transformed,
                    variables = "copd",
                    expression = "is.na(x)")

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA---------------------
kable(head(example$queries)) |> 
  kableExtra::row_spec(0, bold = TRUE) |> 
  kableExtra::kable_styling()

example$results

## ----echo=TRUE, message=FALSE, warning=FALSE, comment=NA----------------------
example <- rd_query(covican_transformed,
                    variables = c("copd", "age"),
                    expression = c("is.na(x)", "is.na(x)"),
                    event = "baseline_visit_arm_1")

# Printing results
example$results

## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
covican_transformed$dictionary$branching_logic_show_field_only_if[covican_transformed$dictionary$field_name %in% "potassium"] <- "[available_analytics]='1' and [user-name]='admin'"

## ----message=FALSE, warning=TRUE, comment=NA----------------------------------
example <- rd_query(covican_transformed,
                    variables = c("age", "copd", "potassium"),
                    expression = c("is.na(x)", "is.na(x)", "is.na(x)"),
                    event = "baseline_visit_arm_1")

# Printing results
example$results

## ----message=FALSE, warning=TRUE, comment=NA----------------------------------
example <- rd_query(covican_transformed,
                    variables = c("potassium"),
                    expression = c("is.na(x)"),
                    event = "baseline_visit_arm_1",
                    filter = c("available_analytics=='Yes'"))

# Printing results
example$results

## ----message=FALSE, warning=TRUE, comment=NA----------------------------------
example <- rd_query(variables="age",
                    expression="x>70",
                    event="baseline_visit_arm_1",
                    dic=covican_transformed$dictionary,
                    data=covican_transformed$data)

# Printing results
example$results

## ----message=FALSE, warning=TRUE, comment=NA----------------------------------
example <- rd_query(covican_transformed,
                    variables=c("age", "copd"),
                    expression=c("x > 70", "x == 'Yes'"),
                    event="baseline_visit_arm_1")

# Printing results
example$results

## ----message=FALSE, warning=TRUE, comment=NA----------------------------------
example <- rd_query(covican_transformed,
                    variables="age",
                    expression="(x>70 & x<80) | is.na(x)",
                    event="baseline_visit_arm_1")

# Printing results
example$results

## ----message=FALSE, warning=TRUE, comment=NA----------------------------------
example <- rd_query(covican_transformed,
                    variables = c("copd","age","dm"),
                    expression = "is.na(x)",
                    event = "baseline_visit_arm_1")

# Printing results
example$results

## ----message=FALSE, warning=TRUE, comment=NA----------------------------------
example <- rd_query(covican_transformed,
                    variables = "copd",
                    expression = "is.na(x)")

# Printing results
example$results

## ----message=FALSE, warning=TRUE, comment=NA----------------------------------
my_list <- subset(covican_transformed, !names(covican_transformed) %in% "event_form")

example <- rd_query(my_list,
                    variables = "copd",
                    expression = "is.na(x)")

# Printing results
example$results

## ----echo=TRUE, message=FALSE, warning=FALSE, comment=NA----------------------
example<- rd_query(covican_transformed,
                   variables = c("copd"),
                   variables_names = c("Chronic obstructive pulmonary disease (Yes/No)"),
                   expression = c("is.na(x)"),
                   query_name = c("COPD is a missing value."),
                   instrument = c("Admission"),
                   event = "baseline_visit_arm_1")

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA---------------------
kable(example$queries[1,]) |> kableExtra::row_spec(0,bold=TRUE) |> kableExtra::kable_styling()

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example <- rd_query(covican_transformed,
                    variables = "copd",
                    expression = "is.na(x)",
                    negate = TRUE,
                    event = "baseline_visit_arm_1")

# Printing results
example$results

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example2 <- rd_query(covican_transformed,
                     variables = "age",
                     expression = "is.na(x)",
                     event = "baseline_visit_arm_1",
                     add_to = example)

# Printing results
example2$results

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example <- rd_query(covican_transformed,
                    variables = c("copd", "age"),
                    expression = c("is.na(x)", "x<20"),
                    event = "baseline_visit_arm_1",
                    report_title = "Missing COPD values in the baseline event")

# Printing results
example$results

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example <- rd_query(covican_transformed,
                    variables = c("copd", "age"),
                    expression = c("is.na(x)", "x < 20"),
                    event = "baseline_visit_arm_1",
                    report_zeros = TRUE)

# Printing results
example$results

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example <- rd_query(covican_transformed,
                    variables = c("copd", "age"),
                    expression = c("is.na(x)", "x>60"),
                    event = "baseline_visit_arm_1",
                    by_dag = TRUE)

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
# Printing results
example$results$`Hospital 2`

## ----echo=FALSE, message=FALSE, warning=FALSE, fig.alt="Link of the Project Home page of a project in REDCap, which contains the domain, the REDCap version and the project ID"----
knitr::include_graphics("files/ProjectHome.png")

## ----echo=FALSE, message=FALSE, warning=FALSE, fig.alt="Define My Events page of a project in REDCap, with the event ID of each event"----
knitr::include_graphics("files/EventsID.png", dpi = 150)

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example <- rd_query(covican_transformed,
                    variables = "age",
                    expression = "x>89",
                    event = "baseline_visit_arm_1",
                    link = list(domain = "redcappre.igtp.cat",
                                redcap_version = "13.1.9",
                                proj_id = 800,
                                event_id = c("baseline_visit_arm_1" = 811, "follow_up_visit_da_arm_1" = 812)))

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
# Printing results
example$queries$Link

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example <- rd_event(covican_transformed,
                    event = "follow_up_visit_da_arm_1")

# Print results
example$results

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example <- rd_event(covican_transformed,
                    event = "follow_up_visit_da_arm_1",
                    filter = "screening_fail_crit==0")

# Print results
example$results

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example <- rd_event(covican_transformed,
                    event = c("baseline_visit_arm_1","follow_up_visit_da_arm_1"),
                    filter = "screening_fail_crit==0",
                    report_zeros = TRUE)

# Print results
example$results

## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
example <- rd_query(covican_transformed,
                    variables = c("copd", "age"),
                    expression = c("is.na(x)", "is.na(x)"),
                    event = "baseline_visit_arm_1")
new_example <- example
new_example$queries <- as.data.frame(new_example$queries)
new_example$queries <- new_example$queries[c(1:5, 10:11),] # We take only some of the previously created queries
new_example$queries[nrow(new_example$queries)+1,] <- c("100-79", "Hospital 11", "baseline_visit_arm_1", "Baseline visit", "Comorbidities", "copd", "-", "Chronic obstructive pulmonary disease", "The value is NA and it should not be missing", "100-79-4") # we create a new query
new_example$queries[nrow(new_example$queries)+1,] <- c("105-56", "Hospital 5", "baseline_visit_arm_1", "Baseline visit", "Demographics", "age", "-", "Age", "The value is 80 and it should not be >70", "105-56-2")

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
check <- check_queries(old = example$queries, 
                       new = new_example$queries)

# Print results
check$results

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA---------------------
example <- rbind(head(check$queries, 4), 
                 check$queries |> dplyr::filter(Modification == "Miscorrected") |> dplyr::filter(row_number()==1))
kable(example) |> kableExtra::row_spec(0,bold=TRUE) |> kableExtra::kable_styling()

## ----message=FALSE, warning=FALSE, comment=NA, include=FALSE------------------
example <- rd_query(covican_transformed,
                    variables = c("copd", "age"),
                    expression = c("is.na(x)", "is.na(x)"),
                    event = "baseline_visit_arm_1")

## ----message=FALSE, warning=FALSE, comment=NA, eval=FALSE---------------------
# rd_export(example)

## ----message=FALSE, warning=FALSE, comment=NA, eval=FALSE---------------------
# # The column `Link` is in the reports created with the `link` argument
# example <- rd_query(covican_transformed,
#                     variables = c("copd", "age"),
#                     expression = c("is.na(x)", "is.na(x)"),
#                     event = "baseline_visit_arm_1",
#                     link = list(domain = "redcappre.igtp.cat",
#                                 redcap_version = "13.1.9",
#                                 proj_id = 800,
#                                 event_id = c("baseline_visit_arm_1" = 811, "follow_up_visit_da_arm_1" = 812)))
# 
# rd_export(queries = example$queries,
#           column = "Link",
#           sheet_name = "Queries - Proyecto",
#           path = "C:/User/Desktop/queries.xlsx",
#           password = "123")

