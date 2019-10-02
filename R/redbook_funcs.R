

#' Get NDC code from medication name
#'
#' `get_ndc_from_name` returns the ndc code, or the corresponding redbook
#'  entries, for medications of a given name. The name provided is matched to
#'  the variables "genme" and "prodnme" in the redbook file.
#'
#' @param term A medication name
#' @examples
#' get_ndc_from_name("omeprazole")
#' get_ndc_from_name("omeprazole",TRUE)
get_ndc_from_name <- function(term,return_all=FALSE){
  term <- tolower(term)

  ndc_out <- bind_rows(redbook %>%
                         filter(str_detect(tolower(gennme),term)),
                       redbook %>%
                         filter(str_detect(tolower(prodnme),term))) %>%
    distinct()

  if (return_all==FALSE){
    ndc_out <- ndc_out %>% select(ndcnum)
  }

  return(ndc_out)
}
