data("icd_mappings")

#' Get icd codes corresponding to ccs codes
#'
#' `get_icd_from_ccs` returns the icd-9 or icd-10 codes corresponding
#' to a vector of CCS categories
#'
#' @param ccs_list A vector of CCS values
#' @param icd_version An integer value for the icd version to return
#' @examples
#' get_icd_from_ccs(c(20,22))
#' get_icd_from_ccs(c(20,22),icd_version=10)
get_icd_from_ccs <- function(ccs_list,icd_version=9){
  if (icd_version==9){
    icd_mappings$ccs_mappings %>%
      dplyr::filter(ccs_code %in% ccs_list) %>%
      .$icd_9_code
  } else {
    icd_mappings$ccs10_mappings %>%
      dplyr::filter(ccs_code %in% ccs_list) %>%
      .$icd_10_code
  }
}

#' Get description of ccs codes
#'
#' `explain_ccs` retuns a tibble of descriptions for a vector of
#' of CCS categories
#'
#' @param ccs_list A vector of CCS values
#'
#' @examples
#' explain_ccs(122)
#' explain_ccs(c(1,246))
explain_ccs <- function(ccs_list){

  icd_mappings$ccs_labels %>%
    filter(ccs_code %in% ccs_list)
}


#' Find CCS category containing a given term
#'
#' `explain_ccs` returns a tibble of CCS codes and category labels
#' where the category contains a given term
#'
#' @param term A term (string) to search within CCS categories
#'
#' @examples
#' find_ccs("flu")
#' find_ccs("fever")
find_ccs <- function(term){
  icd_mappings$ccs_labels %>%
    filter(str_detect(tolower(ccs_category),tolower(term)))
}


#' Convert icd-9 values to icd-10
#'
#' `icd9_to_icd10` converts a vector of ICD-9 values to possible ICD-10 values.
#' Uses the NBER crosswalk file.
#'
#' @param icd_list A vector of ICD-9 values
#' @examples
#' icd9_to_icd10("00845")
icd9_to_icd10 <- function(icd_list){
  icd_mappings$icd_9_to_10 %>%
    dplyr::filter(icd9cm %in% icd_list) %>%
    .$icd10cm
}


#' Convert icd-10 values to icd-9
#'
#' `icd10_to_icd9` converts a vector of ICD-10 values to possible ICD-9 values.
#' Uses the NBER crosswalk file.
#'
#' @param icd_list A vector of ICD-10 values
#' @examples
#' icd10_to_icd9("A047")
icd10_to_icd9 <- function(icd_list){
  icd_mappings$icd_10_to_9 %>%
    dplyr::filter(icd10cm %in% icd_list) %>%
    .$icd9cm
}
