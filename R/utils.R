bool_str_title_cs <- stringr::str_to_title(c("YES", "NO", "1", "0", "Y", "N", "T", "F", "TRUE", "FALSE"))

parse_bool <- function(x) {
  x <- stringr::str_to_upper(as.character(x))
  x <- car::recode(x, "c('0','FALSE','F','NO','N')='FALSE';c('1','TRUE','T','Y','YES')='TRUE';else=NA")

  if (is.na(x)) stop("x must be boolean coded: i.e. Yes, No, 1, 0, TRUE, FALSE, Y, N")

  readr::parse_logical(x)
}


parse_bool_and_add <- function(x, additionals = NULL, return_val_true = "Yes", return_val_false = "No") {
  if (!is.null(additionals) && (x %in% additionals)) {
    x
  } else if (is.logical( (bool_res <- parse_bool(x)) )) {
    ifelse(bool_res, return_val_true, return_val_false)
  } else {
    stop(paste0("Expected a boolean string + the following additionals:", paste0(additionals, collapse = ", ")))
  }
}
