#' Make cv split indicators within tibble.
#'
#' @param data tibble object.
#' @param group_id experiment id.
#' @param group_id number of cv splits.
#' @return a tibble with id numbers for cv splits.
#' @import magrittr
make_cv <- function(data, group_id, n_cv = 2) {
  data %>%
    group_by({{ group_id }}) %>%
    mutate(cv_id = rep(seq_len(n_cv), n() / n_cv)) %>%
    ungroup({{ group_id }})
}


