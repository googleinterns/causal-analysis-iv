#' @title Helper function - reshape a list with atomic elements.
#' @param list a list with atomic elements.
#' @param n_list number of nested lists.
#' @param n_var number of atomic elements.
#' @return a reshaped list.
#' @example
rearrange <- function(list, n_list = 2, n_var = 3){
  tibble(list) %>%
    unnest(list) %>%
    unchop(list) %>%
    add_column(id = rep(1:n_list,
                        each = n_var)) %>%
    nest(var = c(list)) %>%
    pull(var) %>%
    map(~unname(.x) %>% as.list)
}
