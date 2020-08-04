make_cv <- function(data, group_id, n_cv = 2) {
  data %>%
    group_by({{ group_id }}) %>%
    mutate(cv_id = rep(seq_len(n_cv), n() / n_cv)) %>%
    ungroup({{ group_id }})
}

