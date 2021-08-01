roll_grp_id <- function(grp_idx, grp_n, width) {
  if (grp_idx == 1L) {
    1:sum(grp_n[1:width])
  } else{
    (1 + sum(grp_n[1:(grp_idx - 1)])):sum(grp_n[1:(grp_idx + width - 1)])
  }
}
