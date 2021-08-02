nms_check <- function(dt, fun) {
  nms_dt <- names(dt)
  nms_fm <- names(formals(fun))
  # nms_ac <- unlist(mget(nms_fm))
  if (sum(nms_dt %in% nms_fm) != 0) {
    nms_out <- paste(nms_fm, collapse = " / ")
    stop(
      "Please check variable names in your DT! ",
      "Your cannot use -- ",
      nms_out,
      " -- as your variable name."
    )
  }
}
