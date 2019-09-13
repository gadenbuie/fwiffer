get_col_pos <- function() {
  ctx <- rstudioapi::getSourceEditorContext()
  # assign("ctx", ctx, envir = globalenv())
  max_chars <- max(nchar(ctx$contents))

  x <- purrr::map(ctx$selection, "range")
  x <- purrr::map(x, "end")
  x <- purrr::map_dbl(x, `[`, 2)

  if (!1 %in% x) x <- c(1, x)
  if (!max_chars %in% x) x <- c(x, max_chars)

  sort(as.integer(x))
}

get_file_name <- function() {
  ctx <- rstudioapi::getSourceEditorContext()
  ctx$path
}

get_col_widths <- function() {
  x <- get_col_pos()

  widths <- x - c(1, x[-length(x)])
  widths <- widths[-1]

  write_readr_code(widths)
}

get_col_start_end <- function() {
  x <- get_col_pos()

  widths <- purrr::map(
    seq_along(x)[-1],
    ~ x[(.x - 1):.x]
  )

  starts <- purrr::map_int(widths, `[`, 1)
  ends <- purrr::map_int(widths, `[`, 2)
  ends[-length(ends)] <- ends[-length(ends)] - 1L

  write_readr_code(list(starts = starts, ends = ends))
}

write_readr_code <- function(widths, path = get_file_name()) {
  if (is.list(widths)) {
    n_cols <- length(widths[[1]])
    col_pos_setup <- write_start_end(widths)
    fwf_pos_fun <- "readr::fwf_positions(col_starts, col_ends, col_names)"
  } else {
    n_cols <- length(widths)
    col_pos_setup <- write_widths(widths)
    fwf_pos_fun <- "readr::fwf_widths(col_widths, col_names)"
  }

  col_names <- sprintf("X%02d", seq_len(n_cols))
  col_names <- sprintf('col_names <- c("%s")', paste(col_names, collapse = '", "'))

  read_fwf <- paste0('readr::read_fwf("', path, '", ', fwf_pos_fun, ")")

  out <- paste(
    col_pos_setup,
    col_names,
    read_fwf,
    sep = "\n"
  )
  rstudioapi::sendToConsole(out, execute = FALSE)
}

write_start_end <- function(w) {
  paste0(
    "col_starts <- c(", paste(w$starts, collapse = ", "), ")\n",
    "col_ends <- c(", paste(w$ends, collapse = ", "), ")"
  )
}

write_widths <- function(w) {
  paste0("col_widths <- c(", paste(w, collapse = ", "), ")")
}
