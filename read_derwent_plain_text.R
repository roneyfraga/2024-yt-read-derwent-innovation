
read_derwent_plain_text <- function(derwent){
  
  stringr::str_locate_all(derwent, '\n[:upper:]{2} ')[[1]] |>
    tibble::as_tibble() |>
    dplyr::mutate(tags_to_collect = stringr::str_extract_all(derwent, '\n[:upper:]{2} ')[[1]]) |>
    dplyr::mutate(end = dplyr::lead(start, n = 1)) |>
    dplyr::mutate(end = ifelse(is.na(end), nchar(derwent), end)) |>
    dplyr::relocate(tags_to_collect) ->
    tags_positions
  
  split(tags_positions, seq(nrow(tags_positions))) |>
    purrr::map(\(x) stringr::str_sub_all(derwent, x$start, x$end)[[1]]) |>
    purrr::set_names(tags_positions$tags_to_collect |> stringr::str_squish()) |>
    dplyr::bind_rows() |>
    dplyr::mutate(dplyr::across(PN:UT, ~ stringr::str_replace_all(.x, '\n[:upper:]{2} ', ''))) |>
    dplyr::mutate(UT = stringr::str_replace_all(UT, '\nER$', '')) |>
    dplyr::mutate(dplyr::across(PN:UT, ~ stringr::str_replace_all(.x, '\n$', ''))) |>
    dplyr::mutate(AU = stringr::str_split(AU, '\n') |> 
                         purrr::map(\(x) stringr::str_squish(x)) |> 
                         purrr::flatten_chr() |>
                         stringr::str_c(collapse = ';')) ->
    dt
  
  if (any('CR' %in% names(dt))) {
  
    stringr::str_split(dt$CR, '\n ')[[1]] |> 
      purrr::map_vec(\(z) stringr::str_squish(z)) |> 
      {\(x) tibble::tibble(to = x[-1], from = x[1])}() |>
      dplyr::mutate(dplyr::across(to:from, ~ stringr::str_squish(.x))) |>
      dplyr::relocate(from, to) ->
      CR
  
      dt |>
        dplyr::select(-CR) |> 
        dplyr::mutate(CR = list(CR)) ->
        dt
  }
  
  if (any('CP' %in% names(dt))) {
  
    stringr::str_split(dt$CP, '\n ')[[1]] |> 
      purrr::map_vec(\(z) stringr::str_squish(z)) |> 
      {\(x) tibble::tibble(to_full = x[-1], from = x[1])}() |>
      dplyr::mutate(dplyr::across(to_full:from, ~ stringr::str_squish(.x))) |>
      dplyr::mutate(to = sub(' .*$', '', to_full)) |>
      dplyr::relocate(from, to, to_full) ->
      CP
  
      dt |>
        dplyr::select(-CP) |> 
        dplyr::mutate(CP = list(CP)) ->
        dt
  }
  
  avoid_squish <- names(dt)[!names(dt) %in% c('CR', 'CP')]
  
  dt |>
    dplyr::mutate(dplyr::across(dplyr::any_of(avoid_squish), ~ stringr::str_squish(.x))) 
}

