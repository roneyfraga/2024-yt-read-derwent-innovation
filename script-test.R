
library(dplyr) 
library(fs) 
library(furrr) 
library(parallel) 
library(purrr) 
library(readr) 
library(rio) 
library(stringr) 
library(tictoc) 
source('read_derwent_plain_text.R')

## --------
# executar para um único arquivo

arquivo <- readr::read_file('query/savedrecs.txt')
arquivo_dividido <- strsplit(arquivo, '\n\n')[[1]]
derwent <- arquivo_dividido[[3]]
read_derwent_plain_text(derwent)

## --------
# múltiplos arquivos; modo seguro

fs::dir_ls(path = 'query/', regexp = '.txt$') |>
  purrr::map(\(x) readr::read_file(x)) |>
  purrr::map(\(x) strsplit(x, '\n\n')[[1]]) |>
  purrr::flatten_chr() ->
  derwent_ls

safely_read_derwent_plain_text <- purrr::safely(read_derwent_plain_text)

tictoc::tic() 
derwent_ls |>
  purrr::map(safely_read_derwent_plain_text) |>
  purrr::map(purrr::pluck('result')) ->
  res
tictoc::toc() 
# 59 sec

res |>
  dplyr::bind_rows() |>
  dplyr::select(-VR, - PT) ->
  derwent_result

rio::export(derwent_result, 'derwent.rds') 
rio::export(dplyr::select(derwent_result, - CP,  - CR), 'derwent.xlsx') 

## --------
# multiprocessado 

# set the number of cores 
# 80% of all cores available
threads <- round(parallel::detectCores() * .8, digits = 0)
future::plan(multisession, workers = threads)

tictoc::tic() 
derwent_ls |>
  furrr::future_map(safely_read_derwent_plain_text) |>
  purrr::map(purrr::pluck('result')) ->
  res_mp
tictoc::toc() 
# 9.5 sec



