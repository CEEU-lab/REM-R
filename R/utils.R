read_config <- function(path="config/building_types_agg.yaml") {
  require(yaml)
  config <- yaml.load_file(path)
  return(config)
}


rename_cat_agg <- function(df, cat_col, new_col_str, 
                         cat_a, cat_b) {
 
  config <- read_config()
  require(rlang)
  cat_a_types <- config[[cat_a]]
  cat_b_types <- config[[cat_b]]
  
  cat_col_quo <- enquo(cat_col) 
  df_ <- df %>% mutate({{new_col_str}} := case_when(!!cat_col_quo %in% cat_a_types ~ cat_a,
                                                    !!cat_col_quo %in% cat_b_types ~ cat_b))
  df_nanout <- df_[!is.na(df_[new_col_str]),]
  
  old_len <- nrow(df)
  new_len <- nrow(df_nanout)
  remvd_row <- old_len-new_len
  print(paste("Number of rows out of categories:", remvd_row)) 
  
  return(df_nanout)
}

index_cat_agg <- function(x, cat_a_idx, cat_b_idx) {
  
  config <- read_config()
  agg_cats <-names(config)
  cat_a <- agg_cats[[cat_a_idx]]
  cat_b <- agg_cats[[cat_b_idx]]
  
  if (x %in% config[[cat_a]]) {
    return(cat_a)
  } else if (x %in% config[[cat_b]]) {
    return(cat_b)
  } else {
    return("other")
  }
}
