require(gt)
# 2x2 table function ------------------------------------------------------
twoXtwo <- function(df, exp, out){
  df1 <- df %>% 
    ungroup() %>% 
    dplyr::select(exp = {{ exp }}, out = {{ out }})
  tab <- table(df1$exp, df1$out, useNA = "ifany")
  tab_p <- prop.table(tab,1) %>% 
    as.data.frame.matrix() %>% 
    janitor::clean_names() %>% 
    rename_if(is.numeric, ~paste0(., "_pc")) %>% 
    mutate_if(is.numeric, ~signif(.*100, 3))
  tab_tib <- tab %>% 
    as.data.frame.matrix() %>% 
    janitor::clean_names()
  
  no_outcomes <- select(df, {{ out }}) %>% pull() %>% unique() %>% length()
  
  gt0 <- tibble(
    exposure = eval(exp),
    val = rownames(tab)) %>% 
    bind_cols(tab_tib) %>% 
    bind_cols(tab_p) %>% 
    gt() %>% 
    fmt_number(where(is.numeric), use_seps = T, drop_trailing_zeros = T) %>% 
    cols_align(columns = 3:(3+no_outcomes-1), align = "right")
  for(ii in 1:no_outcomes){
    if(ii == 1){gt1 <- gt0}
    gttemp <- get(paste0("gt", ii)) %>% 
      cols_merge(columns = c((2+ii),(2+ii+no_outcomes)), pattern = "{1} ({2}%)") 
    assign(x = paste0("gt", ii+1), gttemp)
  }
  get(paste0("gt", no_outcomes+1)) %>% 
    tab_spanner(columns = 3:(no_outcomes+2), label = eval(out))
}