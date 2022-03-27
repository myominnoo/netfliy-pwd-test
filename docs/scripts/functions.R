

calc_ci <- function(var, ref = 1) {
    t <- table(var)
    rn <- row.names(t)
    if (dim(t) > 1) {
        x <- prop.test(t)
        x <- sprintf("%.1f", x$conf.int * 100)
        t <- sprintf("%.1f", t[rn == ref] / sum(t) * 100)
        paste0(t, " (", x[1], ", ", x[2], ")")
    } else {
        if (names(t) == 1) 
            paste0("100 (-)")
        else 
            paste0("0 (-)")
    }
}


calc_pct <- function(var, hf_num) {
    s <- sum(as.numeric(var))
    p <- s / hf_num * 100
    paste0(s, " (", sprintf("%.1f", p), ")")
}

disaggregate <- function(df, var, ...) {
    bind_rows(
        df %>% 
            select({{ var }}, ...) %>%  
            group_by({{ var }}) %>% 
            group_split(.keep = TRUE) %>% 
            map_dfr(function(x) {
                x %>% 
                    select(-hf_type) %>% 
                    map_dfc(calc_ci, ref = 1) %>% 
                    mutate(hf_type = x$hf_type[1])
            }), 
        df %>% 
            select(...) %>% 
            map_dfc(calc_ci, ref = 1) %>% 
            mutate(hf_type = "Overall")
    )
}
