# created November 15, 2020

#' get_test_intervals
#' @description get_test_intervals is a utility function that takes a date,
#' interpreted as the end date of the test period, along with the start date
#' of the post-period and the end date of the pre-period, and then computes
#' the start date of the pre-period.
#'
#' @param today_dt A string; the end date of the test period (YYYY-MM_DD)
#' @param end_pre A string; the end date of the prior period (YYYY-MM_DD)
#' @param start_post A string; the start date of the test period (YYYY-MM_DD)
#'
#' @return A list containing the duration of the test- and prior-periods,
#' the start date of the prior-period and a String vector containing
#' all the dates.
#' @export
#'
get_test_intervals <-
  function(today_dt, end_pre="2020-11-04", start_post="2020-11-11"){
    intvl <- lubridate::date(today_dt) - lubridate::date(start_post)
    return(list(
      last_interval = intvl
      , first_date =
        as.character( lubridate::date(end_pre) - intvl )
      , all_dates =
        c(
          as.character( lubridate::date(end_pre) - intvl )
          , end_pre
          , start_post
          , today_dt)
    ))
  }


#' get_NMD_by_market_table
#' @description get_NMD_by_market_table decomposes the analysis into a new margin table
#'
#' @param dat A tibble containing the premium grade data: a column with a name containing 'data'
#' and a column with a name containing 'summary'
#' @param days An integer; the number of days in the test period
#'
#' @return A tibble containing the analysis
#' @export
#'
get_NMD_by_market_table <-
  function(dat, days=28){
    cat("nmd days:", days, "\n")
    NMD_by_market <-
      dat %>%
      dplyr::select(data=tidyselect::contains("data"),nm_summary=tidyselect::contains("summary")) %>%
      dplyr::mutate(
        nm_summary =
          purrr::map(
            nm_summary
            , .f =
              function(x){
                x %>%
                  dplyr::filter(name=="imputed nm total")
              }
          )
      ) %>%
      # dplyr::select(-base_nm_summary) %>%
      tidyr::unnest(cols=c(data, nm_summary )) %>%
      dplyr::mutate(market =
                      forcats::fct_relevel(
                        .f = as.factor(market)
                        , "Calgary", "Edmonton", "AB Secondary"
                        , "Saskatoon", "Regina", "SK Secondary"
                        , "Winnipeg", "MB Secondary"
                        , "LM Zone 1", "LM Zone 2", "LM Zone 3", "BC Secondary"
                        , "ON Secondary", "YT Secondary" )
      ) %>%
      dplyr::mutate(prov =
                      forcats::fct_relevel(
                        .f = as.factor(prov)
                        , "AB", "SK", "MB", "BC", "ON", "YT" )
      ) %>%
      dplyr::group_by(prov, market, change) %>%
      dplyr::summarize(`imputed nm total MID` = sum(MID)
                       , `imputed nm total PUL` = sum(PUL)
                       , .groups="keep") %>%
      dplyr::filter(!prov %in% c("YT","ON")) %>%
      dplyr::rename(
        `NM$ MID`=`imputed nm total MID`
        , `NM$ PUL`=`imputed nm total PUL`
        , `tactic change`=change) %>%
      dplyr::mutate(`NM$ Total` = `NM$ MID` + `NM$ PUL`)

    NMD_by_market_table <-
      NMD_by_market %>%
      dplyr::bind_rows(
        NMD_by_market %>%
          dplyr::ungroup() %>%
          dplyr::group_by(prov) %>%
          dplyr::summarize(
            dplyr::across(
              tidyselect::vars_select_helpers$where(is.numeric),sum), .groups="keep") %>%
          dplyr::mutate(market = paste(as.character(prov), "Total"), `tactic change`=" ", .after="prov")
      ) %>%
      # join with predictions table
      dplyr::left_join(
        premium_diff_target_dat %>%
          tidyr::unnest(data) %>%
          dplyr::select(market, ALL_tot), by="market") %>%
      dplyr::mutate(ALL_tot = ALL_tot * days/30) %>%
      dplyr::rename(`NM$ predicted`=ALL_tot) %>%
      dplyr::mutate(`NM$ annual` = `NM$ Total` * 365.2/days) %>%
      return()
  }

#' get_VOL_by_market_table
#' @description get_NMD_by_market_table decomposes the analysis into a new volume summary table
#'
#' @param dat A tibble containing the premium grade data: a column with a name containing 'data'
#' and a column with a name containing 'summary'
#' @param days An integer; the number of days in the test period
#'
#' @return A tibble containing the analysis
#' @export
#'
get_VOL_by_market_table <-
  function(dat, days=28){
    cat("volume days:", days, "\n")

    VOL_by_market <-
      dat %>%
      # function starts here
      dplyr::select(data=tidyselect::contains("data"),nm_summary=tidyselect::contains("summary")) %>%
      dplyr::mutate(
        pre_vol =
          purrr::map(
            nm_summary
            , .f =
              function(x){
                x %>%
                  dplyr::filter(name=="pre volume") %>%
                  dplyr::select(-name) %>%
                  dplyr::rename_with(.fn=~paste0("pre_",.), .cols=tidyselect::everything())

              }
          )
        , post_vol =
          purrr::map(
            nm_summary
            , .f =
              function(x){
                x %>%
                  dplyr::filter(name=="imputed post volume") %>%
                  dplyr::select(-name) %>%
                  dplyr::rename_with(.fn=~paste0("post_",.), .cols=tidyselect::everything())
              }
          )
      ) %>%
      # dplyr::select(-base_nm_summary) %>%
      tidyr::unnest(cols=c(data, pre_vol, post_vol)) %>%
      dplyr::mutate(market =
                      forcats::fct_relevel(
                        .f = as.factor(market)
                        , "Calgary", "Edmonton", "AB Secondary"
                        , "Saskatoon", "Regina", "SK Secondary"
                        , "Winnipeg", "MB Secondary"
                        , "LM Zone 1", "LM Zone 2", "LM Zone 3", "BC Secondary"
                        , "ON Secondary", "YT Secondary" )
      ) %>%
      dplyr::mutate(prov =
                      forcats::fct_relevel(
                        .f = as.factor(prov)
                        , "AB", "SK", "MB", "BC", "ON", "YT" )
      ) %>%
      dplyr::group_by(prov, market, change) %>%
      dplyr::summarize(`volume pre- total MID` = sum(pre_MID)
                       , `volume pre- total PUL` = sum(pre_PUL)
                       , `volume post- total MID` = sum(post_MID)
                       , `volume post- total PUL` = sum(post_PUL)
                       , `pct delta MID` =
                         scales::percent(`volume post- total MID`/`volume pre- total MID` - 1, accuracy=0.1)
                       , `pct delta PUL` =
                         scales::percent(`volume post- total PUL`/`volume pre- total PUL` - 1, accuracy=0.1)
                       , .groups="keep") %>%
      dplyr::filter(!prov %in% c("YT","ON")) %>%
      dplyr::rename(`tactic change`= change)

    VOL_by_market_table <-
      VOL_by_market %>%
      dplyr::bind_rows(
        VOL_by_market %>%
          dplyr::ungroup() %>%
          dplyr::group_by(prov) %>%
          dplyr::summarize(dplyr::across(`volume pre- total MID`:`volume post- total PUL`,sum), .groups="keep") %>%
          dplyr::mutate(market = paste(as.character(prov), "Total"), `tactic change`=" ", .after="prov") %>%
          dplyr::mutate(
            `pct delta MID` =
              scales::percent(`volume post- total MID`/`volume pre- total MID` - 1, accuracy=0.1)
            , `pct delta PUL` =
              scales::percent(`volume post- total PUL`/`volume pre- total PUL` - 1, accuracy=0.1)
          ) %>%
          return()
      )
  }
