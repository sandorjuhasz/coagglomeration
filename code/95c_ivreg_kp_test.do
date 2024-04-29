* SETUP

** Install packages

*ssc install ivreg2, replace
*ssc install ranktest, replace

** Set working directory

cd "d:/_Hellforge/Vanguard/Operations/Papers/bnet02_coagglo/03_scripts"

** Load data

insheet using "../02_data/source/reg_df_with_normalized_variables_2017.csv", delimiter(";") clear

* ANALYSIS

** IV models

/*
Example R regression for comparison

ivm1 <- feols(egk_coagg_stand_nuts3 ~ 1 | io_wiot_hun_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
              cluster = ~ind1 + ind2,
              data = reg_df)
*/


ivreg2 egk_coagg_stand_nuts3 (io_wiot_hun_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), cluster(ind1 ind2)

ivreg2 egk_coagg_stand_nuts3 (io3_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), cluster(ind1 ind2)
