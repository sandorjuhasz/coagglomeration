* SETUP

** Install packages

*ssc install ivreg2, replace
*ssc install ranktest, replace

** Set working directory

cd "d:/_Hellforge/Vanguard/Operations/Papers/bnet02_coagglo/03_scripts"

** Load data

insheet using "../02_data/source/reg_df_with_normalized_variables_2017.csv", delimiter(";") clear

* ANALYSIS


** Table 3 -- Labor IV -- univariate

* uiv_labor_m1
ivreg2 egk_coagg_stand_nuts3 (lab_stand = lab_stand), cluster(ind1 ind2)

* uiv_labor_m2
ivreg2 egk_coagg_stand_nuts4 (lab_stand = lab_stand), cluster(ind1 ind2)

* uiv_labor_m3
ivreg2 coagg_porter_rca01_stand_nuts3 (lab_stand = lab_stand), cluster(ind1 ind2)

* uiv_labor_m4
ivreg2 coagg_porter_rca01_stand_nuts4 (lab_stand = lab_stand), cluster(ind1 ind2)




** Table 4 -- IO IV -- univariate

* uiv_io_m1
ivreg2 egk_coagg_stand_nuts3 (io_wiot_hun_stand = iv_wiot_mean_stand), cluster(ind1 ind2)

* uiv_io_m2
ivreg2 egk_coagg_stand_nuts4 (io_wiot_hun_stand = iv_wiot_mean_stand), cluster(ind1 ind2)

* uiv_io_m3
ivreg2 egk_coagg_stand_nuts3 (io3_stand = iv_wiot_mean_stand), cluster(ind1 ind2)

* uiv_io_m4
ivreg2 egk_coagg_stand_nuts4 (io3_stand = iv_wiot_mean_stand), cluster(ind1 ind2)

* uiv_io_m5
ivreg2 coagg_porter_rca01_stand_nuts3 (io_wiot_hun_stand = iv_wiot_mean_stand), cluster(ind1 ind2)

* uiv_io_m6
ivreg2 coagg_porter_rca01_stand_nuts4 (io_wiot_hun_stand = iv_wiot_mean_stand), cluster(ind1 ind2)

* uiv_io_m7
ivreg2 coagg_porter_rca01_stand_nuts3 (io3_stand = iv_wiot_mean_stand), cluster(ind1 ind2)

* uiv_io_m8
ivreg2 coagg_porter_rca01_stand_nuts4 (io3_stand = iv_wiot_mean_stand), cluster(ind1 ind2)




** SI -- multivariate -- clustered SE

* iv_cse_mu1
ivreg2 egk_coagg_stand_nuts3 (io_wiot_hun_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), cluster(ind1 ind2)

* iv_cse_mu2
ivreg2 egk_coagg_stand_nuts4 (io_wiot_hun_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), cluster(ind1 ind2)

* iv_cse_mu3
ivreg2 egk_coagg_stand_nuts3 (io3_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), cluster(ind1 ind2)

* iv_cse_mu4
ivreg2 egk_coagg_stand_nuts4 (io3_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), cluster(ind1 ind2)

* iv_cse_mu5
ivreg2 coagg_porter_rca01_stand_nuts3 (io_wiot_hun_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), cluster(ind1 ind2)

* iv_cse_mu6
ivreg2 coagg_porter_rca01_stand_nuts4 (io_wiot_hun_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), cluster(ind1 ind2)

* iv_cse_mu7
ivreg2 coagg_porter_rca01_stand_nuts3 (io3_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), cluster(ind1 ind2)

* iv_cse_mu8
ivreg2 coagg_porter_rca01_stand_nuts4 (io3_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), cluster(ind1 ind2)




** SI -- multivariate -- robust SE

* iv_rob_mu1
ivreg2 egk_coagg_stand_nuts3 (io_wiot_hun_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), robust

* iv_rob_mu2
ivreg2 egk_coagg_stand_nuts4 (io_wiot_hun_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), robust

* iv_rob_mu3
ivreg2 egk_coagg_stand_nuts3 (io3_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), robust

* iv_rob_mu4
ivreg2 egk_coagg_stand_nuts4 (io3_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), robust

* iv_rob_mu5
ivreg2 coagg_porter_rca01_stand_nuts3 (io_wiot_hun_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), robust

* iv_rob_mu6
ivreg2 coagg_porter_rca01_stand_nuts4 (io_wiot_hun_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), robust

* iv_rob_mu7
ivreg2 coagg_porter_rca01_stand_nuts3 (io3_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), robust

* iv_rob_mu8
ivreg2 coagg_porter_rca01_stand_nuts4 (io3_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), robust




** SI -- univar US supply

* iv_us1
ivreg2 egk_coagg_stand_nuts3 (io_wiot_hun_stand = iv_us_supply_stand), cluster(ind1 ind2)

* iv_us2
ivreg2 egk_coagg_stand_nuts4 (io_wiot_hun_stand = iv_us_supply_stand), cluster(ind1 ind2)

* iv_us3
ivreg2 egk_coagg_stand_nuts3 (io3_stand = iv_us_supply_stand), cluster(ind1 ind2)

* iv_us4
ivreg2 egk_coagg_stand_nuts4 (io3_stand = iv_us_supply_stand), cluster(ind1 ind2)

* iv_us5
ivreg2 coagg_porter_rca01_stand_nuts3 (io_wiot_hun_stand = iv_us_supply_stand), cluster(ind1 ind2)

* iv_us6
ivreg2 coagg_porter_rca01_stand_nuts4 (io_wiot_hun_stand = iv_us_supply_stand), cluster(ind1 ind2)

* iv_us7
ivreg2 coagg_porter_rca01_stand_nuts3 (io3_stand = iv_us_supply_stand), cluster(ind1 ind2)

* iv_us8
ivreg2 coagg_porter_rca01_stand_nuts4 (io3_stand = iv_us_supply_stand), cluster(ind1 ind2)





** SI -- univar CZE WIOD

* iv_cze1
ivreg2 egk_coagg_stand_nuts3 (io_wiot_hun_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* iv_cze2
ivreg2 egk_coagg_stand_nuts4 (io_wiot_hun_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* iv_cze3
ivreg2 egk_coagg_stand_nuts3 (io3_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* iv_cze4
ivreg2 egk_coagg_stand_nuts4 (io3_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* iv_cze5
ivreg2 coagg_porter_rca01_stand_nuts3 (io_wiot_hun_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* iv_cze6
ivreg2 coagg_porter_rca01_stand_nuts4 (io_wiot_hun_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* iv_cze7
ivreg2 coagg_porter_rca01_stand_nuts3 (io3_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* iv_cze8
ivreg2 coagg_porter_rca01_stand_nuts4 (io3_stand = iv_wiot_cze_stand), cluster(ind1 ind2)










/*
Example R regression for comparison

ivm1 <- feols(egk_coagg_stand_nuts3 ~ 1 | io_wiot_hun_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
              cluster = ~ind1 + ind2,
              data = reg_df)
*/


ivreg2 egk_coagg_stand_nuts3 (io_wiot_hun_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), cluster(ind1 ind2)

ivreg2 egk_coagg_stand_nuts3 (io3_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), cluster(ind1 ind2)
