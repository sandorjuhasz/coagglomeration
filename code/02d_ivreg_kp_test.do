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






/*
Example R regression for comparison

ivm1 <- feols(egk_coagg_stand_nuts3 ~ 1 | io_wiot_hun_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
              cluster = ~ind1 + ind2,
              data = reg_df)
*/


ivreg2 egk_coagg_stand_nuts3 (io_wiot_hun_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), cluster(ind1 ind2)

ivreg2 egk_coagg_stand_nuts3 (io3_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), cluster(ind1 ind2)
