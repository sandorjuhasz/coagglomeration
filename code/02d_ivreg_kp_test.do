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




** Table SI8 -- multivariate -- clustered SE

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




** Table SI9 -- multivariate -- robust SE

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




** Table SI10 -- univar US supply

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





** Table SI11 -- univar CZE WIOD

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




** Table SI13 -- univar manufacturing LABOR
insheet using "../02_data/source/manufacturing_reg_df_with_normalized_variables_2017.csv", delimiter(";") clear

* uiv_labor_manu1
ivreg2 egk_coagg_stand_nuts3 (lab_stand = iv_swe_lab_stand), cluster(ind1 ind2)

* uiv_labor_manu2
ivreg2 egk_coagg_stand_nuts4 (lab_stand = iv_swe_lab_stand), cluster(ind1 ind2)

* uiv_labor_manu3
ivreg2 coagg_porter_rca01_stand_nuts3 (lab_stand = lab_stand), cluster(ind1 ind2)

* uiv_labor_manu4
ivreg2 coagg_porter_rca01_stand_nuts4 (lab_stand = lab_stand), cluster(ind1 ind2)


** Table SI14 -- univar manufacturing IO

* uiv_io_manu1
ivreg2 egk_coagg_stand_nuts3 (io_wiot_hun_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* uiv_io_manu2
ivreg2 egk_coagg_stand_nuts4 (io_wiot_hun_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* uiv_io_manu3
ivreg2 egk_coagg_stand_nuts3 (io3_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* uiv_io_manu4
ivreg2 egk_coagg_stand_nuts4 (io3_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* uiv_io_manu5
ivreg2 coagg_porter_rca01_stand_nuts3 (io_wiot_hun_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* uiv_io_manu6
ivreg2 coagg_porter_rca01_stand_nuts4 (io_wiot_hun_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* uiv_io_manu7
ivreg2 coagg_porter_rca01_stand_nuts3 (io3_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* uiv_io_manu8
ivreg2 coagg_porter_rca01_stand_nuts4 (io3_stand = iv_wiot_cze_stand), cluster(ind1 ind2)






** Table SI16 -- univar services LABOR
insheet using "../02_data/source/services_reg_df_with_normalized_variables_2017.csv", delimiter(";") clear

* uiv_labor_servm1
ivreg2 egk_coagg_stand_nuts3 (lab_stand = iv_swe_lab_stand), cluster(ind1 ind2)

* uiv_labor_servm2
ivreg2 egk_coagg_stand_nuts4 (lab_stand = iv_swe_lab_stand), cluster(ind1 ind2)

* uiv_labor_servm3
ivreg2 coagg_porter_rca01_stand_nuts3 (lab_stand = lab_stand), cluster(ind1 ind2)

* uiv_labor_servm4
ivreg2 coagg_porter_rca01_stand_nuts4 (lab_stand = lab_stand), cluster(ind1 ind2)


** Table SI17 -- univar services IO

* uiv_io_servm1
ivreg2 egk_coagg_stand_nuts3 (io_wiot_hun_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* uiv_io_servm2
ivreg2 egk_coagg_stand_nuts4 (io_wiot_hun_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* uiv_io_servm3
ivreg2 egk_coagg_stand_nuts3 (io3_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* uiv_io_servm4
ivreg2 egk_coagg_stand_nuts4 (io3_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* uiv_io_servm5
ivreg2 coagg_porter_rca01_stand_nuts3 (io_wiot_hun_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* uiv_io_servm6
ivreg2 coagg_porter_rca01_stand_nuts4 (io_wiot_hun_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* uiv_io_servm7
ivreg2 coagg_porter_rca01_stand_nuts3 (io3_stand = iv_wiot_cze_stand), cluster(ind1 ind2)

* uiv_io_servm8
ivreg2 coagg_porter_rca01_stand_nuts4 (io3_stand = iv_wiot_cze_stand), cluster(ind1 ind2)






** Table SI19 -- univar excluding BP
insheet using "../02_data/source/budapest_excluded_reg_df_with_normalized_variables_2017.csv", delimiter(";") clear

* uiv_io_bpe1
ivreg2 egk_coagg_stand (lab_stand = iv_swe_lab_stand), cluster(ind1 ind2)

* uiv_io_bpe2
ivreg2 egk_coagg_stand (io_wiot_hun_stand = iv_wiot_mean_stand), cluster(ind1 ind2)

* uiv_io_bpe3
ivreg2 egk_coagg_stand (io3_stand = iv_wiot_mean_stand), cluster(ind1 ind2)

* uiv_io_bpe4
ivreg2 coagg_porter_rca01_stand (lab_stand = iv_swe_lab_stand), cluster(ind1 ind2)

* uiv_io_bpe5
ivreg2 coagg_porter_rca01_stand (io_wiot_hun_stand = iv_wiot_mean_stand), cluster(ind1 ind2)

* uiv_io_bpe6
ivreg2 coagg_porter_rca01_stand (io3_stand = iv_wiot_mean_stand), cluster(ind1 ind2)








** Table SI21 -- univar single plants
insheet using "../02_data/source/single_plants_reg_df_with_normalized_variables_2017.csv", delimiter(";") clear

* uiv_single1
ivreg2 egk_coagg_stand (lab_stand = iv_swe_lab_stand), cluster(ind1 ind2)

* uiv_single2
ivreg2 egk_coagg_stand (io_wiot_hun_stand = iv_wiot_mean_stand), cluster(ind1 ind2)

* uiv_single3
ivreg2 egk_coagg_stand (io3_stand = iv_wiot_mean_stand), cluster(ind1 ind2)

* uiv_single4
ivreg2 coagg_porter_rca01_stand (lab_stand = iv_swe_lab_stand), cluster(ind1 ind2)

* uiv_single5
ivreg2 coagg_porter_rca01_stand (io_wiot_hun_stand = iv_wiot_mean_stand), cluster(ind1 ind2)

* uiv_single6
ivreg2 coagg_porter_rca01_stand (io3_stand = iv_wiot_mean_stand), cluster(ind1 ind2)








/*
Example R regression for comparison

ivm1 <- feols(egk_coagg_stand_nuts3 ~ 1 | io_wiot_hun_stand + lab_stand ~ iv_wiot_mean_stand + iv_swe_lab_stand,
              cluster = ~ind1 + ind2,
              data = reg_df)
*/


ivreg2 egk_coagg_stand_nuts3 (io_wiot_hun_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), cluster(ind1 ind2)

ivreg2 egk_coagg_stand_nuts3 (io3_stand lab_stand = iv_wiot_mean_stand iv_swe_lab_stand), cluster(ind1 ind2)
