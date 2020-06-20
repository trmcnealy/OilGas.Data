WITH AggrRecord AS (
    SELECT
        "Api",
        "DISTRICT_NO",
        "LEASE_NO",
        "WELL_NO",
        "FIELD_CLASS"
    FROM "ProductionAggr"
    WHERE "Api"='42-049-32539-00-00'
), LeaseGasData AS (
    SELECT
        "OgLeaseCycleData"."DISTRICT_NO",
        "OgLeaseCycleData"."LEASE_NO",
        "OgLeaseCycleData"."OIL_GAS_CODE",
        make_date("OgLeaseCycleData"."CYCLE_YEAR", "OgLeaseCycleData"."CYCLE_MONTH", 1) AS "Date",
        "OgLeaseCycleData"."LEASE_GAS_PROD_VOL" AS "GAS_VOL",
        "OgLeaseCycleData"."LEASE_COND_PROD_VOL" AS "COND_VOL",
        "OgLeaseCycleData"."LEASE_OIL_PROD_VOL" AS "OIL_VOL"
    FROM "OgLeaseCycleData",AggrRecord
    WHERE "OgLeaseCycleData"."OIL_GAS_CODE"='G' AND "OgLeaseCycleData"."DISTRICT_NO"=AggrRecord."DISTRICT_NO" AND "OgLeaseCycleData"."LEASE_NO"=AggrRecord."LEASE_NO"
), LeaseOilData AS (
    SELECT
        "OgLeaseCycleData"."DISTRICT_NO",
        "OgLeaseCycleData"."LEASE_NO",
        "OgLeaseCycleData"."OIL_GAS_CODE",
        make_date("OgLeaseCycleData"."CYCLE_YEAR", "OgLeaseCycleData"."CYCLE_MONTH", 1) AS "Date",
        "OgLeaseCycleData"."LEASE_GAS_PROD_VOL" AS "GAS_VOL",
        "OgLeaseCycleData"."LEASE_COND_PROD_VOL" AS "COND_VOL",
        "OgLeaseCycleData"."LEASE_OIL_PROD_VOL" AS "OIL_VOL"
    FROM "OgLeaseCycleData",AggrRecord
    WHERE "OgLeaseCycleData"."OIL_GAS_CODE"='O' AND "OgLeaseCycleData"."DISTRICT_NO"=AggrRecord."DISTRICT_NO" AND "OgLeaseCycleData"."LEASE_NO"=AggrRecord."LEASE_NO"
), LeaseData AS (
    SELECT
        COALESCE(LeaseGasData."DISTRICT_NO",LeaseOilData."DISTRICT_NO") AS "DISTRICT_NO",
        COALESCE(LeaseGasData."LEASE_NO",LeaseOilData."LEASE_NO") AS "LEASE_NO",
        COALESCE(LeaseGasData."OIL_GAS_CODE",LeaseOilData."OIL_GAS_CODE") AS "OIL_GAS_CODE",
        COALESCE(LeaseGasData."Date",LeaseOilData."Date") AS "Date",
        COALESCE(LeaseGasData."GAS_VOL", 0) + COALESCE(LeaseOilData."GAS_VOL", 0) AS "GAS_VOL",
        COALESCE(LeaseGasData."COND_VOL", 0) + COALESCE(LeaseOilData."COND_VOL", 0) AS "COND_VOL",
        COALESCE(LeaseGasData."OIL_VOL", 0) + COALESCE(LeaseOilData."OIL_VOL", 0) AS "OIL_VOL"
    FROM LeaseGasData
    LEFT JOIN LeaseOilData
    ON LeaseGasData."DISTRICT_NO"=LeaseOilData."DISTRICT_NO" AND LeaseGasData."LEASE_NO"=LeaseOilData."LEASE_NO" AND LeaseGasData."Date"=LeaseOilData."Date"
    UNION
    SELECT
        COALESCE(LeaseOilData."DISTRICT_NO",LeaseGasData."DISTRICT_NO") AS "DISTRICT_NO",
        COALESCE(LeaseOilData."LEASE_NO",LeaseGasData."LEASE_NO") AS "LEASE_NO",
        COALESCE(LeaseOilData."OIL_GAS_CODE",LeaseGasData."OIL_GAS_CODE") AS "OIL_GAS_CODE",
        COALESCE(LeaseOilData."Date",LeaseGasData."Date") AS "Date",
        COALESCE(LeaseOilData."GAS_VOL", 0) + COALESCE(LeaseGasData."GAS_VOL", 0) AS "GAS_VOL",
        COALESCE(LeaseOilData."COND_VOL", 0) + COALESCE(LeaseGasData."COND_VOL", 0) AS "COND_VOL",
        COALESCE(LeaseOilData."OIL_VOL", 0) + COALESCE(LeaseGasData."OIL_VOL", 0) AS "OIL_VOL"
    FROM LeaseGasData
    RIGHT JOIN LeaseOilData
    ON LeaseGasData."DISTRICT_NO"=LeaseOilData."DISTRICT_NO" AND LeaseGasData."LEASE_NO"=LeaseOilData."LEASE_NO" AND LeaseGasData."Date"=LeaseOilData."Date"
), WellProduction AS (
    SELECT
        AggrRecord."Api",
        LeaseData."Date",
        LeaseData."GAS_VOL",
        LeaseData."COND_VOL",
        LeaseData."OIL_VOL"
    FROM AggrRecord
    RIGHT JOIN LeaseData
    ON AggrRecord."DISTRICT_NO"=LeaseData."DISTRICT_NO" AND AggrRecord."LEASE_NO"=LeaseData."LEASE_NO"
    WHERE AggrRecord."Api" IS NOT NULL
)
SELECT *
FROM WellProduction;
