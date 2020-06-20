
SELECT
    "Api",
    LeaseData."Date",
    LeaseData."GAS_VOL",
    LeaseData."COND_VOL",
    LeaseData."OIL_VOL"
FROM "ProductionAggr",(
    SELECT
        COALESCE(LeaseGasData."DISTRICT_NO",LeaseOilData."DISTRICT_NO") AS "DISTRICT_NO",
        COALESCE(LeaseGasData."LEASE_NO",LeaseOilData."LEASE_NO") AS "LEASE_NO",
        COALESCE(LeaseGasData."OIL_GAS_CODE",LeaseOilData."OIL_GAS_CODE") AS "OIL_GAS_CODE",
        COALESCE(LeaseGasData."Date",LeaseOilData."Date") AS "Date",
        COALESCE(LeaseGasData."GAS_VOL", 0) + COALESCE(LeaseOilData."GAS_VOL", 0) AS "GAS_VOL",
        COALESCE(LeaseGasData."COND_VOL", 0) + COALESCE(LeaseOilData."COND_VOL", 0) AS "COND_VOL",
        COALESCE(LeaseGasData."OIL_VOL", 0) + COALESCE(LeaseOilData."OIL_VOL", 0) AS "OIL_VOL"
    FROM (
        SELECT
            "OgLeaseCycleData"."DISTRICT_NO",
            "OgLeaseCycleData"."LEASE_NO",
            "OgLeaseCycleData"."OIL_GAS_CODE",
            make_date("OgLeaseCycleData"."CYCLE_YEAR", "OgLeaseCycleData"."CYCLE_MONTH", 1) AS "Date",
            "OgLeaseCycleData"."LEASE_GAS_PROD_VOL" AS "GAS_VOL",
            "OgLeaseCycleData"."LEASE_COND_PROD_VOL" AS "COND_VOL",
            "OgLeaseCycleData"."LEASE_OIL_PROD_VOL" AS "OIL_VOL"
        FROM "OgLeaseCycleData",(
            SELECT
                "Api",
                "DISTRICT_NO",
                "LEASE_NO",
                "WELL_NO",
                "FIELD_CLASS"
            FROM "ProductionAggr"
            WHERE "Api"='42-049-32539-00-00'
        )AggrRecord
        WHERE "OgLeaseCycleData"."OIL_GAS_CODE"='G' AND "OgLeaseCycleData"."DISTRICT_NO"=AggrRecord."DISTRICT_NO" AND "OgLeaseCycleData"."LEASE_NO"=AggrRecord."LEASE_NO"
    ) LeaseGasData
    LEFT JOIN (
        SELECT
            "OgLeaseCycleData"."DISTRICT_NO",
            "OgLeaseCycleData"."LEASE_NO",
            "OgLeaseCycleData"."OIL_GAS_CODE",
            make_date("OgLeaseCycleData"."CYCLE_YEAR", "OgLeaseCycleData"."CYCLE_MONTH", 1) AS "Date",
            "OgLeaseCycleData"."LEASE_GAS_PROD_VOL" AS "GAS_VOL",
            "OgLeaseCycleData"."LEASE_COND_PROD_VOL" AS "COND_VOL",
            "OgLeaseCycleData"."LEASE_OIL_PROD_VOL" AS "OIL_VOL"
        FROM "OgLeaseCycleData",(
            SELECT
                "Api",
                "DISTRICT_NO",
                "LEASE_NO",
                "WELL_NO",
                "FIELD_CLASS"
            FROM "ProductionAggr"
            WHERE "Api"='42-049-32539-00-00'
        )AggrRecord
        WHERE "OgLeaseCycleData"."OIL_GAS_CODE"='O' AND "OgLeaseCycleData"."DISTRICT_NO"=AggrRecord."DISTRICT_NO" AND "OgLeaseCycleData"."LEASE_NO"=AggrRecord."LEASE_NO"
    ) LeaseOilData
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
    FROM (
        SELECT
            "OgLeaseCycleData"."DISTRICT_NO",
            "OgLeaseCycleData"."LEASE_NO",
            "OgLeaseCycleData"."OIL_GAS_CODE",
            make_date("OgLeaseCycleData"."CYCLE_YEAR", "OgLeaseCycleData"."CYCLE_MONTH", 1) AS "Date",
            "OgLeaseCycleData"."LEASE_GAS_PROD_VOL" AS "GAS_VOL",
            "OgLeaseCycleData"."LEASE_COND_PROD_VOL" AS "COND_VOL",
            "OgLeaseCycleData"."LEASE_OIL_PROD_VOL" AS "OIL_VOL"
        FROM "OgLeaseCycleData",(
            SELECT
                "Api",
                "DISTRICT_NO",
                "LEASE_NO",
                "WELL_NO",
                "FIELD_CLASS"
            FROM "ProductionAggr"
            WHERE "Api"='42-049-32539-00-00'
        )AggrRecord
        WHERE "OgLeaseCycleData"."OIL_GAS_CODE"='G' AND "OgLeaseCycleData"."DISTRICT_NO"=AggrRecord."DISTRICT_NO" AND "OgLeaseCycleData"."LEASE_NO"=AggrRecord."LEASE_NO"
    ) LeaseGasData
    RIGHT JOIN (
        SELECT
            "OgLeaseCycleData"."DISTRICT_NO",
            "OgLeaseCycleData"."LEASE_NO",
            "OgLeaseCycleData"."OIL_GAS_CODE",
            make_date("OgLeaseCycleData"."CYCLE_YEAR", "OgLeaseCycleData"."CYCLE_MONTH", 1) AS "Date",
            "OgLeaseCycleData"."LEASE_GAS_PROD_VOL" AS "GAS_VOL",
            "OgLeaseCycleData"."LEASE_COND_PROD_VOL" AS "COND_VOL",
            "OgLeaseCycleData"."LEASE_OIL_PROD_VOL" AS "OIL_VOL"
        FROM "OgLeaseCycleData",(
            SELECT
                "Api",
                "DISTRICT_NO",
                "LEASE_NO",
                "WELL_NO",
                "FIELD_CLASS"
            FROM "ProductionAggr"
            WHERE "Api"='42-049-32539-00-00'
        )AggrRecord
        WHERE "OgLeaseCycleData"."OIL_GAS_CODE"='O' AND "OgLeaseCycleData"."DISTRICT_NO"=AggrRecord."DISTRICT_NO" AND "OgLeaseCycleData"."LEASE_NO"=AggrRecord."LEASE_NO"
    ) LeaseOilData
    ON LeaseGasData."DISTRICT_NO"=LeaseOilData."DISTRICT_NO" AND LeaseGasData."LEASE_NO"=LeaseOilData."LEASE_NO" AND LeaseGasData."Date"=LeaseOilData."Date"
) LeaseData
WHERE "Api"='42-049-32539-00-00'
