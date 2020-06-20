WITH AggrRecord AS (
    SELECT "Api", "DISTRICT_NO","LEASE_NO","WELL_NO","FIELD_CLASS"
    FROM "ProductionAggr"
), GasRecord AS (
    SELECT
        "GasWellRootSegment"."WL_GAS_OR_OIL",
        "GasWellRootSegment"."WL_GAS_NUMERIC_DISTRICT",
        "GasWellRootSegment"."WL_GAS_RRC_ID",
        "GasWellRootSegment"."WL_GAS_WELL_NUMBER",
        "GasFormG1Segment"."WL_GAS_G1_POTENTIAL",
        "GasFormG1Segment"."WL_GAS_G1_SLOPE",
        "GasFormG1Segment"."WL_GAS_G1_SIWH",
        "GasFormG1Segment"."WL_GAS_G1_BHP",
        "GasFormG1Segment"."WL_GAS_G1_GAS_GRAVITY",
        "GasFormG1Segment"."WL_GAS_G1_COND_GRAVITY",
        "GasFormG1Segment"."WL_GAS_G1_GOR",
        "GasFormG10Segment"."WL_G10_GAS_GRAVITY",
        "GasFormG10Segment"."WL_G10_COND_GRAVITY",
        "GasFormG10Segment"."WL_G10_RATIO",
        "GasFormG10Segment"."WL_G10_SIWH",
        "GasFormG10Segment"."WL_G10_FLOW_PRESS",
        "GasFormG10Segment"."WL_G10_POTENTIAL",
        "GasFormG10Segment"."WL_G10_BHP",
        "GasFormG10Segment"."WL_G10_WHP_SHUTIN_WELL",
        "GasFormG10Segment"."WL_G10_ROCK_PRESS"
    FROM "GasWellRootSegment"
    LEFT JOIN "GasFormG1Segment"
    ON "GasWellRootSegment"."GasFormG1SegmentId"="GasFormG1Segment"."Id"
    LEFT JOIN "GasFormG10Segment"
    ON "GasWellRootSegment"."GasFormG10SegmentId"="GasFormG10Segment"."Id"
    WHERE "GasFormG1Segment"."WL_GAS_G1_GAS_GRAVITY" IS NOT NULL AND "GasFormG10Segment"."WL_G10_GAS_GRAVITY" IS NOT NULL
), OilRecord AS (
    SELECT
        "OilWellRootSegment"."WL_OIL_OR_GAS",
        "OilWellRootSegment"."WL_OIL_NUMERIC_DISTRICT",
        "OilWellRootSegment"."WL_OIL_LEASE_NUMBER",
        "OilWellRootSegment"."WL_OIL_WELL_NUMBER",
        "OilWellRootSegment"."WL_OIL_RESERVOIR",
        "OilFormW10Segment"."WL_W10_GAS_OIL_RATIO",
        "OilFormW10Segment"."WL_W10_SIWH_PRESSURE",
        "OilReportingCycleSegment"."WL_OIL_ACRE_FEET",
        "OilReportingCycleSegment"."WL_OIL_BHP",
        "OilReportingCycleSegment"."WL_GAS_OIL_RATIO"
    FROM "OilWellRootSegment"
    LEFT JOIN "OilFormW10Segment" 
    ON "OilWellRootSegment"."OilFormW10SegmentId"="OilFormW10Segment"."Id"
    LEFT JOIN "OilReportingCycleSegment"
    ON "OilWellRootSegment"."OilReportingCycleSegmentId"="OilReportingCycleSegment"."Id"
    WHERE "OilFormW10Segment"."WL_W10_GAS_OIL_RATIO" IS NOT NULL AND "OilFormW10Segment"."WL_W10_GAS_OIL_RATIO" != 0
), GasOilRecord AS (
    SELECT *
       FROM GasRecord
       LEFT JOIN OilRecord
       ON (
           GasRecord."WL_GAS_NUMERIC_DISTRICT"=OilRecord."WL_OIL_NUMERIC_DISTRICT" AND GasRecord."WL_GAS_RRC_ID"=OilRecord."WL_OIL_LEASE_NUMBER" AND GasRecord."WL_GAS_WELL_NUMBER"=OilRecord."WL_OIL_WELL_NUMBER"
       )
       OR (
           GasRecord."WL_GAS_NUMERIC_DISTRICT"=OilRecord."WL_OIL_NUMERIC_DISTRICT" AND GasRecord."WL_GAS_RRC_ID"=OilRecord."WL_OIL_LEASE_NUMBER"
       ) 
    UNION
       SELECT *
       FROM GasRecord
       RIGHT JOIN OilRecord
       ON (
           GasRecord."WL_GAS_NUMERIC_DISTRICT"=OilRecord."WL_OIL_NUMERIC_DISTRICT" AND GasRecord."WL_GAS_RRC_ID"=OilRecord."WL_OIL_LEASE_NUMBER" AND GasRecord."WL_GAS_WELL_NUMBER"=OilRecord."WL_OIL_WELL_NUMBER"
       )
       OR (
           GasRecord."WL_GAS_NUMERIC_DISTRICT"=OilRecord."WL_OIL_NUMERIC_DISTRICT" AND GasRecord."WL_GAS_RRC_ID"=OilRecord."WL_OIL_LEASE_NUMBER"
       )
)
SELECT *
FROM AggrRecord
RIGHT JOIN GasOilRecord
ON (
    AggrRecord."DISTRICT_NO"=GasOilRecord."WL_GAS_NUMERIC_DISTRICT" AND AggrRecord."LEASE_NO"=GasOilRecord."WL_GAS_RRC_ID" AND AggrRecord."WELL_NO" LIKE GasOilRecord."WL_GAS_WELL_NUMBER"
)
OR (
    AggrRecord."DISTRICT_NO"=GasOilRecord."WL_GAS_NUMERIC_DISTRICT" AND AggrRecord."LEASE_NO"=GasOilRecord."WL_GAS_RRC_ID"
)
OR (
    AggrRecord."DISTRICT_NO"=GasOilRecord."WL_OIL_NUMERIC_DISTRICT" AND AggrRecord."LEASE_NO"=GasOilRecord."WL_OIL_LEASE_NUMBER" AND AggrRecord."WELL_NO" LIKE GasOilRecord."WL_OIL_WELL_NUMBER"
)
OR (
    AggrRecord."DISTRICT_NO"=GasOilRecord."WL_OIL_NUMERIC_DISTRICT" AND AggrRecord."LEASE_NO"=GasOilRecord."WL_OIL_LEASE_NUMBER"
)
