SELECT
    "GasWellRootSegment"."WL_GAS_OR_OIL" AS "FIELD_CLASS",
    "GasWellRootSegment"."WL_GAS_NUMERIC_DISTRICT" AS "DISTRICT_NUMBER",
    "GasWellRootSegment"."WL_GAS_RRC_ID" AS "LEASE_NUMBER",
    "GasWellRootSegment"."WL_GAS_WELL_NUMBER" AS "WELL_NUMBER",
    COALESCE("GasFormG1Segment"."WL_GAS_G1_SIWH", "GasFormG10Segment"."WL_G10_SIWH") AS "SIWH",
    COALESCE("GasFormG1Segment"."WL_GAS_G1_BHP", "GasFormG10Segment"."WL_G10_BHP") AS "AVG_RESERVOIR_BHP",
    COALESCE("GasFormG1Segment"."WL_GAS_G1_GAS_GRAVITY", "GasFormG10Segment"."WL_G10_GAS_GRAVITY") AS "GAS_GRAVITY",
    COALESCE("GasFormG1Segment"."WL_GAS_G1_COND_GRAVITY", "GasFormG10Segment"."WL_G10_COND_GRAVITY") AS "OIL_GRAVITY",
    COALESCE("GasFormG1Segment"."WL_GAS_G1_GOR", "GasFormG10Segment"."WL_G10_RATIO") AS "SOLUTION_GAS_OIL_RATIO"
FROM "GasWellRootSegment"
LEFT JOIN "GasFormG1Segment"
ON "GasWellRootSegment"."GasFormG1SegmentId"="GasFormG1Segment"."Id"
LEFT JOIN "GasFormG10Segment"
ON "GasWellRootSegment"."GasFormG10SegmentId"="GasFormG10Segment"."Id"
WHERE (
    "GasWellRootSegment"."WL_GAS_NUMERIC_DISTRICT"={0} AND "GasWellRootSegment"."WL_GAS_RRC_ID"={1} AND "GasWellRootSegment"."WL_GAS_WELL_NUMBER"='{2}'
)
OR (
    "GasWellRootSegment"."WL_GAS_NUMERIC_DISTRICT"={0} AND "GasWellRootSegment"."WL_GAS_RRC_ID"={1}
)