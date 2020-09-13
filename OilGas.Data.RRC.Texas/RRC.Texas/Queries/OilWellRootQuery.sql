SELECT
    "WellTestAggr"."FL_DISTRICT" AS "DISTRICT_NUMBER",
    "WellTestAggr"."FL_RESERVOIR_NUMBER" AS "RESERVOIR_NUMBER",
    "WellTestAggr"."FL_FIELD_CLASS" AS "FIELD_CLASS",
    "WellTestAggr"."FL_RESERVOIR_NAME" AS "RESERVOIR_NAME",
    "WellTestAggr"."FL_OIL_DISC_WELL_GRAVITY" AS "OIL_GRAVITY",
    "WellTestAggr"."FL_RRCID_DETERMINING_WELL" AS "LEASE_NUMBER",
    "WellTestAggr"."FL_G_1_GAS_GRAVITY" AS "GAS_GRAVITY",
    "WellTestAggr"."FL_AVG_RESERVOIR_BHP" AS "AVG_RESERVOIR_BHP",
    "WellTestAggr"."FL_AVG_RESERVOIR_BH_TEMP" AS "AVG_RESERVOIR_BH_TEMP",
    "WellTestAggr"."FL_FORMATION_VOLUME_FACTOR" AS "FORMATION_VOLUME_FACTOR",
    "WellTestAggr"."FL_SOLUTION_GAS_OIL_RATIO" AS "SOLUTION_GAS_OIL_RATIO"
FROM "WellTestAggr"
WHERE "WellTestAggr"."FL_DISTRICT"={0} AND "WellTestAggr"."FL_RRCID_DETERMINING_WELL"={1};