using System.IO;

namespace OilGas.Data.RRC.Texas
{
    public static class DrillingClassTemplates
    {
        public static void Run()
        {
            string   className;
            string[] lines;
            string   newClass;
            //

            className = "DrillingPermitRoot";

            lines = new string[]
            {
                "02 DA-STATUS-ROOT.",
                "03 DA-PRIMARY-KEY.",
                "05 DA-STATUS-NUMBER PIC 9(07) VALUE ZERO. 3",
                "05 DA-STATUS-SEQUENCE-NUMBER PIC 9(02) VALUE ZERO. 10",
                "03 DA-SECONDARY-KEYS.",
                "05 DA-COUNTY-CODE PIC 9(03) VALUE ZEROS. 12",
                "05 DA-LEASE-NAME PIC X(32) VALUE SPACES. 15",
                "05 DA-DISTRICT PIC 9(02) VALUE ZERO. 47",
                "05 DA-OPERATOR-NUMBER PIC 9(06) VALUE ZERO. 49",
                "05 DA-CONVERTED-DATE COMP PIC S9(08) VALUE ZERO. 55",
                "05 DA-DATE-APP-RECEIVED.",
                "10 DA-APP-RCVD-CENTURY PIC X(02) VALUE SPACES. 59",
                "10 DA-APP-RCVD-YEAR PIC X(02) VALUE SPACES. 61",
                "10 DA-APP-RCVD-MONTH PIC X(02) VALUE SPACES. 63",
                "10 DA-APP-RCVD-DAY PIC X(02) VALUE SPACES. 65",
                "05 DA-DATE-APP-RCVD-RE REDEFINES",
                "DA-DATE-APP-RECEIVED PIC 9(08).",
                "03 DA-OPERATOR-NAME PIC X(32) VALUE ZEROS. 67",
                "03 FILLER PIC X(01) VALUE ZEROS. 99",
                "03 DA-HB1407-PROBLEM-FLAG PIC X(01) VALUE ZEROS. 100",
                "88 HB1407-PROBLEM VALUE 'Y'.",
                "03 DA-STATUS-OF-APP-FLAG PIC X(01) VALUE 'N'. 101",
                "88 PENDING-APPROVAL VALUE 'P'.",
                "88 APPROVED VALUE 'A'.",
                "88 WITHDRAWN VALUE 'W'.",
                "88 DISMISSED VALUE 'D'.",
                "88 DENIED VALUE 'E'.",
                "88 CLOSED VALUE 'C'.",
                "88 OTHER VALUE 'O'.",
                "88 DELETED VALUE 'X'.",
                "88 CANCELLED VALUE 'Z'.",
                "03 DA-PROBLEM-FLAGS.",
                "05 DA-NOT-ENOUGH-MONEY-FLAG PIC X VALUE 'N'. 102",
                "88 NOT-ENOUGH-MONEY VALUE 'Y'.",
                "88 RESOLVED-NOT-ENOUGH-MONEY VALUE 'R'.",
                "05 DA-TOO-MUCH-MONEY-FLAG PIC X VALUE 'N'. 103",
                "88 TOO-MUCH-MONEY VALUE 'Y'.",
                "88 RESOLVED-TOO-MUCH-MONEY VALUE 'R'.",
                "05 DA-P5-PROBLEM-FLAG PIC X VALUE 'N'. 104",
                "88 P5-PROBLEM VALUE 'Y'.",
                "88 RESOLVED-P5-PROBLEM VALUE 'R'.",
                "05 DA-P12-PROBLEM-FLAG PIC X VALUE 'N'. 105",
                "88 P12-PROBLEM VALUE 'Y'.",
                "88 RESOLVED-P12-PROBLEM VALUE 'R'.",
                "05 DA-PLAT-PROBLEM-FLAG PIC X VALUE 'N'. 106",
                "88 PLAT-PROBLEM VALUE 'Y'.",
                "88 RESOLVED-PLAT-PROBLEM VALUE 'R'.",
                "05 DA-W1A-PROBLEM-FLAG PIC X VALUE 'N'. 107",
                "88 W1A-PROBLEM VALUE 'Y'.",
                "88 RESOLVED-W1A-PROBLEM VALUE 'R'.",
                "05 DA-OTHER-PROBLEM-FLAG PIC X VALUE 'N'. 108",
                "88 OTHER-PROBLEM VALUE 'Y'.",
                "88 RESOLVED-OTHER-PROBLEM VALUE 'R'.",
                "05 DA-RULE37-PROBLEM-FLAG PIC X VALUE 'N'. 109",
                "88 RULE37-PROBLEM VALUE 'Y'.",
                "88 RESOLVED-RULE37-PROBLEM VALUE 'R'.",
                "05 DA-RULE38-PROBLEM-FLAG PIC X VALUE 'N'. 110",
                "88 RULE38-PROBLEM VALUE 'Y'.",
                "88 RESOLVED-RULE38-PROBLEM VALUE 'R'.",
                "05 DA-RULE39-PROBLEM-FLAG PIC X VALUE 'N'. 111",
                "88 RULE39-PROBLEM VALUE 'Y'.",
                "88 RESOLVED-RULE39-PROBLEM VALUE 'R'.",
                "05 DA-NO-MONEY-FLAG PIC X VALUE 'N'. 112",
                "88 NO-MONEY VALUE 'Y'.",
                "88 RESOLVED-NO-MONEY VALUE 'R'.",
                "03 DA-PERMIT PIC 9(07) VALUE ZERO. 113",
                "03 DA-ISSUE-DATE.",
                "05 DA-ISSUE-CENTURY PIC 9(02) VALUE ZERO. 120",
                "05 DA-ISSUE-YEAR PIC 9(02) VALUE ZERO. 122",
                "05 DA-ISSUE-MONTH PIC 9(02) VALUE ZERO. 124",
                "05 DA-ISSUE-DAY PIC 9(02) VALUE ZERO. 126",
                "03 DA-WITHDRAWN-DATE.",
                "05 DA-WITHDRAWN-CENTURY PIC 9(02) VALUE ZERO. 128",
                "05 DA-WITHDRAWN-YEAR PIC 9(02) VALUE ZERO. 130",
                "05 DA-WITHDRAWN-MONTH PIC 9(02) VALUE ZERO. 132",
                "05 DA-WITHDRAWN-DAY PIC 9(02) VALUE ZERO. 134",
                "03 DA-WALKTHROUGH-FLAG PIC X(01) VALUE 'N'. 136",
                "88 WALKTHROUGH VALUE 'Y'.",
                "03 DA-OTHER-PROBLEM-TEXT PIC X(20) VALUE SPACES. 137",
                "03 DA-WELL-NUMBER PIC X(06) VALUE SPACES. 157",
                "03 DA-BUILT-FROM-OLD-MASTER-FLAG PIC X(01) VALUE 'N'. 163",
                "88 BUILT-FROM-OLD-MASTER VALUE 'Y'.",
                "03 DA-STATUS-RENUMBERED-TO PIC 9(09) VALUE ZEROS. 164",
                "03 DA-STATUS-RENUMBERED-FROM PIC 9(09) VALUE ZEROS. 173",
                "03 DA-APPLICATION-RETURNED-FLAG PIC X(01) VALUE ZEROS. 182",
                "03 DA-ECAP-FILING-FLAG PIC X(01) VALUE SPACES. 183",
                "88 DA-ECAP-FILING VALUE 'E'.",
                "88 DA-ECAP-EXPEDITED-FILING VALUE 'X'.",
                "03 FILLER PIC X(29) VALUE ZEROS. 184",
                "02 RRC-TAPE-FILLER PIC X(0324). 187",
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);
            //

            className = "DrillingPermitMaster";

            lines = new string[]
            {
                "02 DA-PERMIT-SEGMENT.",
                "03 DA-PERMIT-KEY.",
                "05 DA-PERMIT-NUMBER PIC 9(07) VALUE ZEROS. 3",
                "05 DA-PERMIT-SEQUENCE-NUMBER PIC 9(02) VALUE ZEROS. 10",
                "03 DA-PERMIT-COUNTY-CODE PIC 9(03) VALUE ZEROS. 12",
                "03 DA-PERMIT-LEASE-NAME PIC X(32) VALUE SPACES.15",
                "03 DA-PERMIT-DISTRICT PIC 9(02) VALUE ZEROS. 47",
                "03 DA-PERMIT-WELL-NUMBER PIC X(06) VALUE SPACES.49",
                "03 DA-PERMIT-TOTAL-DEPTH PIC 9(05) VALUE ZEROS. 55",
                "03 DA-PERMIT-OPERATOR-NUMBER PIC 9(06) VALUE ZEROS. 60",
                "03 DA-TYPE-APPLICATION PIC X(02) VALUE SPACES.66",
                "88 DA-DRILL VALUE '01'.",
                "88 DA-DEEPEN-BELOW-CASING VALUE '02'.",
                "88 DA-DEEPEN-WITHIN-CASING VALUE '03'.",
                "88 DA-PLUG-BACK VALUE '04'.",
                "88 DA-OTHER VALUE '05'.",
                "88 DA-AMENDED-DRILL VALUE '06'.",
                "88 DA-RE-ENTER VALUE '07'.",
                "88 DA-SIDETRACK VALUE '08'.",
                "88 DA-FIELD-TRANSFER VALUE '09'.",
                "88 DA-AMENDED-PRIOR-TO-1977 VALUE '10'.",
                "88 DA-DRILL-DIRECT-SIDETRACK VALUE '11'.",
                "88 DA-DRILL-HORIZONTAL VALUE '12'.",
                "88 DA-SIDETRACK-HORIZONTAL VALUE '13'.",
                "88 DA-RECOMPLETION VALUE '14'.",
                "88 DA-RECLASS VALUE '15'.",
                "88 DA-VALID-TYPE-APPL-AFTER-1994 VALUE '01' '05' '07' '09' '14' '15'.",
                "03 DA-OTHER-EXPLANATION PIC X(30) VALUE SPACES.68",
                "03 DA-ADDRESS-UNIQUE-NUMBER PIC 9(06) VALUE ZEROS. 98",
                "03 DA-ZIP-CODE.",
                "05 DA-ZIP-CODE-PREFIX PIC 9(05) VALUE ZEROS. 104",
                "05 DA-ZIP-CODE-SUFFIX PIC 9(04) VALUE ZEROS. 109",
                "03 DA-FICHE-SET-NUMBER PIC 9(06) VALUE ZEROS. 113",
                "03 DA-ONSHORE-COUNTY PIC 9(03) VALUE ZEROS. 119",
                "03 DA-RECEIVED-DATE.",
                "05 DA-RECEIVED-CENTURY PIC 9(02) VALUE ZEROS. 122",
                "05 DA-RECEIVED-YEAR PIC 9(02) VALUE ZEROS. 124",
                "05 DA-RECEIVED-MONTH PIC 9(02) VALUE ZEROS. 126",
                "05 DA-RECEIVED-DAY PIC 9(02) VALUE ZEROS. 128",
                "03 DA-PERMIT-ISSUED-DATE.",
                "05 DA-PMT-ISSUED-CENTURY PIC 9(02) VALUE ZEROS. 130",
                "05 DA-PMT-ISSUED-YEAR PIC 9(02) VALUE ZEROS. 132",
                "05 DA-PMT-ISSUED-MONTH PIC 9(02) VALUE ZEROS. 134",
                "05 DA-PMT-ISSUED-DAY PIC 9(02) VALUE ZEROS. 136",
                "03 DA-PERMIT-AMENDED-DATE.",
                "05 DA-PMT-AMENDED-CENTURY PIC 9(02) VALUE ZEROS. 138",
                "05 DA-PMT-AMENDED-YEAR PIC 9(02) VALUE ZEROS. 140",
                "05 DA-PMT-AMENDED-MONTH PIC 9(02) VALUE ZEROS. 142",
                "05 DA-PMT-AMENDED-DAY PIC 9(02) VALUE ZEROS. 144",
                "03 DA-PERMIT-EXTENDED-DATE.",
                "05 DA-PMT-EXTENDED-CENTURY PIC 9(02) VALUE ZEROS. 146",
                "05 DA-PMT-EXTENDED-YEAR PIC 9(02) VALUE ZEROS. 148",
                "05 DA-PMT-EXTENDED-MONTH PIC 9(02) VALUE ZEROS. 150",
                "05 DA-PMT-EXTENDED-DAY PIC 9(02) VALUE ZEROS. 152",
                "03 DA-PERMIT-SPUD-DATE.",
                "05 DA-PMT-SPUD-CENTURY PIC 9(02) VALUE ZEROS. 154",
                "05 DA-PMT-SPUD-YEAR PIC 9(02) VALUE ZEROS. 156",
                "05 DA-PMT-SPUD-MONTH PIC 9(02) VALUE ZEROS. 158",
                "05 DA-PMT-SPUD-DAY PIC 9(02) VALUE ZEROS. 160",
                "03 DA-PERMIT-SURFACE-CASING-DATE.",
                "05 DA-PMT-SURFACE-CASING-CENTURY PIC 9(02) VALUE ZEROS. 162",
                "05 DA-PMT-SURFACE-CASING-YEAR PIC 9(02) VALUE ZEROS. 164",
                "05 DA-PMT-SURFACE-CASING-MONTH PIC 9(02) VALUE ZEROS. 166",
                "05 DA-PMT-SURFACE-CASING-DAY PIC 9(02) VALUE ZEROS. 168",
                "03 DA-WELL-STATUS PIC X(01) VALUE SPACE. 170",
                "88 DA-LONG-STRING-CASING VALUE 'A'.",
                "88 DA-CONDUCTOR-CASING VALUE 'B'.",
                "88 DA-BOTTOM-HOLE-PRESSURE VALUE 'C'.",
                "88 DA-DRY-HOLE VALUE 'D'.",
                "88 DA-CIRC-PROD-STRING-CASING VALUE 'F'.",
                "88 DA-DRIVE-PIPE VALUE 'G'.",
                "88 DA-P-A-DRY-HOLE-LETTER VALUE 'H'.",
                "88 DA-TEMP-SURVEY VALUE 'I'.",
                "88 DA-LINER VALUE 'J'.",
                "88 DA-P-A-SULPHUR-CORE-TEST VALUE 'K'.",
                "88 DA-WELL-PLUG-BACK VALUE 'L'.",
                "88 DA-P-A-CORE-TEST VALUE 'M'.",
                "88 DA-INTERM-STRING-CASING VALUE 'N'.",
                "88 DA-PLUG-DRY-HOLE-OIL VALUE 'O'.",
                "88 DA-PLUG-DRY-HOLE-GAS VALUE 'P'.",
                "88 DA-PLUG-FRESH-WATER VALUE 'Q'.",
                "88 DA-PLUG-STAT-TEST VALUE 'R'.",
                "88 DA-PLUG-DRY-HOLE-EXPLORATE VALUE 'S'.",
                "88 DA-CATHODIC-PROTECTION-WELL VALUE 'T'.",
                "88 DA-UNSUCCESSFUL VALUE 'U'.",
                "88 DA-PLUG-URANIUM-TEST VALUE 'V'.",
                "88 DA-FINAL-COMPLETION VALUE 'W'.",
                "88 DA-P-A-LIGNITE-EXPL VALUE 'X'.",
                "88 DA-SIDE-1-MULTI-COMPL VALUE 'Y'.",
                "88 DA-UNPERFED-COMPLETION VALUE 'Z'.",
                "88 DA-SIDE-1-COMMINGLE VALUE '1'.",
                "03 DA-1995-WELL-STATUS REDEFINES DA-WELL-STATUS PIC X(1).",
                "88 DA-1995-SIDE-1-MULTI-COMPL VALUE 'Y'.",
                "88 DA-1995-DRY-HOLE VALUE 'D'.",
                "88 DA-1995-P-A-EXPLORATORY-TEST VALUE 'T'.",
                "88 DA-1995-UNPERFED-COMPLETION VALUE 'Z'.",
                "88 DA-1995-FINAL-COMPLETION VALUE 'W'.",
                "88 DA-1995-CATHODIC-PROT-WELL VALUE 'C'.",
                "88 DA-1995-WELL-STATUS-CODE VALUE 'Y' 'D' 'T' 'Z' 'W' 'C' ' '.",
                "03 DA-PERMIT-WELL-STATUS-DATE.",
                "05 DA-PMT-WELL-STATUS-CENTURY PIC 9(02) VALUE ZEROS. 171",
                "05 DA-PMT-WELL-STATUS-YEAR PIC 9(02) VALUE ZEROS. 173",
                "05 DA-PMT-WELL-STATUS-MONTH PIC 9(02) VALUE ZEROS. 175",
                "05 DA-PMT-WELL-STATUS-DAY PIC 9(02) VALUE ZEROS. 177",
                "03 DA-PERMIT-EXPIRED-DATE.",
                "05 DA-PMT-EXPIRED-CENTURY PIC 9(02) VALUE ZEROS. 179",
                "05 DA-PMT-EXPIRED-YEAR PIC 9(02) VALUE ZEROS. 181",
                "05 DA-PMT-EXPIRED-MONTH PIC 9(02) VALUE ZEROS. 183",
                "05 DA-PMT-EXPIRED-DAY PIC 9(02) VALUE ZEROS. 185",
                "03 DA-PERMIT-CANCELLED-DATE.",
                "05 DA-PMT-CANCELLED-CENTURY PIC 9(02) VALUE ZEROS. 187",
                "05 DA-PMT-CANCELLED-YEAR PIC 9(02) VALUE ZEROS. 189",
                "05 DA-PMT-CANCELLED-MONTH PIC 9(02) VALUE ZEROS. 191",
                "05 DA-PMT-CANCELLED-DAY PIC 9(02) VALUE ZEROS. 193",
                "03 DA-CANCELLATION-REASON PIC X(30) VALUE SPACES.195",
                "03 DA-P12-FILED-FLAG PIC X(01) VALUE 'N'. 225",
                "88 DA-P12-FILED VALUE 'Y'.",
                "88 DA-P12-NOT-FILED VALUE 'N'.",
                "03 DA-SUBSTANDARD-ACREAGE-FLAG PIC X(01) VALUE 'N'. 226",
                "88 DA-SUBSTANDARD-ACREAGE VALUE 'Y'.",
                "88 DA-NOT-SUBSTANDARD-ACREAGE VALUE 'N'.",
                "03 DA-RULE-36-FLAG PIC X(01) VALUE 'N'. 227",
                "88 DA-RULE-36 VALUE 'Y'.",
                "88 DA-NOT-RULE-36 VALUE 'N'.",
                "03 DA-H9-FLAG PIC X(01) VALUE 'N'. 228",
                "88 DA-H9 VALUE 'Y'.",
                "88 DA-NOT-H9 VALUE 'N'.",
                "03 DA-RULE-37-CASE-NUMBER PIC 9(07) VALUE ZEROS. 229",
                "03 DA-RULE-38-DOCKET-NUMBER PIC 9(07) VALUE ZEROS. 236",
                "03 DA-LOCATION-FORMATION-FLAG PIC X(01) VALUE SPACE. 243",
                "88 OLD-LOCATION-FORMAT VALUE 'O'.",
                "88 NEW-LOCATION-FORMAT VALUE 'N'.",
                "03 DA-OLD-SURFACE-LOCATION.",
                "05 DA-OLD-LOCATION PIC X(52) VALUE SPACES.244",
                "05 FILLER PIC X(30) VALUE ZEROS. 296",
                "03 DA-NEW-SURFACE-LOCATION REDEFINES",
                "DA-OLD-SURFACE-LOCATION.",
                "05 DA-SURFACE-SECTION PIC X(08).",
                "05 DA-SURFACE-BLOCK PIC X(10).",
                "05 DA-SURFACE-SURVEY PIC X(55).",
                "05 DA-SURFACE-ABSTRACT PIC X(06).",
                "05 FILLER PIC X(03).",
                "03 DA-SURFACE-ACRES PIC 9(06)V9(2) VALUE ZEROS.326",
                "03 DA-SURFACE-MILES-FROM-CITY PIC 9(04)V9(2) VALUE ZEROS.334",
                "03 DA-SURFACE-DIRECTION-FROM-CITY PIC X(06) VALUE SPACES.340",
                "03 DA-SURFACE-NEAREST-CITY PIC X(13) VALUE SPACES.346",
                "03 DA-SURFACE-LEASE-DISTANCE.",
                "05 DA-SURFACE-LEASE-FEET-1 PIC 9(06)V9(2) VALUE ZEROS.359",
                "05 DA-SURFACE-LEASE-DIRECTION-1 PIC X(13) VALUE SPACES.367",
                "05 DA-SURFACE-LEASE-FEET-2 PIC 9(06)V9(2) VALUE ZEROS.380",
                "05 DA-SURFACE-LEASE-DIRECTION-2 PIC X(13) VALUE SPACES.388",
                "03 DA-OLD-LEASE-DISTANCE-FORMAT REDEFINES",
                "DA-SURFACE-LEASE-DISTANCE.",
                "05 DA-OLD-SURFACE-LEASE-DISTANCE PIC X(28).",
                "05 DA-LEASE-FILLER PIC X(14).",
                "03 DA-SURFACE-SURVEY-DISTANCE.",
                "05 DA-SURFACE-SURVEY-FEET-1 PIC 9(06)V9(2) VALUE ZEROS.401",
                "05 DA-SURFACE-SURVEY-DIRECTION-1 PIC X(13) VALUE SPACES.409",
                "05 DA-SURFACE-SURVEY-FEET-2 PIC 9(06)V9(2) VALUE ZEROS.422",
                "05 DA-SURFACE-SURVEY-DIRECTION-2 PIC X(13) VALUE SPACES.430",
                "03 DA-OLD-SURVEY-DISTANCE-FORMAT REDEFINES",
                "DA-SURFACE-SURVEY-DISTANCE.",
                "05 DA-OLD-SURFACE-SURVEY-DISTANCE PIC X(28).",
                "05 DA-SURVEY-FILLER PIC X(14).",
                "03 DA-NEAREST-WELL PIC X(28) VALUE SPACES.443",
                "03 DA-NEAREST-WELL-NEW-FORMAT REDEFINES DA-NEAREST-WELL.",
                "05 DA-NEAREST-WELL-FEET PIC 9(06)V9(2).",
                "05 DA-NEAREST-WELL-DIRECTION PIC X(13).",
                "05 FILLER PIC X(07).",
                "03 DA-NEAREST-WELL-FORMAT-FLAG PIC X(01) VALUE SPACE. 471",
                "88 OLD-NEAREST-WELL-FORMAT VALUE 'O'.",
                "88 NEW-NEAREST-WELL-FORMAT VALUE 'N'.",
                "03 DA-FINAL-UPDATE.",
                "05 DA-FINAL-CENTURY PIC 9(02) VALUE ZEROS. 472",
                "05 DA-FINAL-YEAR PIC 9(02) VALUE ZEROS. 474",
                "05 DA-FINAL-MONTH PIC 9(02) VALUE ZEROS. 476",
                "05 DA-FINAL-DAY PIC 9(02) VALUE ZEROS. 478",
                "03 DA-CANCELLED-FLAG PIC X(01) VALUE SPACES.480",
                "88 DA-PERMIT-CANCELLED VALUE 'P'.",
                "88 DA-FILING-CANCELLED VALUE 'F'.",
                "03 DA-SPUD-IN-FLAG PIC X(01) VALUE 'N'. 481",
                "88 DA-SPUD-IN VALUE 'Y'.",
                "03 DA-DIRECTIONAL-WELL-FLAG PIC X(01) VALUE 'N'. 482",
                "88 DA-DIRECTIONAL-WELL VALUE 'Y'.",
                "03 DA-SIDETRACK-WELL-FLAG PIC X(01) VALUE 'N'. 483",
                "88 DA-SIDETRACK-WELL VALUE 'Y'.",
                "03 DA-MOVED-INDICATOR PIC X(01) VALUE 'N'. 484",
                "88 DA-MOVED VALUE 'Y'.",
                "03 DA-PERMIT-CONV-ISSUED-DATE PIC 9(08) VALUE ZEROS. 485",
                "03 DA-RULE-37-GRANTED-CODE PIC X(01) VALUE SPACE. 493",
                "03 DA-HORIZONTAL-WELL-FLAG PIC X(01) VALUE 'N'. 494",
                "88 DA-HORIZONTAL-WELL VALUE 'Y'.",
                "03 DA-DUPLICATE-PERMIT-FLAG PIC X(01) VALUE 'N'. 495",
                "88 DA-DUPLICATE-PERMIT VALUE 'Y'.",
                "03 DA-NEAREST-LEASE-LINE PIC X(07) VALUE ZEROS. 496",
                "02 API-NUMBER PIC 9(08) VALUE ZEROS. 503",
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);
            //

            className = "DrillingPermitField";

            lines = new string[]
            {
                "02 DA-FIELD-SEGMENT.",
                "03 DA-FIELD-NUMBER PIC 9(08) VALUE ZEROS. 3",
                "03 DA-FIELD-APPLICATION-WELL-CODE PIC X(01) VALUE SPACE. 11",
                "88 DA-PROPOSED-OIL-COMP VALUE 'O'.",
                "88 DA-PROPOSED-GAS-COMP VALUE 'G'.",
                "88 DA-PROPOSED-O-G-COMP VALUE 'B'.",
                "88 DA-PROPOSED-ABANDONED-COMP VALUE 'A'.",
                "88 DA-PROPOSED-CORE-COMP VALUE 'C'.",
                "88 DA-PROPOSED-SALTWATER-DSP-COMP VALUE 'D'.",
                "88 DA-PROPOSED-EXPL-TEST-COMP VALUE 'E'.",
                "88 DA-PROPOSED-GAS-INJ-COMP VALUE 'F'.",
                "88 DA-PROPOSED-OBSERVATION-COMP VALUE 'H'.",
                "88 DA-PROPOSED-SALTWATER-INJ-COMP VALUE 'I'.",
                "88 DA-PROPOSED-GPRES-GTHERM-COMP VALUE 'J'.",
                "88 DA-PROPOSED-FLUID-INJ-COMP VALUE 'K'.",
                "88 DA-PROPOSED-LQ-PET-GS-STG-COMP VALUE 'L'.",
                "88 DA-PROPOSED-UNDERGRND-STG-COMP VALUE 'M'.",
                "88 DA-PROPOSED-GAS-STORAGE-COMP VALUE 'N'.",
                "88 DA-PROPOSED-REENTER-PLUG-COMP VALUE 'P'.",
                "88 DA-PROPOSED-AIR-INJ-COMP VALUE 'Q'.",
                "88 DA-PROPOSED-RELIEF-COMP VALUE 'R'.",
                "88 DA-PROPOSED-SULFUR-COMP VALUE 'S'.",
                "88 DA-PROPOSED-STRAT-TEST-COMP VALUE 'T'.",
                "88 DA-PROPOSED-URANIUM-COMP VALUE 'U'.",
                "88 DA-PROPOSED-WATER-SUPPLY-COMP VALUE 'V'.",
                "88 DA-PROPOSED-WATER-INJ-COMP VALUE 'W'.",
                "88 DA-PROPOSED-STREAM-INJ-COMP VALUE 'X'.",
                "88 DA-PROPOSED-SLIM-HOLE-TST-COMP VALUE 'Y'.",
                "88 DA-PROPOSED-COAL-COMP VALUE 'Z'.",
                "88 DA-PROPOSED-TESTING-TOOLS-COMP VALUE '1'.",
                "88 DA-PROPOSED-CATH-PROT-COMP VALUE '2'.",
                "88 DA-PROPOSED-SEISMIC-EQUIP-COMP VALUE '3'.",
                "88 DA-PROPOSED-WATER-SOURCE-COMP VALUE '4'.",
                "88 DA-PROPOSED-GAS-COND-COMP VALUE '5'.",
                "88 DA-PROPOSED-DOMESTIC-COMP VALUE '6'.",
                "88 DA-PROPOSED-MONITOR-COMP VALUE '7'.",
                "88 DA-PROPOSED-DEMO-COMP VALUE '8'.",
                "03 DA-1995-FIELD-APPL-WELL-CODE REDEFINES",
                "DA-FIELD-APPLICATION-WELL-CODE PIC X(01).",
                "88 DA-1995-PROPOSED-OIL-COMP VALUE 'O'.",
                "88 DA-1995-PROPOSED-GAS-COMP VALUE 'G'.",
                "88 DA-1995-PROPOSED-O-G-COMP VALUE 'B'.",
                "88 DA-1995-PROPOSED-INJECTION VALUE 'I'.",
                "88 DA-1995-PROPOSED-STORAGE VALUE 'R'.",
                "88 DA-1995-PROPOSED-SERVICE VALUE 'S'.",
                "88 DA-1995-PROPOSED-WATER-SUPPLY VALUE 'V'.",
                "88 DA-1995-PROPOSED-CATHODIC-PROT VALUE 'C'.",
                "88 DA-1995-PROPOSED-EXPLORATORY VALUE 'T'.",
                "88 DA-1995-FLD-APPL-WELL-CODE VALUE 'O' 'G' 'B' 'I' 'R' 'S' 'V' 'C' 'T'.",
                "03 DA-FIELD-COMPLETION-WELL-CODE PIC X(01) VALUE SPACE. 12",
                "88 DA-OIL-COMPLETION VALUE 'O'.",
                "88 DA-GAS-COMPLETION VALUE 'G'.",
                "88 DA-O-G-COMPLETION VALUE 'B'.",
                "DA-1995-FIELD-COMPL-WELL-CODE REDEFINES",
                "DA-FIELD-COMPLETION-WELL-CODE PIC X(01).",
                "88 DA-1995-OIL-COMPLETION VALUE 'O'.",
                "88 DA-1995-GAS-COMPLETION VALUE 'G'.",
                "88 DA-1995-INJECTION-COMPLETION VALUE 'I'.",
                "88 DA-1995-STORAGE-COMPLETION VALUE 'R'.",
                "88 DA-1995-SERVICE-COMPLETION VALUE 'S'.",
                "88 DA-1995-WATER-SUPPLY-COMPL VALUE 'V'.",
                "88 DA-1995-FLD-COMP-WELL-CODE VALUE 'O' 'G' 'I' 'R' 'S' 'V'.",
                "03 DA-FIELD-COMPLETION-CODE PIC X(01) VALUE SPACE. 13",
                "88 DA-SUCCESSFUL-COMPLETION VALUE 'S'.",
                "88 DA-UNSUCCESSFUL-COMPLETION VALUE 'U'.",
                "03 DA-FIELD-TRANSFER-CODE PIC X(01) VALUE 'N'. 14",
                "88 DA-FIELD-TRANSFERRED VALUE 'Y'.",
                "DA-FIELD-VALIDATION-DATE.",
                "05 DA-FIELD-VALIDATION-CENTURY PIC 9(02) VALUE ZEROS. 15",
                "05 DA-FIELD-VALIDATION-YEAR PIC 9(02) VALUE ZEROS. 17",
                "05 DA-FIELD-VALIDATION-MONTH PIC 9(02) VALUE ZEROS. 19",
                "05 DA-FIELD-VALIDATION-DAY PIC 9(02) VALUE ZEROS. 21",
                "DA-FIELD-COMPLETION-DATE.",
                "05 DA-FIELD-COMPLETION-CENTURY PIC 9(02) VALUE ZEROS. 23",
                "05 DA-FIELD-COMPLETION-YEAR PIC 9(02) VALUE ZEROS. 25",
                "05 DA-FIELD-COMPLETION-MONTH PIC 9(02) VALUE ZEROS. 27",
                "05 DA-FIELD-COMPLETION-DAY PIC 9(02) VALUE ZEROS. 29",
                "03 DA-FIELD-RULE37-FLAG PIC X(01) VALUE 'N'. 31",
                "88 DA-FIELD-RULE37 VALUE 'Y'.",
                "03 DA-FIELD-RULE38-FLAG PIC X(01) VALUE 'N'. 32",
                "88 DA-FIELD-RULE38 VALUE 'Y'.",
                "03 FILLER PIC X(18) VALUE ZERO. 33",
                "02 RRC-TAPE-FILLER PIC X(0460). 51",
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);
            //

            className = "DrillingPermitFieldSpecific";

            lines = new string[]
            {
                "02 DA-FIELD-SPECIFIC-DATA-SEGMENT.",
                "03 DA-FIELD-DISTRICT PIC 9(02) VALUE ZEROS. 3",
                "03 DA-FIELD-LEASE-NAME PIC X(32) VALUE SPACES.5",
                "03 DA-FIELD-TOTAL-DEPTH PIC 9(05) VALUE ZEROS. 37",
                "03 DA-FIELD-WELL-NUMBER PIC X(06) VALUE SPACES.42",
                "03 DA-FIELD-ACRES PIC 9(06)V9(2) VALUE ZEROS. 46",
                "03 FILLER PIC X(17) VALUE ZEROS. 56",
                "02 RRC-TAPE-FILLER PIC X(0453). 58",
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);
            //

            className = "DrillingPermitFieldBottomHoleLocation";

            lines = new string[]
            {
                "02 DA-FIELD-BHL-SEGMENT.",
                "05 DA-FLD-BHL-SECTION PIC X(08) VALUE SPACES.3",
                "05 DA-FLD-BHL-BLOCK PIC X(10) VALUE SPACES.11",
                "05 DA-FLD-BHL-ABSTRACT PIC X(06) VALUE SPACES.21",
                "05 DA-FLD-BHL-SURVEY PIC X(55) VALUE SPACES.27",
                "05 DA-FLD-BHL-ACRES PIC 9(06)V9(2) VALUE ZEROS. 82",
                "05 DA-FLD-BHL-NEAREST-WELL PIC X(28) VALUE SPACES.90",
                "05 DA-FLD-BHL-LEASE-FEET-1 PIC 9(06)V9(2) VALUE ZEROS. 118",
                "05 DA-FLD-BHL-LEASE-DIRECTION-1 PIC X(13) VALUE SPACES.126",
                "05 DA-FLD-BHL-LEASE-FEET-2 PIC 9(06)V9(2) VALUE ZEROS. 139",
                "05 DA-FLD-BHL-LEASE-DIRECTION-2 PIC X(13) VALUE SPACES.147",
                "05 DA-FLD-BHL-SURVEY-FEET-1 PIC 9(06)V9(2) VALUE ZEROS. 160",
                "05 DA-FLD-BHL-SURVEY-DIRECTION-1 PIC X(13) VALUE ZEROS. 168",
                "05 DA-FLD-BHL-SURVEY-FEET-2 PIC 9(06)V9(2) VALUE ZEROS. 181",
                "05 DA-FLD-BHL-SURVEY-DIRECTION-2 PIC X(13) VALUE ZEROS. 189",
                "05 DA-FLD-BHL-COUNTY PIC X(13) VALUE SPACES.202",
                "05 DA-FLD-BHL-PNTRT-DIST-1 PIC 9(06)V9(2) VALUE ZEROS. 215",
                "05 DA-FLD-BHL-PNTRT-DIR-1 PIC X(13) VALUE SPACES.223",
                "05 DA-FLD-BHL-PNTRT-DIST-2 PIC 9(06)V9(2) VALUE ZEROS. 236",
                "05 DA-FLD-BHL-PNTRT-DIR-2 PIC X(13) VALUE ZEROS. 244",
                "05 FILLER PIC X(06) VALUE ZEROS. 257",
                "02 RRC-TAPE-FILLER PIC X(0288). 223",
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);
            //

            //className = "DrillingPermitCannedRestrictions";

            //lines = new string[]
            //{
            //    "02 DA-CAN-RESTR-SEGMENT.",
            //    "03 DA-CAN-RESTR-KEY PIC 9(02) VALUE ZEROS. 3",
            //    "03 DA-CAN-RESTR-TYPE PIC X(02) VALUE SPACES. 5",
            //    "03 DA-CAN-RESTR-REMARK PIC X(35) VALUE SPACES. 7",
            //    "03 DA-CAN-RESTR-FEET REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-D-FEET.",
            //    "10 DA-CAN-RESTR-FEET-1 PIC 9(04).",
            //    "10 DA-CAN-RESTR-D-FILLER PIC X(01).",
            //    "10 DA-CAN-RESTR-FEET-2 PIC 9(04).",
            //    "05 FILLER PIC X(26).",
            //    "03 DA-CAN-RESTR-WELL REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-WELL-NUMBER PIC X(06).",
            //    "05 FILLER PIC X(29).",
            //    "03 DA-CAN-RESTR-O REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-O-FEET PIC 9(05).",
            //    "05 FILLER PIC X(30).",
            //    "03 DA-CAN-RESTR-O-ALPHA REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-O-FEET-ALPHA PIC X(05).",
            //    "05 FILLER PIC X(30).",
            //    "03 DA-CAN-RESTR-P REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-P-DATE PIC 9(06).",
            //    "05 FILLER PIC X(29).",
            //    "03 DA-CAN-RESTR-P-ALPHA REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-P-DATE-ALPHA PIC X(08).",
            //    "05 FILLER PIC X(27).",
            //    "03 DA-CAN-RESTR-Q REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-Q-FEET PIC 9(04).",
            //    "05 FILLER PIC X(31).",
            //    "03 DA-CAN-RESTR-Q-ALPHA REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-Q-FEET-ALPHA PIC X(04).",
            //    "05 FILLER PIC X(31).",
            //    "03 DA-CAN-RESTR-W REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-W-FEET PIC 9(05).",
            //    "05 FILLER PIC X(30).",
            //    "03 DA-CAN-RESTR-W-ALPHA REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-W-FEET-ALPHA PIC X(05).",
            //    "05 FILLER PIC X(30).",
            //    "03 DA-CAN-RESTR-AA REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-AA-FEET-ALPHA PIC X(04).",
            //    "05 FILLER PIC X(01).",
            //    "05 DA-CAN-RESTR-AA-FIELD-NO PIC 9(08).",
            //    "05 FILLER PIC X(22).",
            //    "03 DA-CAN-RESTR-AB REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-AB-FEET1-ALPHA PIC X(04).",
            //    "05 DA-CAN-RESTR-AB-FIELD1-NO PIC 9(08).",
            //    "05 DA-CAN-RESTR-AB-FEET2-ALPHA PIC X(04).",
            //    "05 DA-CAN-RESTR-AB-FIELD2-NO PIC 9(08).",
            //    "05 FILLER PIC X(11).",
            //    "03 DA-CAN-RESTR-AJ REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-AJ-PERMIT-NUM PIC 9(07).",
            //    "05 FILLER PIC X(28).",
            //    "03 DA-CAN-RESTR-AK REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-AK-DOCKET-NUM PIC X(10).",
            //    "05 FILLER PIC X(25).",
            //    "03 DA-CAN-RESTR-AM REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-AM-DOCKET-NUM PIC X(10).",
            //    "05 FILLER PIC X(25).",
            //    "03 DA-CAN-RESTR-AN REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-AN-FEET PIC X(04).",
            //    "05 FILLER PIC X(01).",
            //    "05 DA-CAN-RESTR-AN-RULE PIC X(02).",
            //    "05 FILLER PIC X(28).",
            //    "03 DA-CAN-RESTR-AO REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-AO-DATE.",
            //    "07 DA-CAN-RESTR-AO-MM PIC 9(02).",
            //    "07 DA-CAN-RESTR-AO-DD PIC 9(02).",
            //    "07 DA-CAN-RESTR-AO-CC PIC 9(02).",
            //    "07 DA-CAN-RESTR-AO-YY PIC 9(02).",
            //    "05 FILLER PIC X(01).",
            //    "05 DA-CAN-RESTR-AO-NAME PIC X(26).",
            //    "03 DA-CAN-RESTR-AS REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-AS-DOCKET-NUM PIC X(10).",
            //    "05 FILLER PIC X(25).",
            //    "03 DA-CAN-RESTR-04 REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-04-OG-CODE PIC X(01).",
            //    "05 FILLER PIC X(34).",
            //    "03 DA-CAN-RESTR-10 REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-10-FEET-ALPHA PIC X(05).",
            //    "05 FILLER PIC X(01).",
            //    "05 DA-CAN-RESTR-10-FIELD-NO PIC 9(08).",
            //    "05 FILLER PIC X(21).",
            //    "03 DA-CAN-RESTR-11 REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-11-FEET1-ALPHA PIC X(05).",
            //    "05 DA-CAN-RESTR-11-FIELD1-NO PIC 9(08).",
            //    "05 DA-CAN-RESTR-11-FEET2-ALPHA PIC X(05).",
            //    "05 DA-CAN-RESTR-11-FIELD2-NO PIC 9(08).",
            //    "05 FILLER PIC X(09).",
            //    "03 DA-CAN-RESTR-16 REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-16-PERMIT-NUM PIC 9(07).",
            //    "05 FILLER PIC X(28).",
            //    "03 DA-CAN-RESTR-17-23 REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-17-23-DOCKET-NUM PIC X(10).",
            //    "05 FILLER PIC X(25).",
            //    "03 DA-CAN-RESTR-19 REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-19-FEET PIC 9(05).",
            //    "05 FILLER PIC X(01).",
            //    "05 DA-CAN-RESTR-19-RULE PIC X(02).",
            //    "05 FILLER PIC X(27).",
            //    "03 DA-CAN-RESTR-21 REDEFINES DA-CAN-RESTR-REMARK.",
            //    "05 DA-CAN-RESTR-21-REMARKS PIC X(35).",
            //    "03 DA-CAN-RESTR-FLAG PIC X(01) VALUE 'P'.",
            //    "88 DA-CAN-FIELD-RESTRICTION VALUE 'F'.",
            //    "88 DA-CAN-PERMIT-RESTRICTION VALUE 'P'.",
            //    "03 FILLER PIC X(10) VALUE ZEROS.",
            //    "02 RRC-TAPE-FILLER PIC X(0458).",
            //};

            //newClass = TapeRecordClassBuilder.Process(className, lines);

            //File.WriteAllText($"R:/{className}.cs", newClass);
            //

            className = "DrillingPermitCannedRestrictionFields";

            lines = new string[]
            {
                "02 DA-CAN-RESTR-FLD-SEGMENT.",
                "03 DA-CAN-RESTR-FLD-NUMBER PIC 9(08) VALUE ZEROS. 3",
                "03 FILLER PIC 9(05) VALUE ZEROS. 11",
                "02 RRC-TAPE-FILLER PIC X(0495). 16",
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);
            //

            className = "DrillingPermitFreeFormRestrictions";

            lines = new string[]
            {
                "02 DA-FREE-RESTR-SEGMENT.",
                "03 DA-FREE-RESTR-KEY PIC 9(02) VALUE ZEROS. 3",
                "03 DA-FREE-RESTR-TYPE PIC X(02) VALUE SPACES. 5",
                "03 DA-FREE-RESTR-REMARK PIC X(70) VALUE SPACES. 7",
                "03 DA-FREE-RESTR-FLAG PIC X(01) VALUE 'P'. 77",
                "88 DA-FREE-FIELD-RESTRICTION VALUE 'F'.",
                "88 DA-FREE-PERMIT-RESTRICTION VALUE 'P'.",
                "03 FILLER PIC X(10) VALUE ZEROS. 78",
                "02 RRC-TAPE-FILLER PIC X(0423). 88",
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);
            //

            className = "DrillingPermitFreeFormRestrictionFields";

            lines = new string[]
            {
                "02 DA-FREE-RESTR-FLD-SEGMENT.",
                "03 DA-FREE-RESTR-FLD-NUMBER PIC 9(08) VALUE ZEROS. 3",
                "03 FILLER PIC 9(05) VALUE ZEROS. 11",
                "02 RRC-TAPE-FILLER PIC X(0495). 16",
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);
            //

            className = "DrillingPermitBottomHoleLocation";

            lines = new string[]
            {
                "02 DA-PERMIT-BHL-SEGMENT.",
                "05 DA-PMT-BHL-SECTION PIC X(08) VALUE SPACES. 3",
                "05 DA-PMT-BHL-BLOCK PIC X(10) VALUE SPACES. 11",
                "05 DA-PMT-BHL-ABSTRACT PIC X(06) VALUE SPACES. 21",
                "05 DA-PMT-BHL-SURVEY PIC X(55) VALUE SPACES. 27",
                "05 DA-PMT-BHL-ACRES PIC 9(06)V9(2) VALUE ZEROS. 82",
                "05 DA-PMT-BHL-NEAREST-WELL PIC X(28) VALUE SPACES. 90",
                "05 DA-PMT-BHL-LEASE-FEET-1 PIC 9(06)V9(2) VALUE ZEROS. 118",
                "05 DA-PMT-BHL-LEASE-DIRECTION-1 PIC X(13) VALUE SPACES. 126",
                "05 DA-PMT-BHL-LEASE-FEET-2 PIC 9(06)V9(2) VALUE ZEROS. 139",
                "05 DA-PMT-BHL-LEASE-DIRECTION-2 PIC X(13) VALUE SPACES. 147",
                "05 DA-PMT-BHL-SURVEY-FEET-1 PIC 9(06)V9(2) VALUE ZEROS. 160",
                "05 DA-PMT-BHL-SURVEY-DIRECTION-1 PIC X(13) VALUE ZEROS. 168",
                "05 DA-PMT-BHL-SURVEY-FEET-2 PIC 9(06)V9(2) VALUE ZEROS. 181",
                "05 DA-PMT-BHL-SURVEY-DIRECTION-2 PIC X(13) VALUE ZEROS. 189",
                "05 DA-PMT-BHL-COUNTY PIC X(13) VALUE SPACES. 202",
                "05 DA-PMT-BHL-PNTRT-DIST-1 PIC 9(06)V9(2) VALUE ZEROS. 215",
                "05 DA-PMT-BHL-PNTRT-DIR-1 PIC X(13) VALUE SPACES. 223",
                "05 DA-PMT-BHL-PNTRT-DIST-2 PIC 9(06)V9(2) VALUE ZEROS. 236",
                "05 DA-PMT-BHL-PNTRT-DIR-2 PIC X(13) VALUE ZEROS. 244",
                "05 FILLER PIC X(06) VALUE ZEROS. 257",
                "02 RRC-TAPE-FILLER PIC X(0288). 223",
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);
            //

            className = "DrillingPermitAlternateAddress";

            lines = new string[]
            {
                "02 DA-ALTERNATE-ADDRESS-SEGMENT.",
                "03 DA-ALTERNATE-ADDRESS-1.",
                "05 DA-ALT-ADDRESS-KEY PIC X(02) VALUE SPACES. 3",
                "05 DA-ALT-ADDRESS-LINE-1 PIC X(33) VALUE SPACES. 5",
                "03 DA-ALTERNATE-ADDRESS-2 PIC X(35) VALUE SPACES. 5",
                "02 RRC-TAPE-FILLER PIC X(0471). 40",
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);
            //

            className = "DrillingPermitRemarks";

            lines = new string[]
            {
                "02 DA-REMARKS-SEGMENT.",
                "03 DA-REMARK-SEQUENCE-NUMBER PIC 9(03) VALUE ZEROS. 3",
                "03 DA-REMARK-FILE-DATE.",
                "05 DA-REMARK-FILE-CENTURY PIC 9(02) VALUE ZEROS. 6",
                "05 DA-REMARK-FILE-YEAR PIC 9(02) VALUE ZEROS. 8",
                "05 DA-REMARK-FILE-MONTH PIC 9(02) VALUE ZEROS. 10",
                "05 DA-REMARK-FILE-DAY PIC 9(02) VALUE ZEROS. 12",
                "03 DA-REMARK-LINE PIC X(70) VALUE SPACES. 14",
                "03 FILLER PIC X(10) VALUE ZEROS. 84",
                "02 RRC-TAPE-FILLER PIC X(0417). 94",
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);
            //

            className = "DrillingPermitCheckRegister";

            lines = new string[]
            {
                "02 DA-CHECK-REGISTER-SEGMENT.",
                "03 DA-CHECK-REGISTER-KEY.",
                "05 DA-CHECK-REGISTER-DATE.",
                "10 DA-CHECK-REGISTER-CENTURY PIC 9(02) VALUE ZEROS. 3",
                "10 DA-CHECK-REGISTER-YEAR PIC 9(02) VALUE ZEROS. 5",
                "10 DA-CHECK-REGISTER-MONTH PIC 9(02) VALUE ZEROS. 7",
                "10 DA-CHECK-REGISTER-DAY PIC 9(02) VALUE ZEROS. 9",
                "05 DA-CHECK-REGISTER-NUMBER PIC 9(08) VALUE ZEROS. 11",
                "03 FILLER PIC X(10) VALUE ZEROS. 19",
                "02 RRC-TAPE-FILLER PIC X(0482). 29",
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);
        }

    }
}
