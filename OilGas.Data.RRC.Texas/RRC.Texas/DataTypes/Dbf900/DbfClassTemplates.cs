using System.IO;

namespace OilGas.Data.RRC.Texas
{
    public static class DbfClassTemplates
    {
        public static void Run()
        {
            string   className;
            string[] lines;
            string   newClass;
            //

            className = "WellBoreTechnicalDataRoot";

            lines = new string[]
            {
                "02 WELL-BORE-API-ROOT.", "03 WB-API-NUMBER VALUE ZEROS.", "05 WB-API-CNTY PIC 9(03). 3", "05 WB-API-UNIQUE PIC 9(05). 6", "03 WB-NXT-AVAIL-SUFFIX PIC 9(02). 11",
                "03 WB-NXT-AVAIL-HOLE-CHGE-NBR PIC 9(02). 13", "03 WB-FIELD-DISTRICT PIC 9(02). 15", "03 WB-RES-CNTY-CODE PIC 9(03). 17", "03 WB-ORIG-COMPL-CC PIC X(01). 20",
                "88 WB-ZEROS-FOR-DATE VALUE '0'.", "88 WB-19TH-CENTURY VALUE '1'.", "88 WB-20TH-CENTURY VALUE '2'.", "88 WB-21ST-CENTURY VALUE '3'.", "03 WB-ORIG-COMPL-DATE.",
                "05 WB-ORIG-COMPL-CENT PIC 9(02). 21", "05 WB-ORIG-COMPL-YY PIC 9(02). 23", "05 WB-ORIG-COMPL-MM PIC 9(02). 25", "05 WB-ORIG-COMPL-DD PIC 9(02). 27",
                "03 WB-TOTAL-DEPTH PIC 9(05). 29", "03 WB-VALID-FLUID-LEVEL PIC 9(05). 34", "03 WB-CERTIFICATION-REVOKED-DATE.", "05 WB-CERT-REVOKED-CC PIC 9(02). 39",
                "05 WB-CERT-REVOKED-YY PIC 9(02). 41", "05 WB-CERT-REVOKED-MM PIC 9(02). 43", "05 WB-CERT-REVOKED-DD PIC 9(02). 45", "03 WB-CERTIFICATION-DENIAL-DATE.",
                "05 WB-CERTIFICATION-DENIAL-CC PIC 9(02). 47", "05 WB-CERTIFICATION-DENIAL-YY PIC 9(02). 49", "05 WB-CERTIFICATION-DENIAL-MM PIC 9(02). 51",
                "05 WB-CERTIFICATION-DENIAL-DD PIC 9(02). 53", "03 WB-DENIAL-REASON-FLAG PIC X(01). 55", "88 WB-DENIED-AUTOMATICALLY VALUE 'A'.", "88 WB-DENIED-MANUALLY VALUE 'M'.",
                "03 WB-ERROR-API-ASSIGN-CODE PIC X(01). 56", "03 WB-REFER-CORRECT-API-NBR PIC 9(08). 57", "03 WB-DUMMY-API-NUMBER PIC 9(08). 65",
                "03 WB-DATE-DUMMY-REPLACED PIC 9(08). 73", "03 WB-NEWEST-DRL-PMT-NBR PIC 9(06). 81", "03 WB-CANCEL-EXPIRE-CODE PIC X(01). 87", "03 FILLER PIC X(01). 88",
                "03 WB-EXCEPT-13-A VALUE 'N' PIC X(01). 89", "88 WB-EXCEPT-13A-FILED VALUE 'Y'.", "88 WB-EXCEPT-13A-NOT-FILED VALUE 'N'.",
                "03 WB-FRESH-WATER-FLAG VALUE 'N' PIC X(01). 90", "88 WB-FRESH-WATER VALUE 'Y'.", "03 WB-PLUG-FLAG VALUE 'N' PIC X(01). 91", "88 WB-BORE-PLUGGED VALUE 'Y'.",
                "03 WB-PREVIOUS-API-NBR PIC 9(08). 92", "03 WB-COMPLETION-DATA-IND VALUE 'N' PIC X(01). 100", "88 WB-DATA-ON-FILE VALUE 'Y'.", "88 WB-DATA-NOT-ON-FILE VALUE 'N'.",
                "03 WB-HIST-DATE-SOURCE-FLAG VALUE ZERO PIC 9(01). 101", "88 WB-DATE-FROM-P-I-TAPE VALUE 1.", "88 WB-DATE-FROM-OTHER VALUE 2.", "03 FILLER PIC X(01). 102",
                "03 WB-EX14B2-COUNT PIC 9(02). 103", "03 WB-DESIGNATION-HB-1975-FLAG PIC X(01). 105", "88 WB-AUTO-DESIGNATED-HB-1975 VALUE 'A'.",
                "88 WB-MANUAL-DESIGNATED-HB-1975 VALUE 'M'.", "03 WB-DESIGNATION-EFFECTIVE-DATE.", "05 WB-DESIGNATION-EFFEC-CC PIC 9(02). 106",
                "05 WB-DESIGNATION-EFFEC-YY PIC 9(02). 108", "05 WB-DESIGNATION-EFFEC-MM PIC 9(02). 110", "03 WB-DESIGNATION-REVISED-DATE.",
                "05 WB-DESIGNATION-REVISED-CC PIC 9(02). 112", "05 WB-DESIGNATION-REVISED-YY PIC 9(02). 114", "05 WB-DESIGNATION-REVISED-MM PIC 9(02). 116",
                "03 WB-DESIGNATION-LETTER-DATE.", "05 WB-DESIGNATION-LETTER-CC PIC 9(02). 118", "05 WB-DESIGNATION-LETTER-YY PIC 9(02). 120",
                "05 WB-DESIGNATION-LETTER-MM PIC 9(02). 122", "05 WB-DESIGNATION-LETTER-DD PIC 9(02). 124", "03 WB-CERTIFICATION-EFFECT-DATE.",
                "05 WB-CERTIFICATION-EFFEC-CC PIC 9(02). 126", "05 WB-CERTIFICATION-EFFEC-YY PIC 9(02). 128", "05 WB-CERTIFICATION-EFFEC-MM PIC 9(02). 130",
                "03 WB-WATER-LAND-CODE PIC X(01). 132", "88 WB-INLAND-WATERWAY VALUE 'I'.", "88 WB-BAY-ESTUARY VALUE 'B'.", "88 WB-OFFSHORE VALUE 'O'.", "88 WB-LAND VALUE 'L'.",
                "03 WB-TOTAL-BONDED-DEPTH PIC 9(06). 133", "03 WB-OVERRIDE-EST-PLUG-COST PIC 9(07). 139", "03 WB-SHUT-IN-DATE PIC 9(06). 146", "03 FILLER REDEFINES WB-SHUT-IN-DATE.",
                "05 WB-SHUT-IN-YEAR PIC 9(04).", "05 WB-SHUT-IN-MONTH PIC 9(02).", "03 WB-OVERRIDE-BONDED-DEPTH PIC 9(06). 152", "03 WB-SUBJ-TO-14B2-FLAG PIC X(01). 158",
                "88 WB-SUBJ-TO-14B2 VALUE 'Y'.", "03 WB-PEND-REMOVAL-14B2-FLAG PIC X(01). 159", "88 WB-PEND-REMOVAL-14B2 VALUE 'Y'.", "03 WB-ORPHAN-WELL-HOLD-FLAG PIC X(01). 160",
                "88 WB-ORPHAN-WELL-HOLD VALUE 'Y'.", "03 WB-W3X-WELL-FLAG PIC X(01). 160", "88 WB-W3X-WELL VALUE 'Y'.", "03 FILLER PIC X(06). 161", "02 RRC-TAPE-FILLER PIC X(0080). 168"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreCompletionInformation";

            lines = new string[]
            {
                "02 WELL-BORE-COMPLETION-SEG.", "03 WB-OIL-KEY.", "05 WB-OIL-CODE PIC X(01). 3", "05 WB-OIL-DIST PIC 9(02). 4", "05 WB-OIL-LSE-NBR PIC 9(05). 6",
                "05 WB-OIL-WELL-NBR PIC X(06). 11", "03 WB-GAS-KEY REDEFINES WB-OIL-KEY.", "05 WB-GAS-CODE PIC X(01).", "05 WB-GAS-RRC-ID PIC 9(06).", "05 WB-GAS-FILLER PIC X(07).",
                "03 WB-GAS-DIST PIC 9(02). 17", "03 WB-GAS-WELL-NO PIC X(06). 19", "03 WB-MULTI-WELL-REC-NBR PIC X(01). 25", "03 WB-API-SUFFIX PIC 9(02). 26", "03 FILLER PIC X(18). 28",
                "03 WB-ACTIVE-INACTIVE-CODE PIC X(01). 46", "03 FILLER PIC X(40). 47", "03 WB-DWN-HOLE-COMMINGLE-CODE PIC X(01). 87", "03 FILLER PIC X(34). 88",
                "03 WB-CREATED-FROM-PI-FLAG PIC X(01). 122", "88 WB-CREATED-FROM-PI VALUE 'Y'.", "88 WB-NOT-CREATED-FROM-PI VALUE 'N'.", "03 WB-RULE-37-NBR PIC 9(07). 123",
                "03 FILLER PIC X(05). 130", "03 FILLER PIC X(05). 135", "03 FILLER PIC X(10). 140", "03 FILLER PIC X(04). 150", "03 FILLER PIC X(02). 154",
                "03 WB-P-15 VALUE 'N' PIC X(01). 156", "88 WB-P-15-FILED VALUE 'Y'.", "88 WB-P-15-NOT-FILED VALUE 'N'.", "03 WB-P-12 VALUE 'N' PIC X(01). 157",
                "88 WB-P-12-FILED VALUE 'Y'.", "88 WB-P-12-NOT-FILED VALUE 'N'.", "03 WB-PLUG-DATE-POINTER PIC 9(08). 158", "03 FILLER PIC X(06). 166",
                "02 RRC-TAPE-FILLER PIC X(0076). 172"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreTechnicalDataFormsFileDate";

            lines = new string[]
            {
                "02 WELL-BORE-FILE-DATE.", "03 WB-FILE-KEY PIC 9(08) VALUE ZEROS. 3", "03 WB-FILE-DATE PIC 9(08) VALUE ZEROS. 11", "03 FILLER PIC X(08) VALUE SPACES. 19",
                "03 WB-EXCEPT-RULE-11 PIC X(01) VALUE 'N'. 27", "88 WB-EXCEPT-11-FILED VALUE 'Y'.", "88 WB-EXCEPT-11-NOT-FILED VALUE 'N'.",
                "03 WB-CEMENT-AFFIDAVIT PIC X(01) VALUE 'N'. 28", "88 WB-CMT-AFF-NOT-FILED VALUE 'N'.", "88 WB-CMT-AFF-FILED VALUE 'Y'.", "88 WB-CMT-AFF-NOT-FOUND VALUE 'B'.",
                "88 WB-CMT-AFF-AVAIL VALUE 'A'.", "88 WB-CMT-AFF-NOT-REQ VALUE 'R'.", "03 WB-G-5 PIC X(01) VALUE 'N'. 29", "88 WB-G-5-FILED VALUE 'Y'.", "88 WB-G-5-NOT-FILED VALUE 'N'.",
                "03 WB-W-12 PIC X(01) VALUE 'N'. 30", "88 WB-W-12-FILED VALUE 'Y'.", "88 WB-W-12-NOT-FILED VALUE 'N'.", "03 WB-DIR-SURVEY PIC X(01) VALUE 'N'. 31",
                "88 WB-DIR-SURVEY-FILED VALUE 'Y'.", "88 WB-DIR-SURVEY-NOT-FILED VALUE 'N'.", "03 WB-W2-G1-DATE PIC 9(08) VALUE ZEROS. 32", "03 WB-COMPL-DATE.",
                "05 WB-COMPL-CENTURY PIC 9(02) VALUE ZEROS. 40", "05 WB-COMPL-YEAR PIC 9(02) VALUE ZEROS. 42", "05 WB-COMPL-MONTH PIC 9(02) VALUE ZEROS. 44",
                "05 WB-COMPL-DAY PIC 9(02) VALUE ZEROS. 46", "03 WB-DRL-COMPL-DATE PIC 9(08) VALUE ZEROS. 48", "03 WB-PLUGB-DEPTH1 PIC 9(05) VALUE ZEROS. 56",
                "03 WB-PLUGB-DEPTH2 PIC 9(05) VALUE ZEROS. 61", "03 WB-WATER-INJECTION-NBR PIC X(06) VALUE SPACES. 66", "03 WB-SALT-WTR-NBR PIC 9(05) VALUE ZEROS. 72",
                "03 FILLER PIC X(08) VALUE SPACES. 77", "03 WB-REMARKS-IND PIC X(01) VALUE 'N'. 85", "88 WB-REMARKS-ON-FILE VALUE 'Y'.", "88 WB-REMARKS-DATA-NOT-ON-FILE VALUE 'N'.",
                "03 WB-ELEVATION PIC 9(04) VALUE ZEROS. 86", "03 WB-ELEVATION-CODE PIC X(02) VALUE SPACES. 90", "88 WB-GROUND-LEVEL VALUE 'GL'.", "88 WB-DERRICK-FLOOR VALUE 'DF'.",
                "88 WB-KELLY-BUSHING VALUE 'KB'.", "88 WB-ROTARY-TABLE VALUE 'RT'.", "88 WB-GROUND VALUE 'GR'.", "03 WB-LOG-FILE-RBA PIC 9(08) VALUE ZEROS. 92",
                "03 WB-DOCKET-NBR PIC X(10) VALUE SPACES. 100", "03 WB-PSA-WELL-FLAG PIC X(01) VALUE SPACES. 110", "03 WB-ALLOCATION-WELL-FLAG PIC X(01) VALUE SPACES. 111",
                "03 FILLER PIC X(06) VALUE SPACES. 112", "02 RRC-TAPE-FILLER PIC X(0130). 118"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreRemarks";

            lines = new string[]
            {
                "02 WELL-BORE-COMPL-RMKS-SEG.", "03 WB-RMK-LNE-CNT PIC 9(03). 3", "03 WB-RMK-TYPE-CODE PIC X(01). 6", "03 WB-REMARKS PIC X(70). 7", "02 RRC-TAPE-FILLER PIC X(0171). 77"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreTubing";

            lines = new string[]
            {
                "02 WELL-BORE-TUBING-SEG.", "03 WB-SEGMENT-COUNTER PIC 9(03). 3", "03 WB-TUBING-SIZE.", "05 WB-TUBING-INCHES PIC 9(02). 6", "05 WB-TUBING-FRACTION.",
                "10 WB-FR-NUMERATOR PIC 9(02). 8", "10 WB-FR-DENOMINATOR PIC 9(02). 10", "03 WB-DEPTH-SET PIC 9(05). 12", "03 WB-PACKER-SET PIC 9(05). 17", "03 FILLER PIC 9(10). 22",
                "03 FILLER PIC X(16). 32", "02 RRC-TAPE-FILLER PIC X(0200). 48"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreCasing";

            lines = new string[]
            {
                "02 WELL-BORE-CASING-SEG.", "03 WB-CASING-COUNT PIC 9(03). 3", "03 WB-CASING-SIZE-DATA.", "05 WB-CAS-INCH PIC 9(02). 6", "05 WB-CAS-FRACTION.",
                "10 WB-CAS-FRAC-NUM PIC 9(02). 8", "10 WB-CAS-FRAC-DENOM PIC 9(02). 10", "03 WB-CAS-WT-TABLE PIC 9(08). 12",
                "03 WB-CASING-WEIGHT-LBS-FT REDEFINES WB-CAS-WT-TABLE OCCURS 2 TIMES.", "05 WB-WGT-WHOLE PIC 9(03).", "05 WB-WGT-TENTHS PIC 9(01).",
                "03 WB-CASING-DEPTH-SET PIC 9(05). 20", "03 WB-MLTI-STG-TOOL-DPTH PIC 9(05). 25", "03 WB-AMOUNT-OF-CEMENT PIC 9(05). 30",
                "03 WB-CEMENT-MEASUREMENT VALUE 'S' PIC X(01). 35", "88 WB-SACKS VALUE 'S'.", "88 WB-CUBIC-YARDS VALUE 'Y'.", "88 WB-CUBIC-FEET VALUE 'F'.", "03 WB-CASING-HOLE-SIZE.",
                "05 WB-HOLE-INCH PIC 9(02). 36", "05 WB-HOLE-FRACTION.", "10 WB-HOLE-FRAC-NUM PIC 9(02). 38", "10 WB-HOLE-FRAC-DENOM PIC 9(02). 40", "03 FILLER PIC X(01). 42",
                "03 WB-TOP-OF-CEMENT-CASING PIC X(07). 43", "03 WB-AMOUNT-CASING-LEFT PIC 9(05). 50", "03 FILLER PIC X(50). 55", "02 RRC-TAPE-FILLER PIC X(0143). 105"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBorePerforations";

            lines = new string[]
            {
                "02 WELL-BORE-PERF-SEG.", "03 WB-PERF-COUNT PIC 9(03). 3", "03 WB-FROM-PERF PIC 9(05). 6", "03 WB-TO-PERF PIC 9(05). 11", "03 WB-OPEN-HOLE-CODE PIC X(02). 16",
                "88 WB-OPEN-HOLE VALUE 'OH'.", "03 FILLER PIC X(10). 18", "02 RRC-TAPE-FILLER PIC X(0220). 28"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreLiner";

            lines = new string[]
            {
                "02 WELL-BORE-LINER-SEG.", "03 WB-LINE-COUNT PIC 9(03). 3", "03 WB-LINER-SIZE-DATA.", "05 WB-LIN-INCH PIC 9(02). 6", "05 WB-LIN-FRACTION.",
                "10 WB-LIN-FRAC-NUM PIC 9(02). 8", "10 WB-LIN-FRAC-DENOM PIC 9(02). 10", "03 WB-SACKS-OF-CEMENT PIC 9(05). 12", "03 WB-TOP-OF-LINER PIC 9(05). 17",
                "03 WB-BOTTOM-OF-LINER PIC 9(05). 22", "03 FILLER PIC X(20). 27", "02 RRC-TAPE-FILLER PIC X(0201). 47"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreFormation";

            lines = new string[]
            {
                "02 WELL-FORMATION-DATA-SEG.", "03 WB-FORMATION-CNTR PIC 9(03). 3", "03 WB-FORMATION-NAME PIC X(32). 6", "03 WB-FORMATION-DEPTH PIC 9(05). 38", "03 FILLER PIC X(05). 43",
                "02 RRC-TAPE-FILLER PIC X(0200). 48"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreSqueeze";

            lines = new string[]
            {
                "02 WELL-SQUEEZE-DATA-SEG.", "03 WB-SQUEEZE-CNTR PIC 9(03). 3", "03 WB-SQUEEZE-UPPER-DEPTH PIC 9(05). 6", "03 WB-SQUEEZE-LOWER-DEPTH PIC 9(05). 11",
                "03 WB-SQUEEZE-KIND-AMOUNT PIC X(50). 16", "03 FILLER PIC X(07). 66", "02 RRC-TAPE-FILLER PIC X(0175). 73"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreUsableQualityWaterProtection";

            lines = new string[]
            {
                "02 WELL-BORE-TWDB-SEG.", "03 WB-FRESH-WATER-CNTR PIC 9(03). 3", "03 WB-TWDB-DATE PIC 9(08). 6", "03 WB-SURFACE-CASING-DETER-CODE PIC X(01) 14",
                "88 WB-FIELD-RULE-CODE VALUE 'Y'.", "03 WB-UQWP-DATA.", "05 WB-UQWP-FROM PIC 9(04). 15", "05 WB-UQWP-TO PIC 9(04). 19", "03 FILLER PIC X(10). 23",
                "02 RRC-TAPE-FILLER PIC X(0215). 33"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreOldLocation";

            lines = new string[]
            {
                "02 WELL-BORE-OLD-LOCATION-SEG.", "03 WB-LEASE-NAME PIC X(32). 3", "03 WB-SEC-BLK-SURVEY-LOC PIC X(52). 35", "03 WB-WELL-LOC-MILES PIC 9(04). 87",
                "03 WB-WELL-LOC-DIRECTION PIC X(06). 91", "03 WB-WELL-LOC-NEAREST-TOWN PIC X(13). 97", "03 FILLER PIC X(28). 110", "03 WB-DIST-FROM-SURVEY-LINES PIC X(28). 138",
                "03 WB-DIST-DIRECT-NEAR-WELL PIC X(28). 166", "03 FILLER PIC X(28). 194", "02 RRC-TAPE-FILLER PIC X(0026). 222"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreNewLocation";

            lines = new string[]
            {
                "02 WELL-BORE-NEW-LOC-SEG.", "02 WELL-BORE-NEW-LOC-DATA.", "03 WB-NEW-LOC-KEY.", "05 WB-LOC-COUNTY PIC 9(03). 3", "05 WB-ABSTRACT PIC X(06). 6",
                "03 WB-SURVEY PIC X(55). 12", "03 WB-BLOCK-NUMBER PIC X(10). 67", "03 WB-SECTION PIC X(08). 77", "03 WB-ALT-SECTION PIC X(04). 85", "03 WB-ALT-ABSTRACT PIC X(06). 89",
                "03 WB-DISTANCE-FROM-SURVEY-LINES.", "05 WB-FEET-FROM-SUR-SECT-1 PIC 9(06). 95", "05 WB-DIREC-FROM-SUR-SECT-1 PIC X(13). 101",
                "05 WB-FEET-FROM-SUR-SECT-2 PIC 9(06). 114", "05 WB-DIREC-FROM-SUR-SECT-2 PIC X(13). 120", "02 WB-OTHER-NEW-LOC-DATA.", "03 WB-WGS84-LATITUDE PIC S9(3)V9(7).133",
                "03 WB-WGS84-LONGITUDE PIC S9(3)V9(7).143", "03 FILLER PIC X(05). 153", "03 WB-PLANE-ZONE PIC 9(02). 158", "03 WB-PLANE-COORDINATE-EAST PIC S9(8)V9(2).160",
                "03 WB-PLANE-EAST-RE REDEFINES WB-PLANE-COORDINATE-EAST PIC 9(10).", "03 WB-PLANE-COORDINATE-NORTH PIC S9(8)V9(2).170",
                "03 WB-PLANE-NORTH-RE REDEFINES WB-PLANE-COORDINATE-NORTH PIC 9(10).", "02 WB-VERIFICATION-FLAG VALUE 'N' PIC X(1). 178", "88 NEW-LOCATION-VERIFIED VALUE 'Y'.",
                "88 NEW-LOCATION-NOT-VERIFIED VALUE 'N'.", "88 NEW-LOCATION-VERIFIED-CHANGE VALUE 'C'.", "02 FILLER PIC X(01). 179", "02 RRC-TAPE-FILLER PIC X(0066). 182"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBorePluggingData";

            lines = new string[]
            {
                "02 WELL-BORE-PLUGGING-SEG.", "03 WB-DATE-W3-FILED PIC 9(08). 3", "03 WB-DATE-WELL-BORE-PLUGGED PIC 9(08). 11", "03 WB-PLUG-TOTAL-DEPTH PIC 9(05). 19",
                "03 WB-PLUG-CEMENT-COMP PIC X(32). 24", "03 WB-PLUG-MUD-FILLED PIC X(01). 56", "03 WB-PLUG-MUD-DATA.", "05 WB-PLUG-MUD-APPLIED PIC X(12). 57",
                "05 WB-PLUG-MUD-WEIGHT PIC 9(03). 69", "05 FILLER PIC X(04). 72", "03 WB-PLUG-DRIL-PERM-DATE PIC 9(08). 76", "03 WB-PLUG-DRIL-PERM-NO PIC 9(06). 84",
                "03 WB-PLUG-DRIL-COMP-DATE PIC 9(08). 90", "03 WB-PLUG-LOG-ATTACHED PIC X(01). 98", "03 WB-PLUG-LOG-RELEASED-TO PIC X(32). 99", "03 WB-PLUG-TYPE-LOG PIC X(01). 131",
                "88 TYPE-DRILLERS VALUE 'D'.", "88 TYPE-ELECTRIC VALUE 'E'.", "88 TYPE-RADIOACTIVITY VALUE 'R'.", "88 TYPE-ACOUSTICAL-SONIC VALUE 'A'.",
                "88 TYPE-DRIL-AND-ELEC VALUE 'F'.", "88 TYPE-ELEC-AND-RADIO VALUE 'G'.", "88 TYPE-RADIO-AND-ACOUS VALUE 'H'.", "88 TYPE-DRIL-AND-RADIO VALUE 'I'.",
                "88 TYPE-ELEC-AND-ACOUS VALUE 'J'.", "88 TYPE-DRIL-AND-ACOUS VALUE 'K'.", "88 TYPE-DRIL-ELEC-RADIO VALUE 'L'.", "88 TYPE-ELEC-RADIO-ACOUS VALUE 'M'.",
                "88 TYPE-DRIL-ELEC-ACOUS VALUE 'N'.", "88 TYPE-DRIL-RADIO-ACOUS VALUE 'O'.", "88 TYPE-DRIL-ELEC-RADIO-ACOUS VALUE 'P'.", "03 WB-PLUG-FRESH-WATER-DEPTH PIC 9(05). 132",
                "03 WB-PLUG-UWQP PIC X(40). 137", "03 WB-PLUG-UWQP-RE REDEFINES WB-PLUG-UWQP.", "10 WB-PLUG-FROM-UWQP OCCURS 4 TIMES PIC 9(05).",
                "10 WB-PLUG-TO-UWQP OCCURS 4 TIMES PIC 9(05).", "03 WB-PLUG-MATERIAL-LEFT PIC X(01). 177", "03 WB-PLUG-OIL-KEY VALUE SPACES.", "05 WB-PLUG-OIL-CODE PIC X(01). 178",
                "05 WB-PLUG-OIL-DIST PIC 9(02). 179", "05 WB-PLUG-OIL-LSE-NBR PIC 9(05). 181", "05 WB-PLUG-OIL-WELL-NBR PIC X(06). 186", "03 WB-PLUG-GAS-KEY REDEFINES WB-PLUG-OIL-KEY.",
                "05 WB-PLUG-GAS-CODE PIC X(01).", "05 WB-PLUG-GAS-RRC-ID PIC 9(06).", "05 WB-PLUG-GAS-FILLER PIC X(07).", "03 WB-PLUG-GAS-DIST PIC 9(02). VALUE ZEROS 192",
                "03 WB-PLUG-GAS-WELL-NO PIC X(06). VALUE SPACES 194", "03 WB-PLUG-TYPE-WELL PIC X(01). VALUE SPACE 200", "88 OIL-PLUG VALUE 'O'.", "88 GAS-PLUG VALUE 'G'.",
                "88 DRY-PLUG VALUE 'D'.", "88 SERVICE-PLUG VALUE 'S'.", "03 WB-PLUG-MULTI-COMPL-FLAG PIC X(01). VALUE 'N' 201", "88 WELL-WAS-MULTI-COMPLETION VALUE 'Y'.",
                "03 WB-PLUG-CEM-AFF PIC X(01). VALUE 'N' 202", "88 WB-PLUG-CA-FILED VALUE 'Y'.", "88 WB-PLUG-CA-NOT-FILED VALUE 'N'.", "03 WB-PLUG-13A PIC X(01).VALUE 'N'  203",
                "88 WB-PLUG-13A-FILED VALUE 'Y'.", "88 WB-PLUG-13A-NOT-FILED VALUE 'N'.", "03 WB-PLUG-LOG-RELEASED-DATE PIC 9(08). 204", "03 WB-PLUG-LOG-FILE-RBA PIC 9(08). 212",
                "03 WB-STATE-FUNDED-PLUG-NUMBER PIC 9(07). 220", "03 FILLER PIC X(21). 227"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBorePluggingRemarks";

            lines = new string[]
            {
                "02 WELL-BORE-PLUG-RMKS-SEG.", "03 WB-PLUG-RMK-LNE-CNT PIC 9(03). 3", "03 WB-PLUG-RMK-TYPE-CODE PIC X(01). 6", "03 WB-PLUG-REMARKS PIC X(70). 7",
                "02 RRC-TAPE-FILLER PIC X(0171). 77"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBorePluggingRecord";

            lines = new string[]
            {
                "02 WELL-BORE-PLUG-REC-SEG.", "03 WB-PLUG-NUMBER PIC 9(03). 3", "03 WB-NBR-OF-CEMENT-SACKS PIC 9(05). 6", "03 WB-MEAS-TOP-OF-PLUG PIC 9(05). 11",
                "03 WB-BOTTOM-TUBE-PIPE-DEPTH PIC 9(05). 16", "03 WB-PLUG-CALC-TOP PIC 9(05). 21", "03 WB-PLUG-TYPE-CEMENT PIC X(06). 26", "03 FILLER PIC X(19). 32",
                "02 RRC-TAPE-FILLER PIC X(01947). 51"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBorePluggingDataCasingTubing";

            lines = new string[]
            {
                "02 WELL-BORE-PLUG-CAS-SEG.", "03 WB-PLG-CAS-COUNTER PIC 9(06). 3", "03 WB-PLUG-CASING-TUBING-DATA VALUE ZEROS.", "05 WB-PLUG-CT-RECORD.", "07 WB-PLUG-CAS-SIZE.",
                "09 WB-PLUG-CAS-INCH PIC 9(02). 9", "09 WB-PLUG-CAS-FRAC-NUM PIC 9(02). 11", "09 WB-PLUG-CAS-FRAC-DENOM PIC 9(02). 13", "07 WB-PLUG-CAS-LBS-FT.",
                "09 WB-PLUG-WGT-WHOLE PIC 9(03). 15", "09 WB-PLUG-WGT-TENTHS PIC 9(01). 18", "07 WB-PLUG-AMT-PUT PIC 9(05). 19", "07 WB-PLUG-AMT-LEFT PIC 9(05). 24",
                "07 WB-PLUG-HOLE-SIZE.", "09 WB-PLUG-HOLE-INCH PIC 9(02). 29", "09 WB-PLUG-HOLE-FRAC-NUM PIC 9(02). 31", "09 WB-PLUG-HOLE-FRAC-DENOM PIC 9(02). 33",
                "03 FILLER PIC X(20). 35", "02 RRC-TAPE-FILLER PIC X(0193). 55"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBorePluggingPerfs";

            lines = new string[]
            {
                "02 WELL-BORE-PLUG-RECORD.", "03 WB-PLUG-PERF-COUNTER PIC 9(03). 3", "03 WB-PLUG-FROM-PERF PIC 9(05). 6", "03 WB-PLUG-TO-PERF PIC 9(05). 11",
                "03 WB-PLUG-OPEN-HOLE-INDICATOR PIC X(1). 16", "03 FILLER PIC X(19). 17", "02 RRC-TAPE-FILLER PIC X(0212). 36"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBorePluggingDataNomenclature";

            lines = new string[]
            {
                "02 WELL-BORE-PLUG-NOMEN-SEG.", "03 WB-PLUG-FIELD-NO PIC 9(08). 3", "03 WB-PLUG-FIELD-NAME PIC X(32). 11", "03 WB-PLUG-OPER-NO PIC X(06). 43",
                "03 WB-PLUG-OPER-NAME PIC X(32). 49", "03 WB-PLUG-LEASE-NAME PIC X(32). 81", "02 RRC-TAPE-FILLER PIC X(0135). 113"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreDrillingPermitNumber";

            lines = new string[] {"02 WELL-BORE-DRLG-PMT-SEG.", "03 WB-PERMIT-NUMBER PIC 9(06). 3", "03 FILLER PIC X(14). 9", "02 RRC-TAPE-FILLER PIC X(0225). 23"};

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreWellId";

            lines = new string[]
            {
                "02 WELL-BORE-WELL-ID-SEG.", "03 WB-OIL-INFO.", "05 WB-OIL PIC X(01). 3", "05 WB-OIL-DISTRICT PIC 9(02). 4", "05 WB-OIL-LSE-NUMBER PIC 9(05). 6",
                "05 WB-OIL-WELL-NUMBER PIC X(06). 11", "03 WB-GAS-INFO REDEFINES WB-OIL-INFO.", "05 WB-GAS PIC X(01).", "05 WB-GAS-RRCID PIC 9(06).", "05 WB-FILLER PIC X(07).",
                "03 FILLER PIC X(11). 17", "02 RRC-TAPE-FILLER PIC X(0220). 28"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBore14B2Well";

            lines = new string[]
            {
                "02 WB14B2-WELL-SEGMENT.", "05 WB14B2-OIL-KEY.", "10 WB14B2-OIL-CODE PIC X(01). 3", "10 WB14B2-OIL-DISTRICT PIC 9(02). 4", "10 WB14B2-OIL-LEASE-NUMBER PIC 9(05). 6",
                "10 WB14B2-OIL-WELL-NUMBER PIC X(06). 11", "05 WB14B2-GAS-KEY REDEFINES WB14B2-OIL-KEY.", "10 WB14B2-GAS-CODE PIC X(01).", "10 WB14B2-GAS-RRC-ID PIC 9(06).",
                "10 WB14B2-GAS-FILLER PIC X(07).", "05 WB14B2-APPLICATION-NUMBER PIC 9(06). 17", "05 WB14B2-GAS-DISTRICT PIC 9(02). 23", "05 WB14B2-EXT-STATUS-FLAG PIC X(01). 25",
                "88 WB14B2-EXT-APPROVED VALUE 'A'.", "88 WB14B2-EXT-CANCELLED VALUE 'C'.", "88 WB14B2-EXT-DENIED VALUE 'D'.", "88 WB14B2-EXT-EXPIRED VALUE 'E'.",
                "05 WB14B2-EXT-CANCELLED-REASON PIC X(01). 26", "88 WB14B2-EXT-INJECTION-WELL VALUE 'I'.", "88 WB14B2-EXT-PRODUCING-WELL VALUE 'P'.",
                "88 WB14B2-EXT-PLUGGED-WELL VALUE 'G'.", "88 WB14B2-EXT-SERVICE-WELL VALUE 'S'.", "88 WB14B2-EXT-CANCELLED-OTHER VALUE 'O'.", "05 WB14B2-EXT-APPROVED-DATE.",
                "10 WB14B2-EXT-APPROVED-CENT PIC 9(02). 27", "10 WB14B2-EXT-APPROVED-YEAR PIC 9(02). 29", "10 WB14B2-EXT-APPROVED-MONTH PIC 9(02). 31",
                "10 WB14B2-EXT-APPROVED-DAY PIC 9(02). 33", "05 WB14B2-EXT-EXP-DATE.", "10 WB14B2-EXT-EXP-CENT PIC 9(02). 35", "10 WB14B2-EXT-EXP-YEAR PIC 9(02). 37",
                "10 WB14B2-EXT-EXP-MONTH PIC 9(02). 39", "10 WB14B2-EXT-EXP-DAY PIC 9(02). 41", "05 WB14B2-EXT-DENIED-DATE.", "10 WB14B2-EXT-DENIED-CENT PIC 9(02). 43",
                "10 WB14B2-EXT-DENIED-YEAR PIC 9(02). 45", "10 WB14B2-EXT-DENIED-MONTH PIC 9(02). 47", "10 WB14B2-EXT-DENIED-DAY PIC 9(02). 49", "05 WB14B2-EXT-HIST-DATE.",
                "10 WB14B2-EXT-HIST-CENT PIC 9(02). 51", "10 WB14B2-EXT-HIST-YEAR PIC 9(02). 53", "10 WB14B2-EXT-HIST-MONTH PIC 9(02). 55", "10 WB14B2-EXT-HIST-DAY PIC 9(02). 57",
                "05 WB14B2-WELL-VIOLATIONS.", "10 WB14B2-MECH-INTEG-VIOL-FLAG PIC X(01). 59", "88 WB14B2-MECH-INTEG-VIOL VALUE 'H'.", "10 WB14B2-PLUG-ORDER-SF-HOLD-FLAG PIC X(01). 60",
                "88 WB14B2-PLUG-ORDER-SF-HOLD VALUE 'E'.", "10 WB14B2-POLLUTION-VIOL-FLAG PIC X(01). 61", "88 WB14B2-POLLUTION-VIOL VALUE 'P'.",
                "10 WB14B2-FIELD-OPS-HOLD-FLAG PIC X(01). 62", "88 WB14B2-FIELD-OPS-HOLD VALUE 'F'.", "10 WB14B2-H15-PROBLEM-FLAG PIC X(01). 63", "88 WB14B2-H15-PROBLEM VALUE 'V'.",
                "10 WB14B2-H15-NOT-FILED-FLAG PIC X(01). 64", "88 WB14B2-H15-NOT-FILED VALUE 'X'.", "10 WB14B2-OPER-DELQ-FLAG PIC X(01). 65", "88 WB14B2-OPER-DELQ VALUE 'O'.",
                "10 WB14B2-DISTRICT-HOLD-SFP-FLAG PIC X(01). 66", "88 WB14B2-DISTRICT-HOLD-SFP VALUE 'T'.", "10 WB14B2-DIST-SF-CLEAN-UP-FLAG PIC X(01). 67",
                "88 WB14B2-DIST-SF-CLEAN-UP VALUE 'M'.", "10 WB14B2-DIST-STATE-PLUG-FLAG PIC X(01). 68", "88 WB14B2-DIST-STATE-PLUG VALUE 'K'.",
                "10 WB14B2-GOOD-FAITH-VIOL-FLAG PIC X(01). 69", "88 WB14B2-GOOD-FAITH-VIOL VALUE 'R'.", "10 WB14B2-WELL-OTHER-VIOL-FLAG PIC X(01). 70",
                "88 WB14B2-WELL-OTHER-VIOL VALUE 'Q'.", "10 WB14B2-W3C-SURF-EQP-VIOL-FLAG PIC X(01). 71", "88 WB14B2-W3C-SURF-EQUIP-VIOL VALUE 'S'.",
                "10 WB14B2-W3X-VIOL-FLAG PIC X(01). 72", "88 WB14B2-W3X-VIOL VALUE 'W'.", "10 FILLER PIC X(07). 73", "05 WB14B2-HB2259-W3X-OPTIONS.",
                "07 WB14B2-HB2259-W3X-PUB-ENT PIC X(01). 80", "07 WB14B2-HB2259-W3X-10PCT PIC X(01). 81", "07 WB14B2-HB2259-W3X-BONDING PIC X(01). 82",
                "07 WB14B2-HB2259-W3X-H13-EOR PIC X(01). 83", "88 WB14B2-HB2259-EOR-REJECTED VALUE 'R'.", "07 WB14B2-HB2259-W3X-AOP PIC X(01). 84",
                "88 WB14B2-HB2259-AOP-REJECTED VALUE 'R'.", "07 WB14B2-HB2259-W3X-MIT PIC X(01). 85", "88 WB14B2-HB2259-MIT-REJECTED VALUE 'R'.",
                "07 WB14B2-HB2259-W3X-ESCROW PIC X(01). 86", "88 WB14B2-HB2259-ESCROW-REJECTED VALUE 'R'.", "05 WB14B2-W3X-FILING-KEY PIC 9(08). 87",
                "05 WB14B2-W3X-AOP-RECEIVED-DATE PIC 9(08). 95", "05 WB14B2-W3X-AOP-FEE-RCVD-DATE PIC 9(08). 103", "05 WB14B2-W3X-H15-FEE-RCVD-DATE PIC 9(08). 111",
                "05 WB14B2-W3X-ESCROW-FUNDS PIC 9(05)V99. 119", "05 WB14B2-W3X-ESCROW-FUND-SPLIT REDEFINES WB14B2-W3X-ESCROW-FUNDS.", "07 WB14B2-W3X-ESCROW-FUND-WHOLE PIC 9(05).",
                "07 WB14B2-W3X-ESCROW-FUND-DECIMAL PIC 9(02).", "05 WB14B2-60-DAY-LETTER-SENT-FLAG PIC X(01). 126", "88 WB14B2-60-DAY-LETTER-SENT VALUE 'Y'.",
                "05 WB14B2-W1X-36-NEEDS-BOND-FLAG PIC X(01). 127", "88 WB14B2-W1X-36-NEEDS-BOND VALUE 'Y'.", "05 WB14B2-W1X-36-TYPE-COVERAGE PIC X(01). 128",
                "88 WB14B2-W1X-36-BOND VALUE 'B'.", "88 WB14B2-W1X-36-LOC VALUE 'L'.", "05 WB14B2-W1X-36-AMT-FILED PIC 9(09). 129", "05 WB14B2-W1X-36-SURETY PIC 9(05). 138",
                "05 WB14B2-W1X-36-EXP-DATE.", "10 WB14B2-W1X-36-EXP-CENT PIC 9(02). 143", "10 WB14B2-W1X-36-EXP-YEAR PIC 9(02). 145", "10 WB14B2-W1X-36-EXP-MON PIC 9(02). 147",
                "10 WB14B2-W1X-36-EXP-DAY PIC 9(02). 149", "05 WB14B2-W1X-36-BOND-NUMBER PIC X(20). 151", "05 FILLER PIC X(32). 171", "02 RRC-TAPE-FILLER PIC X(0120). 128"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreH15Report";

            lines = new string[]
            {
                "02 WELL-BORE-H15-RECORD.", "03 WB-H15-DATE-KEY PIC 9(08) VALUE ZEROS. 3", "03 WB-H15-STATUS PIC X(01) VALUE SPACE. 11", "88 WB-H15-APPROVED VALUE 'A'.",
                "88 WB-H15-COMPLIANT VALUE 'C'.", "88 WB-H15-DELINQUENT VALUE 'D'.", "88 WB-H15-NOT-APPROVED VALUE 'N'.", "88 WB-H15-APPROVAL-PENDING VALUE 'P'.",
                "88 WB-H15-W3A-EXTENSION VALUE 'W'.", "88 WB-H15-UIC VALUE 'U'.", "88 WB-H15-NO-TEST-PROJ-EXT VALUE 'E'.", "88 WB-H15-W1X-DENIED VALUE 'X'.",
                "03 WB-H15-OPERATOR PIC 9(06) VALUE ZEROS. 12", "03 WB-H15-NEXT-TEST-DUE-DATE.", "05 WB-NEXT-TEST-CCYY PIC 9(04) VALUE ZEROS. 18",
                "05 WB-NEXT-TEST-MM PIC 9(02) VALUE ZEROS. 22", "03 WB-H15-DISTRICT PIC 9(02) VALUE ZEROS. 24", "03 WB-H15-FIELD PIC 9(08) VALUE ZEROS. 26",
                "03 WB-H15-HIST-WELLBORE-FLAG PIC X(01) VALUE SPACES. 34", "88 WB-H15-HIST-DRILLING VALUE 'D'.", "88 WB-H15-HIST-EARLY-COMPL VALUE 'C'.", "03 WB-H15-HIST-WELL-CCYYMMDD.",
                "05 WB-H15-HIST-WELL-CC PIC 9(02) VALUE ZEROS. 35", "05 WB-H15-HIST-WELL-DATE.", "10 WB-H15-HIST-WELL-YY PIC 9(02) VALUE ZEROS. 37",
                "10 WB-H15-HIST-WELL-MM PIC 9(02) VALUE ZEROS. 39", "10 WB-H15-HIST-WELL-DD PIC 9(02) VALUE ZEROS. 41", "03 WB-H15-W1X-WELL PIC X(01) VALUE SPACES. 43",
                "03 WB-H15-OIL-GAS-CODE PIC X(01) VALUE SPACES. 44", "88 WB-H15-GAS-WELL VALUE 'G'.", "88 WB-H15-OIL-WELL VALUE 'O'.", "03 WB-H15-LEASE-NBR PIC 9(05) VALUE ZEROS. 45",
                "03 WB-H15-WELL-NBR PIC X(06) VALUE SPACES. 50", "03 WB-H15-GASID-NBR PIC 9(06) VALUE ZEROS. 56", "03 WB-H15-TEST-DATE.", "05 WB-H15-TEST-YEAR.",
                "10 WB-H15-TEST-CC PIC 9(02) VALUE ZEROS. 62", "10 WB-H15-TEST-YY PIC 9(02) VALUE ZEROS. 64", "05 WB-H15-TEST-MM PIC 9(02) VALUE ZEROS. 66",
                "05 WB-H15-TEST-DD PIC 9(02) VALUE ZEROS. 68", "03 WB-H15-BASE-USABLE-WATER PIC 9(06) VALUE ZEROS. 70", "03 WB-H15-TYPE-TEST-FLAG PIC X(01) VALUE ZEROS. 76",
                "88 WB-H15-FLUID-TEST VALUE 'F'.", "88 WB-H15-MECH-INTEG-TEST VALUE 'M'.", "03 WB-H15-TOP-OF-FLUID PIC 9(06) VALUE ZEROS. 77",
                "03 WB-H15-FLUID-TEST-FLAG PIC X(01) VALUE SPACES. 83", "88 WB-H15-FLUID-WIRE VALUE 'W'.", "88 WB-H15-FLUID-SONIC VALUE 'S'.", "88 WB-H15-FLUID-VISUAL VALUE 'V'.",
                "88 WB-H15-FLUID-OTHER VALUE 'O'.", "03 WB-H15-MECH-INTEG-TEST-FLAG PIC X(01) VALUE SPACE. 84", "88 WB-H15-MECH-HYDRAULIC VALUE 'H'.",
                "88 WB-H15-MECH-INTEG-OTHER VALUE 'O'.", "03 WB-H15-MECH-TEST-REASON-FLAG PIC X(01) VALUE SPACE. 85", "88 WB-H15-MECH-SUBSTITUTE VALUE 'A'.",
                "88 WB-H15-MECH-14B2-REQUIRE VALUE 'B'.", "03 WB-H15-ALTERNATE-TEST-PERIOD PIC 9(02) VALUE ZEROS. 86", "03 WB-H15-OTHER-MIT-TEST-TYPE PIC X(20) VALUE SPACES. 88",
                "03 WB-H15-STATUS-DATE.", "05 WB-H15-STATUS-CC PIC 9(02) VALUE ZEROS. 108", "05 WB-H15-STATUS-YY PIC 9(02) VALUE ZEROS. 110",
                "05 WB-H15-STATUS-MM PIC 9(02) VALUE ZEROS. 112", "05 WB-H15-STATUS-DD PIC 9(02) VALUE ZEROS. 114", "03 WB-H15-NO-DATE-WELL-FLAG PIC X(01) VALUE SPACES. 116",
                "88 WB-H15-NO-DATE-WELL VALUE 'Y'.", "03 WB-H15-RECORD-FROM-EDI-FLAG PIC X(01) VALUE SPACES. 117", "88 WB-H15-RECORD-FROM-EDI VALUE 'Y'.", "03 WB-H15-KEYED-DATE.",
                "05 WB-H15-KEYED-CC PIC 9(02) VALUE ZEROS. 118", "05 WB-H15-KEYED-YY PIC 9(02) VALUE ZEROS. 120", "05 WB-H15-KEYED-MM PIC 9(02) VALUE ZEROS. 122",
                "05 WB-H15-KEYED-DD PIC 9(02) VALUE ZEROS. 124", "03 WB-H15-CHANGED-DATE.", "05 WB-H15-CHANGED-CC PIC 9(02) VALUE ZEROS. 126",
                "05 WB-H15-CHANGED-YY PIC 9(02) VALUE ZEROS. 128", "05 WB-H15-CHANGED-MM PIC 9(02) VALUE ZEROS. 130", "05 WB-H15-CHANGED-DD PIC 9(02) VALUE ZEROS. 132",
                "03 WB-H15-PREVIOUS-STATUS PIC X(01) VALUE SPACE. 134", "03 WB-H15-UIC-TEST-FLAG PIC X(01) VALUE SPACE. 135", "88 WB-H15-UIC-H5-TEST VALUE 'U'.",
                "03 WB-H15-2YRS-APPROVED-FLAG PIC X(01) VALUE SPACE. 136", "88 WB-H15-2YRS-APPROVED VALUE 'Y'.", "03 WB-H15-MAIL-HOLD-FLAG PIC X(01) VALUE SPACE. 137",
                "88 WB-H15-MAIL-HOLD VALUE 'Y'.", "03 WB-H15-10YR-INACTIVE-FLAG PIC X(01) VALUE SPACES. 138", "03 WB-H15-W3X-WELL-FLAG PIC X(01) VALUE SPACE. 139",
                "88 WB-H15-W3X-WELL VALUE 'Y'.", "03 FILLER PIC X(13) VALUE ZEROS. 140", "02 RRC-TAPE-FILLER PIC X(0095). 153"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreH15Remarks";

            lines = new string[]
            {
                "02 WB-H15-REMARKS-SEGMENT.", "03 WB-H15-REMARK-KEY PIC 9(03) VALUE ZEROS. 3", "03 WB-H15-REMARK-TEXT PIC X(70) VALUE SPACES. 6", "03 FILLER PIC X(07) VALUE ZEROS. 76",
                "02 RRC-TAPE-FILLER PIC X(0165). 83"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreSenateBill126";

            lines = new string[]
            {
                "02 WB-SB126-SEGMENT.", "05 WB-SB126-DESIGNATION-FLAG PIC X(1). 3", "88 WB-SB126-AUTO-DESIGNATED VALUE 'A'.", "88 WB-SB126-MANUAL-DESIGNATED VALUE 'M'.",
                "05 WB-SB126-DESIG-EFFECTIVE-DATE.", "10 WB-SB126-DESIG-EFFEC-CCYY.", "15 WB-SB126-DESIG-EFFEC-CC PIC 9(2). 4", "15 WB-SB126-DESIG-EFFEC-YY PIC 9(2). 6",
                "10 WB-SB126-DESIG-EFFEC-MM PIC 9(2). 8", "05 WB-SB126-DESIG-REVISED-DATE.", "10 WB-SB126-DESIG-REVISED-CC PIC 9(2). 10", "10 WB-SB126-DESIG-REVISED-YY PIC 9(2). 12",
                "10 WB-SB126-DESIG-REVISED-MM PIC 9(2). 14", "05 WB-SB126-DESIG-LETTER-DATE.", "10 WB-SB126-DESIG-LETTER-CC PIC 9(2). 16", "10 WB-SB126-DESIG-LETTER-YY PIC 9(2). 18",
                "10 WB-SB126-DESIG-LETTER-MM PIC 9(2). 20", "10 WB-SB126-DESIG-LETTER-DD PIC 9(2). 22", "05 WB-SB126-CERT-EFFECT-DATE.", "10 WB-SB126-CERT-EFFEC-CC PIC 9(2). 24",
                "10 WB-SB126-CERT-EFFEC-YY PIC 9(2). 26", "10 WB-SB126-CERT-EFFEC-MM PIC 9(2). 28", "05 WB-SB126-CERT-REVOKED-DATE.", "10 WB-SB126-CERT-REVOKED-CC PIC 9(2). 30",
                "10 WB-SB126-CERT-REVOKED-YY PIC 9(2). 32", "10 WB-SB126-CERT-REVOKED-MM PIC 9(2). 34", "10 WB-SB126-CERT-REVOKED-DD PIC 9(2). 36", "05 WB-SB126-CERT-DENIAL-DATE.",
                "10 WB-SB126-CERT-DENIAL-CC PIC 9(2). 38", "10 WB-SB126-CERT-DENIAL-YY PIC 9(2). 40", "10 WB-SB126-CERT-DENIAL-MM PIC 9(2). 42",
                "10 WB-SB126-CERT-DENIAL-DD PIC 9(2). 44", "05 WB-SB126-DENIAL-REASON-FLAG PIC X(1). 46", "88 WB-SB126-DENIED-AUTO VALUE 'A'.", "88 WB-SB126-DENIED-MANUAL VALUE 'M'.",
                "05 FILLER PIC X(16). 47", "02 RRC-TAPE-FILLER PIC X(0185). 63"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreDrillingPermitStatus";

            lines = new string[]
            {
                "02 WBDASTAT-SEGMENT.", "03 WBDASTAT-NUMBER. 3", "05 WBDASTAT-STAT-NUM PIC 9(07). 3", "05 WBDASTAT-UNIQ-NUM PIC 9(02). 10", "03 WBDASTAT-DELETED-FLAG PIC X(01). 12",
                "88 WBDASTAT-DELETED VALUE 'Y'.", "03 FILLER PIC X(10). 13", "02 RRC-TAPE-FILLER PIC X(0225). 23"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBoreW3C";

            lines = new string[]
            {
                "02 WELL-BORE-FORM-W3C-SEG.", "03 WB-W3C-1YR-FLAG PIC X(01) VALUE SPACES. 3", "88 WB-W3C-1YR-CERTIFY VALUE 'Y'.", "88 WB-W3C-1YR-FALSE-CERT VALUE 'F'.",
                "03 WB-W3C-1YR-FILED-DATE PIC 9(08) VALUE ZEROS. 4", "03 WB-W3C-1YR-FILING-OPER PIC 9(06) VALUE ZEROS. 12", "03 WB-W3C-5YR-FLAG PIC X(01) VALUE SPACES. 18",
                "88 WB-W3C-5YR-REMOVED VALUE 'R'.", "88 WB-W3C-5YR-OWNER VALUE 'O'.", "88 WB-W3C-5YR-FALSE-CERT VALUE 'F'.", "03 WB-W3C-5YR-FILED-DATE PIC 9(08) VALUE ZEROS. 19",
                "03 WB-W3C-5YR-FILING-OPER PIC 9(06) VALUE ZEROS. 27", "03 WB-W3C-10YR-FLAG PIC X(01) VALUE SPACES. 33", "88 WB-W3C-10YR-REMOVED VALUE 'R'.",
                "88 WB-W3C-10YR-OWNER VALUE 'O'.", "88 WB-W3C-10YR-EOR VALUE 'E'.", "88 WB-W3C-10YR-REJ VALUE 'J'.", "88 WB-W3C-10YR-FALSE-CERT VALUE 'F'.",
                "03 WB-W3C-10YR-FILED-DATE PIC 9(08) VALUE ZEROS. 34", "03 WB-W3C-10YR-FILING-OPER PIC 9(06) VALUE ZEROS. 42", "03 WB-W3C-14B2-REMOVAL-DATE PIC 9(08) VALUE ZEROS. 48",
                "03 WB-W3C-EXTENSION-FLAG PIC X(01) VALUE 'N'. 56", "88 WB-W3C-EXTENSION VALUE 'Y'.", "88 WB-W3C-EXTENSION-FALSE-CERT VALUE 'F'.", "03 WB-W3C-EXTENSION-DATE.",
                "05 WB-W3C-EXTENSION-YEAR PIC 9(04) VALUE ZEROS. 57", "05 WB-W3C-EXTENSION-MONTH PIC 9(02) VALUE ZEROS. 61", "05 WB-W3C-EXTENSION-DAY PIC 9(02) VALUE ZEROS. 63",
                "03 WB-W3C-5YR-FLAG-PREVIOUS PIC X(01) VALUE SPACES. 65", "03 WB-W3C-10YR-FLAG-PREVIOUS PIC X(01) VALUE SPACES. 66", "03 FILLER PIC X(36) VALUE SPACES. 67",
                "03 RRC-TAPE-FILLER PIC X(145). 103"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);

            className = "WellBore14B2Remarks";

            lines = new string[]
            {
                "02 RRC-TAPE-RECORD_ID PIC X(02) 1", "02 WELL-BORE-14B2-RMKS-SEG.", "03 WB-14B2-RMK-LNE-CNT PIC 9(03) VALUE ZEROS. 3", "03 WB-14B2-RMK-DATE PIC 9(08) VALUE ZEROS. 6",
                "03 WB-14B2-RMK-USERID PIC X(08) VALUE SPACES. 14", "03 WB-14B2-REMARKS PIC X(66) VALUE SPACES. 22", "03 FILLER PIC X(15) VALUE SPACES. 88",
                "02 RRC-TAPE-FILLER PIC X(144) VALUE SPACES. 103"
            };

            newClass = TapeRecordClassBuilder.Process(className, lines);

            File.WriteAllText($"R:/{className}.cs", newClass);
        }
    }
}
