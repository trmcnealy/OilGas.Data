// ReSharper disable RedundantArgumentDefaultValue

#nullable enable

using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Xml.Serialization;

using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace OilGas.Data.RRC.Texas
{
    //internal struct Buffer
    //{
    //    [StructLayout(LayoutKind.Sequential, Size = 256)]
    //    [UnsafeValueType]
    //    public struct e__FixedBuffer
    //    {
    //        public char FixedElementField;
    //    }
    //
    //    [FixedBuffer(typeof(char), 128)]
    //    public e__FixedBuffer fixedBuffer;
    //}

    /// <summary>
    /// 
    /// </summary>
    public class PhysicalTapeCharacteristics
    {
        public const int KeyLength      = 2;
        public const int RecordLength   = 240;
        public const int BlockingFactor = 134;
        public const int Blocksize      = 32160;

        public static readonly Dictionary<int, (string SegmentName, string Description)> Keys = new Dictionary<int, (string SegmentName, string Description)>
        {
            {1, ("FLDROOT", "Field Information")},
            {2, ("GASSEG", "Field Gas Information")},
            {3, ("FLMKTDMD", "Field Gas Market Demand Forecast")},
            {4, ("OPRMKTDM", "Field Gas Operator Market Demand Forecast")},
            {5, ("FLMKTRMK", "Field Gas Market Demand Remark")},
            {6, ("FLSUPRMK", "Field Market Demand Supplement Adjustment Remark")},
            {7, ("GASCYCLE", "Field Gas Cycle Information")},
            {8, ("GSFLDRUL", "Field Gas Rule")},
            {9, ("GASAFORM", "Field Gas Allocation Formula")},
            {10, ("GASRMRKS", "Field Gas Remarks")},
            {11, ("GSCOUNTY", "Gas County")},
            {12, ("GASAFACT", "Field Gas Allocation Factors")},
            {13, ("ASHEET", "A-Sheet Balancing Period")},
            {14, ("ASHEETMO", "A-Sheet Monthly Information")},
            {15, ("FLT3ROOT", "T-3 Root")},
            {16, ("FLDT3", "T-3 Form")},
            {17, ("FLDMO", "Field Monthly Statistics")},
            {18, ("CALC49B", "Field 49(B) Calculations")},
            {19, ("OILSEG", "Field Oil Information")},
            {20, ("OILCYCLE", "Field Oil Cycle Information")},
            {21, ("OLFLDRUL", "Field Oil Rules")},
            {22, ("OILAFORM", "Field Oil Allocation Formula")},
            {23, ("OILRMRKS", "Field Oil Remarks")},
            {24, ("OILFTROT", "Field Oil Factors Root")},
            {25, ("OILAFACT", "Field Oil Allocation Factors")},
            {26, ("OLCOUNTY", "Oil County")},
            {27, ("ASSOCGAS", "Associated Gas Fields")},
            {28, ("FLDMAP", "Field Map Index")},
            {29, ("GSOPTRUL", "Field Gas Optional Rule")},
            {30, ("OLOPTRUL", "Field Oil Optional Rule")}
        };

        public static readonly Dictionary<int, Type> SegmentIndexToTypeMap = new Dictionary<int, Type>()
        {
            {1, typeof(FieldInformation)},
            {2, typeof(FieldGasInformation)},
            {3, typeof(FieldGasMarketDemandForecast)},
            {4, typeof(FieldGasOperatorMarketDemandForecast)},
            {5, typeof(FieldGasMarketDemandRemark)},
            {6, typeof(FieldMarketDemandSupplementAdjustmentRemark)},
            {7, typeof(FieldGasCycleInformation)},
            {8, typeof(FieldGasRule)},
            {9, typeof(FieldGasAllocationFormula)},
            {10, typeof(FieldGasRemarks)},
            {11, typeof(GasCounty)},
            {12, typeof(FieldGasAllocationFactors)},
            {13, typeof(ASheetBalancingPeriod)},
            {14, typeof(ASheetMonthlyInformation)},
            {15, typeof(T3Root)},
            {16, typeof(T3Form)},
            {17, typeof(FieldMonthlyStatistics)},
            {18, typeof(Field49BCalculations)},
            {19, typeof(FieldOilInformation)},
            {20, typeof(FieldOilCycleInformation)},
            {21, typeof(FieldOilRules)},
            {22, typeof(FieldOilAllocationFormula)},
            {23, typeof(FieldOilRemarks)},
            {24, typeof(FieldOilFactorsRoot)},
            {25, typeof(FieldOilAllocationFactors)},
            {26, typeof(OilCounty)},
            {27, typeof(AssociatedGasFields)},
            {28, typeof(FieldMapIndex)},
            {29, typeof(FieldGasOptionalRule)},
            {30, typeof(FieldOilOptionalRule)}
        };

        public static readonly Dictionary<string, Type> SegmentNameToTypeMap = new Dictionary<string, Type>()
        {
            {"FLDROOT", typeof(FieldInformation)},
            {"GASSEG", typeof(FieldGasInformation)},
            {"FLMKTDMD", typeof(FieldGasMarketDemandForecast)},
            {"OPRMKTDM", typeof(FieldGasOperatorMarketDemandForecast)},
            {"FLMKTRMK", typeof(FieldGasMarketDemandRemark)},
            {"FLSUPRMK", typeof(FieldMarketDemandSupplementAdjustmentRemark)},
            {"GASCYCLE", typeof(FieldGasCycleInformation)},
            {"GSFLDRUL", typeof(FieldGasRule)},
            {"GASAFORM", typeof(FieldGasAllocationFormula)},
            {"GASRMRKS", typeof(FieldGasRemarks)},
            {"GSCOUNTY", typeof(GasCounty)},
            {"GASAFACT", typeof(FieldGasAllocationFactors)},
            {"ASHEET", typeof(ASheetBalancingPeriod)},
            {"ASHEETMO", typeof(ASheetMonthlyInformation)},
            {"FLT3ROOT", typeof(T3Root)},
            {"FLDT3", typeof(T3Form)},
            {"FLDMO", typeof(FieldMonthlyStatistics)},
            {"CALC49B", typeof(Field49BCalculations)},
            {"OILSEG", typeof(FieldOilInformation)},
            {"OILCYCLE", typeof(FieldOilCycleInformation)},
            {"OLFLDRUL", typeof(FieldOilRules)},
            {"OILAFORM", typeof(FieldOilAllocationFormula)},
            {"OILRMRKS", typeof(FieldOilRemarks)},
            {"OILFTROT", typeof(FieldOilFactorsRoot)},
            {"OILAFACT", typeof(FieldOilAllocationFactors)},
            {"OLCOUNTY", typeof(OilCounty)},
            {"ASSOCGAS", typeof(AssociatedGasFields)},
            {"FLDMAP", typeof(FieldMapIndex)},
            {"GSOPTRUL", typeof(FieldGasOptionalRule)},
            {"OLOPTRUL", typeof(FieldOilOptionalRule)}
        };

        public static readonly Dictionary<int, List<int>> FieldDatabaseHierarchy = new Dictionary<int, List<int>>
        {
            //Oil Field
            {1, new List<int> {17, 19, 28}},
            {19, new List<int> {20, 21, 23, 24, 26, 27}},
            {21, new List<int> {30, 22}},
            {24, new List<int> {25}},

            //Gas Field
            {1, new List<int> {17, 18, 02, 12, 13, 28}},
            {2, new List<int> {3, 7, 11, 10, 8}},
            {28, new List<int> {14}},
            {7, new List<int> {4, 5, 6}},
            {8, new List<int> {29, 09}}
        };
    }

    /// <summary>
    /// 140 BYTES
    /// </summary>
    [Serializable]
    [DataContract]
    [XmlRoot(nameof(FieldInformation))]
    [Table(nameof(FieldInformation))]
    public class FieldInformation : IFieldInformationRecord, IEquatable<FieldInformation>
    {
        public const int RecordLength = 240;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public const char FL_GAS_FIELD        = 'G';
        public const char FL_OIL_FIELD        = 'O';
        public const char FL_ASSOCIATED_FIELD = 'B';

        public const string FL_SANDSTONE         = "SS";
        public const string FL_LIMESTONE         = "LS";
        public const string FL_DOLOMITE          = "DO";
        public const string FL_ANHYDRITE         = "AN";
        public const string FL_CONGLOMERATE      = "CG";
        public const string FL_GRANITE_WASH      = "GW";
        public const string FL_WEATHERED_GRANITE = "WG";
        public const string FL_SERPENTINE        = "SP";
        public const string FL_DOLOMITE_LIMES    = "DL";

        public const char ONE_MONTH_TEST   = '1';
        public const char THREE_MONTH_TEST = '3';
        public const char FOUR_MONTH_TEST  = '4';

        public const char FL_NO_H2S                 = 'N';
        public const char FL_H2S_PRESENT            = 'Y';
        public const char FL_H2S_PRESENT_BUT_EXEMPT = 'E';

        public const char FL_GAS_OIL_RATIO               = 'G';
        public const char FL_GAS_LIMIT                   = 'L';
        public const char FL_GAS_LIMIT_BASED_ON_TOP_WELL = 'K';

        public const char FL_REGULAR_RULE         = 'R';
        public const char FL_NET_BASED_ON_GOR     = 'G';
        public const char FL_NET_LIMITED_AMOUNT   = 'L';
        public const char FL_NET_UNLIMITED_AMOUNT = 'U';

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        [Key]
        public int Id
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember][XmlElement][JsonProperty(nameof(RRC_TAPE_RECORD_ID), NamingStrategyType = typeof(DefaultNamingStrategy))] public short? RRC_TAPE_RECORD_ID { [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] get; [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)] set; }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_DISTRICT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_DISTRICT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_FIELD_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_FIELD_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_RESERVOIR_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? FL_RESERVOIR_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? FL_NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_FIELD_CLASS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FL_FIELD_CLASS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_RESERVOIR_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? FL_RESERVOIR_NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_FORMATION_COMPOSITION), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? FL_FORMATION_COMPOSITION
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_4_MONTH_TEST_EXCPT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public sbyte? FL_4_MONTH_TEST_EXCPT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_HYDROGEN_SULFIDE_CD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FL_HYDROGEN_SULFIDE_CD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_OIL_RATIO_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FL_GAS_OIL_RATIO_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_OIL_RATIO_OR_LIMIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_GAS_OIL_RATIO_OR_LIMIT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_NET_GOR_RULE_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FL_NET_GOR_RULE_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_NET_GOR_RATIO_OR_LIMIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_NET_GOR_RATIO_OR_LIMIT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_OIL_DISC_WELL_GRAVITY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FL_OIL_DISC_WELL_GRAVITY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_ASSOC_OIL_FIELD_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_ASSOC_OIL_FIELD_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_DISCOV_PERMIT_NUM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_DISCOV_PERMIT_NUM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_NEW_FIELD_DISCOV_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FL_NEW_FIELD_DISCOV_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_NFD_SYS_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_NFD_SYS_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_NFD_SYS_YR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_NFD_SYS_YR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_NFD_SYS_MO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_NFD_SYS_MO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_NFD_SYS_DA), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_NFD_SYS_DA
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_RRC_RETURNED_TO_PR_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FL_RRC_RETURNED_TO_PR_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_RRC_RET_TO_PR_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_RRC_RET_TO_PR_CENTURY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_RRC_RET_TO_PR_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_RRC_RET_TO_PR_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_RRC_RET_TO_PR_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_RRC_RET_TO_PR_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_SET_TO_AOF_BY_ORDER_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FL_SET_TO_AOF_BY_ORDER_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_SET_TO_AOF_BY_ORDER_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_SET_TO_AOF_BY_ORDER_CENTURY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_SET_TO_AOF_BY_ORDER_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_SET_TO_AOF_BY_ORDER_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_SET_TO_AOF_BY_ORDER_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public sbyte? FL_SET_TO_AOF_BY_ORDER_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_MANUAL_REVIEW_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FL_MANUAL_REVIEW_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        [ForeignKey("Id")]
        public FieldGasInformation? FieldGasInformation
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        [ForeignKey("Id")]
        public Field49BCalculations? Field49BCalculations
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldInformation()
        {
        }

        public FieldInformation(ReadOnlySpan<byte> source)
        {
            RRC_TAPE_RECORD_ID       = StringParser.ReadAsInt16(source, 1   - 1, 2);
            FL_DISTRICT              = StringParser.ReadAsInt16(source, 3   - 1, 2);
            FL_FIELD_NUMBER          = StringParser.ReadAsInt64(source, 5   - 1, 5);
            FL_RESERVOIR_NUMBER      = StringParser.ReadAsInt32(source, 10  - 1, 3);
            FL_NAME                  = StringParser.ReadAsString(source, 13 - 1, 32);
            FL_FIELD_CLASS           = StringParser.ReadAsChar(source, 45   - 1, 1);
            FL_RESERVOIR_NAME        = StringParser.ReadAsString(source, 46 - 1, 30);
            FL_FORMATION_COMPOSITION = StringParser.ReadAsString(source, 76 - 1, 2);
            StringParser.ReadAsString(source, 78 - 1, 5);
            FL_4_MONTH_TEST_EXCPT = StringParser.ReadAsSByte(source, 83 - 1);
            StringParser.ReadAsString(source, 84 - 1, 1);
            FL_HYDROGEN_SULFIDE_CD     = StringParser.ReadAsChar(source, 85      - 1, 1);
            FL_GAS_OIL_RATIO_CODE      = StringParser.ReadAsChar(source, 86      - 1);
            FL_GAS_OIL_RATIO_OR_LIMIT  = StringParser.ReadAsInt64(source, 87     - 1, 5);
            FL_NET_GOR_RULE_CODE       = StringParser.ReadAsChar(source, 92      - 1);
            FL_NET_GOR_RATIO_OR_LIMIT  = StringParser.ReadAsInt64(source, 93     - 1, 5);
            FL_OIL_DISC_WELL_GRAVITY   = StringParser.ReadAsSingle(source, 2, 98 - 1, 3);
            FL_ASSOC_OIL_FIELD_NUMBER  = StringParser.ReadAsInt64(source, 101    - 1, 8);
            FL_DISCOV_PERMIT_NUM       = StringParser.ReadAsInt64(source, 109    - 1, 7);
            FL_NEW_FIELD_DISCOV_FLAG   = StringParser.ReadAsChar(source, 116     - 1, 1);
            FL_NFD_SYS_CC              = StringParser.ReadAsInt16(source, 117    - 1, 2);
            FL_NFD_SYS_YR              = StringParser.ReadAsInt16(source, 119    - 1, 2);
            FL_NFD_SYS_MO              = StringParser.ReadAsInt16(source, 121    - 1, 2);
            FL_NFD_SYS_DA              = StringParser.ReadAsInt16(source, 123    - 1, 2);
            FL_RRC_RETURNED_TO_PR_FLAG = StringParser.ReadAsChar(source, 125     - 1, 1);

            FL_RRC_RET_TO_PR_CENTURY       = StringParser.ReadAsInt16(source, 126 - 1, 2);
            FL_RRC_RET_TO_PR_YEAR          = StringParser.ReadAsInt16(source, 128 - 1, 2);
            FL_RRC_RET_TO_PR_MONTH         = StringParser.ReadAsInt16(source, 130 - 1, 2);
            FL_SET_TO_AOF_BY_ORDER_FLAG    = StringParser.ReadAsChar(source, 132  - 1, 2);
            FL_SET_TO_AOF_BY_ORDER_CENTURY = StringParser.ReadAsInt16(source, 133 - 1, 2);
            FL_SET_TO_AOF_BY_ORDER_YEAR    = StringParser.ReadAsInt16(source, 135 - 1, 2);
            FL_SET_TO_AOF_BY_ORDER_MONTH   = StringParser.ReadAsSByte(source, 137 - 1);
            FL_MANUAL_REVIEW_FLAG          = StringParser.ReadAsChar(source, 139  - 1);
            StringParser.ReadAsString(source, 140 - 1, 3);
            StringParser.ReadAsString(source, 143 - 1, 98);
        }

        public bool Equals(FieldInformation? other)
        {
            if(ReferenceEquals(null, other))
            {
                return false;
            }

            if(ReferenceEquals(this, other))
            {
                return true;
            }

            return FL_DISTRICT         == other.FL_DISTRICT                                  &&
                   FL_FIELD_NUMBER     == other.FL_FIELD_NUMBER                              &&
                   FL_RESERVOIR_NUMBER == other.FL_RESERVOIR_NUMBER                          &&
                   FL_FIELD_CLASS      == other.FL_FIELD_CLASS                               &&
                   Nullable.Equals(FL_OIL_DISC_WELL_GRAVITY, other.FL_OIL_DISC_WELL_GRAVITY) &&
                   FL_ASSOC_OIL_FIELD_NUMBER == other.FL_ASSOC_OIL_FIELD_NUMBER              &&
                   FL_DISCOV_PERMIT_NUM      == other.FL_DISCOV_PERMIT_NUM;
        }

        public override bool Equals(object? obj)
        {
            if(ReferenceEquals(null, obj))
            {
                return false;
            }

            if(ReferenceEquals(this, obj))
            {
                return true;
            }

            if(obj.GetType() != GetType())
            {
                return false;
            }

            return Equals((FieldInformation)obj);
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(FL_DISTRICT, FL_FIELD_NUMBER, FL_RESERVOIR_NUMBER, FL_FIELD_CLASS, FL_OIL_DISC_WELL_GRAVITY, FL_ASSOC_OIL_FIELD_NUMBER, FL_DISCOV_PERMIT_NUM);
        }

        public static bool operator ==(FieldInformation? left,
                                       FieldInformation? right)
        {
            return Equals(left, right);
        }

        public static bool operator !=(FieldInformation? left,
                                       FieldInformation? right)
        {
            return !Equals(left, right);
        }
    }

    /// <summary>
    /// 200 BYTES
    /// </summary>
    [Serializable]
    [DataContract]
    [XmlRoot(nameof(FieldGasInformation))]
    [Table(nameof(FieldGasInformation))]
    public class FieldGasInformation : IFieldInformationRecord, IEquatable<FieldGasInformation>
    {
        public const int RecordLength = 200;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public const char FL_GASSEG_KEY = 'G';

        public const char FL_ASSOCIATED     = 'A';
        public const char FL_NON_ASSOCIATED = 'N';

        public const char FL_ANNUAL_TESTING      = 'A';
        public const char FL_SEMI_ANNUAL_TESTING = 'S';

        public const string FL_LAND                        = "L ";
        public const string FL_BAYS_ESTUARIES              = "B ";
        public const string FL_STATE_OFFSHORE              = "SO";
        public const string FL_LAND_BAYS_ESTUARIES         = "LB";
        public const string FL_BAYS_ESTUARIES_OFFSHORE     = "BO";
        public const string FL_LAND_BAYS_ESTUARIE_OFFSHORE = "AL";
        public const string FL_STATE_FEDERAL               = "SF";

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        [Key]
        public int Id
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(RRC_TAPE_RECORD_ID), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_DISC_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_GAS_DISC_CENTURY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_DISC_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_GAS_DISC_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_DISC_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_GAS_DISC_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_DISC_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_GAS_DISC_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_DISC_COUNTY_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? FL_GAS_DISC_COUNTY_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_PERF_1ST_WELL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_GAS_PERF_1ST_WELL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_TYPE_FIELD_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FL_TYPE_FIELD_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_TESTING_COUNTY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? FL_GAS_TESTING_COUNTY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_TEST_FREQUENCY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FL_GAS_TEST_FREQUENCY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_PRI_ALTER_TEST_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_GAS_PRI_ALTER_TEST_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_SEC_ALTER_TEST_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_GAS_SEC_ALTER_TEST_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_PRI_TEST_MON_G10_REQUIRE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FL_PRI_TEST_MON_G10_REQUIRE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_SEC_TEST_MON_G10_REQUIRE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FL_SEC_TEST_MON_G10_REQUIRE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_COMMINGLING_COUNTER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_GAS_COMMINGLING_COUNTER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_EXEMPT_MINIMUM_GOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FL_GAS_EXEMPT_MINIMUM_GOR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_OFFSHORE_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? FL_OFFSHORE_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_CUM_GAS_PRODUCTION_TO_CONV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_CUM_GAS_PRODUCTION_TO_CONV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_CUM_COND_PRODUCTION_TO_CONV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_CUM_COND_PRODUCTION_TO_CONV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_CUM_GAS_ALLOWABLE_TO_CONV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_CUM_GAS_ALLOWABLE_TO_CONV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_CUM_LIQ_ALLOWABLE_TO_CONV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_CUM_LIQ_ALLOWABLE_TO_CONV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_OFF_FILE_CUM_GAS_ALLOWABLE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_OFF_FILE_CUM_GAS_ALLOWABLE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_OFF_FILE_CUM_LIQ_ALLOWABLE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_OFF_FILE_CUM_LIQ_ALLOWABLE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_OFF_FILE_CUM_GAS_PROD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_OFF_FILE_CUM_GAS_PROD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_OFF_FILE_CUM_COND_PROD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_OFF_FILE_CUM_COND_PROD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_ON_FILE_CUM_GAS_ALLOWABLE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_ON_FILE_CUM_GAS_ALLOWABLE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_ON_FILE_CUM_LIQ_ALLOWABLE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_ON_FILE_CUM_LIQ_ALLOWABLE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_ON_FILE_CUM_GAS_PROD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_ON_FILE_CUM_GAS_PROD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_ON_FILE_CUM_COND_PROD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_ON_FILE_CUM_COND_PROD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_YR_TO_DT_CUM_GAS_ALLOWABLE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_YR_TO_DT_CUM_GAS_ALLOWABLE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_YR_TO_DT_CUM_LIQ_ALLOWABLE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_YR_TO_DT_CUM_LIQ_ALLOWABLE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_YR_TO_DT_CUM_GAS_PROD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_YR_TO_DT_CUM_GAS_PROD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_YR_TO_DT_CUM_COND_PROD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_YR_TO_DT_CUM_COND_PROD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_SALT_DOME_EXEMPTION), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FL_SALT_DOME_EXEMPTION
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_COUNTY_REGULAR_EXEMPTION), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FL_COUNTY_REGULAR_EXEMPTION
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_LEDGER_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_LEDGER_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_NEW_GAS_FLD_APPR_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_NEW_GAS_FLD_APPR_CENTURY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_NEW_GAS_FLD_APPR_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_NEW_GAS_FLD_APPR_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_NEW_GAS_FLD_APPR_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_NEW_GAS_FLD_APPR_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_NEW_GAS_FLD_APPR_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_NEW_GAS_FLD_APPR_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_CUM_PROD_PRIOR_1970), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_CUM_PROD_PRIOR_1970
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_SCHED_START_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_GAS_SCHED_START_CENTURY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_SCHED_START_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_GAS_SCHED_START_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_SCHED_START_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_GAS_SCHED_START_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_SCHED_START_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_GAS_SCHED_START_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_CONSOLIDATED_FIELD_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FL_GAS_CONSOLIDATED_FIELD_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_CORRELATIVE_INTER_FROM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_GAS_CORRELATIVE_INTER_FROM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_CORRELATIVE_INTER_TO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_GAS_CORRELATIVE_INTER_TO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        [ForeignKey("Id")]
        public int? FieldInformationId
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldGasInformation()
        {
        }

        public FieldGasInformation(ReadOnlySpan<byte> source)
        {
            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, 1 - 1, 2);

            if(FL_GASSEG_KEY != StringParser.ReadAsChar(source, 3 - 1, 1))
            {
            }

            FL_GAS_DISC_CENTURY     = StringParser.ReadAsInt16(source, 4  - 1, 2);
            FL_GAS_DISC_YEAR        = StringParser.ReadAsInt16(source, 6  - 1, 2);
            FL_GAS_DISC_MONTH       = StringParser.ReadAsInt16(source, 8  - 1, 2);
            FL_GAS_DISC_DAY         = StringParser.ReadAsInt16(source, 10 - 1, 2);
            FL_GAS_DISC_COUNTY_CODE = StringParser.ReadAsInt32(source, 12 - 1, 3);
            FL_GAS_PERF_1ST_WELL    = StringParser.ReadAsInt64(source, 15 - 1, 5);
            FL_TYPE_FIELD_CODE      = StringParser.ReadAsChar(source, 20  - 1, 1);
            StringParser.ReadAsString(source, 21 - 1, 8);
            FL_GAS_TESTING_COUNTY       = StringParser.ReadAsInt32(source, 29 - 1, 3);
            FL_GAS_TEST_FREQUENCY       = StringParser.ReadAsChar(source, 32  - 1, 1);
            FL_GAS_PRI_ALTER_TEST_MONTH = StringParser.ReadAsInt16(source, 33 - 1, 2);
            FL_GAS_SEC_ALTER_TEST_MONTH = StringParser.ReadAsInt16(source, 35 - 1, 2);
            FL_PRI_TEST_MON_G10_REQUIRE = StringParser.ReadAsChar(source, 37  - 1, 1);
            FL_SEC_TEST_MON_G10_REQUIRE = StringParser.ReadAsChar(source, 38  - 1, 1);
            StringParser.ReadAsString(source, 39 - 1, 2);
            FL_GAS_COMMINGLING_COUNTER = StringParser.ReadAsInt64(source, 41  - 1, 5);
            FL_GAS_EXEMPT_MINIMUM_GOR  = StringParser.ReadAsChar(source, 46   - 1, 1);
            FL_OFFSHORE_CODE           = StringParser.ReadAsString(source, 47 - 1, 2);
            StringParser.ReadAsString(source, 49 - 1, 1);
            FL_CUM_GAS_PRODUCTION_TO_CONV  = StringParser.ReadAsPackedInt64(source, 50  - 1, 7);
            FL_CUM_COND_PRODUCTION_TO_CONV = StringParser.ReadAsPackedInt64(source, 57  - 1, 6);
            FL_CUM_GAS_ALLOWABLE_TO_CONV   = StringParser.ReadAsPackedInt64(source, 63  - 1, 7);
            FL_CUM_LIQ_ALLOWABLE_TO_CONV   = StringParser.ReadAsPackedInt64(source, 70  - 1, 7);
            FL_OFF_FILE_CUM_GAS_ALLOWABLE  = StringParser.ReadAsPackedInt64(source, 77  - 1, 7);
            FL_OFF_FILE_CUM_LIQ_ALLOWABLE  = StringParser.ReadAsPackedInt64(source, 84  - 1, 7);
            FL_OFF_FILE_CUM_GAS_PROD       = StringParser.ReadAsPackedInt64(source, 91  - 1, 7);
            FL_OFF_FILE_CUM_COND_PROD      = StringParser.ReadAsPackedInt64(source, 98  - 1, 6);
            FL_ON_FILE_CUM_GAS_ALLOWABLE   = StringParser.ReadAsPackedInt64(source, 104 - 1, 7);
            FL_ON_FILE_CUM_LIQ_ALLOWABLE   = StringParser.ReadAsPackedInt64(source, 111 - 1, 7);
            FL_ON_FILE_CUM_GAS_PROD        = StringParser.ReadAsPackedInt64(source, 118 - 1, 7);
            FL_ON_FILE_CUM_COND_PROD       = StringParser.ReadAsPackedInt64(source, 125 - 1, 6);
            FL_YR_TO_DT_CUM_GAS_ALLOWABLE  = StringParser.ReadAsPackedInt64(source, 131 - 1, 6);
            FL_YR_TO_DT_CUM_LIQ_ALLOWABLE  = StringParser.ReadAsPackedInt64(source, 137 - 1, 6);
            FL_YR_TO_DT_CUM_GAS_PROD       = StringParser.ReadAsPackedInt64(source, 143 - 1, 6);
            FL_YR_TO_DT_CUM_COND_PROD      = StringParser.ReadAsPackedInt64(source, 149 - 1, 6);
            FL_SALT_DOME_EXEMPTION         = StringParser.ReadAsChar(source, 155   - 1, 1);
            FL_COUNTY_REGULAR_EXEMPTION    = StringParser.ReadAsChar(source, 156   - 1, 1);
            FL_LEDGER_MONTH                = StringParser.ReadAsInt16(source, 157  - 1, 2);
            FL_NEW_GAS_FLD_APPR_CENTURY    = StringParser.ReadAsInt16(source, 159  - 1, 2);
            FL_NEW_GAS_FLD_APPR_YEAR       = StringParser.ReadAsInt16(source, 161  - 1, 2);
            FL_NEW_GAS_FLD_APPR_MONTH      = StringParser.ReadAsInt16(source, 163  - 1, 2);
            FL_NEW_GAS_FLD_APPR_DAY        = StringParser.ReadAsInt16(source, 165  - 1, 2);
            FL_CUM_PROD_PRIOR_1970         = StringParser.ReadAsPackedInt64(source, 167 - 1, 7);
            FL_GAS_SCHED_START_CENTURY     = StringParser.ReadAsInt16(source, 174  - 1, 2);
            FL_GAS_SCHED_START_YEAR        = StringParser.ReadAsInt16(source, 176  - 1, 2);
            FL_GAS_SCHED_START_MONTH       = StringParser.ReadAsInt16(source, 178  - 1, 2);
            FL_GAS_SCHED_START_DAY         = StringParser.ReadAsInt16(source, 180  - 1, 2);
            FL_GAS_CONSOLIDATED_FIELD_FLAG = StringParser.ReadAsChar(source, 182   - 1, 1);
            FL_GAS_CORRELATIVE_INTER_FROM  = StringParser.ReadAsInt64(source, 183  - 1, 5);
            FL_GAS_CORRELATIVE_INTER_TO    = StringParser.ReadAsInt64(source, 188  - 1, 5);
            StringParser.ReadAsString(source, 193 - 1, 10);
            StringParser.ReadAsString(source, 203 - 1, 38);
        }

        public bool Equals(FieldGasInformation? other)
        {
            if(ReferenceEquals(null, other))
            {
                return false;
            }

            if(ReferenceEquals(this, other))
            {
                return true;
            }

            return FL_GAS_DISC_CENTURY     == other.FL_GAS_DISC_CENTURY     &&
                   FL_GAS_DISC_YEAR        == other.FL_GAS_DISC_YEAR        &&
                   FL_GAS_DISC_MONTH       == other.FL_GAS_DISC_MONTH       &&
                   FL_GAS_DISC_DAY         == other.FL_GAS_DISC_DAY         &&
                   FL_GAS_DISC_COUNTY_CODE == other.FL_GAS_DISC_COUNTY_CODE &&
                   FL_GAS_PERF_1ST_WELL    == other.FL_GAS_PERF_1ST_WELL    &&
                   FL_TYPE_FIELD_CODE      == other.FL_TYPE_FIELD_CODE      &&
                   FL_OFFSHORE_CODE        == other.FL_OFFSHORE_CODE;
        }

        public override bool Equals(object? obj)
        {
            if(ReferenceEquals(null, obj))
            {
                return false;
            }

            if(ReferenceEquals(this, obj))
            {
                return true;
            }

            if(obj.GetType() != GetType())
            {
                return false;
            }

            return Equals((FieldGasInformation)obj);
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(FL_GAS_DISC_CENTURY,
                                    FL_GAS_DISC_YEAR,
                                    FL_GAS_DISC_MONTH,
                                    FL_GAS_DISC_DAY,
                                    FL_GAS_DISC_COUNTY_CODE,
                                    FL_GAS_PERF_1ST_WELL,
                                    FL_TYPE_FIELD_CODE,
                                    FL_OFFSHORE_CODE);
        }

        public static bool operator ==(FieldGasInformation? left,
                                       FieldGasInformation? right)
        {
            return Equals(left, right);
        }

        public static bool operator !=(FieldGasInformation? left,
                                       FieldGasInformation? right)
        {
            return !Equals(left, right);
        }
    }

    public class FieldGasMarketDemandForecast : IFieldInformationRecord
    {
        public const int RecordLength = 100;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public char? FL_MKT_DEMAND_SCHED_CCYY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public short? FL_MKT_DEMAND_SCHED_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_MKT_DEMAND_FORECAST
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_MKT_DEMAND_CAPABILITY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_MKT_DMD_FORECAST_CORR_ADJ
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_MKT_DEMAND_SUPP_CHG_ADJ
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_MKT_DEMAND_COMMISSION_ADJ
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_MKT_DEMAND_REV_FORECAST
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_MKT_DMD_ADJ_RES_FORECAST
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_MKT_DMD_CALC_RES_FORECAST
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_MKT_DMD_TOTAL_RES_FORECAST
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_MKT_DMD_HEARING_SPEC_AMT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_MKT_DMD_3RD_MONTH_PREVIOUS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_MKT_DMD_SPECIAL_UNDERAGE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_MKT_DMD_PRECALC_ALLOWABLE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_MKT_DMD_12_MONTH_PEAK
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldGasMarketDemandForecast(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID            = StringParser.ReadAsInt16(source, ref index, 2);
            FL_MKT_DEMAND_SCHED_CCYY      = StringParser.ReadAsChar(source, ref index, 4);
            FL_MKT_DEMAND_SCHED_MM        = StringParser.ReadAsInt16(source, ref index, 2);
            FL_MKT_DEMAND_FORECAST        = StringParser.ReadAsString(source, ref index, 5);
            FL_MKT_DEMAND_CAPABILITY      = StringParser.ReadAsString(source, ref index, 5);
            FL_MKT_DMD_FORECAST_CORR_ADJ  = StringParser.ReadAsString(source, ref index, 5);
            FL_MKT_DEMAND_SUPP_CHG_ADJ    = StringParser.ReadAsString(source, ref index, 5);
            FL_MKT_DEMAND_COMMISSION_ADJ  = StringParser.ReadAsString(source, ref index, 5);
            FL_MKT_DEMAND_REV_FORECAST    = StringParser.ReadAsString(source, ref index, 5);
            FL_MKT_DMD_ADJ_RES_FORECAST   = StringParser.ReadAsString(source, ref index, 5);
            FL_MKT_DMD_CALC_RES_FORECAST  = StringParser.ReadAsString(source, ref index, 5);
            FL_MKT_DMD_TOTAL_RES_FORECAST = StringParser.ReadAsString(source, ref index, 5);
            FL_MKT_DMD_HEARING_SPEC_AMT   = StringParser.ReadAsString(source, ref index, 5);
            FL_MKT_DMD_3RD_MONTH_PREVIOUS = StringParser.ReadAsString(source, ref index, 5);
            FL_MKT_DMD_SPECIAL_UNDERAGE   = StringParser.ReadAsString(source, ref index, 9);
            FL_MKT_DMD_PRECALC_ALLOWABLE  = StringParser.ReadAsString(source, ref index, 5);
            FL_MKT_DMD_12_MONTH_PEAK      = StringParser.ReadAsString(source, ref index, 5);
            StringParser.ReadAsString(source, ref index, 20);
            StringParser.ReadAsString(source, ref index, 138);
        }
    }

    public class FieldGasOperatorMarketDemandForecast : IFieldInformationRecord
    {
        public const int RecordLength = 100;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public long? FL_OPERATOR_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_OPR_MKT_DMD_FORECAST
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_OPR_MKT_DMD_OPT_FORECAST
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_OPR_CAPABILITY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_OPR_SUBSTITUTE_CAPABILITY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_OPR_MKT_DMD_ADJ_FORECAST
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_OPR_MKT_DMD_REV_FORECAST
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_OPR_MKT_DMD_G10_TOTAL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_OPR_MKT_DMD_HIGH_PROD_TOTAL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_OPR_MKT_DMD_HIGH_PROD_WELLS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_OPR_MKT_DMD_SUB_CAP_WELLS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_OPR_MKT_DMD_G10_WELLS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_OPR_MKT_DMD_DELQ_P2_WELLS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_OPR_MKT_DMD_3RD_MO_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_OPR_MKT_DMD_12_MONTH_PEAK
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldGasOperatorMarketDemandForecast(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
            FL_OPERATOR_NUMBER = StringParser.ReadAsInt64(source, ref index, 6);

            FL_OPR_MKT_DMD_FORECAST        = StringParser.ReadAsString(source, ref index, 5);
            FL_OPR_MKT_DMD_OPT_FORECAST    = StringParser.ReadAsString(source, ref index, 5);
            FL_OPR_CAPABILITY              = StringParser.ReadAsString(source, ref index, 5);
            FL_OPR_SUBSTITUTE_CAPABILITY   = StringParser.ReadAsString(source, ref index, 5);
            FL_OPR_MKT_DMD_ADJ_FORECAST    = StringParser.ReadAsString(source, ref index, 5);
            FL_OPR_MKT_DMD_REV_FORECAST    = StringParser.ReadAsString(source, ref index, 5);
            FL_OPR_MKT_DMD_G10_TOTAL       = StringParser.ReadAsString(source, ref index, 5);
            FL_OPR_MKT_DMD_HIGH_PROD_TOTAL = StringParser.ReadAsString(source, ref index, 5);
            FL_OPR_MKT_DMD_HIGH_PROD_WELLS = StringParser.ReadAsString(source, ref index, 5);
            FL_OPR_MKT_DMD_SUB_CAP_WELLS   = StringParser.ReadAsString(source, ref index, 5);
            FL_OPR_MKT_DMD_G10_WELLS       = StringParser.ReadAsString(source, ref index, 5);
            FL_OPR_MKT_DMD_DELQ_P2_WELLS   = StringParser.ReadAsString(source, ref index, 5);
            FL_OPR_MKT_DMD_3RD_MO_PREV     = StringParser.ReadAsString(source, ref index, 5);
            FL_OPR_MKT_DMD_12_MONTH_PEAK   = StringParser.ReadAsString(source, ref index, 5);

            StringParser.ReadAsString(source, ref index, 31);
            StringParser.ReadAsString(source, ref index, 138);
        }
    }

    public class FieldGasMarketDemandRemark : IFieldInformationRecord
    {
        public const int RecordLength = 80;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_MKT_DMD_COMM_ADJ_REMARKS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldGasMarketDemandRemark(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID          = StringParser.ReadAsInt16(source, ref index, 2);
            FL_MKT_DMD_COMM_ADJ_REMARKS = StringParser.ReadAsString(source, ref index, 80);
            StringParser.ReadAsString(source, ref index, 158);
        }
    }

    public class FieldMarketDemandSupplementAdjustmentRemark : IFieldInformationRecord
    {
        public const int RecordLength = 80;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FL_MKT_DMD_SUPP_ADJ_REMARKS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldMarketDemandSupplementAdjustmentRemark(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID          = StringParser.ReadAsInt16(source, ref index, 2);
            FL_MKT_DMD_SUPP_ADJ_REMARKS = StringParser.ReadAsString(source, ref index, 80);
            StringParser.ReadAsString(source, ref index, 158);
        }
    }

    public class FieldGasCycleInformation : IFieldInformationRecord
    {
        public const int RecordLength = 55;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldGasCycleInformation(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class FieldGasRule : IFieldInformationRecord
    {
        public const int RecordLength = 238;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldGasRule(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class FieldGasAllocationFormula : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldGasAllocationFormula(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class FieldGasRemarks : IFieldInformationRecord
    {
        public const int RecordLength = 100;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldGasRemarks(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class GasCounty : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public GasCounty(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class FieldGasAllocationFactors : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldGasAllocationFactors(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class ASheetBalancingPeriod : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public ASheetBalancingPeriod(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class ASheetMonthlyInformation : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public ASheetMonthlyInformation(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class T3Root : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public T3Root(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class T3Form : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public T3Form(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class FieldMonthlyStatistics : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldMonthlyStatistics(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    [Serializable]
    [DataContract]
    [XmlRoot(nameof(Field49BCalculations))]
    [Table(nameof(Field49BCalculations))]
    public class Field49BCalculations : IFieldInformationRecord, IEquatable<Field49BCalculations>
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        [Key]
        public int Id
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(RRC_TAPE_RECORD_ID), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_ALLOW_EFFECTIVE_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_GAS_ALLOW_EFFECTIVE_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_ALLOW_EFFECTIVE_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_GAS_ALLOW_EFFECTIVE_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_ALLOW_EFFECTIVE_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_GAS_ALLOW_EFFECTIVE_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_GAS_ALLOW_EFFECTIVE_DD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? FL_GAS_ALLOW_EFFECTIVE_DD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_RRCID_DETERMINING_WELL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_RRCID_DETERMINING_WELL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_G_1_GAS_GRAVITY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FL_G_1_GAS_GRAVITY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_AVG_RESERVOIR_BHP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FL_AVG_RESERVOIR_BHP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_AVG_RESERVOIR_BH_TEMP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FL_AVG_RESERVOIR_BH_TEMP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_FORMATION_VOLUME_FACTOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FL_FORMATION_VOLUME_FACTOR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_SOLUTION_GAS_OIL_RATIO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FL_SOLUTION_GAS_OIL_RATIO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_DEVIATION_FACTOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FL_DEVIATION_FACTOR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_TOP_DAILY_GAS_ALLOW_CU_FT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_TOP_DAILY_GAS_ALLOW_CU_FT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_TOP_DAILY_GAS_ALLOW_MCF), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_TOP_DAILY_GAS_ALLOW_MCF
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        [ForeignKey("Id")]
        public int? FieldInformationId
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public Field49BCalculations()
        {
        }

        public Field49BCalculations(ReadOnlySpan<byte> source)
        {
            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, 1 - 1, 2);

            FL_GAS_ALLOW_EFFECTIVE_CC = StringParser.ReadAsInt16(source, 3  - 1, 2);
            FL_GAS_ALLOW_EFFECTIVE_YY = StringParser.ReadAsInt16(source, 5  - 1, 2);
            FL_GAS_ALLOW_EFFECTIVE_MM = StringParser.ReadAsInt16(source, 7  - 1, 2);
            FL_GAS_ALLOW_EFFECTIVE_DD = StringParser.ReadAsInt16(source, 9  - 1, 2);
            FL_RRCID_DETERMINING_WELL = StringParser.ReadAsInt64(source, 11 - 1, 6);
            StringParser.ReadAsString(source, 17 - 1, 2);
            FL_G_1_GAS_GRAVITY           = StringParser.ReadAsSingle(source, 3, 19 - 1, 3);
            FL_AVG_RESERVOIR_BHP         = StringParser.ReadAsSingle(source, 6, 22 - 1, 5);
            FL_AVG_RESERVOIR_BH_TEMP     = StringParser.ReadAsSingle(source, 4, 27 - 1, 3);
            FL_FORMATION_VOLUME_FACTOR   = StringParser.ReadAsSingle(source, 4, 30 - 1, 5);
            FL_SOLUTION_GAS_OIL_RATIO    = StringParser.ReadAsSingle(source, 4, 35 - 1, 9);
            FL_DEVIATION_FACTOR          = StringParser.ReadAsSingle(source, 4, 44 - 1, 5);
            FL_TOP_DAILY_GAS_ALLOW_CU_FT = StringParser.ReadAsPackedInt64(source, 49    - 1, 6);
            FL_TOP_DAILY_GAS_ALLOW_MCF   = StringParser.ReadAsPackedInt64(source, 55    - 1, 4);

            StringParser.ReadAsString(source, 59 - 1, 14);

            StringParser.ReadAsString(source, 73 - 1, 168);
        }

        public bool Equals(Field49BCalculations? other)
        {
            if(ReferenceEquals(null, other))
            {
                return false;
            }

            if(ReferenceEquals(this, other))
            {
                return true;
            }

            return FL_RRCID_DETERMINING_WELL == other.FL_RRCID_DETERMINING_WELL                  &&
                   Nullable.Equals(FL_G_1_GAS_GRAVITY,         other.FL_G_1_GAS_GRAVITY)         &&
                   Nullable.Equals(FL_AVG_RESERVOIR_BHP,       other.FL_AVG_RESERVOIR_BHP)       &&
                   Nullable.Equals(FL_AVG_RESERVOIR_BH_TEMP,   other.FL_AVG_RESERVOIR_BH_TEMP)   &&
                   Nullable.Equals(FL_FORMATION_VOLUME_FACTOR, other.FL_FORMATION_VOLUME_FACTOR) &&
                   Nullable.Equals(FL_SOLUTION_GAS_OIL_RATIO,  other.FL_SOLUTION_GAS_OIL_RATIO)  &&
                   Nullable.Equals(FL_DEVIATION_FACTOR,        other.FL_DEVIATION_FACTOR);
        }

        public override bool Equals(object? obj)
        {
            if(ReferenceEquals(null, obj))
            {
                return false;
            }

            if(ReferenceEquals(this, obj))
            {
                return true;
            }

            if(obj.GetType() != GetType())
            {
                return false;
            }

            return Equals((Field49BCalculations)obj);
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(FL_RRCID_DETERMINING_WELL,
                                    FL_G_1_GAS_GRAVITY,
                                    FL_AVG_RESERVOIR_BHP,
                                    FL_AVG_RESERVOIR_BH_TEMP,
                                    FL_FORMATION_VOLUME_FACTOR,
                                    FL_SOLUTION_GAS_OIL_RATIO,
                                    FL_DEVIATION_FACTOR);
        }

        public static bool operator ==(Field49BCalculations? left,
                                       Field49BCalculations? right)
        {
            return Equals(left, right);
        }

        public static bool operator !=(Field49BCalculations? left,
                                       Field49BCalculations? right)
        {
            return !Equals(left, right);
        }
    }

    public class FieldOilInformation : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldOilInformation(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class FieldOilCycleInformation : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldOilCycleInformation(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class FieldOilRules : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldOilRules(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class FieldOilAllocationFormula : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldOilAllocationFormula(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class FieldOilRemarks : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldOilRemarks(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class FieldOilFactorsRoot : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldOilFactorsRoot(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class FieldOilAllocationFactors : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldOilAllocationFactors(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class OilCounty : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OilCounty(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class AssociatedGasFields : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public AssociatedGasFields(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class FieldMapIndex : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldMapIndex(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class FieldGasOptionalRule : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldGasOptionalRule(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }

    public class FieldOilOptionalRule : IFieldInformationRecord
    {
        public const int RecordLength = 0;

        public int GetRecordLength()
        {
            return RecordLength;
        }

        public short? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public FieldOilOptionalRule(ReadOnlySpan<char> source)
        {
            int index = 0;

            RRC_TAPE_RECORD_ID = StringParser.ReadAsInt16(source, ref index, 2);
        }
    }
}
