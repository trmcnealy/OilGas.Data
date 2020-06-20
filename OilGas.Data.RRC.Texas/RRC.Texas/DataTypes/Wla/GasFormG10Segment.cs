#nullable enable
using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Xml.Serialization;

using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace OilGas.Data.RRC.Texas
{
    [Serializable]
    [DataContract]
    [XmlRoot(nameof(GasFormG10Segment))]
    [Table(nameof(GasFormG10Segment))]
    public class GasFormG10Segment
    {
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

        //02 WL-G10-INFORMATION-SEGMENT.

        //03 WL-G10-KEY.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_CYCLE_KEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_G10_CYCLE_KEY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_DAY_KEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_DAY_KEY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-G10-EFF-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_EFF_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_G10_EFF_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_EFF_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_EFF_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_EFF_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_EFF_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-G10-TEST-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_TEST_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_G10_TEST_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_TEST_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_TEST_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_TEST_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_TEST_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_TYPE_TEST_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_G10_TYPE_TEST_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_DELIVERABILITY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_G10_DELIVERABILITY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_DAILY_COND), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? WL_G10_DAILY_COND
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_DAILY_WATER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_G10_DAILY_WATER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_GAS_GRAVITY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? WL_G10_GAS_GRAVITY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_COND_GRAVITY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? WL_G10_COND_GRAVITY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_RATIO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_G10_RATIO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_SIWH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_G10_SIWH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_FLOW_PRESS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_G10_FLOW_PRESS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_POTENTIAL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_G10_POTENTIAL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_BHP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_G10_BHP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_CD_SHUTIN), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_G10_CD_SHUTIN
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_WHP_SHUTIN_WELL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_G10_WHP_SHUTIN_WELL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_ROCK_PRESS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_G10_ROCK_PRESS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_CAL_DEL_POTE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_G10_CAL_DEL_POTE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-G10-ISSUE-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_ISSUE_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_G10_ISSUE_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_ISSUE_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_ISSUE_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_ISSUE_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_ISSUE_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-G10-SHUTIN-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_SHUTIN_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_G10_SHUTIN_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_SHUTIN_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_SHUTIN_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_ANNUAL_CMGL_TST_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_G10_ANNUAL_CMGL_TST_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_SURVEY_COUNTED_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_G10_SURVEY_COUNTED_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_FILED_EDI_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_G10_FILED_EDI_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_2YR_SURVEY_COUNTED_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_G10_2YR_SURVEY_COUNTED_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-G10-WH-PRES-TEST-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_WHP_TEST_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_G10_WHP_TEST_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_WHP_TEST_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_WHP_TEST_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_WHP_TEST_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_WHP_TEST_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public GasFormG10PreviousSegment? GasFormG10PreviousSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public GasFormG10Segment()
        {
        }

        public GasFormG10Segment(ReadOnlySpan<byte> source)
        {
            //02 WL-G10-INFORMATION-SEGMENT.
            //03 WL-G10-KEY.
            WL_G10_CYCLE_KEY = StringParser.ReadAsInt32(source, 3 - 1, 4);
            WL_G10_DAY_KEY   = StringParser.ReadAsInt16(source, 7 - 1, 2);
            //03 WL-G10-EFF-DATE.
            WL_G10_EFF_YEAR  = StringParser.ReadAsInt32(source, 9  - 1, 4);
            WL_G10_EFF_MONTH = StringParser.ReadAsInt16(source, 13 - 1, 2);
            WL_G10_EFF_DAY   = StringParser.ReadAsInt16(source, 15 - 1, 2);
            //03 WL-G10-TEST-DATE.
            WL_G10_TEST_YEAR       = StringParser.ReadAsInt32(source, 17           - 1, 4);
            WL_G10_TEST_MONTH      = StringParser.ReadAsInt16(source, 21           - 1, 2);
            WL_G10_TEST_DAY        = StringParser.ReadAsInt16(source, 23           - 1, 2);
            WL_G10_TYPE_TEST_FLAG  = StringParser.ReadAsChar(source, 25            - 1, 1);
            WL_G10_DELIVERABILITY  = StringParser.ReadAsPackedDouble(source, 0, 26 - 1, 5);
            WL_G10_DAILY_COND      = StringParser.ReadAsPackedSingle(source, 1, 31 - 1, 4);
            WL_G10_DAILY_WATER     = StringParser.ReadAsPackedInt64(source, 35     - 1, 4);
            WL_G10_GAS_GRAVITY     = StringParser.ReadAsPackedSingle(source, 3, 39 - 1, 3);
            WL_G10_COND_GRAVITY    = StringParser.ReadAsPackedSingle(source, 1, 42 - 1, 2);
            WL_G10_RATIO           = StringParser.ReadAsPackedInt64(source, 44     - 1, 3);
            WL_G10_SIWH            = StringParser.ReadAsPackedInt64(source, 47     - 1, 4);
            WL_G10_FLOW_PRESS      = StringParser.ReadAsPackedInt64(source, 51     - 1, 3);
            WL_G10_POTENTIAL       = StringParser.ReadAsPackedDouble(source, 0, 54 - 1, 5);
            WL_G10_BHP             = StringParser.ReadAsPackedInt64(source, 59     - 1, 3);
            WL_G10_CD_SHUTIN       = StringParser.ReadAsChar(source, 62            - 1, 1);
            WL_G10_WHP_SHUTIN_WELL = StringParser.ReadAsPackedInt64(source, 63     - 1, 4);
            WL_G10_ROCK_PRESS      = StringParser.ReadAsPackedInt64(source, 67     - 1, 3);
            WL_G10_CAL_DEL_POTE    = StringParser.ReadAsPackedDouble(source, 0, 70 - 1, 5);
            //03 WL-G10-ISSUE-DATE.
            WL_G10_ISSUE_YEAR  = StringParser.ReadAsInt32(source, 75 - 1, 4);
            WL_G10_ISSUE_MONTH = StringParser.ReadAsInt16(source, 79 - 1, 2);
            WL_G10_ISSUE_DAY   = StringParser.ReadAsInt16(source, 81 - 1, 2);
            //03 WL-G10-SHUTIN-DATE.
            WL_G10_SHUTIN_YEAR             = StringParser.ReadAsInt32(source, 83 - 1, 4);
            WL_G10_SHUTIN_MONTH            = StringParser.ReadAsInt16(source, 87 - 1, 2);
            WL_G10_ANNUAL_CMGL_TST_FLAG    = StringParser.ReadAsChar(source, 89  - 1, 1);
            WL_G10_SURVEY_COUNTED_FLAG     = StringParser.ReadAsChar(source, 90  - 1, 1);
            WL_G10_FILED_EDI_FLAG          = StringParser.ReadAsChar(source, 91  - 1, 1);
            WL_G10_2YR_SURVEY_COUNTED_FLAG = StringParser.ReadAsChar(source, 92  - 1, 1);
            //03 WL-G10-WH-PRES-TEST-DATE.
            WL_G10_WHP_TEST_YEAR  = StringParser.ReadAsInt32(source, 93 - 1, 4);
            WL_G10_WHP_TEST_MONTH = StringParser.ReadAsInt16(source, 97 - 1, 2);
            WL_G10_WHP_TEST_DAY   = StringParser.ReadAsInt16(source, 99 - 1, 2);
        }
    }
}
