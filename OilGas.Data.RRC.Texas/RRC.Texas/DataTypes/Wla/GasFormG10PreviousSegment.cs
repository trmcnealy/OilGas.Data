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
    [XmlRoot(nameof(GasFormG10PreviousSegment))]
    [Table(nameof(GasFormG10PreviousSegment))]
    public class GasFormG10PreviousSegment
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

        //02 WL-G10-PREV-INFO-SEGMENT.

        //03 WL-G10-KEY-PREV.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_CYCLE_KEY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_G10_CYCLE_KEY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_DAY_KEY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_DAY_KEY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-G10-EFF-DATE-PREV.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_EFF_YEAR_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_G10_EFF_YEAR_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_EFF_MONTH_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_EFF_MONTH_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_EFF_DAY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_EFF_DAY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-G10-TEST-DATE-PREV.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_TEST_YEAR_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_G10_TEST_YEAR_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_TEST_MONTH_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_TEST_MONTH_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_TEST_DAY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_TEST_DAY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_TYPE_TEST_FLAG_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_G10_TYPE_TEST_FLAG_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_DELIVERABILITY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_G10_DELIVERABILITY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_DAILY_COND_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? WL_G10_DAILY_COND_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_DAILY_WATER_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_G10_DAILY_WATER_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_GAS_GRAVITY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? WL_G10_GAS_GRAVITY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_COND_GRAVITY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? WL_G10_COND_GRAVITY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_RATIO_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_G10_RATIO_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_SIWH_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_G10_SIWH_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_FLOW_PRESS_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_G10_FLOW_PRESS_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_POTENTIAL_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_G10_POTENTIAL_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_BHP_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_G10_BHP_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_CD_SHUTIN_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_G10_CD_SHUTIN_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_WHP_SHUTIN_WELL_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_G10_WHP_SHUTIN_WELL_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_ROCK_PRESS_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_G10_ROCK_PRESS_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_CAL_DEL_POTE_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_G10_CAL_DEL_POTE_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-G10-ISSUE-DATE-PREV.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_ISSUE_YEAR_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_G10_ISSUE_YEAR_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_ISSUE_MONTH_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_ISSUE_MONTH_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_ISSUE_DAY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_ISSUE_DAY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-G10-SHUTIN-DATE-PREV.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_SHUTIN_YEAR_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_G10_SHUTIN_YEAR_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_SHUTIN_MONTH_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_SHUTIN_MONTH_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_ANN_CMGL_TST_FLAG_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_G10_ANN_CMGL_TST_FLAG_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_SURVEY_CNTED_FLAG_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_G10_SURVEY_CNTED_FLAG_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_FILING_EDI_PREV_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_G10_FILING_EDI_PREV_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_2YR_SRVY_CNTED_FLAG_PRV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_G10_2YR_SRVY_CNTED_FLAG_PRV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-G10-WH-PRES-TEST-DATE-PREV.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_WHP_TEST_YEAR_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_G10_WHP_TEST_YEAR_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_WHP_TEST_MONTH_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_WHP_TEST_MONTH_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G10_WHP_TEST_DAY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G10_WHP_TEST_DAY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public GasFormG10PreviousSegment()
        {
        }

        public GasFormG10PreviousSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-G10-PREV-INFO-SEGMENT.
            //03 WL-G10-KEY-PREV.
            WL_G10_CYCLE_KEY_PREV = StringParser.ReadAsInt32(source, 3 - 1, 4);
            WL_G10_DAY_KEY_PREV   = StringParser.ReadAsInt16(source, 7 - 1, 2);
            //03 WL-G10-EFF-DATE-PREV.
            WL_G10_EFF_YEAR_PREV  = StringParser.ReadAsInt32(source, 9  - 1, 4);
            WL_G10_EFF_MONTH_PREV = StringParser.ReadAsInt16(source, 13 - 1, 2);
            WL_G10_EFF_DAY_PREV   = StringParser.ReadAsInt16(source, 15 - 1, 2);
            //03 WL-G10-TEST-DATE-PREV.
            WL_G10_TEST_YEAR_PREV       = StringParser.ReadAsInt32(source, 17           - 1, 4);
            WL_G10_TEST_MONTH_PREV      = StringParser.ReadAsInt16(source, 21           - 1, 2);
            WL_G10_TEST_DAY_PREV        = StringParser.ReadAsInt16(source, 23           - 1, 2);
            WL_G10_TYPE_TEST_FLAG_PREV  = StringParser.ReadAsChar(source, 25            - 1, 1);
            WL_G10_DELIVERABILITY_PREV  = StringParser.ReadAsPackedDouble(source, 0, 26 - 1, 5);
            WL_G10_DAILY_COND_PREV      = StringParser.ReadAsPackedSingle(source, 1, 31 - 1, 4);
            WL_G10_DAILY_WATER_PREV     = StringParser.ReadAsPackedInt64(source, 35     - 1, 4);
            WL_G10_GAS_GRAVITY_PREV     = StringParser.ReadAsPackedSingle(source, 3, 39 - 1, 3);
            WL_G10_COND_GRAVITY_PREV    = StringParser.ReadAsPackedSingle(source, 1, 42 - 1, 2);
            WL_G10_RATIO_PREV           = StringParser.ReadAsPackedInt64(source, 44     - 1, 3);
            WL_G10_SIWH_PREV            = StringParser.ReadAsPackedInt64(source, 47     - 1, 4);
            WL_G10_FLOW_PRESS_PREV      = StringParser.ReadAsPackedInt64(source, 51     - 1, 3);
            WL_G10_POTENTIAL_PREV       = StringParser.ReadAsPackedDouble(source, 0, 54 - 1, 5);
            WL_G10_BHP_PREV             = StringParser.ReadAsPackedInt64(source, 59     - 1, 3);
            WL_G10_CD_SHUTIN_PREV       = StringParser.ReadAsChar(source, 62            - 1, 1);
            WL_G10_WHP_SHUTIN_WELL_PREV = StringParser.ReadAsPackedInt64(source, 63     - 1, 4);
            WL_G10_ROCK_PRESS_PREV      = StringParser.ReadAsPackedInt64(source, 67     - 1, 3);
            WL_G10_CAL_DEL_POTE_PREV    = StringParser.ReadAsPackedDouble(source, 0, 70 - 1, 5);
            //03 WL-G10-ISSUE-DATE-PREV.
            WL_G10_ISSUE_YEAR_PREV  = StringParser.ReadAsInt32(source, 75 - 1, 4);
            WL_G10_ISSUE_MONTH_PREV = StringParser.ReadAsInt16(source, 79 - 1, 2);
            WL_G10_ISSUE_DAY_PREV   = StringParser.ReadAsInt16(source, 81 - 1, 2);
            //03 WL-G10-SHUTIN-DATE-PREV.
            WL_G10_SHUTIN_YEAR_PREV        = StringParser.ReadAsInt32(source, 83 - 1, 4);
            WL_G10_SHUTIN_MONTH_PREV       = StringParser.ReadAsInt16(source, 87 - 1, 2);
            WL_G10_ANN_CMGL_TST_FLAG_PREV  = StringParser.ReadAsChar(source, 89  - 1, 1);
            WL_G10_SURVEY_CNTED_FLAG_PREV  = StringParser.ReadAsChar(source, 90  - 1, 1);
            WL_G10_FILING_EDI_PREV_FLAG    = StringParser.ReadAsChar(source, 91  - 1, 1);
            WL_G10_2YR_SRVY_CNTED_FLAG_PRV = StringParser.ReadAsChar(source, 92  - 1, 1);
            //03 WL-G10-WH-PRES-TEST-DATE-PREV.
            WL_G10_WHP_TEST_YEAR_PREV  = StringParser.ReadAsInt32(source, 93 - 1, 4);
            WL_G10_WHP_TEST_MONTH_PREV = StringParser.ReadAsInt16(source, 97 - 1, 2);
            WL_G10_WHP_TEST_DAY_PREV   = StringParser.ReadAsInt16(source, 99 - 1, 2);
        }
    }
}
