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
    [XmlRoot(nameof(GasPreviousAllowableSegment))]
    [Table(nameof(GasPreviousAllowableSegment))]
    public class GasPreviousAllowableSegment
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

        //02 WL-GAS-PREV-ALLOW-TECH-INFO.

        //03 WL-GAS-ALLOW-EFFECT-DATE-PREV.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOW_EFF_YEAR_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_GAS_ALLOW_EFF_YEAR_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOW_EFF_MONTH_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_ALLOW_EFF_MONTH_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOW_EFF_DAY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_ALLOW_EFF_DAY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-ALLOW-ISSUE-DATE-PREV.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOW_ISS_YEAR_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_GAS_ALLOW_ISS_YEAR_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOW_ISS_MONTH_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_ALLOW_ISS_MONTH_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOW_ISS_DAY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_ALLOW_ISS_DAY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOW_CYCLE_AMT_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_ALLOW_CYCLE_AMT_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOW_DAILY_AMT_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_ALLOW_DAILY_AMT_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOW_CODE_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GAS_ALLOW_CODE_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_TOP_SCHEDULE_ALLOW_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_TOP_SCHEDULE_ALLOW_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_WORD_ALLOW_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GAS_WORD_ALLOW_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_SPEC_ALLOW_DAILY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_SPEC_ALLOW_DAILY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_SPECIAL_ALLOW_CODE_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_SPECIAL_ALLOW_CODE_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_LIQUID_ALLOW_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_LIQUID_ALLOW_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_LIQUID_ALLOW_CODE_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_LIQUID_ALLOW_CODE_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_49B_DAILY_RATE_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_49B_DAILY_RATE_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_PERM_EXC_CYCS_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GAS_PERM_EXC_CYCS_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_CAL_DEL_POTE_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_CAL_DEL_POTE_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_CAL_DEL_POTE_CHNG_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_CAL_DEL_POTE_CHNG_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_DELIV_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_DELIV_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_DELIV_CHNG_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_DELIV_CHNG_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_BHP_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_GAS_BHP_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_BHP_CHNG_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_BHP_CHNG_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_SIWH_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_GAS_SIWH_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_SIWH_CHNG_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_SIWH_CHNG_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ROCK_PRESS_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_GAS_ROCK_PRESS_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ROCK_PRESS_CHNG_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_ROCK_PRESS_CHNG_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ACRES_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_ACRES_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ACRES_CHNG_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_ACRES_CHNG_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ACRE_FEET_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? WL_GAS_ACRE_FEET_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ACRE_FEET_CHNG_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_ACRE_FEET_CHNG_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_WELL_TYPE_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GAS_WELL_TYPE_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOW_REMARKS_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GAS_ALLOW_REMARKS_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_14B2_CODE_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_14B2_CODE_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-DELIV-LETTER-EFF-PREV.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_DELIV_LTR_EFF_YR_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_GAS_DELIV_LTR_EFF_YR_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_DELIV_LTR_EFF_MO_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_DELIV_LTR_EFF_MO_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_DELIV_LTR_EFF_DA_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_DELIV_LTR_EFF_DA_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-TEST-DATE-PREV.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_TEST_YEAR_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_GAS_TEST_YEAR_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_TEST_MONTH_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_TEST_MONTH_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_TEST_DAY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_TEST_DAY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_LEASE_PERCENT_RESERVES_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_LEASE_PERCENT_RESERVES_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public GasPreviousAllowableSegment()
        {
        }

        public GasPreviousAllowableSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-GAS-PREV-ALLOW-TECH-INFO.
            //03 WL-GAS-ALLOW-EFFECT-DATE-PREV.
            WL_GAS_ALLOW_EFF_YEAR_PREV  = StringParser.ReadAsInt32(source, 3 - 1, 4);
            WL_GAS_ALLOW_EFF_MONTH_PREV = StringParser.ReadAsInt16(source, 7 - 1, 2);
            WL_GAS_ALLOW_EFF_DAY_PREV   = StringParser.ReadAsInt16(source, 9 - 1, 2);
            //03 WL-GAS-ALLOW-ISSUE-DATE-PREV.
            WL_GAS_ALLOW_ISS_YEAR_PREV     = StringParser.ReadAsInt32(source, 11            - 1, 4);
            WL_GAS_ALLOW_ISS_MONTH_PREV    = StringParser.ReadAsInt16(source, 15            - 1, 2);
            WL_GAS_ALLOW_ISS_DAY_PREV      = StringParser.ReadAsInt16(source, 17            - 1, 2);
            WL_GAS_ALLOW_CYCLE_AMT_PREV    = StringParser.ReadAsPackedDouble(source, 0, 19  - 1, 5);
            WL_GAS_ALLOW_DAILY_AMT_PREV    = StringParser.ReadAsPackedDouble(source, 0, 24  - 1, 5);
            WL_GAS_ALLOW_CODE_PREV         = StringParser.ReadAsString(source, 29           - 1, 2);
            WL_GAS_TOP_SCHEDULE_ALLOW_PREV = StringParser.ReadAsPackedDouble(source, 0, 31  - 1, 5);
            WL_GAS_WORD_ALLOW_PREV         = StringParser.ReadAsString(source, 36           - 1, 8);
            WL_GAS_SPEC_ALLOW_DAILY_PREV   = StringParser.ReadAsPackedDouble(source, 0, 44  - 1, 5);
            WL_GAS_SPECIAL_ALLOW_CODE_PREV = StringParser.ReadAsChar(source, 49             - 1, 1);
            WL_GAS_LIQUID_ALLOW_PREV       = StringParser.ReadAsPackedDouble(source, 0, 50  - 1, 5);
            WL_GAS_LIQUID_ALLOW_CODE_PREV  = StringParser.ReadAsChar(source, 55             - 1, 1);
            WL_GAS_49B_DAILY_RATE_PREV     = StringParser.ReadAsPackedDouble(source, 0, 56  - 1, 5);
            WL_GAS_PERM_EXC_CYCS_PREV      = StringParser.ReadAsString(source, 66           - 1, 2);
            WL_GAS_CAL_DEL_POTE_PREV       = StringParser.ReadAsPackedDouble(source, 0, 73  - 1, 5);
            WL_GAS_CAL_DEL_POTE_CHNG_FLAG  = StringParser.ReadAsChar(source, 78             - 1, 1);
            WL_GAS_DELIV_PREV              = StringParser.ReadAsPackedDouble(source, 0, 79  - 1, 5);
            WL_GAS_DELIV_CHNG_FLAG         = StringParser.ReadAsChar(source, 84             - 1, 1);
            WL_GAS_BHP_PREV                = StringParser.ReadAsPackedInt64(source, 85      - 1, 3);
            WL_GAS_BHP_CHNG_FLAG           = StringParser.ReadAsChar(source, 88             - 1, 1);
            WL_GAS_SIWH_PREV               = StringParser.ReadAsPackedInt64(source, 89      - 1, 4);
            WL_GAS_SIWH_CHNG_FLAG          = StringParser.ReadAsChar(source, 93             - 1, 1);
            WL_GAS_ROCK_PRESS_PREV         = StringParser.ReadAsPackedInt64(source, 94      - 1, 3);
            WL_GAS_ROCK_PRESS_CHNG_FLAG    = StringParser.ReadAsChar(source, 97             - 1, 1);
            WL_GAS_ACRES_PREV              = StringParser.ReadAsPackedDouble(source, 4, 98  - 1, 6);
            WL_GAS_ACRES_CHNG_FLAG         = StringParser.ReadAsChar(source, 104            - 1, 1);
            WL_GAS_ACRE_FEET_PREV          = StringParser.ReadAsPackedSingle(source, 1, 105 - 1, 4);
            WL_GAS_ACRE_FEET_CHNG_FLAG     = StringParser.ReadAsChar(source, 109            - 1, 1);
            WL_GAS_WELL_TYPE_PREV          = StringParser.ReadAsString(source, 110          - 1, 2);
            WL_GAS_ALLOW_REMARKS_PREV      = StringParser.ReadAsString(source, 112          - 1, 33);
            WL_GAS_14B2_CODE_PREV          = StringParser.ReadAsChar(source, 145            - 1, 1);
            //03 WL-GAS-DELIV-LETTER-EFF-PREV.
            WL_GAS_DELIV_LTR_EFF_YR_PREV = StringParser.ReadAsInt32(source, 146 - 1, 4);
            WL_GAS_DELIV_LTR_EFF_MO_PREV = StringParser.ReadAsInt16(source, 150 - 1, 2);
            WL_GAS_DELIV_LTR_EFF_DA_PREV = StringParser.ReadAsInt16(source, 152 - 1, 2);
            //03 WL-GAS-TEST-DATE-PREV.
            WL_GAS_TEST_YEAR_PREV          = StringParser.ReadAsInt32(source, 154           - 1, 4);
            WL_GAS_TEST_MONTH_PREV         = StringParser.ReadAsInt16(source, 158           - 1, 2);
            WL_GAS_TEST_DAY_PREV           = StringParser.ReadAsInt16(source, 160           - 1, 2);
            WL_LEASE_PERCENT_RESERVES_PREV = StringParser.ReadAsPackedDouble(source, 4, 162 - 1, 5);
        }
    }
}
