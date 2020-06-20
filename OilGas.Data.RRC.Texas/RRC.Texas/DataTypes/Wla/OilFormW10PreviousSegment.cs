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
    [XmlRoot(nameof(OilFormW10PreviousSegment))]
    [Table(nameof(OilFormW10PreviousSegment))]
    public class OilFormW10PreviousSegment
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

        //02 WL-W10-PREV-INFO-SEGMENT.

        //03 WL-W10-KEY-PREV.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_CYCLE_KEY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_W10_CYCLE_KEY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_DAY_KEY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_W10_DAY_KEY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-W10-EFF-DATE-PREV.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_EFF_YEAR_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_W10_EFF_YEAR_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_EFF_MONTH_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_W10_EFF_MONTH_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_EFF_DAY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_W10_EFF_DAY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_TYPE_STATUS_FLAG_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_W10_TYPE_STATUS_FLAG_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_PRODUCING_METHOD_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_W10_PRODUCING_METHOD_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_PRD_METH_OTHER_RPT_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_W10_PRD_METH_OTHER_RPT_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-W10-TEST-DATE-PREV.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_TEST_YEAR_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_W10_TEST_YEAR_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_TEST_MONTH_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_W10_TEST_MONTH_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_TEST_DAY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_W10_TEST_DAY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_DAILY_OIL_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? WL_W10_DAILY_OIL_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_DAILY_WATER_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_W10_DAILY_WATER_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_DAILY_GAS_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_W10_DAILY_GAS_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_GAS_OIL_RATIO_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_W10_GAS_OIL_RATIO_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_SIWH_PRESSURE_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_W10_SIWH_PRESSURE_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-W10-SIWH-DATE-PREV.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_SIWH_YEAR_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_W10_SIWH_YEAR_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_SIWH_MONTH_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_W10_SIWH_MONTH_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_TYPE_WELL_FLAG_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_W10_TYPE_WELL_FLAG_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-W10-ISSUE-DATE-PREV.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_ISSUE_YEAR_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_W10_ISSUE_YEAR_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_ISSUE_MONTH_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_W10_ISSUE_MONTH_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_ISSUE_DAY_PREV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_W10_ISSUE_DAY_PREV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_FILING_EDI_PREV_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_W10_FILING_EDI_PREV_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OilFormW10PreviousSegment()
        {
        }

        public OilFormW10PreviousSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-W10-PREV-INFO-SEGMENT.
            //03 WL-W10-KEY-PREV.
            WL_W10_CYCLE_KEY_PREV = StringParser.ReadAsInt32(source, 3 - 1, 4);
            WL_W10_DAY_KEY_PREV   = StringParser.ReadAsInt16(source, 7 - 1, 2);
            //03 WL-W10-EFF-DATE-PREV.
            WL_W10_EFF_YEAR_PREV           = StringParser.ReadAsInt32(source, 9   - 1, 4);
            WL_W10_EFF_MONTH_PREV          = StringParser.ReadAsInt16(source, 13  - 1, 2);
            WL_W10_EFF_DAY_PREV            = StringParser.ReadAsInt16(source, 15  - 1, 2);
            WL_W10_TYPE_STATUS_FLAG_PREV   = StringParser.ReadAsChar(source, 17   - 1, 1);
            WL_W10_PRODUCING_METHOD_PREV   = StringParser.ReadAsChar(source, 18   - 1, 1);
            WL_W10_PRD_METH_OTHER_RPT_PREV = StringParser.ReadAsString(source, 19 - 1, 20);
            //03 WL-W10-TEST-DATE-PREV.
            WL_W10_TEST_YEAR_PREV     = StringParser.ReadAsInt32(source, 39           - 1, 4);
            WL_W10_TEST_MONTH_PREV    = StringParser.ReadAsInt16(source, 43           - 1, 2);
            WL_W10_TEST_DAY_PREV      = StringParser.ReadAsInt16(source, 45           - 1, 2);
            WL_W10_DAILY_OIL_PREV     = StringParser.ReadAsPackedSingle(source, 1, 47 - 1, 4);
            WL_W10_DAILY_WATER_PREV   = StringParser.ReadAsPackedInt32(source, 51     - 1, 3);
            WL_W10_DAILY_GAS_PREV     = StringParser.ReadAsPackedInt64(source, 54     - 1, 4);
            WL_W10_GAS_OIL_RATIO_PREV = StringParser.ReadAsPackedInt64(source, 58     - 1, 3);
            WL_W10_SIWH_PRESSURE_PREV = StringParser.ReadAsPackedInt64(source, 61     - 1, 4);
            //03 WL-W10-SIWH-DATE-PREV.
            WL_W10_SIWH_YEAR_PREV      = StringParser.ReadAsInt32(source, 65  - 1, 4);
            WL_W10_SIWH_MONTH_PREV     = StringParser.ReadAsInt16(source, 69  - 1, 2);
            WL_W10_TYPE_WELL_FLAG_PREV = StringParser.ReadAsString(source, 77 - 1, 2);
            //03 WL-W10-ISSUE-DATE-PREV.
            WL_W10_ISSUE_YEAR_PREV      = StringParser.ReadAsInt32(source, 85 - 1, 4);
            WL_W10_ISSUE_MONTH_PREV     = StringParser.ReadAsInt16(source, 89 - 1, 2);
            WL_W10_ISSUE_DAY_PREV       = StringParser.ReadAsInt16(source, 91 - 1, 2);
            WL_W10_FILING_EDI_PREV_FLAG = StringParser.ReadAsChar(source, 93  - 1, 1);
        }
    }
}
