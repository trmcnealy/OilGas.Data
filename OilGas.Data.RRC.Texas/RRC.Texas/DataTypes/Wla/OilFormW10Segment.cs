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
    [XmlRoot(nameof(OilFormW10Segment))]
    [Table(nameof(OilFormW10Segment))]
    public class OilFormW10Segment
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

        //02 WL-W10-INFORMATION-SEGMENT.

        //03 WL-W10-KEY.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_CYCLE_KEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_W10_CYCLE_KEY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_DAY_KEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_W10_DAY_KEY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-W10-EFF-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_EFF_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_W10_EFF_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_EFF_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_W10_EFF_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_EFF_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_W10_EFF_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_TYPE_STATUS_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_W10_TYPE_STATUS_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_PRODUCING_METHOD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_W10_PRODUCING_METHOD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_PROD_METH_OTHER_RPT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_W10_PROD_METH_OTHER_RPT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-W10-TEST-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_TEST_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_W10_TEST_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_TEST_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_W10_TEST_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_TEST_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_W10_TEST_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_DAILY_OIL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? WL_W10_DAILY_OIL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_DAILY_WATER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_W10_DAILY_WATER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_DAILY_GAS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_W10_DAILY_GAS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_GAS_OIL_RATIO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_W10_GAS_OIL_RATIO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_SIWH_PRESSURE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_W10_SIWH_PRESSURE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-W10-SIWH-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_SIWH_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_W10_SIWH_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_SIWH_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_W10_SIWH_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_TYPE_WELL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_W10_TYPE_WELL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-W10-ISSUE-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_ISSUE_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_W10_ISSUE_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_ISSUE_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_W10_ISSUE_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_ISSUE_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_W10_ISSUE_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_SURVEY_COUNTED_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_W10_SURVEY_COUNTED_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_FILED_EDI_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_W10_FILED_EDI_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_ALLOCATION_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_W10_ALLOCATION_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_W10_2YR_SURVEY_COUNTED_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_W10_2YR_SURVEY_COUNTED_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public OilFormW10PreviousSegment? OilFormW10PreviousSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OilFormW10Segment()
        {
        }

        public OilFormW10Segment(ReadOnlySpan<byte> source)
        {
            //02 WL-W10-INFORMATION-SEGMENT.
            //03 WL-W10-KEY.
            WL_W10_CYCLE_KEY = StringParser.ReadAsInt32(source, 3 - 1, 4);
            WL_W10_DAY_KEY   = StringParser.ReadAsInt16(source, 7 - 1, 2);
            //03 WL-W10-EFF-DATE.
            WL_W10_EFF_YEAR            = StringParser.ReadAsInt32(source, 9   - 1, 4);
            WL_W10_EFF_MONTH           = StringParser.ReadAsInt16(source, 13  - 1, 2);
            WL_W10_EFF_DAY             = StringParser.ReadAsInt16(source, 15  - 1, 2);
            WL_W10_TYPE_STATUS_FLAG    = StringParser.ReadAsChar(source, 17   - 1, 1);
            WL_W10_PRODUCING_METHOD    = StringParser.ReadAsChar(source, 18   - 1, 1);
            WL_W10_PROD_METH_OTHER_RPT = StringParser.ReadAsString(source, 19 - 1, 20);
            //03 WL-W10-TEST-DATE.
            WL_W10_TEST_YEAR     = StringParser.ReadAsInt32(source, 39           - 1, 4);
            WL_W10_TEST_MONTH    = StringParser.ReadAsInt16(source, 43           - 1, 2);
            WL_W10_TEST_DAY      = StringParser.ReadAsInt16(source, 45           - 1, 2);
            WL_W10_DAILY_OIL     = StringParser.ReadAsPackedSingle(source, 1, 47 - 1, 4);
            WL_W10_DAILY_WATER   = StringParser.ReadAsPackedInt32(source, 51     - 1, 3);
            WL_W10_DAILY_GAS     = StringParser.ReadAsPackedInt64(source, 54     - 1, 4);
            WL_W10_GAS_OIL_RATIO = StringParser.ReadAsPackedInt64(source, 58     - 1, 3);
            WL_W10_SIWH_PRESSURE = StringParser.ReadAsPackedInt64(source, 61     - 1, 4);
            //03 WL-W10-SIWH-DATE.
            WL_W10_SIWH_YEAR      = StringParser.ReadAsInt32(source, 65  - 1, 4);
            WL_W10_SIWH_MONTH     = StringParser.ReadAsInt16(source, 69  - 1, 2);
            WL_W10_TYPE_WELL_FLAG = StringParser.ReadAsString(source, 77 - 1, 2);
            //03 WL-W10-ISSUE-DATE.
            WL_W10_ISSUE_YEAR              = StringParser.ReadAsInt32(source, 85 - 1, 4);
            WL_W10_ISSUE_MONTH             = StringParser.ReadAsInt16(source, 89 - 1, 2);
            WL_W10_ISSUE_DAY               = StringParser.ReadAsInt16(source, 91 - 1, 2);
            WL_W10_SURVEY_COUNTED_FLAG     = StringParser.ReadAsChar(source, 93  - 1, 1);
            WL_W10_FILED_EDI_FLAG          = StringParser.ReadAsChar(source, 94  - 1, 1);
            WL_W10_ALLOCATION_FLAG         = StringParser.ReadAsChar(source, 95  - 1, 1);
            WL_W10_2YR_SURVEY_COUNTED_FLAG = StringParser.ReadAsChar(source, 96  - 1, 1);
        }
    }
}
