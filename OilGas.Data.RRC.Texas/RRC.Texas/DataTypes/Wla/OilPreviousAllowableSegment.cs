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
    [XmlRoot(nameof(OilPreviousAllowableSegment))]
    [Table(nameof(OilPreviousAllowableSegment))]
    public class OilPreviousAllowableSegment
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

        //02 WL-OIL-PREV-ALLOWABLE-INFO.

        //03 WL-OIL-PREV-EFFECTIVE-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_PREV_EFFECTIVE_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_PREV_EFFECTIVE_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_PREV_EFFECTIVE_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_PREV_EFFECTIVE_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_PREV_EFFECTIVE_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_PREV_EFFECTIVE_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-OIL-PREV-ISSUE-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_PREV_ISSUE_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_PREV_ISSUE_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_PREV_ISSUE_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_PREV_ISSUE_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_PREV_ISSUE_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_PREV_ISSUE_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_PREV_WORK_POTENTIAL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? WL_OIL_PREV_WORK_POTENTIAL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_PREV_ALLOW_AMOUNT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_PREV_ALLOW_AMOUNT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_PREV_GAS_LIMIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_PREV_GAS_LIMIT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_PREV_GAS_OIL_RATIO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_PREV_GAS_OIL_RATIO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_PREV_REMARKS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_OIL_PREV_REMARKS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ALLOWABLE_CODES_PRV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_OIL_ALLOWABLE_CODES_PRV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ALLOW_LIMITED_CD_PRV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_ALLOW_LIMITED_CD_PRV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_TYPE_WELLS_PRV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_OIL_TYPE_WELLS_PRV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_WELL_MONTHLY_ALLOW_PRV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_WELL_MONTHLY_ALLOW_PRV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CSH_MONTHLY_LIMIT_PRV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_OIL_CSH_MONTHLY_LIMIT_PRV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_14B2_CODE_PRV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_14B2_CODE_PRV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ETEX_PRO_MARG_PRV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_ETEX_PRO_MARG_PRV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ETEX_OIL_TRNS_PRV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_ETEX_OIL_TRNS_PRV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ETEX_GAS_TRNS_PRV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_ETEX_GAS_TRNS_PRV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ALLOCATION_PRV_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_ALLOCATION_PRV_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OilPreviousAllowableSegment()
        {
        }

        public OilPreviousAllowableSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-OIL-PREV-ALLOWABLE-INFO.
            //03 WL-OIL-PREV-EFFECTIVE-DATE.
            WL_OIL_PREV_EFFECTIVE_YEAR  = StringParser.ReadAsInt32(source, 3 - 1, 4);
            WL_OIL_PREV_EFFECTIVE_MONTH = StringParser.ReadAsInt16(source, 7 - 1, 2);
            WL_OIL_PREV_EFFECTIVE_DAY   = StringParser.ReadAsInt16(source, 9 - 1, 2);
            //03 WL-OIL-PREV-ISSUE-DATE.
            WL_OIL_PREV_ISSUE_YEAR        = StringParser.ReadAsInt32(source, 11           - 1, 4);
            WL_OIL_PREV_ISSUE_MONTH       = StringParser.ReadAsInt16(source, 15           - 1, 2);
            WL_OIL_PREV_ISSUE_DAY         = StringParser.ReadAsInt16(source, 17           - 1, 2);
            WL_OIL_PREV_WORK_POTENTIAL    = StringParser.ReadAsPackedSingle(source, 1, 19 - 1, 4);
            WL_OIL_PREV_ALLOW_AMOUNT      = StringParser.ReadAsPackedInt64(source, 24     - 1, 3);
            WL_OIL_PREV_GAS_LIMIT         = StringParser.ReadAsPackedInt64(source, 27     - 1, 4);
            WL_OIL_PREV_GAS_OIL_RATIO     = StringParser.ReadAsPackedInt64(source, 31     - 1, 3);
            WL_OIL_PREV_REMARKS           = StringParser.ReadAsString(source, 34          - 1, 30);
            WL_OIL_ALLOWABLE_CODES_PRV    = StringParser.ReadAsString(source, 64          - 1, 2);
            WL_OIL_ALLOW_LIMITED_CD_PRV   = StringParser.ReadAsChar(source, 66            - 1, 1);
            WL_OIL_TYPE_WELLS_PRV         = StringParser.ReadAsString(source, 67          - 1, 2);
            WL_OIL_WELL_MONTHLY_ALLOW_PRV = StringParser.ReadAsPackedInt64(source, 69     - 1, 4);
            WL_OIL_CSH_MONTHLY_LIMIT_PRV  = StringParser.ReadAsPackedDouble(source, 0, 73 - 1, 5);
            WL_OIL_14B2_CODE_PRV          = StringParser.ReadAsChar(source, 78            - 1, 1);
            WL_OIL_ETEX_PRO_MARG_PRV      = StringParser.ReadAsChar(source, 79            - 1, 1);
            WL_OIL_ETEX_OIL_TRNS_PRV      = StringParser.ReadAsInt16(source, 80           - 1, 2);
            WL_OIL_ETEX_GAS_TRNS_PRV      = StringParser.ReadAsInt16(source, 82           - 1, 2);
            WL_OIL_ALLOCATION_PRV_FLAG    = StringParser.ReadAsChar(source, 84            - 1, 1);
        }
    }
}
