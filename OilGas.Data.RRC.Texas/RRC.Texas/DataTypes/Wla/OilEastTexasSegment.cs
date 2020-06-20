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
    [XmlRoot(nameof(OilEastTexasSegment))]
    [Table(nameof(OilEastTexasSegment))]
    public class OilEastTexasSegment
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

        //02 WL-EAST-TEX-OIL-WELL-TRANSFERS.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ESWA_BONUS_AMT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_ESWA_BONUS_AMT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_TSWA_BONUS_AMT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_TSWA_BONUS_AMT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_WELL_IS_CLAT_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_WELL_IS_CLAT_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_WELL_IS_DESIGNATED_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_WELL_IS_DESIGNATED_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_WELL_IS_KEY_WELL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_WELL_IS_KEY_WELL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_WELL_CLAT_REMARKS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_OIL_WELL_CLAT_REMARKS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_WATER_OIL_RATIO_EXC_FLG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_WATER_OIL_RATIO_EXC_FLG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_WATER_WELL_SHUT_DOWN), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_WATER_WELL_SHUT_DOWN
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-OIL-CLAT-REMOVAL-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CLAT_REMOVAL_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_CLAT_REMOVAL_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CLAT_REMOVAL_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_CLAT_REMOVAL_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CLAT_REMOVAL_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_CLAT_REMOVAL_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CLAT_PLUGGED_OUT_AMT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_CLAT_PLUGGED_OUT_AMT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CLAT_PLUGGED_OUT_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_CLAT_PLUGGED_OUT_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public OilDesignatedCasingLeakageWellSegment? OilDesignatedCasingLeakageWellSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OilEastTexasSegment()
        {
        }

        public OilEastTexasSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-EAST-TEX-OIL-WELL-TRANSFERS.
            WL_OIL_ESWA_BONUS_AMT          = StringParser.ReadAsInt16(source, 3   - 1, 2);
            WL_OIL_TSWA_BONUS_AMT          = StringParser.ReadAsInt16(source, 5   - 1, 2);
            WL_OIL_WELL_IS_CLAT_FLAG       = StringParser.ReadAsChar(source, 7    - 1, 1);
            WL_OIL_WELL_IS_DESIGNATED_FLAG = StringParser.ReadAsChar(source, 8    - 1, 1);
            WL_OIL_WELL_IS_KEY_WELL_FLAG   = StringParser.ReadAsChar(source, 9    - 1, 1);
            WL_OIL_WELL_CLAT_REMARKS       = StringParser.ReadAsString(source, 10 - 1, 50);
            WL_OIL_WATER_OIL_RATIO_EXC_FLG = StringParser.ReadAsChar(source, 60   - 1, 1);
            WL_OIL_WATER_WELL_SHUT_DOWN    = StringParser.ReadAsChar(source, 61   - 1, 1);
            //03 WL-OIL-CLAT-REMOVAL-DATE.
            WL_OIL_CLAT_REMOVAL_YEAR     = StringParser.ReadAsInt32(source, 62 - 1, 4);
            WL_OIL_CLAT_REMOVAL_MONTH    = StringParser.ReadAsInt16(source, 66 - 1, 2);
            WL_OIL_CLAT_REMOVAL_DAY      = StringParser.ReadAsInt16(source, 68 - 1, 2);
            WL_OIL_CLAT_PLUGGED_OUT_AMT  = StringParser.ReadAsInt32(source, 70 - 1, 3);
            WL_OIL_CLAT_PLUGGED_OUT_CODE = StringParser.ReadAsChar(source, 73  - 1, 1);
        }
    }
}
