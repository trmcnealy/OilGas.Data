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
    [XmlRoot(nameof(WellBoreUsableQualityWaterProtection))]
    [Table(nameof(WellBoreUsableQualityWaterProtection))]
    public class WellBoreUsableQualityWaterProtection
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
        //02 WELL-BORE-TWDB-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_FRESH_WATER_CNTR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_FRESH_WATER_CNTR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_TWDB_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_TWDB_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SURFACE_CASING_DETER_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_SURFACE_CASING_DETER_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-UQWP-DATA.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_UQWP_FROM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_UQWP_FROM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_UQWP_TO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_UQWP_TO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBoreUsableQualityWaterProtection()
        {
        }

        public WellBoreUsableQualityWaterProtection(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-TWDB-SEG.
            WB_FRESH_WATER_CNTR          = StringParser.ReadAsInt32(source, 3 - 1, 3);
            WB_TWDB_DATE                 = StringParser.ReadAsInt64(source, 6 - 1, 8);
            WB_SURFACE_CASING_DETER_CODE = StringParser.ReadAsChar(source, 14 - 1, 1);
            //03 WB-UQWP-DATA.
            WB_UQWP_FROM = StringParser.ReadAsInt32(source, 15 - 1, 4);
            WB_UQWP_TO   = StringParser.ReadAsInt32(source, 19 - 1, 4);
        }
    }
}
