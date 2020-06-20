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
    [XmlRoot(nameof(WellBorePerforations))]
    [Table(nameof(WellBorePerforations))]
    public class WellBorePerforations
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
        //02 WELL-BORE-PERF-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PERF_COUNT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_PERF_COUNT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_FROM_PERF), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_FROM_PERF
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_TO_PERF), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_TO_PERF
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_OPEN_HOLE_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_OPEN_HOLE_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBorePerforations()
        {
        }

        public WellBorePerforations(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-PERF-SEG.
            WB_PERF_COUNT     = StringParser.ReadAsInt32(source, 3   - 1, 3);
            WB_FROM_PERF      = StringParser.ReadAsInt64(source, 6   - 1, 5);
            WB_TO_PERF        = StringParser.ReadAsInt64(source, 11  - 1, 5);
            WB_OPEN_HOLE_CODE = StringParser.ReadAsString(source, 16 - 1, 2);
        }
    }
}
