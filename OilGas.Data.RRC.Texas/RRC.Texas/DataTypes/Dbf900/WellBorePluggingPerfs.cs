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
    [XmlRoot(nameof(WellBorePluggingPerfs))]
    [Table(nameof(WellBorePluggingPerfs))]
    public class WellBorePluggingPerfs
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
        //02 WELL-BORE-PLUG-RECORD.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_PERF_COUNTER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_PLUG_PERF_COUNTER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_FROM_PERF), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLUG_FROM_PERF
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_TO_PERF), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLUG_TO_PERF
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_OPEN_HOLE_INDICATOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_PLUG_OPEN_HOLE_INDICATOR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBorePluggingPerfs()
        {
        }

        public WellBorePluggingPerfs(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-PLUG-RECORD.
            WB_PLUG_PERF_COUNTER        = StringParser.ReadAsInt32(source, 3  - 1, 3);
            WB_PLUG_FROM_PERF           = StringParser.ReadAsInt64(source, 6  - 1, 5);
            WB_PLUG_TO_PERF             = StringParser.ReadAsInt64(source, 11 - 1, 5);
            WB_PLUG_OPEN_HOLE_INDICATOR = StringParser.ReadAsChar(source, 16  - 1, 1);
        }
    }
}
