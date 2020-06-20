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
    [XmlRoot(nameof(WellBoreSqueeze))]
    [Table(nameof(WellBoreSqueeze))]
    public class WellBoreSqueeze
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
        //02 WELL-SQUEEZE-DATA-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SQUEEZE_CNTR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_SQUEEZE_CNTR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SQUEEZE_UPPER_DEPTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_SQUEEZE_UPPER_DEPTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SQUEEZE_LOWER_DEPTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_SQUEEZE_LOWER_DEPTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SQUEEZE_KIND_AMOUNT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_SQUEEZE_KIND_AMOUNT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBoreSqueeze()
        {
        }

        public WellBoreSqueeze(ReadOnlySpan<byte> source)
        {
            //02 WELL-SQUEEZE-DATA-SEG.
            WB_SQUEEZE_CNTR        = StringParser.ReadAsInt32(source, 3   - 1, 3);
            WB_SQUEEZE_UPPER_DEPTH = StringParser.ReadAsInt64(source, 6   - 1, 5);
            WB_SQUEEZE_LOWER_DEPTH = StringParser.ReadAsInt64(source, 11  - 1, 5);
            WB_SQUEEZE_KIND_AMOUNT = StringParser.ReadAsString(source, 16 - 1, 50);
        }
    }
}
