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
    [XmlRoot(nameof(WellBoreFormation))]
    [Table(nameof(WellBoreFormation))]
    public class WellBoreFormation
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
        //02 WELL-FORMATION-DATA-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_FORMATION_CNTR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_FORMATION_CNTR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_FORMATION_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_FORMATION_NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_FORMATION_DEPTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_FORMATION_DEPTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBoreFormation()
        {
        }

        public WellBoreFormation(ReadOnlySpan<byte> source)
        {
            //02 WELL-FORMATION-DATA-SEG.
            WB_FORMATION_CNTR  = StringParser.ReadAsInt32(source, 3  - 1, 3);
            WB_FORMATION_NAME  = StringParser.ReadAsString(source, 6 - 1, 32);
            WB_FORMATION_DEPTH = StringParser.ReadAsInt64(source, 38 - 1, 5);
        }
    }
}
