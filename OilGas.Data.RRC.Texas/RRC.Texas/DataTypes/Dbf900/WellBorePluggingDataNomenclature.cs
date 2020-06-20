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
    [XmlRoot(nameof(WellBorePluggingDataNomenclature))]
    [Table(nameof(WellBorePluggingDataNomenclature))]
    public class WellBorePluggingDataNomenclature
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
        //02 WELL-BORE-PLUG-NOMEN-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_FIELD_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLUG_FIELD_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_FIELD_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_PLUG_FIELD_NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_OPER_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_PLUG_OPER_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_OPER_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_PLUG_OPER_NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_LEASE_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_PLUG_LEASE_NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBorePluggingDataNomenclature()
        {
        }

        public WellBorePluggingDataNomenclature(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-PLUG-NOMEN-SEG.
            WB_PLUG_FIELD_NO   = StringParser.ReadAsInt64(source, 3   - 1, 8);
            WB_PLUG_FIELD_NAME = StringParser.ReadAsString(source, 11 - 1, 32);
            WB_PLUG_OPER_NO    = StringParser.ReadAsString(source, 43 - 1, 6);
            WB_PLUG_OPER_NAME  = StringParser.ReadAsString(source, 49 - 1, 32);
            WB_PLUG_LEASE_NAME = StringParser.ReadAsString(source, 81 - 1, 32);
        }
    }
}
