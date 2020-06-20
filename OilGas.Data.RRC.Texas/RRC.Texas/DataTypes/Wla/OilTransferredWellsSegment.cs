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
    [XmlRoot(nameof(OilTransferredWellsSegment))]
    [Table(nameof(OilTransferredWellsSegment))]
    public class OilTransferredWellsSegment
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

        //02 WL-OIL-WELL-TRANSFERS.

        //03 WL-OIL-TRANSFERS-KEY.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_WELL_TRANSFERRED), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_WELL_TRANSFERRED
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_TRANSFER_TYPE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_TRANSFER_TYPE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_OIL_TRANSFER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_OIL_TRANSFER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_GAS_TRANSFER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_GAS_TRANSFER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OilTransferredWellsSegment()
        {
        }

        public OilTransferredWellsSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-OIL-WELL-TRANSFERS.
            //03 WL-OIL-TRANSFERS-KEY.
            WL_OIL_WELL_TRANSFERRED = StringParser.ReadAsInt64(source, 3  - 1, 8);
            WL_OIL_TRANSFER_TYPE    = StringParser.ReadAsChar(source, 11  - 1, 1);
            WL_OIL_OIL_TRANSFER     = StringParser.ReadAsInt32(source, 12 - 1, 4);
            WL_OIL_GAS_TRANSFER     = StringParser.ReadAsInt32(source, 16 - 1, 4);
        }
    }
}
