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
    [XmlRoot(nameof(OilAllowableTransfersSegment))]
    [Table(nameof(OilAllowableTransfersSegment))]
    public class OilAllowableTransfersSegment
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

        //02 WL-OIL-ALLOWABLE-TRANSFERS.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_TRANSFER_FROM_TO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_TRANSFER_FROM_TO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_MAX_CAN_RECV), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_MAX_CAN_RECV
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_TOTAL_ACTUAL_TRANSFER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_TOTAL_ACTUAL_TRANSFER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_TOTAL_ACTUAL_TRANSFER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_GAS_TOTAL_ACTUAL_TRANSFER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ACTUAL_TRANSFER_TYPE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_ACTUAL_TRANSFER_TYPE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public OilTransferredWellsSegment? OilTransferredWellsSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OilAllowableTransfersSegment()
        {
        }

        public OilAllowableTransfersSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-OIL-ALLOWABLE-TRANSFERS.
            WL_OIL_TRANSFER_FROM_TO      = StringParser.ReadAsChar(source, 3   - 1, 1);
            WL_OIL_MAX_CAN_RECV          = StringParser.ReadAsInt32(source, 4  - 1, 4);
            WL_OIL_TOTAL_ACTUAL_TRANSFER = StringParser.ReadAsInt32(source, 8  - 1, 4);
            WL_GAS_TOTAL_ACTUAL_TRANSFER = StringParser.ReadAsInt32(source, 12 - 1, 4);
            WL_OIL_ACTUAL_TRANSFER_TYPE  = StringParser.ReadAsChar(source, 16  - 1, 1);
        }
    }
}
