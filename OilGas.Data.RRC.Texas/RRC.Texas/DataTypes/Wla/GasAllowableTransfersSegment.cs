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
    [XmlRoot(nameof(GasAllowableTransfersSegment))]
    [Table(nameof(GasAllowableTransfersSegment))]
    public class GasAllowableTransfersSegment
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

        //02 WL-GAS-ALLOWABLE-TRANSFERS.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_TRANSFER_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_TRANSFER_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_TRANSFER_AMOUNT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_TRANSFER_AMOUNT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public GasAllowableTransfersSegment()
        {
        }

        public GasAllowableTransfersSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-GAS-ALLOWABLE-TRANSFERS.
            WL_GAS_TRANSFER_CODE   = StringParser.ReadAsChar(source, 3            - 1, 1);
            WL_GAS_TRANSFER_AMOUNT = StringParser.ReadAsPackedDouble(source, 0, 4 - 1, 5);
        }
    }
}
