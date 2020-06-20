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
    [XmlRoot(nameof(OilPanhandleGasProductionSegment))]
    [Table(nameof(OilPanhandleGasProductionSegment))]
    public class OilPanhandleGasProductionSegment
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

        //02 WL-PANHANDLE-GAS-PRODUCTION.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CASINGHEAD_GAS_AMOUNT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_OIL_CASINGHEAD_GAS_AMOUNT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CASINGHEAD_GAS_PERCENT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_OIL_CASINGHEAD_GAS_PERCENT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OilPanhandleGasProductionSegment()
        {
        }

        public OilPanhandleGasProductionSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-PANHANDLE-GAS-PRODUCTION.
            WL_OIL_CASINGHEAD_GAS_AMOUNT  = StringParser.ReadAsPackedDouble(source, 0, 3 - 1, 5);
            WL_OIL_CASINGHEAD_GAS_PERCENT = StringParser.ReadAsPackedDouble(source, 5, 8 - 1, 5);
        }
    }
}
