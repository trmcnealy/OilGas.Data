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
    [XmlRoot(nameof(OilDesignatedCasingLeakageWellSegment))]
    [Table(nameof(OilDesignatedCasingLeakageWellSegment))]
    public class OilDesignatedCasingLeakageWellSegment
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

        //02 WL-OIL-E-TX-DESIGNATED-WL-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_E_TX_DESIGNATED_WL_KEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_E_TX_DESIGNATED_WL_KEY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OilDesignatedCasingLeakageWellSegment()
        {
        }

        public OilDesignatedCasingLeakageWellSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-OIL-E-TX-DESIGNATED-WL-SEG.
            WL_OIL_E_TX_DESIGNATED_WL_KEY = StringParser.ReadAsInt64(source, 3 - 1, 8);
        }
    }
}
