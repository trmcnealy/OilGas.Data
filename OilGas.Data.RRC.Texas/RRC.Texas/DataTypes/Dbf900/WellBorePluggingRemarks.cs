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
    [XmlRoot(nameof(WellBorePluggingRemarks))]
    [Table(nameof(WellBorePluggingRemarks))]
    public class WellBorePluggingRemarks
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
        //02 WELL-BORE-PLUG-RMKS-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_RMK_LNE_CNT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_PLUG_RMK_LNE_CNT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_RMK_TYPE_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_PLUG_RMK_TYPE_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_REMARKS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_PLUG_REMARKS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBorePluggingRemarks()
        {
        }

        public WellBorePluggingRemarks(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-PLUG-RMKS-SEG.
            WB_PLUG_RMK_LNE_CNT   = StringParser.ReadAsInt32(source, 3  - 1, 3);
            WB_PLUG_RMK_TYPE_CODE = StringParser.ReadAsChar(source, 6   - 1, 1);
            WB_PLUG_REMARKS       = StringParser.ReadAsString(source, 7 - 1, 70);
        }
    }
}
