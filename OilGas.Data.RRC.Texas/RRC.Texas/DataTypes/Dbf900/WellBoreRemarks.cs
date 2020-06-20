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
    [XmlRoot(nameof(WellBoreRemarks))]
    [Table(nameof(WellBoreRemarks))]
    public class WellBoreRemarks
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
        //02 WELL-BORE-COMPL-RMKS-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_RMK_LNE_CNT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_RMK_LNE_CNT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_RMK_TYPE_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_RMK_TYPE_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_REMARKS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_REMARKS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBoreRemarks()
        {
        }

        public WellBoreRemarks(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-COMPL-RMKS-SEG.
            WB_RMK_LNE_CNT   = StringParser.ReadAsInt32(source, 3  - 1, 3);
            WB_RMK_TYPE_CODE = StringParser.ReadAsChar(source, 6   - 1, 1);
            WB_REMARKS       = StringParser.ReadAsString(source, 7 - 1, 70);
        }
    }
}
