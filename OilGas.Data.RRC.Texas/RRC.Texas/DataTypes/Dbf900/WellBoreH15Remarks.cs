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
    [XmlRoot(nameof(WellBoreH15Remarks))]
    [Table(nameof(WellBoreH15Remarks))]
    public class WellBoreH15Remarks
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
        //02 WB-H15-REMARKS-SEGMENT.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_REMARK_KEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_H15_REMARK_KEY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_H15_REMARK_TEXT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_H15_REMARK_TEXT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBoreH15Remarks()
        {
        }

        public WellBoreH15Remarks(ReadOnlySpan<byte> source)
        {
            //02 WB-H15-REMARKS-SEGMENT.
            WB_H15_REMARK_KEY  = StringParser.ReadAsInt32(source, 3  - 1, 3);
            WB_H15_REMARK_TEXT = StringParser.ReadAsString(source, 6 - 1, 70);
        }
    }
}
