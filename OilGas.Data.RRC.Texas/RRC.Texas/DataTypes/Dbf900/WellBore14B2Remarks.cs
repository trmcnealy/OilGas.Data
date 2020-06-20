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
    [XmlRoot(nameof(WellBore14B2Remarks))]
    [Table(nameof(WellBore14B2Remarks))]
    public class WellBore14B2Remarks
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

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(RRC_TAPE_RECORD_ID), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? RRC_TAPE_RECORD_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //02 WELL-BORE-14B2-RMKS-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_14B2_RMK_LNE_CNT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_14B2_RMK_LNE_CNT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_14B2_RMK_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_14B2_RMK_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_14B2_RMK_USERID), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_14B2_RMK_USERID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_14B2_REMARKS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_14B2_REMARKS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBore14B2Remarks()
        {
        }

        public WellBore14B2Remarks(ReadOnlySpan<byte> source)
        {
            RRC_TAPE_RECORD_ID = StringParser.ReadAsString(source, 1 - 1, 2);
            //02 WELL-BORE-14B2-RMKS-SEG.
            WB_14B2_RMK_LNE_CNT = StringParser.ReadAsInt32(source, 3   - 1, 3);
            WB_14B2_RMK_DATE    = StringParser.ReadAsInt64(source, 6   - 1, 8);
            WB_14B2_RMK_USERID  = StringParser.ReadAsString(source, 14 - 1, 8);
            WB_14B2_REMARKS     = StringParser.ReadAsString(source, 22 - 1, 66);
        }
    }
}
