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
    [XmlRoot(nameof(WellBorePluggingRecord))]
    [Table(nameof(WellBorePluggingRecord))]
    public class WellBorePluggingRecord
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
        //02 WELL-BORE-PLUG-REC-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_PLUG_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_NBR_OF_CEMENT_SACKS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_NBR_OF_CEMENT_SACKS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_MEAS_TOP_OF_PLUG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_MEAS_TOP_OF_PLUG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_BOTTOM_TUBE_PIPE_DEPTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_BOTTOM_TUBE_PIPE_DEPTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_CALC_TOP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLUG_CALC_TOP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_TYPE_CEMENT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_PLUG_TYPE_CEMENT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBorePluggingRecord()
        {
        }

        public WellBorePluggingRecord(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-PLUG-REC-SEG.
            WB_PLUG_NUMBER            = StringParser.ReadAsInt32(source, 3   - 1, 3);
            WB_NBR_OF_CEMENT_SACKS    = StringParser.ReadAsInt64(source, 6   - 1, 5);
            WB_MEAS_TOP_OF_PLUG       = StringParser.ReadAsInt64(source, 11  - 1, 5);
            WB_BOTTOM_TUBE_PIPE_DEPTH = StringParser.ReadAsInt64(source, 16  - 1, 5);
            WB_PLUG_CALC_TOP          = StringParser.ReadAsInt64(source, 21  - 1, 5);
            WB_PLUG_TYPE_CEMENT       = StringParser.ReadAsString(source, 26 - 1, 6);
        }
    }
}
