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
    [XmlRoot(nameof(OilPanhandleGasBalancingSegment))]
    [Table(nameof(OilPanhandleGasBalancingSegment))]
    public class OilPanhandleGasBalancingSegment
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
        
        //02 WL-PANHANDLE-GAS-BALANCING.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_WELL_LOST_COMPL_2_8_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_WELL_LOST_COMPL_2_8_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-LOST-COMPL-2-8-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_LOST_COMPL_2_8_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_LOST_COMPL_2_8_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_LOST_COMPL_2_8_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_LOST_COMPL_2_8_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_LOST_COMPL_2_8_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_LOST_COMPL_2_8_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OilPanhandleGasBalancingSegment()
        {
        }

        public OilPanhandleGasBalancingSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-PANHANDLE-GAS-BALANCING.
            WL_WELL_LOST_COMPL_2_8_FLAG = StringParser.ReadAsChar(source, 3 - 1, 1);
            //03 WL-LOST-COMPL-2-8-DATE.
            WL_LOST_COMPL_2_8_YEAR  = StringParser.ReadAsInt32(source, 4  - 1, 4);
            WL_LOST_COMPL_2_8_MONTH = StringParser.ReadAsInt16(source, 8  - 1, 2);
            WL_LOST_COMPL_2_8_DAY   = StringParser.ReadAsInt16(source, 10 - 1, 2);
        }
    }
}
