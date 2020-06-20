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
    [XmlRoot(nameof(WellBoreLiner))]
    [Table(nameof(WellBoreLiner))]
    public class WellBoreLiner
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
        //02 WELL-BORE-LINER-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_LINE_COUNT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_LINE_COUNT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-LINER-SIZE-DATA.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_LIN_INCH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_LIN_INCH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB-LIN-FRACTION.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_LIN_FRAC_NUM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_LIN_FRAC_NUM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_LIN_FRAC_DENOM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_LIN_FRAC_DENOM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SACKS_OF_CEMENT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_SACKS_OF_CEMENT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_TOP_OF_LINER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_TOP_OF_LINER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_BOTTOM_OF_LINER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_BOTTOM_OF_LINER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBoreLiner()
        {
        }

        public WellBoreLiner(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-LINER-SEG.
            WB_LINE_COUNT = StringParser.ReadAsInt32(source, 3 - 1, 3);
            //03 WB-LINER-SIZE-DATA.
            WB_LIN_INCH = StringParser.ReadAsInt16(source, 6 - 1, 2);
            //05 WB-LIN-FRACTION.
            WB_LIN_FRAC_NUM    = StringParser.ReadAsInt16(source, 8  - 1, 2);
            WB_LIN_FRAC_DENOM  = StringParser.ReadAsInt16(source, 10 - 1, 2);
            WB_SACKS_OF_CEMENT = StringParser.ReadAsInt64(source, 12 - 1, 5);
            WB_TOP_OF_LINER    = StringParser.ReadAsInt64(source, 17 - 1, 5);
            WB_BOTTOM_OF_LINER = StringParser.ReadAsInt64(source, 22 - 1, 5);
        }
    }
}
