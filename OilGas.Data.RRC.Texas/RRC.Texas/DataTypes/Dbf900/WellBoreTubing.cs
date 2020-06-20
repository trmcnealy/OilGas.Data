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
    [XmlRoot(nameof(WellBoreTubing))]
    [Table(nameof(WellBoreTubing))]
    public class WellBoreTubing
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
        //02 WELL-BORE-TUBING-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SEGMENT_COUNTER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_SEGMENT_COUNTER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-TUBING-SIZE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_TUBING_INCHES), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_TUBING_INCHES
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB-TUBING-FRACTION.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_FR_NUMERATOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_FR_NUMERATOR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_FR_DENOMINATOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_FR_DENOMINATOR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DEPTH_SET), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_DEPTH_SET
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PACKER_SET), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PACKER_SET
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBoreTubing()
        {
        }

        public WellBoreTubing(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-TUBING-SEG.
            WB_SEGMENT_COUNTER = StringParser.ReadAsInt32(source, 3 - 1, 3);
            //03 WB-TUBING-SIZE.
            WB_TUBING_INCHES = StringParser.ReadAsInt16(source, 6 - 1, 2);
            //05 WB-TUBING-FRACTION.
            WB_FR_NUMERATOR   = StringParser.ReadAsInt16(source, 8  - 1, 2);
            WB_FR_DENOMINATOR = StringParser.ReadAsInt16(source, 10 - 1, 2);
            WB_DEPTH_SET      = StringParser.ReadAsInt64(source, 12 - 1, 5);
            WB_PACKER_SET     = StringParser.ReadAsInt64(source, 17 - 1, 5);
        }
    }
}
