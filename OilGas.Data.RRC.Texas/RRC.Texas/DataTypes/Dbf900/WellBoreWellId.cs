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
    [XmlRoot(nameof(WellBoreWellId))]
    [Table(nameof(WellBoreWellId))]
    public class WellBoreWellId
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
        //02 WELL-BORE-WELL-ID-SEG.

        //03 WB-OIL-INFO.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_OIL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_OIL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_OIL_DISTRICT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_OIL_DISTRICT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_OIL_LSE_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_OIL_LSE_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_OIL_WELL_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_OIL_WELL_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-GAS-INFO REDEFINES WB-OIL-INFO.

        //05 WB-GAS PIC X(01).

        //05 WB-GAS-RRCID PIC 9(06).

        public WellBoreWellId()
        {
        }

        public WellBoreWellId(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-WELL-ID-SEG.
            //03 WB-OIL-INFO.
            WB_OIL             = StringParser.ReadAsChar(source, 3    - 1, 1);
            WB_OIL_DISTRICT    = StringParser.ReadAsInt16(source, 4   - 1, 2);
            WB_OIL_LSE_NUMBER  = StringParser.ReadAsInt64(source, 6   - 1, 5);
            WB_OIL_WELL_NUMBER = StringParser.ReadAsString(source, 11 - 1, 6);
            //03 WB-GAS-INFO REDEFINES WB-OIL-INFO.
            //05 WB-GAS PIC X(01).
            //05 WB-GAS-RRCID PIC 9(06).
        }
    }
}
