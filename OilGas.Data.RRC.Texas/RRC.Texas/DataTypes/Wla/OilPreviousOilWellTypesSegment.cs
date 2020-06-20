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
    [XmlRoot(nameof(OilPreviousOilWellTypesSegment))]
    [Table(nameof(OilPreviousOilWellTypesSegment))]
    public class OilPreviousOilWellTypesSegment
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

        //02 WL-PREVIOUS-WELL-TYPE-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_PREV_WELL_TYPE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_PREV_WELL_TYPE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OilPreviousOilWellTypesSegment()
        {
        }

        public OilPreviousOilWellTypesSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-PREVIOUS-WELL-TYPE-SEG.
            WL_PREV_WELL_TYPE = StringParser.ReadAsString(source, 3 - 1, 2);
        }
    }
}
