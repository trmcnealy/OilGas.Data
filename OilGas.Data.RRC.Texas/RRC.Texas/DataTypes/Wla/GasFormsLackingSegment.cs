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
    [XmlRoot(nameof(GasFormsLackingSegment))]
    [Table(nameof(GasFormsLackingSegment))]
    public class GasFormsLackingSegment
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

        //02 WL-GAS-FORMS-LACKING-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_FORM_LACK_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GAS_FORM_LACK_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public GasFormsLackingDescriptionSegment? GasFormsLackingDescriptionSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public GasFormsLackingSegment()
        {
        }

        public GasFormsLackingSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-GAS-FORMS-LACKING-SEG.
            WL_GAS_FORM_LACK_CODE = StringParser.ReadAsString(source, 3 - 1, 4);
        }
    }
}
