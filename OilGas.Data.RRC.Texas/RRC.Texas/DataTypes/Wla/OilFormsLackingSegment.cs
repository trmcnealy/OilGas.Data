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
    [XmlRoot(nameof(OilFormsLackingSegment))]
    [Table(nameof(OilFormsLackingSegment))]
    public class OilFormsLackingSegment
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

        //02 WL-OIL-FORMS-LACKING-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_FORM_LACKING), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_OIL_FORM_LACKING
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public OilFormsLackingDescriptionSegment? OilFormsLackingDescriptionSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OilFormsLackingSegment()
        {
        }

        public OilFormsLackingSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-OIL-FORMS-LACKING-SEG.
            WL_OIL_FORM_LACKING = StringParser.ReadAsString(source, 3 - 1, 4);
        }
    }
}
