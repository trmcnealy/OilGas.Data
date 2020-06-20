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
    [XmlRoot(nameof(GasFormsLackingDescriptionSegment))]
    [Table(nameof(GasFormsLackingDescriptionSegment))]
    public class GasFormsLackingDescriptionSegment
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

        //02 WL-GAS-FORM-LACK-DESCRIPTION.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_FORMS_DESCRIPTION), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GAS_FORMS_DESCRIPTION
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public GasFormsLackingDescriptionSegment()
        {
        }

        public GasFormsLackingDescriptionSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-GAS-FORM-LACK-DESCRIPTION.
            WL_GAS_FORMS_DESCRIPTION = StringParser.ReadAsString(source, 3 - 1, 20);
        }
    }
}
