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
    [XmlRoot(nameof(OilWellRemark))]
    [Table(nameof(OilWellRemark))]
    public class OilWellRemark
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

        //02 WL-WELL-REMARKS.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_REMARK_KEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_REMARK_KEY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-WELL-REMARK-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_REMARK_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_REMARK_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_REMARK_MON), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_REMARK_MON
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_REMARK_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_REMARK_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_REMARK_TEXT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_REMARK_TEXT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OilWellRemark()
        {
        }

        public OilWellRemark(ReadOnlySpan<byte> source)
        {
            //02 WL-WELL-REMARKS.
            WL_REMARK_KEY = StringParser.ReadAsInt32(source, 3 - 1, 3);
            //03 WL-WELL-REMARK-DATE.
            WL_REMARK_YEAR = StringParser.ReadAsInt32(source, 6   - 1, 4);
            WL_REMARK_MON  = StringParser.ReadAsInt16(source, 10  - 1, 2);
            WL_REMARK_DAY  = StringParser.ReadAsInt16(source, 12  - 1, 2);
            WL_REMARK_TEXT = StringParser.ReadAsString(source, 14 - 1, 66);
        }
    }
}
