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
    [XmlRoot(nameof(OilSealedWellSegment))]
    [Table(nameof(OilSealedWellSegment))]
    public class OilSealedWellSegment
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

        //02 WL-OIL-GAS-SEALED-SEGMENT.

        //03 WL-SEALED-EFFECTIVE-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SEALED_EFFECTIVE_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SEALED_EFFECTIVE_CENTURY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SEALED_EFFECTIVE_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SEALED_EFFECTIVE_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SEALED_EFFECTIVE_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SEALED_EFFECTIVE_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SEALED_REASON), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_SEALED_REASON
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-SEALED-ISSUE-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SEALED_ISSUE_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_SEALED_ISSUE_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SEALED_ISSUE_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SEALED_ISSUE_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SEALED_ISSUE_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SEALED_ISSUE_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SEALED_ISSUED_BY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_SEALED_ISSUED_BY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-SEAL-REMOVED-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SEAL_REMOVED_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_SEAL_REMOVED_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SEAL_REMOVED_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SEAL_REMOVED_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SEAL_REMOVED_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SEAL_REMOVED_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OilSealedWellSegment()
        {
        }

        public OilSealedWellSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-OIL-GAS-SEALED-SEGMENT.
            //03 WL-SEALED-EFFECTIVE-DATE.
            WL_SEALED_EFFECTIVE_CENTURY = StringParser.ReadAsInt16(source, 3  - 1, 2);
            WL_SEALED_EFFECTIVE_YEAR    = StringParser.ReadAsInt16(source, 5  - 1, 2);
            WL_SEALED_EFFECTIVE_MONTH   = StringParser.ReadAsInt16(source, 7  - 1, 2);
            WL_SEALED_REASON            = StringParser.ReadAsString(source, 9 - 1, 2);
            //03 WL-SEALED-ISSUE-DATE.
            WL_SEALED_ISSUE_YEAR  = StringParser.ReadAsInt32(source, 11  - 1, 4);
            WL_SEALED_ISSUE_MONTH = StringParser.ReadAsInt16(source, 15  - 1, 2);
            WL_SEALED_ISSUE_DAY   = StringParser.ReadAsInt16(source, 17  - 1, 2);
            WL_SEALED_ISSUED_BY   = StringParser.ReadAsString(source, 19 - 1, 2);
            //03 WL-SEAL-REMOVED-DATE.
            WL_SEAL_REMOVED_YEAR  = StringParser.ReadAsInt32(source, 21 - 1, 4);
            WL_SEAL_REMOVED_MONTH = StringParser.ReadAsInt16(source, 25 - 1, 2);
            WL_SEAL_REMOVED_DAY   = StringParser.ReadAsInt16(source, 27 - 1, 2);
        }
    }
}
