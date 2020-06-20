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
    [XmlRoot(nameof(GasFormGC1Segment))]
    [Table(nameof(GasFormGC1Segment))]
    public class GasFormGC1Segment
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

        //02 WL-GC1-INFORMATION-SEGMENT.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GC1_KEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_GC1_KEY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GC1_NUMBER_OF_EFF_CYCLES), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public byte? WL_GC1_NUMBER_OF_EFF_CYCLES
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GC1-DATE-OF-DETERMINATION.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GC1_TEST_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_GC1_TEST_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GC1_TEST_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GC1_TEST_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GC1_TEST_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GC1_TEST_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GC1_TYPE_TEST_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GC1_TYPE_TEST_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GC1_OTHER_REMARK), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GC1_OTHER_REMARK
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GC1_SUBST_CAPABILITY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GC1_SUBST_CAPABILITY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GC1_ENGINEER_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GC1_ENGINEER_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GC1_CERTIFIER_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GC1_CERTIFIER_NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public GasFormGC1Segment()
        {
        }

        public GasFormGC1Segment(ReadOnlySpan<byte> source)
        {
            //02 WL-GC1-INFORMATION-SEGMENT.
            WL_GC1_KEY                  = StringParser.ReadAsInt64(source, 3 - 1, 6);
            WL_GC1_NUMBER_OF_EFF_CYCLES = StringParser.ReadAsByte(source, 9 - 1, 1);
            //03 WL-GC1-DATE-OF-DETERMINATION.
            WL_GC1_TEST_YEAR        = StringParser.ReadAsInt32(source, 10           - 1, 4);
            WL_GC1_TEST_MONTH       = StringParser.ReadAsInt16(source, 14           - 1, 2);
            WL_GC1_TEST_DAY         = StringParser.ReadAsInt16(source, 16           - 1, 2);
            WL_GC1_TYPE_TEST_FLAG   = StringParser.ReadAsChar(source, 18            - 1, 1);
            WL_GC1_OTHER_REMARK     = StringParser.ReadAsString(source, 19          - 1, 32);
            WL_GC1_SUBST_CAPABILITY = StringParser.ReadAsPackedDouble(source, 0, 51 - 1, 5);
            WL_GC1_ENGINEER_FLAG    = StringParser.ReadAsChar(source, 61            - 1, 1);
            WL_GC1_CERTIFIER_NAME   = StringParser.ReadAsString(source, 62          - 1, 32);
        }
    }
}
