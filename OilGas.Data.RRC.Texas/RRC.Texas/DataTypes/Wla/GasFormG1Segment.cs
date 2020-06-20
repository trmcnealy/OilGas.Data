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
    [XmlRoot(nameof(GasFormG1Segment))]
    [Table(nameof(GasFormG1Segment))]
    public class GasFormG1Segment
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

        //02 WL-G1-INFORMATION-SEGMENT.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G1_KEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_G1_KEY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-G1-TEST-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G1_TEST_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_G1_TEST_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G1_TEST_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G1_TEST_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G1_TEST_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_G1_TEST_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_G1_POTENTIAL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_G1_POTENTIAL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_G1_SLOPE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? WL_GAS_G1_SLOPE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_G1_SIWH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_GAS_G1_SIWH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_G1_BHP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_GAS_G1_BHP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_G1_GAS_GRAVITY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? WL_GAS_G1_GAS_GRAVITY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_G1_COND_GRAVITY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? WL_GAS_G1_COND_GRAVITY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_G1_GOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_GAS_G1_GOR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_G1_TEST_GAS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_G1_TEST_GAS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_G1_ADMIN_ADD_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_G1_ADMIN_ADD_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public GasFormG1Segment()
        {
        }

        public GasFormG1Segment(ReadOnlySpan<byte> source)
        {
            //02 WL-G1-INFORMATION-SEGMENT.
            WL_G1_KEY = StringParser.ReadAsInt64(source, 3 - 1, 8);
            //03 WL-G1-TEST-DATE.
            WL_G1_TEST_YEAR        = StringParser.ReadAsInt32(source, 11           - 1, 4);
            WL_G1_TEST_MONTH       = StringParser.ReadAsInt16(source, 15           - 1, 2);
            WL_G1_TEST_DAY         = StringParser.ReadAsInt16(source, 17           - 1, 2);
            WL_GAS_G1_POTENTIAL    = StringParser.ReadAsPackedDouble(source, 0, 19 - 1, 5);
            WL_GAS_G1_SLOPE        = StringParser.ReadAsPackedSingle(source, 3, 24 - 1, 3);
            WL_GAS_G1_SIWH         = StringParser.ReadAsPackedInt64(source, 27     - 1, 3);
            WL_GAS_G1_BHP          = StringParser.ReadAsPackedInt64(source, 30     - 1, 3);
            WL_GAS_G1_GAS_GRAVITY  = StringParser.ReadAsPackedSingle(source, 3, 33 - 1, 3);
            WL_GAS_G1_COND_GRAVITY = StringParser.ReadAsPackedSingle(source, 1, 36 - 1, 2);
            WL_GAS_G1_GOR          = StringParser.ReadAsPackedInt64(source, 38     - 1, 3);
            WL_GAS_G1_TEST_GAS     = StringParser.ReadAsPackedDouble(source, 0, 41 - 1, 5);
            WL_G1_ADMIN_ADD_FLAG   = StringParser.ReadAsChar(source, 46            - 1, 1);
        }
    }
}
