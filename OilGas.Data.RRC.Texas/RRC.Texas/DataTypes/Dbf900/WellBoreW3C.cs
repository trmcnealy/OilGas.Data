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
    [XmlRoot(nameof(WellBoreW3C))]
    [Table(nameof(WellBoreW3C))]
    public class WellBoreW3C
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
        //02 WELL-BORE-FORM-W3C-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W3C_1YR_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_W3C_1YR_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W3C_1YR_FILED_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_W3C_1YR_FILED_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W3C_1YR_FILING_OPER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_W3C_1YR_FILING_OPER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W3C_5YR_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_W3C_5YR_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W3C_5YR_FILED_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_W3C_5YR_FILED_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W3C_5YR_FILING_OPER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_W3C_5YR_FILING_OPER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W3C_10YR_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_W3C_10YR_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W3C_10YR_FILED_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_W3C_10YR_FILED_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W3C_10YR_FILING_OPER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_W3C_10YR_FILING_OPER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W3C_14B2_REMOVAL_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_W3C_14B2_REMOVAL_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W3C_EXTENSION_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_W3C_EXTENSION_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-W3C-EXTENSION-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W3C_EXTENSION_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_W3C_EXTENSION_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W3C_EXTENSION_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_W3C_EXTENSION_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W3C_EXTENSION_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_W3C_EXTENSION_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W3C_5YR_FLAG_PREVIOUS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_W3C_5YR_FLAG_PREVIOUS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W3C_10YR_FLAG_PREVIOUS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_W3C_10YR_FLAG_PREVIOUS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBoreW3C()
        {
        }

        public WellBoreW3C(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-FORM-W3C-SEG.
            WB_W3C_1YR_FLAG          = StringParser.ReadAsChar(source, 3   - 1, 1);
            WB_W3C_1YR_FILED_DATE    = StringParser.ReadAsInt64(source, 4  - 1, 8);
            WB_W3C_1YR_FILING_OPER   = StringParser.ReadAsInt64(source, 12 - 1, 6);
            WB_W3C_5YR_FLAG          = StringParser.ReadAsChar(source, 18  - 1, 1);
            WB_W3C_5YR_FILED_DATE    = StringParser.ReadAsInt64(source, 19 - 1, 8);
            WB_W3C_5YR_FILING_OPER   = StringParser.ReadAsInt64(source, 27 - 1, 6);
            WB_W3C_10YR_FLAG         = StringParser.ReadAsChar(source, 33  - 1, 1);
            WB_W3C_10YR_FILED_DATE   = StringParser.ReadAsInt64(source, 34 - 1, 8);
            WB_W3C_10YR_FILING_OPER  = StringParser.ReadAsInt64(source, 42 - 1, 6);
            WB_W3C_14B2_REMOVAL_DATE = StringParser.ReadAsInt64(source, 48 - 1, 8);
            WB_W3C_EXTENSION_FLAG    = StringParser.ReadAsChar(source, 56  - 1, 1);
            //03 WB-W3C-EXTENSION-DATE.
            WB_W3C_EXTENSION_YEAR     = StringParser.ReadAsInt32(source, 57 - 1, 4);
            WB_W3C_EXTENSION_MONTH    = StringParser.ReadAsInt16(source, 61 - 1, 2);
            WB_W3C_EXTENSION_DAY      = StringParser.ReadAsInt16(source, 63 - 1, 2);
            WB_W3C_5YR_FLAG_PREVIOUS  = StringParser.ReadAsChar(source, 65  - 1, 1);
            WB_W3C_10YR_FLAG_PREVIOUS = StringParser.ReadAsChar(source, 66  - 1, 1);
        }
    }
}
