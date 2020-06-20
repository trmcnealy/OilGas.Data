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
    [XmlRoot(nameof(WellBoreCasing))]
    [Table(nameof(WellBoreCasing))]
    public class WellBoreCasing
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
        //02 WELL-BORE-CASING-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CASING_COUNT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_CASING_COUNT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-CASING-SIZE-DATA.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CAS_INCH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_CAS_INCH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB-CAS-FRACTION.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CAS_FRAC_NUM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_CAS_FRAC_NUM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CAS_FRAC_DENOM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_CAS_FRAC_DENOM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CAS_WT_TABLE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_CAS_WT_TABLE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-CASING-WEIGHT-LBS-FT REDEFINES WB-CAS-WT-TABLE OCCURS 2 TIMES.

        //05 WB-WGT-WHOLE PIC 9(03).

        //05 WB-WGT-TENTHS PIC 9(01).

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CASING_DEPTH_SET), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_CASING_DEPTH_SET
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_MLTI_STG_TOOL_DPTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_MLTI_STG_TOOL_DPTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_AMOUNT_OF_CEMENT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_AMOUNT_OF_CEMENT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-CASING-HOLE-SIZE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_HOLE_INCH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_HOLE_INCH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB-HOLE-FRACTION.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_HOLE_FRAC_NUM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_HOLE_FRAC_NUM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_HOLE_FRAC_DENOM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_HOLE_FRAC_DENOM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_TOP_OF_CEMENT_CASING), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_TOP_OF_CEMENT_CASING
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_AMOUNT_CASING_LEFT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_AMOUNT_CASING_LEFT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBoreCasing()
        {
        }

        public WellBoreCasing(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-CASING-SEG.
            WB_CASING_COUNT = StringParser.ReadAsInt32(source, 3 - 1, 3);
            //03 WB-CASING-SIZE-DATA.
            WB_CAS_INCH = StringParser.ReadAsInt16(source, 6 - 1, 2);
            //05 WB-CAS-FRACTION.
            WB_CAS_FRAC_NUM   = StringParser.ReadAsInt16(source, 8  - 1, 2);
            WB_CAS_FRAC_DENOM = StringParser.ReadAsInt16(source, 10 - 1, 2);
            WB_CAS_WT_TABLE   = StringParser.ReadAsInt64(source, 12 - 1, 8);
            //03 WB-CASING-WEIGHT-LBS-FT REDEFINES WB-CAS-WT-TABLE OCCURS 2 TIMES.
            //05 WB-WGT-WHOLE PIC 9(03).
            //05 WB-WGT-TENTHS PIC 9(01).
            WB_CASING_DEPTH_SET   = StringParser.ReadAsInt64(source, 20 - 1, 5);
            WB_MLTI_STG_TOOL_DPTH = StringParser.ReadAsInt64(source, 25 - 1, 5);
            WB_AMOUNT_OF_CEMENT   = StringParser.ReadAsInt64(source, 30 - 1, 6);
            //03 WB-CASING-HOLE-SIZE.
            WB_HOLE_INCH = StringParser.ReadAsInt16(source, 36 - 1, 2);
            //05 WB-HOLE-FRACTION.
            WB_HOLE_FRAC_NUM        = StringParser.ReadAsInt16(source, 38  - 1, 2);
            WB_HOLE_FRAC_DENOM      = StringParser.ReadAsInt16(source, 40  - 1, 2);
            WB_TOP_OF_CEMENT_CASING = StringParser.ReadAsString(source, 43 - 1, 7);
            WB_AMOUNT_CASING_LEFT   = StringParser.ReadAsInt64(source, 50  - 1, 5);
        }
    }
}
