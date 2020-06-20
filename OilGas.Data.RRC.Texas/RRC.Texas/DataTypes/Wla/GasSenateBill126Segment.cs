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
    [XmlRoot(nameof(GasSenateBill126Segment))]
    [Table(nameof(GasSenateBill126Segment))]
    public class GasSenateBill126Segment
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

        //02 WL-SB126-SEGMENT.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SB126_CERT_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_SB126_CERT_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //05 WL-SB126-CERT-EFFECT-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SB126_CERT_EFFECT_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SB126_CERT_EFFECT_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SB126_CERT_EFFECT_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SB126_CERT_EFFECT_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SB126_CERT_EFFECT_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SB126_CERT_EFFECT_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //05 WL-SB126-CERT-LTR-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SB126_CERT_LTR_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SB126_CERT_LTR_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SB126_CERT_LTR_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SB126_CERT_LTR_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SB126_CERT_LTR_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SB126_CERT_LTR_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SB126_CERT_LTR_DD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SB126_CERT_LTR_DD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //05 WL-SB126-TURNAROUND-LTR-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SB126_TURNAROUND_LTR_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SB126_TURNAROUND_LTR_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SB126_TURNAROUND_LTR_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SB126_TURNAROUND_LTR_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SB126_TURNAROUND_LTR_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SB126_TURNAROUND_LTR_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_SB126_TURNAROUND_LTR_DD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_SB126_TURNAROUND_LTR_DD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public GasSenateBill126Segment()
        {
        }

        public GasSenateBill126Segment(ReadOnlySpan<byte> source)
        {
            //02 WL-SB126-SEGMENT.
            WL_SB126_CERT_FLAG = StringParser.ReadAsChar(source, 3 - 1, 1);
            //05 WL-SB126-CERT-EFFECT-DATE.
            WL_SB126_CERT_EFFECT_CC = StringParser.ReadAsInt16(source, 4 - 1, 2);
            WL_SB126_CERT_EFFECT_YY = StringParser.ReadAsInt16(source, 6 - 1, 2);
            WL_SB126_CERT_EFFECT_MM = StringParser.ReadAsInt16(source, 8 - 1, 2);
            //05 WL-SB126-CERT-LTR-DATE.
            WL_SB126_CERT_LTR_CC = StringParser.ReadAsInt16(source, 10 - 1, 2);
            WL_SB126_CERT_LTR_YY = StringParser.ReadAsInt16(source, 12 - 1, 2);
            WL_SB126_CERT_LTR_MM = StringParser.ReadAsInt16(source, 14 - 1, 2);
            WL_SB126_CERT_LTR_DD = StringParser.ReadAsInt16(source, 16 - 1, 2);
            //05 WL-SB126-TURNAROUND-LTR-DATE.
            WL_SB126_TURNAROUND_LTR_CC = StringParser.ReadAsInt16(source, 18 - 1, 2);
            WL_SB126_TURNAROUND_LTR_YY = StringParser.ReadAsInt16(source, 20 - 1, 2);
            WL_SB126_TURNAROUND_LTR_MM = StringParser.ReadAsInt16(source, 22 - 1, 2);
            WL_SB126_TURNAROUND_LTR_DD = StringParser.ReadAsInt16(source, 24 - 1, 2);
        }
    }
}
