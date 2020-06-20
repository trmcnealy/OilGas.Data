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
    [XmlRoot(nameof(WellBoreSenateBill126))]
    [Table(nameof(WellBoreSenateBill126))]
    public class WellBoreSenateBill126
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
        //02 WB-SB126-SEGMENT.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_DESIGNATION_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_SB126_DESIGNATION_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB-SB126-DESIG-EFFECTIVE-DATE.

        //10 WB-SB126-DESIG-EFFEC-CCYY.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_DESIG_EFFEC_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_DESIG_EFFEC_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_DESIG_EFFEC_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_DESIG_EFFEC_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_DESIG_EFFEC_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_DESIG_EFFEC_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB-SB126-DESIG-REVISED-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_DESIG_REVISED_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_DESIG_REVISED_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_DESIG_REVISED_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_DESIG_REVISED_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_DESIG_REVISED_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_DESIG_REVISED_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB-SB126-DESIG-LETTER-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_DESIG_LETTER_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_DESIG_LETTER_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_DESIG_LETTER_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_DESIG_LETTER_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_DESIG_LETTER_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_DESIG_LETTER_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_DESIG_LETTER_DD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_DESIG_LETTER_DD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB-SB126-CERT-EFFECT-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_CERT_EFFEC_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_CERT_EFFEC_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_CERT_EFFEC_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_CERT_EFFEC_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_CERT_EFFEC_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_CERT_EFFEC_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB-SB126-CERT-REVOKED-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_CERT_REVOKED_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_CERT_REVOKED_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_CERT_REVOKED_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_CERT_REVOKED_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_CERT_REVOKED_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_CERT_REVOKED_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_CERT_REVOKED_DD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_CERT_REVOKED_DD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB-SB126-CERT-DENIAL-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_CERT_DENIAL_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_CERT_DENIAL_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_CERT_DENIAL_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_CERT_DENIAL_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_CERT_DENIAL_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_CERT_DENIAL_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_CERT_DENIAL_DD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_SB126_CERT_DENIAL_DD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SB126_DENIAL_REASON_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_SB126_DENIAL_REASON_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBoreSenateBill126()
        {
        }

        public WellBoreSenateBill126(ReadOnlySpan<byte> source)
        {
            //02 WB-SB126-SEGMENT.
            WB_SB126_DESIGNATION_FLAG = StringParser.ReadAsChar(source, 3 - 1, 1);
            //05 WB-SB126-DESIG-EFFECTIVE-DATE.
            //10 WB-SB126-DESIG-EFFEC-CCYY.
            WB_SB126_DESIG_EFFEC_CC = StringParser.ReadAsInt16(source, 4 - 1, 2);
            WB_SB126_DESIG_EFFEC_YY = StringParser.ReadAsInt16(source, 6 - 1, 2);
            WB_SB126_DESIG_EFFEC_MM = StringParser.ReadAsInt16(source, 8 - 1, 2);
            //05 WB-SB126-DESIG-REVISED-DATE.
            WB_SB126_DESIG_REVISED_CC = StringParser.ReadAsInt16(source, 10 - 1, 2);
            WB_SB126_DESIG_REVISED_YY = StringParser.ReadAsInt16(source, 12 - 1, 2);
            WB_SB126_DESIG_REVISED_MM = StringParser.ReadAsInt16(source, 14 - 1, 2);
            //05 WB-SB126-DESIG-LETTER-DATE.
            WB_SB126_DESIG_LETTER_CC = StringParser.ReadAsInt16(source, 16 - 1, 2);
            WB_SB126_DESIG_LETTER_YY = StringParser.ReadAsInt16(source, 18 - 1, 2);
            WB_SB126_DESIG_LETTER_MM = StringParser.ReadAsInt16(source, 20 - 1, 2);
            WB_SB126_DESIG_LETTER_DD = StringParser.ReadAsInt16(source, 22 - 1, 2);
            //05 WB-SB126-CERT-EFFECT-DATE.
            WB_SB126_CERT_EFFEC_CC = StringParser.ReadAsInt16(source, 24 - 1, 2);
            WB_SB126_CERT_EFFEC_YY = StringParser.ReadAsInt16(source, 26 - 1, 2);
            WB_SB126_CERT_EFFEC_MM = StringParser.ReadAsInt16(source, 28 - 1, 2);
            //05 WB-SB126-CERT-REVOKED-DATE.
            WB_SB126_CERT_REVOKED_CC = StringParser.ReadAsInt16(source, 30 - 1, 2);
            WB_SB126_CERT_REVOKED_YY = StringParser.ReadAsInt16(source, 32 - 1, 2);
            WB_SB126_CERT_REVOKED_MM = StringParser.ReadAsInt16(source, 34 - 1, 2);
            WB_SB126_CERT_REVOKED_DD = StringParser.ReadAsInt16(source, 36 - 1, 2);
            //05 WB-SB126-CERT-DENIAL-DATE.
            WB_SB126_CERT_DENIAL_CC     = StringParser.ReadAsInt16(source, 38 - 1, 2);
            WB_SB126_CERT_DENIAL_YY     = StringParser.ReadAsInt16(source, 40 - 1, 2);
            WB_SB126_CERT_DENIAL_MM     = StringParser.ReadAsInt16(source, 42 - 1, 2);
            WB_SB126_CERT_DENIAL_DD     = StringParser.ReadAsInt16(source, 44 - 1, 2);
            WB_SB126_DENIAL_REASON_FLAG = StringParser.ReadAsChar(source, 46  - 1, 1);
        }
    }
}
