#nullable enable
using System;
using System.Collections.Generic;
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
    [XmlRoot(nameof(WellBoreTechnicalDataRoot))]
    [Table(nameof(WellBoreTechnicalDataRoot))]
    public class WellBoreTechnicalDataRoot
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
        //02 WELL-BORE-API-ROOT.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_API_CNTY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_API_CNTY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_API_UNIQUE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_API_UNIQUE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_NXT_AVAIL_SUFFIX), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_NXT_AVAIL_SUFFIX
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_NXT_AVAIL_HOLE_CHGE_NBR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_NXT_AVAIL_HOLE_CHGE_NBR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_FIELD_DISTRICT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_FIELD_DISTRICT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_RES_CNTY_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_RES_CNTY_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_ORIG_COMPL_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_ORIG_COMPL_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-ORIG-COMPL-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_ORIG_COMPL_CENT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_ORIG_COMPL_CENT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_ORIG_COMPL_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_ORIG_COMPL_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_ORIG_COMPL_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_ORIG_COMPL_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_ORIG_COMPL_DD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_ORIG_COMPL_DD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_TOTAL_DEPTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_TOTAL_DEPTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_VALID_FLUID_LEVEL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_VALID_FLUID_LEVEL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-CERTIFICATION-REVOKED-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CERT_REVOKED_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_CERT_REVOKED_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CERT_REVOKED_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_CERT_REVOKED_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CERT_REVOKED_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_CERT_REVOKED_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CERT_REVOKED_DD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_CERT_REVOKED_DD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-CERTIFICATION-DENIAL-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CERTIFICATION_DENIAL_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_CERTIFICATION_DENIAL_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CERTIFICATION_DENIAL_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_CERTIFICATION_DENIAL_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CERTIFICATION_DENIAL_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_CERTIFICATION_DENIAL_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CERTIFICATION_DENIAL_DD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_CERTIFICATION_DENIAL_DD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DENIAL_REASON_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_DENIAL_REASON_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_ERROR_API_ASSIGN_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_ERROR_API_ASSIGN_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_REFER_CORRECT_API_NBR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_REFER_CORRECT_API_NBR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DUMMY_API_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_DUMMY_API_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DATE_DUMMY_REPLACED), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_DATE_DUMMY_REPLACED
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_NEWEST_DRL_PMT_NBR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_NEWEST_DRL_PMT_NBR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CANCEL_EXPIRE_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_CANCEL_EXPIRE_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PREVIOUS_API_NBR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PREVIOUS_API_NBR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_EX14B2_COUNT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_EX14B2_COUNT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DESIGNATION_HB_1975_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_DESIGNATION_HB_1975_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-DESIGNATION-EFFECTIVE-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DESIGNATION_EFFEC_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_DESIGNATION_EFFEC_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DESIGNATION_EFFEC_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_DESIGNATION_EFFEC_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DESIGNATION_EFFEC_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_DESIGNATION_EFFEC_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-DESIGNATION-REVISED-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DESIGNATION_REVISED_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_DESIGNATION_REVISED_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DESIGNATION_REVISED_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_DESIGNATION_REVISED_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DESIGNATION_REVISED_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_DESIGNATION_REVISED_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-DESIGNATION-LETTER-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DESIGNATION_LETTER_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_DESIGNATION_LETTER_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DESIGNATION_LETTER_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_DESIGNATION_LETTER_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DESIGNATION_LETTER_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_DESIGNATION_LETTER_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DESIGNATION_LETTER_DD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_DESIGNATION_LETTER_DD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-CERTIFICATION-EFFECT-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CERTIFICATION_EFFEC_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_CERTIFICATION_EFFEC_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CERTIFICATION_EFFEC_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_CERTIFICATION_EFFEC_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CERTIFICATION_EFFEC_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_CERTIFICATION_EFFEC_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_WATER_LAND_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_WATER_LAND_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_TOTAL_BONDED_DEPTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_TOTAL_BONDED_DEPTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_OVERRIDE_EST_PLUG_COST), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_OVERRIDE_EST_PLUG_COST
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SHUT_IN_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_SHUT_IN_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB-SHUT-IN-YEAR PIC 9(04).

        //05 WB-SHUT-IN-MONTH PIC 9(02).

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_OVERRIDE_BONDED_DEPTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_OVERRIDE_BONDED_DEPTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SUBJ_TO_14B2_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_SUBJ_TO_14B2_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PEND_REMOVAL_14B2_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_PEND_REMOVAL_14B2_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_ORPHAN_WELL_HOLD_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_ORPHAN_WELL_HOLD_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W3X_WELL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_W3X_WELL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBoreCompletionInformation>? WellBoreCompletionInformation
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBoreCompletionInformation>();

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual WellBoreOldLocation? WellBoreOldLocation
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual WellBoreNewLocation? WellBoreNewLocation
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBorePluggingData>? WellBorePluggingData
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBorePluggingData>();

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBoreDrillingPermitNumber>? WellBoreDrillingPermitNumber
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBoreDrillingPermitNumber>();

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual WellBore14B2Well? WellBore14B2Well
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBoreH15Report>? WellBoreH15Report
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBoreH15Report>();

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual WellBoreSenateBill126? WellBoreSenateBill126
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBoreDrillingPermitStatus>? WellBoreDrillingPermitStatus
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBoreDrillingPermitStatus>();

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBoreW3C>? WellBoreW3C
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBoreW3C>();

        public WellBoreTechnicalDataRoot()
        {
        }

        public WellBoreTechnicalDataRoot(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-API-ROOT.
            WB_API_CNTY                = StringParser.ReadAsInt32(source, 3  - 1, 3);
            WB_API_UNIQUE              = StringParser.ReadAsInt64(source, 6  - 1, 5);
            WB_NXT_AVAIL_SUFFIX        = StringParser.ReadAsInt16(source, 11 - 1, 2);
            WB_NXT_AVAIL_HOLE_CHGE_NBR = StringParser.ReadAsInt16(source, 13 - 1, 2);
            WB_FIELD_DISTRICT          = StringParser.ReadAsInt16(source, 15 - 1, 2);
            WB_RES_CNTY_CODE           = StringParser.ReadAsInt32(source, 17 - 1, 3);
            WB_ORIG_COMPL_CC           = StringParser.ReadAsChar(source, 20  - 1, 1);
            //03 WB-ORIG-COMPL-DATE.
            WB_ORIG_COMPL_CENT   = StringParser.ReadAsInt16(source, 21 - 1, 2);
            WB_ORIG_COMPL_YY     = StringParser.ReadAsInt16(source, 23 - 1, 2);
            WB_ORIG_COMPL_MM     = StringParser.ReadAsInt16(source, 25 - 1, 2);
            WB_ORIG_COMPL_DD     = StringParser.ReadAsInt16(source, 27 - 1, 2);
            WB_TOTAL_DEPTH       = StringParser.ReadAsInt64(source, 29 - 1, 5);
            WB_VALID_FLUID_LEVEL = StringParser.ReadAsInt64(source, 34 - 1, 5);
            //03 WB-CERTIFICATION-REVOKED-DATE.
            WB_CERT_REVOKED_CC = StringParser.ReadAsInt16(source, 39 - 1, 2);
            WB_CERT_REVOKED_YY = StringParser.ReadAsInt16(source, 41 - 1, 2);
            WB_CERT_REVOKED_MM = StringParser.ReadAsInt16(source, 43 - 1, 2);
            WB_CERT_REVOKED_DD = StringParser.ReadAsInt16(source, 45 - 1, 2);
            //03 WB-CERTIFICATION-DENIAL-DATE.
            WB_CERTIFICATION_DENIAL_CC  = StringParser.ReadAsInt16(source, 47  - 1, 2);
            WB_CERTIFICATION_DENIAL_YY  = StringParser.ReadAsInt16(source, 49  - 1, 2);
            WB_CERTIFICATION_DENIAL_MM  = StringParser.ReadAsInt16(source, 51  - 1, 2);
            WB_CERTIFICATION_DENIAL_DD  = StringParser.ReadAsInt16(source, 53  - 1, 2);
            WB_DENIAL_REASON_FLAG       = StringParser.ReadAsChar(source, 55   - 1, 1);
            WB_ERROR_API_ASSIGN_CODE    = StringParser.ReadAsChar(source, 56   - 1, 1);
            WB_REFER_CORRECT_API_NBR    = StringParser.ReadAsInt64(source, 57  - 1, 8);
            WB_DUMMY_API_NUMBER         = StringParser.ReadAsInt64(source, 65  - 1, 8);
            WB_DATE_DUMMY_REPLACED      = StringParser.ReadAsInt64(source, 73  - 1, 8);
            WB_NEWEST_DRL_PMT_NBR       = StringParser.ReadAsInt64(source, 81  - 1, 6);
            WB_CANCEL_EXPIRE_CODE       = StringParser.ReadAsChar(source, 87   - 1, 1);
            WB_PREVIOUS_API_NBR         = StringParser.ReadAsInt64(source, 92  - 1, 10);
            WB_EX14B2_COUNT             = StringParser.ReadAsInt16(source, 103 - 1, 2);
            WB_DESIGNATION_HB_1975_FLAG = StringParser.ReadAsChar(source, 105  - 1, 1);
            //03 WB-DESIGNATION-EFFECTIVE-DATE.
            WB_DESIGNATION_EFFEC_CC = StringParser.ReadAsInt16(source, 106 - 1, 2);
            WB_DESIGNATION_EFFEC_YY = StringParser.ReadAsInt16(source, 108 - 1, 2);
            WB_DESIGNATION_EFFEC_MM = StringParser.ReadAsInt16(source, 110 - 1, 2);
            //03 WB-DESIGNATION-REVISED-DATE.
            WB_DESIGNATION_REVISED_CC = StringParser.ReadAsInt16(source, 112 - 1, 2);
            WB_DESIGNATION_REVISED_YY = StringParser.ReadAsInt16(source, 114 - 1, 2);
            WB_DESIGNATION_REVISED_MM = StringParser.ReadAsInt16(source, 116 - 1, 2);
            //03 WB-DESIGNATION-LETTER-DATE.
            WB_DESIGNATION_LETTER_CC = StringParser.ReadAsInt16(source, 118 - 1, 2);
            WB_DESIGNATION_LETTER_YY = StringParser.ReadAsInt16(source, 120 - 1, 2);
            WB_DESIGNATION_LETTER_MM = StringParser.ReadAsInt16(source, 122 - 1, 2);
            WB_DESIGNATION_LETTER_DD = StringParser.ReadAsInt16(source, 124 - 1, 2);
            //03 WB-CERTIFICATION-EFFECT-DATE.
            WB_CERTIFICATION_EFFEC_CC = StringParser.ReadAsInt16(source, 126 - 1, 2);
            WB_CERTIFICATION_EFFEC_YY = StringParser.ReadAsInt16(source, 128 - 1, 2);
            WB_CERTIFICATION_EFFEC_MM = StringParser.ReadAsInt16(source, 130 - 1, 2);
            WB_WATER_LAND_CODE        = StringParser.ReadAsChar(source, 132  - 1, 1);
            WB_TOTAL_BONDED_DEPTH     = StringParser.ReadAsInt64(source, 133 - 1, 6);
            WB_OVERRIDE_EST_PLUG_COST = StringParser.ReadAsInt64(source, 139 - 1, 7);
            WB_SHUT_IN_DATE           = StringParser.ReadAsInt64(source, 146 - 1, 6);
            //05 WB-SHUT-IN-YEAR PIC 9(04).
            //05 WB-SHUT-IN-MONTH PIC 9(02).
            WB_OVERRIDE_BONDED_DEPTH  = StringParser.ReadAsInt64(source, 152 - 1, 6);
            WB_SUBJ_TO_14B2_FLAG      = StringParser.ReadAsChar(source, 158  - 1, 1);
            WB_PEND_REMOVAL_14B2_FLAG = StringParser.ReadAsChar(source, 159  - 1, 1);
            WB_ORPHAN_WELL_HOLD_FLAG  = StringParser.ReadAsChar(source, 160  - 1, 0);
            WB_W3X_WELL_FLAG          = StringParser.ReadAsChar(source, 160  - 1, 1);
        }
    }
}
