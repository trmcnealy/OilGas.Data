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
    [XmlRoot(nameof(GasWellRootSegment))]
    [Table(nameof(GasWellRootSegment))]
    public class GasWellRootSegment
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

        //02 WL-GAS-ROOT-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ROOT_KEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_GAS_ROOT_KEY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-INDEX.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ON_OFF_SCHED_INDICATOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_ON_OFF_SCHED_INDICATOR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_OR_OIL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_OR_OIL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_NUMERIC_DISTRICT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_NUMERIC_DISTRICT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_RRC_ID), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_GAS_RRC_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_WELL_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GAS_WELL_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_REMOVE_FROM_SCHED_REASN), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GAS_REMOVE_FROM_SCHED_REASN
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-REMOVE-FROM-SCHED-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_REMOVE_FROM_SCHED_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_GAS_REMOVE_FROM_SCHED_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_REMOVE_FROM_SCHED_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_REMOVE_FROM_SCHED_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_REMOVE_FROM_SCHED_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_REMOVE_FROM_SCHED_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_PROB_PREVENT_ALLOW_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_PROB_PREVENT_ALLOW_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_BAY_OR_ESTUARY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_BAY_OR_ESTUARY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-LAST-UTILIZED-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_LAST_UTILIZED_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_GAS_LAST_UTILIZED_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_LAST_UTILIZED_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_LAST_UTILIZED_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_LAST_UTILIZED_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_LAST_UTILIZED_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_EXCEPT_TO_SVY_TEST_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_EXCEPT_TO_SVY_TEST_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_RECLASSED_WELL_POINTER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_GAS_RECLASSED_WELL_POINTER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-SURVEY-EFF-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_SURVEY_EFF_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_SURVEY_EFF_CENTURY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_SURVEY_EFF_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_SURVEY_EFF_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_SURVEY_EFF_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_SURVEY_EFF_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_SUBJECT_TO_14B2_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_SUBJECT_TO_14B2_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_UIC_VIOLATION_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_UIC_VIOLATION_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_LEGAL_VIOLATION_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_LEGAL_VIOLATION_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_EXT_FOR_STATE_PLUG_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_EXT_FOR_STATE_PLUG_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_CERT_HB_1975_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_CERT_HB_1975_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-CERT-EFFECTIVE-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_CERT_EFFECTIVE_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_CERT_EFFECTIVE_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_CERT_EFFECTIVE_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_CERT_EFFECTIVE_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_CERT_EFFECTIVE_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_CERT_EFFECTIVE_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-CERTIFICATION-LTR-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_CERTIFICATION_LTR_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_CERTIFICATION_LTR_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_CERTIFICATION_LTR_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_CERTIFICATION_LTR_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_CERTIFICATION_LTR_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_CERTIFICATION_LTR_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_CERTIFICATION_LTR_DD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_CERTIFICATION_LTR_DD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-SHUT-IN-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_SHUT_IN_CCYY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_GAS_SHUT_IN_CCYY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_SHUT_IN_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_SHUT_IN_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_PEND_REMOVAL_14B2_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_PEND_REMOVAL_14B2_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public GasWellRemarksSegment? GasWellRemarksSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public GasSealedWellSegment? GasSealedWellSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public GasFormG1Segment? GasFormG1Segment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public GasFormG10Segment? GasFormG10Segment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public GasFormGC1Segment? GasFormGC1Segment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public GasReportingCycleSegment? GasReportingCycleSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public GasSenateBill126Segment? GasSenateBill126Segment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public GasWellRootSegment()
        {
        }

        public GasWellRootSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-GAS-ROOT-SEG.
            WL_GAS_ROOT_KEY = StringParser.ReadAsInt64(source, 3 - 1, 8);
            //03 WL-GAS-INDEX.
            WL_GAS_ON_OFF_SCHED_INDICATOR  = StringParser.ReadAsChar(source, 11   - 1, 1);
            WL_GAS_OR_OIL                  = StringParser.ReadAsChar(source, 12   - 1, 1);
            WL_GAS_NUMERIC_DISTRICT        = StringParser.ReadAsInt16(source, 13  - 1, 2);
            WL_GAS_RRC_ID                  = StringParser.ReadAsInt64(source, 15  - 1, 6);
            WL_GAS_WELL_NUMBER             = StringParser.ReadAsString(source, 21 - 1, 6);
            WL_GAS_REMOVE_FROM_SCHED_REASN = StringParser.ReadAsString(source, 27 - 1, 2);
            //03 WL-GAS-REMOVE-FROM-SCHED-DATE.
            WL_GAS_REMOVE_FROM_SCHED_YEAR  = StringParser.ReadAsInt32(source, 29 - 1, 4);
            WL_GAS_REMOVE_FROM_SCHED_MONTH = StringParser.ReadAsInt16(source, 33 - 1, 2);
            WL_GAS_REMOVE_FROM_SCHED_DAY   = StringParser.ReadAsInt16(source, 35 - 1, 2);
            WL_GAS_PROB_PREVENT_ALLOW_FLAG = StringParser.ReadAsChar(source, 37  - 1, 1);
            WL_GAS_BAY_OR_ESTUARY          = StringParser.ReadAsChar(source, 38  - 1, 1);
            //03 WL-GAS-LAST-UTILIZED-DATE.
            WL_GAS_LAST_UTILIZED_YEAR      = StringParser.ReadAsInt32(source, 39 - 1, 4);
            WL_GAS_LAST_UTILIZED_MONTH     = StringParser.ReadAsInt16(source, 43 - 1, 2);
            WL_GAS_LAST_UTILIZED_DAY       = StringParser.ReadAsInt16(source, 45 - 1, 2);
            WL_GAS_EXCEPT_TO_SVY_TEST_FLAG = StringParser.ReadAsChar(source, 47  - 1, 1);
            WL_GAS_RECLASSED_WELL_POINTER  = StringParser.ReadAsInt64(source, 48 - 1, 8);
            //03 WL-GAS-SURVEY-EFF-DATE.
            WL_GAS_SURVEY_EFF_CENTURY      = StringParser.ReadAsInt16(source, 56 - 1, 2);
            WL_GAS_SURVEY_EFF_YEAR         = StringParser.ReadAsInt16(source, 58 - 1, 2);
            WL_GAS_SURVEY_EFF_MONTH        = StringParser.ReadAsInt16(source, 60 - 1, 2);
            WL_GAS_SUBJECT_TO_14B2_FLAG    = StringParser.ReadAsChar(source, 87  - 1, 1);
            WL_GAS_UIC_VIOLATION_FLAG      = StringParser.ReadAsChar(source, 88  - 1, 1);
            WL_GAS_LEGAL_VIOLATION_FLAG    = StringParser.ReadAsChar(source, 89  - 1, 1);
            WL_GAS_EXT_FOR_STATE_PLUG_FLAG = StringParser.ReadAsChar(source, 90  - 1, 1);
            WL_GAS_CERT_HB_1975_FLAG       = StringParser.ReadAsChar(source, 91  - 1, 1);
            //03 WL-GAS-CERT-EFFECTIVE-DATE.
            WL_GAS_CERT_EFFECTIVE_CC = StringParser.ReadAsInt16(source, 92 - 1, 2);
            WL_GAS_CERT_EFFECTIVE_YY = StringParser.ReadAsInt16(source, 94 - 1, 2);
            WL_GAS_CERT_EFFECTIVE_MM = StringParser.ReadAsInt16(source, 96 - 1, 2);
            //03 WL-GAS-CERTIFICATION-LTR-DATE.
            WL_GAS_CERTIFICATION_LTR_CC = StringParser.ReadAsInt16(source, 98  - 1, 2);
            WL_GAS_CERTIFICATION_LTR_YY = StringParser.ReadAsInt16(source, 100 - 1, 2);
            WL_GAS_CERTIFICATION_LTR_MM = StringParser.ReadAsInt16(source, 102 - 1, 2);
            WL_GAS_CERTIFICATION_LTR_DD = StringParser.ReadAsInt16(source, 104 - 1, 2);
            //03 WL-GAS-SHUT-IN-DATE.
            WL_GAS_SHUT_IN_CCYY           = StringParser.ReadAsInt32(source, 106 - 1, 4);
            WL_GAS_SHUT_IN_MM             = StringParser.ReadAsInt16(source, 110 - 1, 2);
            WL_GAS_PEND_REMOVAL_14B2_FLAG = StringParser.ReadAsChar(source, 112  - 1, 1);
        }
    }
}
