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
    [XmlRoot(nameof(OilWellRootSegment))]
    [Table(nameof(OilWellRootSegment))]
    public class OilWellRootSegment
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

        //02 WL-OIL-ROOT-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ROOT_KEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_ROOT_KEY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-OIL-INDEX.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ON_OFF_SCHED_INDICATOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_ON_OFF_SCHED_INDICATOR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_OR_GAS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_OR_GAS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_NUMERIC_DISTRICT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_NUMERIC_DISTRICT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_LEASE_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_LEASE_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_UNIT_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_UNIT_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_WELL_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_OIL_WELL_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_REMOVE_FROM_SCHED_REASN), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_OIL_REMOVE_FROM_SCHED_REASN
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-OIL-REMOVE-FROM-SCHED-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_REMOVE_FROM_SCHED_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_REMOVE_FROM_SCHED_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_REMOVE_FROM_SCHED_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_REMOVE_FROM_SCHED_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_REMOVE_FROM_SCHED_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_REMOVE_FROM_SCHED_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_RECLASSED_WELL_POINTER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_RECLASSED_WELL_POINTER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_PROB_PREVENT_ALLOW_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_PROB_PREVENT_ALLOW_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_RESERVOIR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_OIL_RESERVOIR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_BAY_OR_ESTUARY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_BAY_OR_ESTUARY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_W10_EXC_TO_SVY_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_W10_EXC_TO_SVY_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ONSHORE_CO_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_OIL_ONSHORE_CO_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ORIGINAL_POTENTIAL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_OIL_ORIGINAL_POTENTIAL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_UNIT_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_UNIT_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-OIL-SURVEY-EFF-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_SURVEY_EFF_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_SURVEY_EFF_CENTURY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_SURVEY_EFF_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_SURVEY_EFF_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_SURVEY_EFF_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_SURVEY_EFF_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_NEW_WB_CONNECT_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_NEW_WB_CONNECT_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //05 WL-OIL-NEW-WB-CONNECT-CCYY.

        //15 WL-OIL-NEW-WB-CONNECT-CC PIC 9(02).

        //15 WL-OIL-NEW-WB-CONNECT-YEAR PIC 9(02).

        //05 WL-OIL-NEW-WB-CONNECT-CENTURY REDEFINES

        //WL-OIL-NEW-WB-CONNECT-CCYY PIC 9(04).

        //05 WL-OIL-NEW-WB-CONNECT-MONTH PIC 9(02).

        //05 WL-OIL-NEW-WB-CONNECT-DAY PIC 9(02).

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_SUBJECT_TO_14B2_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_SUBJECT_TO_14B2_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_UIC_VIOLATION_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_UIC_VIOLATION_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_LEGAL_VIOLATION_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_LEGAL_VIOLATION_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_EXT_FOR_STATE_PLUG_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_EXT_FOR_STATE_PLUG_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CERT_HB_1975_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_CERT_HB_1975_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-OIL-CERT-EFFECTIVE-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CERT_EFFECTIVE_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_CERT_EFFECTIVE_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CERT_EFFECTIVE_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_CERT_EFFECTIVE_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CERT_EFFECTIVE_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_CERT_EFFECTIVE_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-OIL-CERTIFICATION-LTR-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CERTIFICATION_LTR_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_CERTIFICATION_LTR_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CERTIFICATION_LTR_YY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_CERTIFICATION_LTR_YY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CERTIFICATION_LTR_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_CERTIFICATION_LTR_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CERTIFICATION_LTR_DD), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_CERTIFICATION_LTR_DD
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-OIL-SHUT-IN-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_SHUT_IN_CCYY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_SHUT_IN_CCYY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_SHUT_IN_MM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_SHUT_IN_MM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_PEND_REMOVAL_14B2_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_PEND_REMOVAL_14B2_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public OilReportingCycleSegment? OilReportingCycleSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public OilPreviousOilWellTypesSegment? OilPreviousOilWellTypesSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public OilPanhandleGasBalancingSegment? OilPanhandleGasBalancingSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public OilFormW10Segment? OilFormW10Segment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public OilWellRemark? OilWellRemark
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public OilSealedWellSegment? OilSealedWellSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public OilSenateBill126Segment? OilSenateBill126Segment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OilWellRootSegment()
        {
        }

        public OilWellRootSegment(ReadOnlySpan<byte> source)
        {
            WL_OIL_ROOT_KEY = StringParser.ReadAsInt64(source, 3 - 1, 8);
            //03 WL-OIL-INDEX.
            WL_OIL_ON_OFF_SCHED_INDICATOR  = StringParser.ReadAsChar(source, 11   - 1, 1);
            WL_OIL_OR_GAS                  = StringParser.ReadAsChar(source, 12   - 1, 1);
            WL_OIL_NUMERIC_DISTRICT        = StringParser.ReadAsInt16(source, 13  - 1, 2);
            WL_OIL_LEASE_NUMBER            = StringParser.ReadAsInt64(source, 15  - 1, 5);
            WL_OIL_UNIT_NUMBER             = StringParser.ReadAsChar(source, 20   - 1, 1);
            WL_OIL_WELL_NUMBER             = StringParser.ReadAsString(source, 21 - 1, 6);
            WL_OIL_REMOVE_FROM_SCHED_REASN = StringParser.ReadAsString(source, 27 - 1, 2);
            //03 WL-OIL-REMOVE-FROM-SCHED-DATE.
            WL_OIL_REMOVE_FROM_SCHED_YEAR  = StringParser.ReadAsInt32(source, 29           - 1, 4);
            WL_OIL_REMOVE_FROM_SCHED_MONTH = StringParser.ReadAsInt16(source, 33           - 1, 2);
            WL_OIL_REMOVE_FROM_SCHED_DAY   = StringParser.ReadAsInt16(source, 35           - 1, 2);
            WL_OIL_RECLASSED_WELL_POINTER  = StringParser.ReadAsInt64(source, 37           - 1, 8);
            WL_OIL_PROB_PREVENT_ALLOW_FLAG = StringParser.ReadAsChar(source, 45            - 1, 1);
            WL_OIL_RESERVOIR               = StringParser.ReadAsString(source, 47          - 1, 5);
            WL_OIL_BAY_OR_ESTUARY          = StringParser.ReadAsChar(source, 52            - 1, 1);
            WL_OIL_W10_EXC_TO_SVY_FLAG     = StringParser.ReadAsChar(source, 57            - 1, 1);
            WL_OIL_ONSHORE_CO_CODE         = StringParser.ReadAsString(source, 58          - 1, 3);
            WL_OIL_ORIGINAL_POTENTIAL      = StringParser.ReadAsPackedDouble(source, 3, 61 - 1, 5);
            WL_OIL_UNIT_NO                 = StringParser.ReadAsChar(source, 69            - 1, 1);
            //03 WL-OIL-SURVEY-EFF-DATE.
            WL_OIL_SURVEY_EFF_CENTURY  = StringParser.ReadAsInt16(source, 73 - 1, 2);
            WL_OIL_SURVEY_EFF_YEAR     = StringParser.ReadAsInt16(source, 75 - 1, 2);
            WL_OIL_SURVEY_EFF_MONTH    = StringParser.ReadAsInt16(source, 77 - 1, 2);
            WL_OIL_NEW_WB_CONNECT_DATE = StringParser.ReadAsInt64(source, 79 - 1, 8);
            //05 WL-OIL-NEW-WB-CONNECT-CCYY.
            //15 WL-OIL-NEW-WB-CONNECT-CC PIC 9(02).
            //15 WL-OIL-NEW-WB-CONNECT-YEAR PIC 9(02).
            //05 WL-OIL-NEW-WB-CONNECT-CENTURY REDEFINES
            //WL-OIL-NEW-WB-CONNECT-CCYY PIC 9(04).
            //05 WL-OIL-NEW-WB-CONNECT-MONTH PIC 9(02).
            //05 WL-OIL-NEW-WB-CONNECT-DAY PIC 9(02).
            WL_OIL_SUBJECT_TO_14B2_FLAG    = StringParser.ReadAsChar(source, 87 - 1, 1);
            WL_OIL_UIC_VIOLATION_FLAG      = StringParser.ReadAsChar(source, 88 - 1, 1);
            WL_OIL_LEGAL_VIOLATION_FLAG    = StringParser.ReadAsChar(source, 89 - 1, 1);
            WL_OIL_EXT_FOR_STATE_PLUG_FLAG = StringParser.ReadAsChar(source, 90 - 1, 1);
            WL_OIL_CERT_HB_1975_FLAG       = StringParser.ReadAsChar(source, 91 - 1, 1);
            //03 WL-OIL-CERT-EFFECTIVE-DATE.
            WL_OIL_CERT_EFFECTIVE_CC = StringParser.ReadAsInt16(source, 92 - 1, 2);
            WL_OIL_CERT_EFFECTIVE_YY = StringParser.ReadAsInt16(source, 94 - 1, 2);
            WL_OIL_CERT_EFFECTIVE_MM = StringParser.ReadAsInt16(source, 96 - 1, 2);
            //03 WL-OIL-CERTIFICATION-LTR-DATE.
            WL_OIL_CERTIFICATION_LTR_CC = StringParser.ReadAsInt16(source, 98  - 1, 2);
            WL_OIL_CERTIFICATION_LTR_YY = StringParser.ReadAsInt16(source, 100 - 1, 2);
            WL_OIL_CERTIFICATION_LTR_MM = StringParser.ReadAsInt16(source, 102 - 1, 2);
            WL_OIL_CERTIFICATION_LTR_DD = StringParser.ReadAsInt16(source, 104 - 1, 2);
            //03 WL-OIL-SHUT-IN-DATE.
            WL_OIL_SHUT_IN_CCYY           = StringParser.ReadAsInt32(source, 106 - 1, 4);
            WL_OIL_SHUT_IN_MM             = StringParser.ReadAsInt16(source, 110 - 1, 2);
            WL_OIL_PEND_REMOVAL_14B2_FLAG = StringParser.ReadAsChar(source, 112  - 1, 1);
        }
    }
}
