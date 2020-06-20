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
    [XmlRoot(nameof(GasReportingCycleSegment))]
    [Table(nameof(GasReportingCycleSegment))]
    public class GasReportingCycleSegment
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

        //02 WL-GAS-REPORTING-CYCLE-INFO.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_REPORT_CYCLE_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_GAS_REPORT_CYCLE_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-ALLOWABLE-EFFECT-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOW_EFFECT_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_GAS_ALLOW_EFFECT_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOW_EFFECT_MON), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_ALLOW_EFFECT_MON
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOW_EFFECT_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_ALLOW_EFFECT_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-ALLOWABLE-ISSUE-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOW_ISSUE_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_GAS_ALLOW_ISSUE_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOW_ISSUE_MON), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_ALLOW_ISSUE_MON
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOW_ISSUE_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_ALLOW_ISSUE_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_TYPE_WELLS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GAS_TYPE_WELLS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOWABLE_CYCLE_AMT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_ALLOWABLE_CYCLE_AMT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOWABLE_DAILY_AMT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_ALLOWABLE_DAILY_AMT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOWABLE_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GAS_ALLOWABLE_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_WORD_ALLOWABLE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GAS_WORD_ALLOWABLE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ASSIGNED_ALLOW_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GAS_ASSIGNED_ALLOW_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ASSIGNED_ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_ASSIGNED_ALLOW
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ADJUSTED_ALLOW_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GAS_ADJUSTED_ALLOW_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ADJUSTED_ALLOW_AMT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_ADJUSTED_ALLOW_AMT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ADMINISTRATIVE_ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_ADMINISTRATIVE_ALLOW
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_SPECIAL_ALLOW_DAILY_AMT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_SPECIAL_ALLOW_DAILY_AMT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_SPECIAL_ALLOW_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_SPECIAL_ALLOW_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_LIQUID_ALLOW_AMT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_LIQUID_ALLOW_AMT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_LIQUID_ALLOW_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_LIQUID_ALLOW_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_49B_DAILY_RATE_AMT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_49B_DAILY_RATE_AMT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_PAST_PROD_CHANGED_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_PAST_PROD_CHANGED_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_HIGH_DAILY_CHNGD_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_HIGH_DAILY_CHNGD_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_IS_A_PF_WELL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_IS_A_PF_WELL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_K2_SPECIAL_ALLOW_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_K2_SPECIAL_ALLOW_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_H15_VIOLATION_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_H15_VIOLATION_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_PERM_EXC_CYCS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GAS_PERM_EXC_CYCS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ACRES), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_ACRES
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ACRE_FEET), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? WL_GAS_ACRE_FEET
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_CAL_DEL_POTE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_CAL_DEL_POTE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_BHP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_GAS_BHP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_SIWH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_GAS_SIWH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ROCK_PRESS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_GAS_ROCK_PRESS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-DELIV-LETTER-EFFECT.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_DELIV_LETTER_EFFECT_YR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_GAS_DELIV_LETTER_EFFECT_YR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_DELIV_LETTER_EFFECT_MO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_DELIV_LETTER_EFFECT_MO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_DELIV_LETTER_EFFECT_DA), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_DELIV_LETTER_EFFECT_DA
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_DELIVERABILITY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_DELIVERABILITY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_TOP_SCHEDULE_ALLOWABLE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_TOP_SCHEDULE_ALLOWABLE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_EXC_TO_G10_TEST_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_EXC_TO_G10_TEST_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ALLOWABLE_REMARKS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_GAS_ALLOWABLE_REMARKS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_AUTO_SPECIAL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_AUTO_SPECIAL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_NO_SUPPLEMENT_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_NO_SUPPLEMENT_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-HIGHEST-DAILY-AVG-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_HIGHEST_DAILY_AVG_CC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_HIGHEST_DAILY_AVG_CC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_HIGHEST_DAILY_AVG_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_HIGHEST_DAILY_AVG_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_HIGHEST_DAILY_AVG_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_HIGHEST_DAILY_AVG_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_HIGHEST_DAILY_AVERAGE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_HIGHEST_DAILY_AVERAGE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-12-MONTH-PEAK-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_12_MONTH_PEAK_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_GAS_12_MONTH_PEAK_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_12_MONTH_PEAK_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_12_MONTH_PEAK_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-PAST-PRODUCTION-DATE REDEFINES

        //WL-GAS-12-MONTH-PEAK-DATE.

        //05 WL-GAS-PAST-PRODUCTION-CC PIC 9(02).

        //05 WL-GAS-PAST-PRODUCTION-YEAR PIC 9(02).

        //05 WL-GAS-PAST-PRODUCTION-MONTH PIC 9(02).

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_12_MONTH_PEAK), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_GAS_12_MONTH_PEAK
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-PAST-PRODUCTION REDEFINES

        //WL-GAS-12-MONTH-PEAK PIC S9(09) COMP-3.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_G10_BHP_EXCEPT_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public byte? WL_GAS_G10_BHP_EXCEPT_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_G10_SIP_EXCEPT_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public byte? WL_GAS_G10_SIP_EXCEPT_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_DPT_CODE_EXCEPT_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public byte? WL_GAS_DPT_CODE_EXCEPT_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_14B2_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_14B2_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-14B2-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_14B2_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_GAS_14B2_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_14B2_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_14B2_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_BH_TEMPERATURE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_GAS_BH_TEMPERATURE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_FIELD_DELIVERABILITY_SHARE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_FIELD_DELIVERABILITY_SHARE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-LEASE-PERCENT-RESERVES REDEFINES

        //WL-FIELD-DELIVERABILITY-SHARE PIC S9(5)V9(4) COMP-3.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_PANHANDLE_COMP_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_PANHANDLE_COMP_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_ADMIN_ALLOW_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_ADMIN_ALLOW_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-TEST-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_TEST_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_GAS_TEST_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_TEST_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_TEST_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_TEST_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_GAS_TEST_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_TMP_MONTHLY_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_TMP_MONTHLY_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_RECOMMENCED_WELL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_RECOMMENCED_WELL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_PERMANENT_GAS_WELL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_PERMANENT_GAS_WELL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-GAS-PREV-DELIV PIC 9(9) COMP-3. 218

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_USING_TMP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_USING_TMP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_STACK_LAT_PARENTAGE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_GAS_STACK_LAT_PARENTAGE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public GasFormsLackingSegment? GasFormsLackingSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public GasAllowableTransfersSegment? GasAllowableTransfersSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public GasPreviousAllowableSegment? GasPreviousAllowableSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public GasScheduleUICSegment? GasScheduleUICSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public GasReportingCycleSegment()
        {
        }

        public GasReportingCycleSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-GAS-REPORTING-CYCLE-INFO.
            WL_GAS_REPORT_CYCLE_DATE = StringParser.ReadAsInt64(source, 3 - 1, 6);
            //03 WL-GAS-ALLOWABLE-EFFECT-DATE.
            WL_GAS_ALLOW_EFFECT_YEAR = StringParser.ReadAsInt32(source, 9  - 1, 4);
            WL_GAS_ALLOW_EFFECT_MON  = StringParser.ReadAsInt16(source, 13 - 1, 2);
            WL_GAS_ALLOW_EFFECT_DAY  = StringParser.ReadAsInt16(source, 15 - 1, 2);
            //03 WL-GAS-ALLOWABLE-ISSUE-DATE.
            WL_GAS_ALLOW_ISSUE_YEAR        = StringParser.ReadAsInt32(source, 17            - 1, 4);
            WL_GAS_ALLOW_ISSUE_MON         = StringParser.ReadAsInt16(source, 21            - 1, 2);
            WL_GAS_ALLOW_ISSUE_DAY         = StringParser.ReadAsInt16(source, 23            - 1, 2);
            WL_GAS_TYPE_WELLS              = StringParser.ReadAsString(source, 25           - 1, 2);
            WL_GAS_ALLOWABLE_CYCLE_AMT     = StringParser.ReadAsPackedDouble(source, 0, 27  - 1, 5);
            WL_GAS_ALLOWABLE_DAILY_AMT     = StringParser.ReadAsPackedDouble(source, 0, 32  - 1, 5);
            WL_GAS_ALLOWABLE_CODE          = StringParser.ReadAsString(source, 37           - 1, 2);
            WL_GAS_WORD_ALLOWABLE          = StringParser.ReadAsString(source, 39           - 1, 8);
            WL_GAS_ASSIGNED_ALLOW_CODE     = StringParser.ReadAsString(source, 47           - 1, 2);
            WL_GAS_ASSIGNED_ALLOW          = StringParser.ReadAsPackedDouble(source, 0, 49  - 1, 5);
            WL_GAS_ADJUSTED_ALLOW_CODE     = StringParser.ReadAsString(source, 54           - 1, 2);
            WL_GAS_ADJUSTED_ALLOW_AMT      = StringParser.ReadAsPackedDouble(source, 0, 56  - 1, 5);
            WL_GAS_ADMINISTRATIVE_ALLOW    = StringParser.ReadAsPackedDouble(source, 0, 61  - 1, 5);
            WL_GAS_SPECIAL_ALLOW_DAILY_AMT = StringParser.ReadAsPackedDouble(source, 0, 66  - 1, 5);
            WL_GAS_SPECIAL_ALLOW_CODE      = StringParser.ReadAsChar(source, 71             - 1, 1);
            WL_GAS_LIQUID_ALLOW_AMT        = StringParser.ReadAsPackedDouble(source, 0, 72  - 1, 5);
            WL_GAS_LIQUID_ALLOW_CODE       = StringParser.ReadAsChar(source, 77             - 1, 1);
            WL_GAS_49B_DAILY_RATE_AMT      = StringParser.ReadAsPackedDouble(source, 0, 78  - 1, 5);
            WL_GAS_PAST_PROD_CHANGED_FLAG  = StringParser.ReadAsChar(source, 83             - 1, 1);
            WL_GAS_HIGH_DAILY_CHNGD_FLAG   = StringParser.ReadAsChar(source, 84             - 1, 1);
            WL_GAS_IS_A_PF_WELL_FLAG       = StringParser.ReadAsChar(source, 85             - 1, 1);
            WL_K2_SPECIAL_ALLOW_FLAG       = StringParser.ReadAsChar(source, 86             - 1, 1);
            WL_GAS_H15_VIOLATION_FLAG      = StringParser.ReadAsChar(source, 87             - 1, 1);
            WL_GAS_PERM_EXC_CYCS           = StringParser.ReadAsString(source, 88           - 1, 2);
            WL_GAS_ACRES                   = StringParser.ReadAsPackedDouble(source, 4, 90  - 1, 6);
            WL_GAS_ACRE_FEET               = StringParser.ReadAsPackedSingle(source, 1, 96  - 1, 4);
            WL_GAS_CAL_DEL_POTE            = StringParser.ReadAsPackedDouble(source, 0, 100 - 1, 5);
            WL_GAS_BHP                     = StringParser.ReadAsPackedInt64(source, 105     - 1, 3);
            WL_GAS_SIWH                    = StringParser.ReadAsPackedInt64(source, 108     - 1, 4);
            WL_GAS_ROCK_PRESS              = StringParser.ReadAsPackedInt64(source, 112     - 1, 3);
            //03 WL-GAS-DELIV-LETTER-EFFECT.
            WL_GAS_DELIV_LETTER_EFFECT_YR = StringParser.ReadAsInt32(source, 115           - 1, 4);
            WL_GAS_DELIV_LETTER_EFFECT_MO = StringParser.ReadAsInt16(source, 119           - 1, 2);
            WL_GAS_DELIV_LETTER_EFFECT_DA = StringParser.ReadAsInt16(source, 121           - 1, 2);
            WL_GAS_DELIVERABILITY         = StringParser.ReadAsPackedDouble(source, 0, 123 - 1, 5);
            WL_GAS_TOP_SCHEDULE_ALLOWABLE = StringParser.ReadAsPackedDouble(source, 0, 128 - 1, 5);
            WL_GAS_EXC_TO_G10_TEST_FLAG   = StringParser.ReadAsChar(source, 133            - 1, 1);
            WL_GAS_ALLOWABLE_REMARKS      = StringParser.ReadAsString(source, 134          - 1, 33);
            WL_GAS_AUTO_SPECIAL_FLAG      = StringParser.ReadAsChar(source, 167            - 1, 1);
            WL_GAS_NO_SUPPLEMENT_FLAG     = StringParser.ReadAsChar(source, 168            - 1, 1);
            //03 WL-GAS-HIGHEST-DAILY-AVG-DATE.
            WL_GAS_HIGHEST_DAILY_AVG_CC    = StringParser.ReadAsInt16(source, 169           - 1, 2);
            WL_GAS_HIGHEST_DAILY_AVG_YEAR  = StringParser.ReadAsInt16(source, 171           - 1, 2);
            WL_GAS_HIGHEST_DAILY_AVG_MONTH = StringParser.ReadAsInt16(source, 173           - 1, 2);
            WL_GAS_HIGHEST_DAILY_AVERAGE   = StringParser.ReadAsPackedDouble(source, 0, 175 - 1, 5);
            //03 WL-GAS-12-MONTH-PEAK-DATE.
            WL_GAS_12_MONTH_PEAK_YEAR  = StringParser.ReadAsInt32(source, 180 - 1, 4);
            WL_GAS_12_MONTH_PEAK_MONTH = StringParser.ReadAsInt16(source, 184 - 1, 2);
            //03 WL-GAS-PAST-PRODUCTION-DATE REDEFINES
            //WL-GAS-12-MONTH-PEAK-DATE.
            //05 WL-GAS-PAST-PRODUCTION-CC PIC 9(02).
            //05 WL-GAS-PAST-PRODUCTION-YEAR PIC 9(02).
            //05 WL-GAS-PAST-PRODUCTION-MONTH PIC 9(02).
            WL_GAS_12_MONTH_PEAK = StringParser.ReadAsPackedDouble(source, 0, 186 - 1, 3);
            //03 WL-GAS-PAST-PRODUCTION REDEFINES
            //WL-GAS-12-MONTH-PEAK PIC S9(09) COMP-3.
            WL_GAS_G10_BHP_EXCEPT_FLAG  = StringParser.ReadAsByte(source, 189 - 1, 1);
            WL_GAS_G10_SIP_EXCEPT_FLAG  = StringParser.ReadAsByte(source, 190 - 1, 1);
            WL_GAS_DPT_CODE_EXCEPT_FLAG = StringParser.ReadAsByte(source, 191 - 1, 1);
            WL_GAS_14B2_CODE            = StringParser.ReadAsChar(source, 192  - 1, 1);
            //03 WL-GAS-14B2-DATE.
            WL_GAS_14B2_YEAR              = StringParser.ReadAsInt32(source, 193 - 1, 4);
            WL_GAS_14B2_MONTH             = StringParser.ReadAsInt16(source, 197 - 1, 2);
            WL_GAS_BH_TEMPERATURE         = StringParser.ReadAsInt32(source, 199 - 1, 3);
            WL_FIELD_DELIVERABILITY_SHARE = StringParser.ReadAsInt32(source, 202 - 1, 3);
            //03 WL-LEASE-PERCENT-RESERVES REDEFINES
            //WL-FIELD-DELIVERABILITY-SHARE PIC S9(5)V9(4) COMP-3.
            WL_GAS_PANHANDLE_COMP_CODE = StringParser.ReadAsChar(source, 205 - 1, 1);
            WL_GAS_ADMIN_ALLOW_FLAG    = StringParser.ReadAsChar(source, 206 - 1, 1);
            //03 WL-GAS-TEST-DATE.
            WL_GAS_TEST_YEAR           = StringParser.ReadAsInt32(source, 207 - 1, 4);
            WL_GAS_TEST_MONTH          = StringParser.ReadAsInt16(source, 211 - 1, 2);
            WL_GAS_TEST_DAY            = StringParser.ReadAsInt16(source, 213 - 1, 2);
            WL_TMP_MONTHLY_FLAG        = StringParser.ReadAsChar(source, 215  - 1, 1);
            WL_RECOMMENCED_WELL_FLAG   = StringParser.ReadAsChar(source, 216  - 1, 1);
            WL_PERMANENT_GAS_WELL_FLAG = StringParser.ReadAsChar(source, 217  - 1, 6);
            //03 WL-GAS-PREV-DELIV PIC 9(9) COMP-3. 218
            WL_GAS_USING_TMP           = StringParser.ReadAsChar(source, 223 - 1, 1);
            WL_GAS_STACK_LAT_PARENTAGE = StringParser.ReadAsChar(source, 224 - 1, 1);
        }
    }
}
