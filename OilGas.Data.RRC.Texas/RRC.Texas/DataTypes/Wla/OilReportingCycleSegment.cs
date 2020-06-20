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
    [XmlRoot(nameof(OilReportingCycleSegment))]
    [Table(nameof(OilReportingCycleSegment))]
    public class OilReportingCycleSegment
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

        //02 WL-OIL-REPORTING-CYCLE-INFO.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_REPORT_CYCLE_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_REPORT_CYCLE_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ACRES), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? WL_OIL_ACRES
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ACRE_FEET), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_ACRE_FEET
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_NET_ACRE_FEET), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_NET_ACRE_FEET
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_BHP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_BHP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_WORKING_POTENTIAL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? WL_OIL_WORKING_POTENTIAL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_TOLERANCE_ACRES_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_TOLERANCE_ACRES_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-OIL-EFFECTIVE-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_EFFECTIVE_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_EFFECTIVE_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_EFFECTIVE_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_EFFECTIVE_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_EFFECTIVE_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_EFFECTIVE_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-OIL-ISSUE-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ISSUE_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_ISSUE_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ISSUE_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_ISSUE_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ISSUE_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_ISSUE_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //*NOTE: WL-OIL-ALLOWABLE-AMOUNT IS THE DAILY AMOUNT BEFORE

        //* PRODUCTION FACTOR HAS BEEN APPLIED - SEE NOTE FOR

        //* WL-OIL-WELL-MONTHLY-ALLOWABLE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ALLOWABLE_AMOUNT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_ALLOWABLE_AMOUNT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_LIMIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_GAS_LIMIT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_GAS_OIL_RATIO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_GAS_OIL_RATIO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_REPORT_CYCLE_REMARK), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_OIL_REPORT_CYCLE_REMARK
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_ORIGINAL_OIL_IN_PLACE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_ORIGINAL_OIL_IN_PLACE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ALLOWABLE_CODES_RPT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_OIL_ALLOWABLE_CODES_RPT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //'01', '03', '06', '07', '08'.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ALLOW_LIMITED_CD_RPT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_ALLOW_LIMITED_CD_RPT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_TYPE_WELLS_RPT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_OIL_TYPE_WELLS_RPT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_SAND_THICKNESS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_SAND_THICKNESS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CO_REG_DEPTH_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public byte? WL_OIL_CO_REG_DEPTH_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_NET_GOR_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_NET_GOR_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_NET_GOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_NET_GOR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_SPECIAL_ALLOW_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public byte? WL_OIL_SPECIAL_ALLOW_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_SPECIAL_ALLOW_AMT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_SPECIAL_ALLOW_AMT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_14B2_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_14B2_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-OIL-14B2-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_14B2_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_14B2_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_14B2_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_14B2_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_WATER_PRODUCTION), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_WATER_PRODUCTION
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_FROZEN_POTENTIAL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_FROZEN_POTENTIAL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_PRODUCING_METHOD_RPT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_PRODUCING_METHOD_RPT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_OTHER_PROD_METHOD_RPT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WL_OIL_OTHER_PROD_METHOD_RPT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_WELL_MONTHLY_ALLOWABLE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_WELL_MONTHLY_ALLOWABLE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_LOWEST_PERFORATION), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_LOWEST_PERFORATION
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_W10_EXC_TO_TEST_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_W10_EXC_TO_TEST_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        //03 WL-OIL-TEST-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_TEST_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_TEST_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_TEST_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_TEST_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_TEST_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WL_OIL_TEST_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_RETEST_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_RETEST_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_12_HOUR_POTENTIAL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_12_HOUR_POTENTIAL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_BONUS_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_BONUS_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_BONUS_AMOUNT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WL_OIL_BONUS_AMOUNT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_EB_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_EB_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ALLOW_POTE_GOR_CHG_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_ALLOW_POTE_GOR_CHG_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_CSH_MONTHLY_LIMIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WL_OIL_CSH_MONTHLY_LIMIT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_EB_CHANGE_MONTH_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public byte? WL_OIL_EB_CHANGE_MONTH_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_WELL_TOP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WL_OIL_WELL_TOP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_PANHANDLE_COMP_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_PANHANDLE_COMP_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_PANHANDLE_SUB_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_PANHANDLE_SUB_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_H15_VIOLATION_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_H15_VIOLATION_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_ALLOCATION_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_ALLOCATION_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WL_OIL_STACK_LAT_PARENTAGE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WL_OIL_STACK_LAT_PARENTAGE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public OilPreviousAllowableSegment? OilPreviousAllowableSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public OilFormsLackingSegment? OilFormsLackingSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public OilEastTexasSegment? OilEastTexasSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public OilAllowableTransfersSegment? OilAllowableTransfersSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public OilPanhandleGasProductionSegment? OilPanhandleGasProductionSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public OilScheduleUICSegment? OilScheduleUICSegment
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OilReportingCycleSegment()
        {
        }

        public OilReportingCycleSegment(ReadOnlySpan<byte> source)
        {
            //02 WL-OIL-REPORTING-CYCLE-INFO.
            WL_OIL_REPORT_CYCLE_DATE    = StringParser.ReadAsInt64(source, 3            - 1, 6);
            WL_OIL_ACRES                = StringParser.ReadAsPackedSingle(source, 2, 9  - 1, 4);
            WL_OIL_ACRE_FEET            = StringParser.ReadAsPackedInt64(source, 13     - 1, 3);
            WL_OIL_NET_ACRE_FEET        = StringParser.ReadAsPackedInt64(source, 16     - 1, 3);
            WL_OIL_BHP                  = StringParser.ReadAsPackedInt64(source, 19     - 1, 3);
            WL_OIL_WORKING_POTENTIAL    = StringParser.ReadAsPackedSingle(source, 1, 22 - 1, 4);
            WL_OIL_TOLERANCE_ACRES_FLAG = StringParser.ReadAsChar(source, 26            - 1, 1);
            //03 WL-OIL-EFFECTIVE-DATE.
            WL_OIL_EFFECTIVE_YEAR  = StringParser.ReadAsInt32(source, 27 - 1, 4);
            WL_OIL_EFFECTIVE_MONTH = StringParser.ReadAsInt16(source, 31 - 1, 2);
            WL_OIL_EFFECTIVE_DAY   = StringParser.ReadAsInt16(source, 33 - 1, 2);
            //03 WL-OIL-ISSUE-DATE.
            WL_OIL_ISSUE_YEAR  = StringParser.ReadAsInt32(source, 35 - 1, 4);
            WL_OIL_ISSUE_MONTH = StringParser.ReadAsInt16(source, 39 - 1, 2);
            WL_OIL_ISSUE_DAY   = StringParser.ReadAsInt16(source, 41 - 1, 2);
            //*NOTE: WL-OIL-ALLOWABLE-AMOUNT IS THE DAILY AMOUNT BEFORE
            //* PRODUCTION FACTOR HAS BEEN APPLIED - SEE NOTE FOR
            //* WL-OIL-WELL-MONTHLY-ALLOWABLE.
            WL_OIL_ALLOWABLE_AMOUNT    = StringParser.ReadAsPackedInt64(source, 43 - 1, 3);
            WL_GAS_LIMIT               = StringParser.ReadAsPackedInt64(source, 46 - 1, 4);
            WL_GAS_OIL_RATIO           = StringParser.ReadAsPackedInt64(source, 50 - 1, 3);
            WL_OIL_REPORT_CYCLE_REMARK = StringParser.ReadAsString(source, 53      - 1, 30);
            WL_ORIGINAL_OIL_IN_PLACE   = StringParser.ReadAsPackedInt64(source, 83 - 1, 4);
            WL_OIL_ALLOWABLE_CODES_RPT = StringParser.ReadAsString(source, 87      - 1, 2);
            //'01', '03', '06', '07', '08'.
            WL_OIL_ALLOW_LIMITED_CD_RPT = StringParser.ReadAsChar(source, 89         - 1, 1);
            WL_OIL_TYPE_WELLS_RPT       = StringParser.ReadAsString(source, 90       - 1, 2);
            WL_OIL_SAND_THICKNESS       = StringParser.ReadAsInt32(source, 92        - 1, 3);
            WL_OIL_CO_REG_DEPTH_CODE    = StringParser.ReadAsByte(source, 95        - 1, 1);
            WL_OIL_NET_GOR_CODE         = StringParser.ReadAsChar(source, 96         - 1, 1);
            WL_OIL_NET_GOR              = StringParser.ReadAsPackedInt64(source, 97  - 1, 3);
            WL_OIL_SPECIAL_ALLOW_CODE   = StringParser.ReadAsByte(source, 100       - 1, 1);
            WL_OIL_SPECIAL_ALLOW_AMT    = StringParser.ReadAsPackedInt32(source, 101 - 1, 3);
            WL_OIL_14B2_CODE            = StringParser.ReadAsChar(source, 104        - 1, 1);
            //03 WL-OIL-14B2-DATE.
            WL_OIL_14B2_YEAR              = StringParser.ReadAsInt32(source, 105       - 1, 4);
            WL_OIL_14B2_MONTH             = StringParser.ReadAsInt16(source, 109       - 1, 2);
            WL_OIL_WATER_PRODUCTION       = StringParser.ReadAsPackedInt32(source, 111 - 1, 3);
            WL_OIL_FROZEN_POTENTIAL       = StringParser.ReadAsPackedInt64(source, 114 - 1, 3);
            WL_OIL_PRODUCING_METHOD_RPT   = StringParser.ReadAsChar(source, 117        - 1, 1);
            WL_OIL_OTHER_PROD_METHOD_RPT  = StringParser.ReadAsString(source, 118      - 1, 20);
            WL_OIL_WELL_MONTHLY_ALLOWABLE = StringParser.ReadAsPackedInt64(source, 138 - 1, 4);
            WL_OIL_LOWEST_PERFORATION     = StringParser.ReadAsPackedInt64(source, 142 - 1, 3);
            WL_OIL_W10_EXC_TO_TEST_FLAG   = StringParser.ReadAsChar(source, 146        - 1, 1);
            //03 WL-OIL-TEST-DATE.
            WL_OIL_TEST_YEAR               = StringParser.ReadAsInt32(source, 147           - 1, 4);
            WL_OIL_TEST_MONTH              = StringParser.ReadAsInt16(source, 151           - 1, 2);
            WL_OIL_TEST_DAY                = StringParser.ReadAsInt16(source, 153           - 1, 2);
            WL_OIL_RETEST_FLAG             = StringParser.ReadAsChar(source, 155            - 1, 1);
            WL_OIL_12_HOUR_POTENTIAL       = StringParser.ReadAsPackedInt64(source, 156     - 1, 3);
            WL_OIL_BONUS_CODE              = StringParser.ReadAsChar(source, 159            - 1, 1);
            WL_OIL_BONUS_AMOUNT            = StringParser.ReadAsInt32(source, 160           - 1, 3);
            WL_OIL_EB_FLAG                 = StringParser.ReadAsChar(source, 163            - 1, 1);
            WL_OIL_ALLOW_POTE_GOR_CHG_FLAG = StringParser.ReadAsChar(source, 164            - 1, 1);
            WL_OIL_CSH_MONTHLY_LIMIT       = StringParser.ReadAsPackedDouble(source, 0, 165 - 1, 5);
            WL_OIL_EB_CHANGE_MONTH_FLAG    = StringParser.ReadAsByte(source, 170           - 1, 1);
            WL_OIL_WELL_TOP                = StringParser.ReadAsPackedInt64(source, 171     - 1, 4);
            WL_OIL_PANHANDLE_COMP_CODE     = StringParser.ReadAsChar(source, 175            - 1, 1);
            WL_OIL_PANHANDLE_SUB_CODE      = StringParser.ReadAsChar(source, 176            - 1, 1);
            WL_OIL_H15_VIOLATION_FLAG      = StringParser.ReadAsChar(source, 177            - 1, 1);
            WL_OIL_ALLOCATION_FLAG         = StringParser.ReadAsChar(source, 178            - 1, 1);
            WL_OIL_STACK_LAT_PARENTAGE     = StringParser.ReadAsChar(source, 179            - 1, 1);
        }
    }
}
