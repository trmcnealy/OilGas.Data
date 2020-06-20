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
    [XmlRoot(nameof(WellBore14B2Well))]
    [Table(nameof(WellBore14B2Well))]
    public class WellBore14B2Well
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
        //02 WB14B2-WELL-SEGMENT.

        //05 WB14B2-OIL-KEY.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_OIL_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_OIL_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_OIL_DISTRICT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_OIL_DISTRICT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_OIL_LEASE_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB14B2_OIL_LEASE_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_OIL_WELL_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB14B2_OIL_WELL_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB14B2-GAS-KEY REDEFINES WB14B2-OIL-KEY.

        //10 WB14B2-GAS-CODE PIC X(01).

        //10 WB14B2-GAS-RRC-ID PIC 9(06).

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_APPLICATION_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB14B2_APPLICATION_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_GAS_DISTRICT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_GAS_DISTRICT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_STATUS_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_EXT_STATUS_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_CANCELLED_REASON), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_EXT_CANCELLED_REASON
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB14B2-EXT-APPROVED-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_APPROVED_CENT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_EXT_APPROVED_CENT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_APPROVED_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_EXT_APPROVED_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_APPROVED_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_EXT_APPROVED_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_APPROVED_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_EXT_APPROVED_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB14B2-EXT-EXP-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_EXP_CENT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_EXT_EXP_CENT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_EXP_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_EXT_EXP_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_EXP_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_EXT_EXP_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_EXP_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_EXT_EXP_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB14B2-EXT-DENIED-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_DENIED_CENT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_EXT_DENIED_CENT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_DENIED_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_EXT_DENIED_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_DENIED_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_EXT_DENIED_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_DENIED_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_EXT_DENIED_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB14B2-EXT-HIST-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_HIST_CENT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_EXT_HIST_CENT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_HIST_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_EXT_HIST_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_HIST_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_EXT_HIST_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_EXT_HIST_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_EXT_HIST_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB14B2-WELL-VIOLATIONS.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_MECH_INTEG_VIOL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_MECH_INTEG_VIOL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_PLUG_ORDER_SF_HOLD_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_PLUG_ORDER_SF_HOLD_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_POLLUTION_VIOL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_POLLUTION_VIOL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_FIELD_OPS_HOLD_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_FIELD_OPS_HOLD_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_H15_PROBLEM_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_H15_PROBLEM_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_H15_NOT_FILED_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_H15_NOT_FILED_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_OPER_DELQ_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_OPER_DELQ_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_DISTRICT_HOLD_SFP_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_DISTRICT_HOLD_SFP_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_DIST_SF_CLEAN_UP_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_DIST_SF_CLEAN_UP_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_DIST_STATE_PLUG_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_DIST_STATE_PLUG_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_GOOD_FAITH_VIOL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_GOOD_FAITH_VIOL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_WELL_OTHER_VIOL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_WELL_OTHER_VIOL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_W3C_SURF_EQP_VIOL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_W3C_SURF_EQP_VIOL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_W3X_VIOL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_W3X_VIOL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB14B2-HB2259-W3X-OPTIONS.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_HB2259_W3X_PUB_ENT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_HB2259_W3X_PUB_ENT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_HB2259_W3X_10PCT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_HB2259_W3X_10PCT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_HB2259_W3X_BONDING), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_HB2259_W3X_BONDING
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_HB2259_W3X_H13_EOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_HB2259_W3X_H13_EOR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_HB2259_W3X_AOP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_HB2259_W3X_AOP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_HB2259_W3X_MIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_HB2259_W3X_MIT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_HB2259_W3X_ESCROW), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_HB2259_W3X_ESCROW
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_W3X_FILING_KEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB14B2_W3X_FILING_KEY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_W3X_AOP_RECEIVED_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB14B2_W3X_AOP_RECEIVED_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_W3X_AOP_FEE_RCVD_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB14B2_W3X_AOP_FEE_RCVD_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_W3X_H15_FEE_RCVD_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB14B2_W3X_H15_FEE_RCVD_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB14B2-W3X-ESCROW-FUNDS PIC 9(05)V99. 119

        //05 WB14B2-W3X-ESCROW-FUND-SPLIT REDEFINES WB14B2-W3X-ESCROW-FUNDS.

        //07 WB14B2-W3X-ESCROW-FUND-WHOLE PIC 9(05).

        //07 WB14B2-W3X-ESCROW-FUND-DECIMAL PIC 9(02).

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_60_DAY_LETTER_SENT_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_60_DAY_LETTER_SENT_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_W1X_36_NEEDS_BOND_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_W1X_36_NEEDS_BOND_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_W1X_36_TYPE_COVERAGE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB14B2_W1X_36_TYPE_COVERAGE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_W1X_36_AMT_FILED), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB14B2_W1X_36_AMT_FILED
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_W1X_36_SURETY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB14B2_W1X_36_SURETY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB14B2-W1X-36-EXP-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_W1X_36_EXP_CENT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_W1X_36_EXP_CENT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_W1X_36_EXP_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_W1X_36_EXP_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_W1X_36_EXP_MON), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_W1X_36_EXP_MON
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_W1X_36_EXP_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB14B2_W1X_36_EXP_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB14B2_W1X_36_BOND_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB14B2_W1X_36_BOND_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBore14B2Remarks>? WellBore14B2Remarks
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBore14B2Remarks>();

        public WellBore14B2Well()
        {
        }

        public WellBore14B2Well(ReadOnlySpan<byte> source)
        {
            //02 WB14B2-WELL-SEGMENT.
            //05 WB14B2-OIL-KEY.
            WB14B2_OIL_CODE         = StringParser.ReadAsChar(source, 3    - 1, 1);
            WB14B2_OIL_DISTRICT     = StringParser.ReadAsInt16(source, 4   - 1, 2);
            WB14B2_OIL_LEASE_NUMBER = StringParser.ReadAsInt64(source, 6   - 1, 5);
            WB14B2_OIL_WELL_NUMBER  = StringParser.ReadAsString(source, 11 - 1, 6);
            //05 WB14B2-GAS-KEY REDEFINES WB14B2-OIL-KEY.
            //10 WB14B2-GAS-CODE PIC X(01).
            //10 WB14B2-GAS-RRC-ID PIC 9(06).
            WB14B2_APPLICATION_NUMBER   = StringParser.ReadAsInt64(source, 17 - 1, 6);
            WB14B2_GAS_DISTRICT         = StringParser.ReadAsInt16(source, 23 - 1, 2);
            WB14B2_EXT_STATUS_FLAG      = StringParser.ReadAsChar(source, 25  - 1, 1);
            WB14B2_EXT_CANCELLED_REASON = StringParser.ReadAsChar(source, 26  - 1, 1);
            //05 WB14B2-EXT-APPROVED-DATE.
            WB14B2_EXT_APPROVED_CENT  = StringParser.ReadAsInt16(source, 27 - 1, 2);
            WB14B2_EXT_APPROVED_YEAR  = StringParser.ReadAsInt16(source, 29 - 1, 2);
            WB14B2_EXT_APPROVED_MONTH = StringParser.ReadAsInt16(source, 31 - 1, 2);
            WB14B2_EXT_APPROVED_DAY   = StringParser.ReadAsInt16(source, 33 - 1, 2);
            //05 WB14B2-EXT-EXP-DATE.
            WB14B2_EXT_EXP_CENT  = StringParser.ReadAsInt16(source, 35 - 1, 2);
            WB14B2_EXT_EXP_YEAR  = StringParser.ReadAsInt16(source, 37 - 1, 2);
            WB14B2_EXT_EXP_MONTH = StringParser.ReadAsInt16(source, 39 - 1, 2);
            WB14B2_EXT_EXP_DAY   = StringParser.ReadAsInt16(source, 41 - 1, 2);
            //05 WB14B2-EXT-DENIED-DATE.
            WB14B2_EXT_DENIED_CENT  = StringParser.ReadAsInt16(source, 43 - 1, 2);
            WB14B2_EXT_DENIED_YEAR  = StringParser.ReadAsInt16(source, 45 - 1, 2);
            WB14B2_EXT_DENIED_MONTH = StringParser.ReadAsInt16(source, 47 - 1, 2);
            WB14B2_EXT_DENIED_DAY   = StringParser.ReadAsInt16(source, 49 - 1, 2);
            //05 WB14B2-EXT-HIST-DATE.
            WB14B2_EXT_HIST_CENT  = StringParser.ReadAsInt16(source, 51 - 1, 2);
            WB14B2_EXT_HIST_YEAR  = StringParser.ReadAsInt16(source, 53 - 1, 2);
            WB14B2_EXT_HIST_MONTH = StringParser.ReadAsInt16(source, 55 - 1, 2);
            WB14B2_EXT_HIST_DAY   = StringParser.ReadAsInt16(source, 57 - 1, 2);
            //05 WB14B2-WELL-VIOLATIONS.
            WB14B2_MECH_INTEG_VIOL_FLAG    = StringParser.ReadAsChar(source, 59 - 1, 1);
            WB14B2_PLUG_ORDER_SF_HOLD_FLAG = StringParser.ReadAsChar(source, 60 - 1, 1);
            WB14B2_POLLUTION_VIOL_FLAG     = StringParser.ReadAsChar(source, 61 - 1, 1);
            WB14B2_FIELD_OPS_HOLD_FLAG     = StringParser.ReadAsChar(source, 62 - 1, 1);
            WB14B2_H15_PROBLEM_FLAG        = StringParser.ReadAsChar(source, 63 - 1, 1);
            WB14B2_H15_NOT_FILED_FLAG      = StringParser.ReadAsChar(source, 64 - 1, 1);
            WB14B2_OPER_DELQ_FLAG          = StringParser.ReadAsChar(source, 65 - 1, 1);
            WB14B2_DISTRICT_HOLD_SFP_FLAG  = StringParser.ReadAsChar(source, 66 - 1, 1);
            WB14B2_DIST_SF_CLEAN_UP_FLAG   = StringParser.ReadAsChar(source, 67 - 1, 1);
            WB14B2_DIST_STATE_PLUG_FLAG    = StringParser.ReadAsChar(source, 68 - 1, 1);
            WB14B2_GOOD_FAITH_VIOL_FLAG    = StringParser.ReadAsChar(source, 69 - 1, 1);
            WB14B2_WELL_OTHER_VIOL_FLAG    = StringParser.ReadAsChar(source, 70 - 1, 1);
            WB14B2_W3C_SURF_EQP_VIOL_FLAG  = StringParser.ReadAsChar(source, 71 - 1, 1);
            WB14B2_W3X_VIOL_FLAG           = StringParser.ReadAsChar(source, 72 - 1, 1);
            //05 WB14B2-HB2259-W3X-OPTIONS.
            WB14B2_HB2259_W3X_PUB_ENT    = StringParser.ReadAsChar(source, 80   - 1, 1);
            WB14B2_HB2259_W3X_10PCT      = StringParser.ReadAsChar(source, 81   - 1, 1);
            WB14B2_HB2259_W3X_BONDING    = StringParser.ReadAsChar(source, 82   - 1, 1);
            WB14B2_HB2259_W3X_H13_EOR    = StringParser.ReadAsChar(source, 83   - 1, 1);
            WB14B2_HB2259_W3X_AOP        = StringParser.ReadAsChar(source, 84   - 1, 1);
            WB14B2_HB2259_W3X_MIT        = StringParser.ReadAsChar(source, 85   - 1, 1);
            WB14B2_HB2259_W3X_ESCROW     = StringParser.ReadAsChar(source, 86   - 1, 1);
            WB14B2_W3X_FILING_KEY        = StringParser.ReadAsInt64(source, 87  - 1, 8);
            WB14B2_W3X_AOP_RECEIVED_DATE = StringParser.ReadAsInt64(source, 95  - 1, 8);
            WB14B2_W3X_AOP_FEE_RCVD_DATE = StringParser.ReadAsInt64(source, 103 - 1, 8);
            WB14B2_W3X_H15_FEE_RCVD_DATE = StringParser.ReadAsInt64(source, 111 - 1, 15);
            //05 WB14B2-W3X-ESCROW-FUNDS PIC 9(05)V99. 119
            //05 WB14B2-W3X-ESCROW-FUND-SPLIT REDEFINES WB14B2-W3X-ESCROW-FUNDS.
            //07 WB14B2-W3X-ESCROW-FUND-WHOLE PIC 9(05).
            //07 WB14B2-W3X-ESCROW-FUND-DECIMAL PIC 9(02).
            WB14B2_60_DAY_LETTER_SENT_FLAG = StringParser.ReadAsChar(source, 126  - 1, 1);
            WB14B2_W1X_36_NEEDS_BOND_FLAG  = StringParser.ReadAsChar(source, 127  - 1, 1);
            WB14B2_W1X_36_TYPE_COVERAGE    = StringParser.ReadAsChar(source, 128  - 1, 1);
            WB14B2_W1X_36_AMT_FILED        = StringParser.ReadAsInt64(source, 129 - 1, 9);
            WB14B2_W1X_36_SURETY           = StringParser.ReadAsInt64(source, 138 - 1, 5);
            //05 WB14B2-W1X-36-EXP-DATE.
            WB14B2_W1X_36_EXP_CENT    = StringParser.ReadAsInt16(source, 143  - 1, 2);
            WB14B2_W1X_36_EXP_YEAR    = StringParser.ReadAsInt16(source, 145  - 1, 2);
            WB14B2_W1X_36_EXP_MON     = StringParser.ReadAsInt16(source, 147  - 1, 2);
            WB14B2_W1X_36_EXP_DAY     = StringParser.ReadAsInt16(source, 149  - 1, 2);
            WB14B2_W1X_36_BOND_NUMBER = StringParser.ReadAsString(source, 151 - 1, 20);
        }
    }
}
