#nullable enable
using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Xml.Serialization;

using Engineering.DataSource;

using Microsoft.Extensions.Primitives;

using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace OilGas.Data.RRC.Texas
{
    /// <summary>
    /// OG_LEASE_CYCLE_DATA_TABLE.dsv
    /// </summary>
    [Serializable]
    [DataContract]
    [XmlRoot(nameof(OgLeaseCycleData))]
    [Table(nameof(OgLeaseCycleData))]
    public class OgLeaseCycleData
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

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(OIL_GAS_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char OIL_GAS_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(DISTRICT_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int DISTRICT_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long LEASE_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(CYCLE_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int CYCLE_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(CYCLE_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int CYCLE_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(CYCLE_YEAR_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int CYCLE_YEAR_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_NO_DISTRICT_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long LEASE_NO_DISTRICT_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(OPERATOR_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? OPERATOR_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FIELD_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FIELD_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FIELD_TYPE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? FIELD_TYPE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(GAS_WELL_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? GAS_WELL_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(PROD_REPORT_FILED_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? PROD_REPORT_FILED_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_OIL_PROD_VOL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? LEASE_OIL_PROD_VOL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_OIL_ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? LEASE_OIL_ALLOW
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_OIL_ENDING_BAL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? LEASE_OIL_ENDING_BAL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_GAS_PROD_VOL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? LEASE_GAS_PROD_VOL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_GAS_ALLOW), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? LEASE_GAS_ALLOW
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_GAS_LIFT_INJ_VOL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? LEASE_GAS_LIFT_INJ_VOL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_COND_PROD_VOL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? LEASE_COND_PROD_VOL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_COND_LIMIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? LEASE_COND_LIMIT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_COND_ENDING_BAL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? LEASE_COND_ENDING_BAL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_CSGD_PROD_VOL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? LEASE_CSGD_PROD_VOL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_CSGD_LIMIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? LEASE_CSGD_LIMIT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_CSGD_GAS_LIFT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? LEASE_CSGD_GAS_LIFT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_OIL_TOT_DISP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? LEASE_OIL_TOT_DISP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_GAS_TOT_DISP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? LEASE_GAS_TOT_DISP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_COND_TOT_DISP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? LEASE_COND_TOT_DISP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_CSGD_TOT_DISP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? LEASE_CSGD_TOT_DISP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(DISTRICT_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? DISTRICT_NAME { get; set; }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? LEASE_NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(OPERATOR_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? OPERATOR_NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FIELD_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? FIELD_NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OgLeaseCycleData()
        {
        }

        public OgLeaseCycleData(StringSegment[] values)
        {
            int index = 0;

            OIL_GAS_CODE           = values[index++][0];
            DISTRICT_NO            = int.Parse(values[index++].AsSpan().Trim());
            LEASE_NO               = long.Parse(values[index++].AsSpan().Trim());
            CYCLE_YEAR             = int.Parse(values[index++].AsSpan().Trim());
            CYCLE_MONTH            = int.Parse(values[index++].AsSpan().Trim());
            CYCLE_YEAR_MONTH       = int.Parse(values[index++].AsSpan().Trim());
            LEASE_NO_DISTRICT_NO   = long.Parse(values[index++].AsSpan().Trim());
            OPERATOR_NO            = long.Parse(values[index++].AsSpan().Trim());
            FIELD_NO               = long.Parse(values[index++].AsSpan().Trim());
            FIELD_TYPE             = values[index++].Value;
            GAS_WELL_NO            = values[index++].Value;
            PROD_REPORT_FILED_FLAG = values.CharValue(ref index);
            LEASE_OIL_PROD_VOL     = float.Parse(values[index++].AsSpan().Trim());
            LEASE_OIL_ALLOW        = float.Parse(values[index++].AsSpan().Trim());
            LEASE_OIL_ENDING_BAL   = float.Parse(values[index++].AsSpan().Trim());
            LEASE_GAS_PROD_VOL     = float.Parse(values[index++].AsSpan().Trim());
            LEASE_GAS_ALLOW        = float.Parse(values[index++].AsSpan().Trim());
            LEASE_GAS_LIFT_INJ_VOL = float.Parse(values[index++].AsSpan().Trim());
            LEASE_COND_PROD_VOL    = float.Parse(values[index++].AsSpan().Trim());
            LEASE_COND_LIMIT       = float.Parse(values[index++].AsSpan().Trim());
            LEASE_COND_ENDING_BAL  = float.Parse(values[index++].AsSpan().Trim());
            LEASE_CSGD_PROD_VOL    = float.Parse(values[index++].AsSpan().Trim());
            LEASE_CSGD_LIMIT       = float.Parse(values[index++].AsSpan().Trim());
            LEASE_CSGD_GAS_LIFT    = float.Parse(values[index++].AsSpan().Trim());
            LEASE_OIL_TOT_DISP     = float.Parse(values[index++].AsSpan().Trim());
            LEASE_GAS_TOT_DISP     = float.Parse(values[index++].AsSpan().Trim());
            LEASE_COND_TOT_DISP    = float.Parse(values[index++].AsSpan().Trim());
            LEASE_CSGD_TOT_DISP    = float.Parse(values[index++].AsSpan().Trim());
            DISTRICT_NAME          = values[index++].Value;
            LEASE_NAME             = values[index++].Value;
            OPERATOR_NAME          = values[index++].Value;
            FIELD_NAME             = values[index].Value;
        }

    }
}
