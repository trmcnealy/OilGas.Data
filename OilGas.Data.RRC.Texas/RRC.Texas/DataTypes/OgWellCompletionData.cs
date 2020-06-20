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
    /// OG_WELL_COMPLETION_DATA_TABLE.dsv
    /// </summary>
    [Serializable]
    [DataContract]
    [XmlRoot(nameof(OgWellCompletionData))]
    [Table(nameof(OgWellCompletionData))]
    public class OgWellCompletionData
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
        [JsonProperty(nameof(WELL_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string WELL_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(API_COUNTY_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string API_COUNTY_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(API_UNIQUE_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string API_UNIQUE_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(ONSHORE_ASSC_CNTY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? ONSHORE_ASSC_CNTY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(DISTRICT_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? DISTRICT_NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(COUNTY_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? COUNTY_NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(OIL_WELL_UNIT_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? OIL_WELL_UNIT_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WELL_ROOT_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WELL_ROOT_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WELLBORE_SHUTIN_DT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WELLBORE_SHUTIN_DT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WELL_SHUTIN_DT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WELL_SHUTIN_DT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WELL_14B2_STATUS_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WELL_14B2_STATUS_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WELL_SUBJECT_14B2_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WELL_SUBJECT_14B2_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WELLBORE_LOCATION_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WELLBORE_LOCATION_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OgWellCompletionData()
        {
            WELL_NO = string.Empty;
            API_COUNTY_CODE = string.Empty;
            API_UNIQUE_NO = string.Empty;
        }

        public OgWellCompletionData(StringSegment[] values)
        {
            int index = 0;

            OIL_GAS_CODE           = values[index++][0];
            DISTRICT_NO            = int.Parse(values[index++].AsSpan());
            LEASE_NO               = long.Parse(values[index++].AsSpan());
            WELL_NO                = values[index++].Value;
            API_COUNTY_CODE        = values[index++].Value;
            API_UNIQUE_NO          = values[index++].Value;
            ONSHORE_ASSC_CNTY      = values[index++].Value;
            DISTRICT_NAME          = values[index++].Value;
            COUNTY_NAME            = values[index++].Value;
            OIL_WELL_UNIT_NO       = values[index++].Value;
            WELL_ROOT_NO           = values[index++].Value;
            WELLBORE_SHUTIN_DT     = values[index++].Value;
            WELL_SHUTIN_DT         = values[index++].Value;
            WELL_14B2_STATUS_CODE = values.CharValue(ref index);
            WELL_SUBJECT_14B2_FLAG = values.CharValue(ref index);
            WELLBORE_LOCATION_CODE = values.CharValue(ref index);
        }
    }
}
