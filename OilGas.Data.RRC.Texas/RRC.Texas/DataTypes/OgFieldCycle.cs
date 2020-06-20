#nullable enable
using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Xml.Serialization;

using Microsoft.Extensions.Primitives;

using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace OilGas.Data.RRC.Texas
{
    /// <summary>
    /// OG_FIELD_CYCLE_DATA_TABLE.dsv
    /// </summary>
    [Serializable]
    [DataContract]
    [XmlRoot(nameof(OgFieldCycle))]
    [Table(nameof(OgFieldCycle))]
    public sealed class OgFieldCycle
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
        [JsonProperty(nameof(FIELD_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long FIELD_NO
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
        public int? CYCLE_YEAR_MONTH
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
        [JsonProperty(nameof(FIELD_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? FIELD_NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FIELD_OIL_PROD_VOL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FIELD_OIL_PROD_VOL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FIELD_GAS_PROD_VOL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FIELD_GAS_PROD_VOL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FIELD_COND_PROD_VOL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FIELD_COND_PROD_VOL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FIELD_CSGD_PROD_VOL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FIELD_CSGD_PROD_VOL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OgFieldCycle()
        {
        }

        public OgFieldCycle(StringSegment[] values)
        {
            int index = 0;

            DISTRICT_NO         = int.Parse(values[index++].AsSpan());
            FIELD_NO            = long.Parse(values[index++].AsSpan());
            CYCLE_YEAR          = int.Parse(values[index++].AsSpan());
            CYCLE_MONTH         = int.Parse(values[index++].AsSpan());
            CYCLE_YEAR_MONTH    = int.Parse(values[index++].AsSpan());
            DISTRICT_NAME       = values[index++].Value;
            FIELD_NAME          = values[index++].Value;
            FIELD_OIL_PROD_VOL  = float.Parse(values[index++].AsSpan());
            FIELD_GAS_PROD_VOL  = float.Parse(values[index++].AsSpan());
            FIELD_COND_PROD_VOL = float.Parse(values[index++].AsSpan());
            FIELD_CSGD_PROD_VOL = float.Parse(values[index].AsSpan());
        }
    }
}
