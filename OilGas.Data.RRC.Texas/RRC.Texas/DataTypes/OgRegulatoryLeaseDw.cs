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
    [Serializable]
    [DataContract]
    [XmlRoot(nameof(OgRegulatoryLeaseDw))]
    [Table(nameof(OgRegulatoryLeaseDw))]
    public sealed class OgRegulatoryLeaseDw
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
        [JsonProperty(nameof(OPERATOR_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long OPERATOR_NO
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
        [JsonProperty(nameof(WELL_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WELL_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_OFF_SCHED_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char LEASE_OFF_SCHED_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_SEVERANCE_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char LEASE_SEVERANCE_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OgRegulatoryLeaseDw()
        {
        }

        public OgRegulatoryLeaseDw(StringSegment[] values)
        {
            int index = 0;

            OIL_GAS_CODE         = values[index++][0];
            DISTRICT_NO          = int.Parse(values[index++].AsSpan());
            LEASE_NO             = long.Parse(values[index++].AsSpan());
            DISTRICT_NAME        = values[index++].Value;
            LEASE_NAME           = values[index++].Value;
            OPERATOR_NO          = long.Parse(values[index++].AsSpan());
            OPERATOR_NAME        = values[index++].Value;
            FIELD_NO             = long.Parse(values[index++].AsSpan());
            FIELD_NAME           = values[index++].Value;
            WELL_NO              = values[index++].Value;
            LEASE_OFF_SCHED_FLAG = values[index++][0];
            LEASE_SEVERANCE_FLAG = values[index][0];
        }

    }
}
