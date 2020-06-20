#nullable enable
using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Globalization;
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
    [XmlRoot(nameof(OgOperatorDw))]
    [Table(nameof(OgOperatorDw))]
    public sealed class OgOperatorDw
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
        [JsonProperty(nameof(P5_STATUS_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? P5_STATUS_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(P5_LAST_FILED_DT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? P5_LAST_FILED_DT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(OPERATOR_TAX_CERT_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? OPERATOR_TAX_CERT_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(OPERATOR_SB639_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? OPERATOR_SB639_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FA_OPTION_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? FA_OPTION_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(RECORD_STATUS_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? RECORD_STATUS_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(EFILE_STATUS_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? EFILE_STATUS_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(EFILE_EFFECTIVE_DT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public DateTime? EFILE_EFFECTIVE_DT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(CREATE_BY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? CREATE_BY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(CREATE_DT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public DateTime? CREATE_DT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(MODIFY_BY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? MODIFY_BY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(MODIFY_DT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public DateTime? MODIFY_DT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public OgOperatorDw()
        {
        }

        public OgOperatorDw(StringSegment[] values)
        {
            int index = 0;

            OPERATOR_NO            = long.Parse(values[index++].AsSpan().Trim());
            OPERATOR_NAME          = values[index++].Value;
            P5_STATUS_CODE         = values[index++].Trim().Value;
            P5_LAST_FILED_DT       = long.Parse(values[index++].AsSpan().Trim());
            OPERATOR_TAX_CERT_FLAG = values.CharValue(ref index);
            OPERATOR_SB639_FLAG    = values.CharValue(ref index);
            FA_OPTION_CODE         = values[index++].Trim().Value;
            RECORD_STATUS_CODE     = values.CharValue(ref index);
            EFILE_STATUS_CODE      = values.IntValue(ref index);
            EFILE_EFFECTIVE_DT     = values.DateTimeValue(ref index, "dd-MMM-yy");
            CREATE_BY              = values[index++].Value;
            CREATE_DT              = values.DateTimeValue(ref index, "dd-MMM-yy");
            MODIFY_BY              = values[index++].Value;
            MODIFY_DT              = values.DateTimeValue(ref index, "dd-MMM-yy");
        }


    }
}
