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
    [XmlRoot(nameof(OgFieldDw))]
    [Table(nameof(OgFieldDw))]
    public sealed class OgFieldDw
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
        public string FIELD_NAME
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
        [JsonProperty(nameof(FIELD_CLASS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FIELD_CLASS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FIELD_H2S_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FIELD_H2S_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FIELD_MANUAL_REV_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? FIELD_MANUAL_REV_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WILDCAT_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WILDCAT_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(O_DERIVED_RULE_TYPE_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? O_DERIVED_RULE_TYPE_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(G_DERIVED_RULE_TYPE_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? G_DERIVED_RULE_TYPE_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(O_RESCIND_DT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public DateTime? O_RESCIND_DT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(G_RESCIND_DT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? G_RESCIND_DT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(O_SALT_DOME_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? O_SALT_DOME_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(G_SALT_DOME_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? G_SALT_DOME_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(O_OFFSHORE_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? O_OFFSHORE_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(G_OFFSHORE_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? G_OFFSHORE_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(O_DONT_PERMIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? O_DONT_PERMIT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(G_DONT_PERMIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? G_DONT_PERMIT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(O_NOA_MAN_REV_RULE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? O_NOA_MAN_REV_RULE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(G_NOA_MAN_REV_RULE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? G_NOA_MAN_REV_RULE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(O_COUNTY_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? O_COUNTY_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(G_COUNTY_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? G_COUNTY_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(O_DISCOVERY_DT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public DateTime? O_DISCOVERY_DT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(G_DISCOVERY_DT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public DateTime? G_DISCOVERY_DT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(O_SCHED_REMARKS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? O_SCHED_REMARKS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(G_SCHED_REMARKS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? G_SCHED_REMARKS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(O_COMMENTS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? O_COMMENTS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(G_COMMENTS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? G_COMMENTS
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

        public OgFieldDw()
        {
            FIELD_NAME = string.Empty;
        }

        public OgFieldDw(StringSegment[] values)
        {
            int index = 0;

            FIELD_NO                 = long.Parse(values[index++].AsSpan().Trim());
            FIELD_NAME               = values[index++].Value;
            DISTRICT_NO              = int.Parse(values[index++].AsSpan().Trim());
            DISTRICT_NAME            = values[index++].Value;
            FIELD_CLASS              = values.CharValue(ref index);
            FIELD_H2S_FLAG           = values.CharValue(ref index);
            FIELD_MANUAL_REV_FLAG    = values.CharValue(ref index);
            WILDCAT_FLAG             = values.CharValue(ref index);
            O_DERIVED_RULE_TYPE_CODE = values[index++].Value;
            G_DERIVED_RULE_TYPE_CODE = values[index++].Value;
            O_RESCIND_DT             = values.DateTimeValue(ref index, "dd-MMM-yy");
            G_RESCIND_DT             = values[index++].Value;
            O_SALT_DOME_FLAG         = values.CharValue(ref index);
            G_SALT_DOME_FLAG         = values.CharValue(ref index);
            O_OFFSHORE_CODE          = values[index++].Value;
            G_OFFSHORE_CODE          = values[index++].Value;
            O_DONT_PERMIT            = values.CharValue(ref index);
            G_DONT_PERMIT            = values.CharValue(ref index);
            O_NOA_MAN_REV_RULE       = values[index++].Value;
            G_NOA_MAN_REV_RULE       = values[index++].Value;
            O_COUNTY_NO              = values.IntValue(ref index);
            G_COUNTY_NO              = values.IntValue(ref index);
            O_DISCOVERY_DT           = values.DateTimeValue(ref index, "dd-MMM-yy");
            G_DISCOVERY_DT           = values.DateTimeValue(ref index, "dd-MMM-yy");
            O_SCHED_REMARKS          = values[index++].Value;
            G_SCHED_REMARKS          = values[index++].Value;
            O_COMMENTS               = values[index++].Value;
            G_COMMENTS               = values[index++].Value;
            CREATE_BY                = values[index++].Value;
            CREATE_DT                = values.DateTimeValue(ref index, "dd-MMM-yy");
            MODIFY_BY                = values[index++].Value;
            MODIFY_DT                = values.DateTimeValue(ref index, "dd-MMM-yy");
        }
    }
}
