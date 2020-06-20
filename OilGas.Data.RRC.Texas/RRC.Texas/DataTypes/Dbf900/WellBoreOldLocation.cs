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
    [XmlRoot(nameof(WellBoreOldLocation))]
    [Table(nameof(WellBoreOldLocation))]
    public class WellBoreOldLocation
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
        //02 WELL-BORE-OLD-LOCATION-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_LEASE_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_LEASE_NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SEC_BLK_SURVEY_LOC), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_SEC_BLK_SURVEY_LOC
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_WELL_LOC_MILES), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_WELL_LOC_MILES
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_WELL_LOC_DIRECTION), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_WELL_LOC_DIRECTION
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_WELL_LOC_NEAREST_TOWN), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_WELL_LOC_NEAREST_TOWN
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DIST_FROM_SURVEY_LINES), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_DIST_FROM_SURVEY_LINES
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DIST_DIRECT_NEAR_WELL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_DIST_DIRECT_NEAR_WELL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBoreOldLocation()
        {
        }

        public WellBoreOldLocation(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-OLD-LOCATION-SEG.
            WB_LEASE_NAME             = StringParser.ReadAsString(source, 3   - 1, 32);
            WB_SEC_BLK_SURVEY_LOC     = StringParser.ReadAsString(source, 35  - 1, 52);
            WB_WELL_LOC_MILES         = StringParser.ReadAsInt32(source, 87   - 1, 4);
            WB_WELL_LOC_DIRECTION     = StringParser.ReadAsString(source, 91  - 1, 6);
            WB_WELL_LOC_NEAREST_TOWN  = StringParser.ReadAsString(source, 97  - 1, 13);
            WB_DIST_FROM_SURVEY_LINES = StringParser.ReadAsString(source, 138 - 1, 28);
            WB_DIST_DIRECT_NEAR_WELL  = StringParser.ReadAsString(source, 166 - 1, 28);
        }
    }
}
