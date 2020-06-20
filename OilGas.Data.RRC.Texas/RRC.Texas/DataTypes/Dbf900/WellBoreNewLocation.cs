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
    [XmlRoot(nameof(WellBoreNewLocation))]
    [Table(nameof(WellBoreNewLocation))]
    public class WellBoreNewLocation
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
        //02 WELL-BORE-NEW-LOC-SEG.

        //02 WELL-BORE-NEW-LOC-DATA.

        //03 WB-NEW-LOC-KEY.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_LOC_COUNTY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_LOC_COUNTY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_ABSTRACT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_ABSTRACT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SURVEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_SURVEY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_BLOCK_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_BLOCK_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SECTION), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_SECTION
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_ALT_SECTION), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_ALT_SECTION
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_ALT_ABSTRACT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_ALT_ABSTRACT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-DISTANCE-FROM-SURVEY-LINES.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_FEET_FROM_SUR_SECT_1), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_FEET_FROM_SUR_SECT_1
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DIREC_FROM_SUR_SECT_1), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_DIREC_FROM_SUR_SECT_1
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_FEET_FROM_SUR_SECT_2), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_FEET_FROM_SUR_SECT_2
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DIREC_FROM_SUR_SECT_2), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_DIREC_FROM_SUR_SECT_2
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //02 WB-OTHER-NEW-LOC-DATA.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_WGS84_LATITUDE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WB_WGS84_LATITUDE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_WGS84_LONGITUDE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WB_WGS84_LONGITUDE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLANE_ZONE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_PLANE_ZONE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLANE_COORDINATE_EAST), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WB_PLANE_COORDINATE_EAST
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-PLANE-EAST-RE REDEFINES WB-PLANE-COORDINATE-EAST PIC 9(10).

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLANE_COORDINATE_NORTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? WB_PLANE_COORDINATE_NORTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-PLANE-NORTH-RE REDEFINES WB-PLANE-COORDINATE-NORTH PIC 9(10).

        public WellBoreNewLocation()
        {
        }

        public WellBoreNewLocation(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-NEW-LOC-SEG.
            //02 WELL-BORE-NEW-LOC-DATA.
            //03 WB-NEW-LOC-KEY.
            WB_LOC_COUNTY   = StringParser.ReadAsInt32(source, 3   - 1, 3);
            WB_ABSTRACT     = StringParser.ReadAsString(source, 6  - 1, 6);
            WB_SURVEY       = StringParser.ReadAsString(source, 12 - 1, 55);
            WB_BLOCK_NUMBER = StringParser.ReadAsString(source, 67 - 1, 10);
            WB_SECTION      = StringParser.ReadAsString(source, 77 - 1, 8);
            WB_ALT_SECTION  = StringParser.ReadAsString(source, 85 - 1, 4);
            WB_ALT_ABSTRACT = StringParser.ReadAsString(source, 89 - 1, 6);
            //03 WB-DISTANCE-FROM-SURVEY-LINES.
            WB_FEET_FROM_SUR_SECT_1  = StringParser.ReadAsInt64(source, 95   - 1, 6);
            WB_DIREC_FROM_SUR_SECT_1 = StringParser.ReadAsString(source, 101 - 1, 13);
            WB_FEET_FROM_SUR_SECT_2  = StringParser.ReadAsInt64(source, 114  - 1, 6);
            WB_DIREC_FROM_SUR_SECT_2 = StringParser.ReadAsString(source, 120 - 1, 13);
            //02 WB-OTHER-NEW-LOC-DATA.
            WB_WGS84_LATITUDE        = StringParser.ReadAsDouble(source, 7, 133 - 1, 10);
            WB_WGS84_LONGITUDE       = StringParser.ReadAsDouble(source, 7, 143 - 1, 10);
            WB_PLANE_ZONE            = StringParser.ReadAsInt16(source, 158     - 1, 2);
            WB_PLANE_COORDINATE_EAST = StringParser.ReadAsDouble(source, 2, 160 - 1, 10);
            //03 WB-PLANE-EAST-RE REDEFINES WB-PLANE-COORDINATE-EAST PIC 9(10).
            WB_PLANE_COORDINATE_NORTH = StringParser.ReadAsDouble(source, 2, 170 - 1, 9);
            //03 WB-PLANE-NORTH-RE REDEFINES WB-PLANE-COORDINATE-NORTH PIC 9(10).
        }
    }
}
