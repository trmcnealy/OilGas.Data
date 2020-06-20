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
    [XmlRoot(nameof(WellBorePluggingData))]
    [Table(nameof(WellBorePluggingData))]
    public class WellBorePluggingData
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
        //02 WELL-BORE-PLUGGING-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DATE_W3_FILED), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_DATE_W3_FILED
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DATE_WELL_BORE_PLUGGED), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_DATE_WELL_BORE_PLUGGED
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_TOTAL_DEPTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLUG_TOTAL_DEPTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_CEMENT_COMP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_PLUG_CEMENT_COMP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_MUD_FILLED), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_PLUG_MUD_FILLED
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-PLUG-MUD-DATA.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_MUD_APPLIED), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_PLUG_MUD_APPLIED
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_MUD_WEIGHT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_PLUG_MUD_WEIGHT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_DRIL_PERM_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLUG_DRIL_PERM_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_DRIL_PERM_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLUG_DRIL_PERM_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_DRIL_COMP_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLUG_DRIL_COMP_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_LOG_ATTACHED), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_PLUG_LOG_ATTACHED
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_LOG_RELEASED_TO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_PLUG_LOG_RELEASED_TO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_TYPE_LOG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_PLUG_TYPE_LOG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_FRESH_WATER_DEPTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLUG_FRESH_WATER_DEPTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_UWQP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_PLUG_UWQP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-PLUG-UWQP-RE REDEFINES WB-PLUG-UWQP.

        //10 WB-PLUG-FROM-UWQP OCCURS 4 TIMES PIC 9(05).

        //10 WB-PLUG-TO-UWQP OCCURS 4 TIMES PIC 9(05).

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_MATERIAL_LEFT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_PLUG_MATERIAL_LEFT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_OIL_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_PLUG_OIL_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_OIL_DIST), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_PLUG_OIL_DIST
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_OIL_LSE_NBR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLUG_OIL_LSE_NBR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_OIL_WELL_NBR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_PLUG_OIL_WELL_NBR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-PLUG-GAS-KEY REDEFINES WB-PLUG-OIL-KEY.

        //05 WB-PLUG-GAS-CODE PIC X(01).

        //05 WB-PLUG-GAS-RRC-ID PIC 9(06).

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_GAS_DIST), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_PLUG_GAS_DIST
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_GAS_WELL_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_PLUG_GAS_WELL_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_TYPE_WELL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_PLUG_TYPE_WELL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_MULTI_COMPL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_PLUG_MULTI_COMPL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_CEM_AFF), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_PLUG_CEM_AFF
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_13A), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_PLUG_13A
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_LOG_RELEASED_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLUG_LOG_RELEASED_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_LOG_FILE_RBA), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLUG_LOG_FILE_RBA
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_STATE_FUNDED_PLUG_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_STATE_FUNDED_PLUG_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBorePluggingRemarks>? WellBorePluggingRemarks
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBorePluggingRemarks>();

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual WellBorePluggingRecord? WellBorePluggingRecord
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBorePluggingDataCasingTubing>? WellBorePluggingDataCasingTubing
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBorePluggingDataCasingTubing>();

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBorePluggingPerfs>? WellBorePluggingPerfs
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBorePluggingPerfs>();

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual WellBorePluggingDataNomenclature? WellBorePluggingDataNomenclature
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBorePluggingData()
        {
        }

        public WellBorePluggingData(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-PLUGGING-SEG.
            WB_DATE_W3_FILED          = StringParser.ReadAsInt64(source, 3   - 1, 8);
            WB_DATE_WELL_BORE_PLUGGED = StringParser.ReadAsInt64(source, 11  - 1, 8);
            WB_PLUG_TOTAL_DEPTH       = StringParser.ReadAsInt64(source, 19  - 1, 5);
            WB_PLUG_CEMENT_COMP       = StringParser.ReadAsString(source, 24 - 1, 32);
            WB_PLUG_MUD_FILLED        = StringParser.ReadAsChar(source, 56   - 1, 1);
            //03 WB-PLUG-MUD-DATA.
            WB_PLUG_MUD_APPLIED       = StringParser.ReadAsString(source, 57  - 1, 12);
            WB_PLUG_MUD_WEIGHT        = StringParser.ReadAsInt32(source, 69   - 1, 3);
            WB_PLUG_DRIL_PERM_DATE    = StringParser.ReadAsInt64(source, 76   - 1, 8);
            WB_PLUG_DRIL_PERM_NO      = StringParser.ReadAsInt64(source, 84   - 1, 6);
            WB_PLUG_DRIL_COMP_DATE    = StringParser.ReadAsInt64(source, 90   - 1, 8);
            WB_PLUG_LOG_ATTACHED      = StringParser.ReadAsChar(source, 98    - 1, 1);
            WB_PLUG_LOG_RELEASED_TO   = StringParser.ReadAsString(source, 99  - 1, 32);
            WB_PLUG_TYPE_LOG          = StringParser.ReadAsChar(source, 131   - 1, 1);
            WB_PLUG_FRESH_WATER_DEPTH = StringParser.ReadAsInt64(source, 132  - 1, 5);
            WB_PLUG_UWQP              = StringParser.ReadAsString(source, 137 - 1, 40);
            //03 WB-PLUG-UWQP-RE REDEFINES WB-PLUG-UWQP.
            //10 WB-PLUG-FROM-UWQP OCCURS 4 TIMES PIC 9(05).
            //10 WB-PLUG-TO-UWQP OCCURS 4 TIMES PIC 9(05).
            WB_PLUG_MATERIAL_LEFT = StringParser.ReadAsChar(source, 177   - 1, 1);
            WB_PLUG_OIL_CODE      = StringParser.ReadAsChar(source, 178   - 1, 1);
            WB_PLUG_OIL_DIST      = StringParser.ReadAsInt16(source, 179  - 1, 2);
            WB_PLUG_OIL_LSE_NBR   = StringParser.ReadAsInt64(source, 181  - 1, 5);
            WB_PLUG_OIL_WELL_NBR  = StringParser.ReadAsString(source, 186 - 1, 6);
            //03 WB-PLUG-GAS-KEY REDEFINES WB-PLUG-OIL-KEY.
            //05 WB-PLUG-GAS-CODE PIC X(01).
            //05 WB-PLUG-GAS-RRC-ID PIC 9(06).
            WB_PLUG_GAS_DIST            = StringParser.ReadAsInt16(source, 192  - 1, 2);
            WB_PLUG_GAS_WELL_NO         = StringParser.ReadAsString(source, 194 - 1, 6);
            WB_PLUG_TYPE_WELL           = StringParser.ReadAsChar(source, 200   - 1, 1);
            WB_PLUG_MULTI_COMPL_FLAG    = StringParser.ReadAsChar(source, 201   - 1, 1);
            WB_PLUG_CEM_AFF             = StringParser.ReadAsChar(source, 202   - 1, 1);
            WB_PLUG_13A                 = StringParser.ReadAsChar(source, 203   - 1, 1);
            WB_PLUG_LOG_RELEASED_DATE   = StringParser.ReadAsInt64(source, 204  - 1, 8);
            WB_PLUG_LOG_FILE_RBA        = StringParser.ReadAsInt64(source, 212  - 1, 8);
            WB_STATE_FUNDED_PLUG_NUMBER = StringParser.ReadAsInt64(source, 220  - 1, 7);
        }
    }
}
