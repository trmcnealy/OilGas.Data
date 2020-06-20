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
    [XmlRoot(nameof(WellBoreCompletionInformation))]
    [Table(nameof(WellBoreCompletionInformation))]
    public class WellBoreCompletionInformation
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
        //02 WELL-BORE-COMPLETION-SEG.

        //03 WB-OIL-KEY.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_OIL_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_OIL_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_OIL_DIST), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_OIL_DIST
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_OIL_LSE_NBR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_OIL_LSE_NBR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_OIL_WELL_NBR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_OIL_WELL_NBR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-GAS-KEY REDEFINES WB-OIL-KEY.

        //05 WB-GAS-CODE PIC X(01).

        //05 WB-GAS-RRC-ID PIC 9(06).

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_GAS_DIST), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_GAS_DIST
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_GAS_WELL_NO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_GAS_WELL_NO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_MULTI_WELL_REC_NBR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_MULTI_WELL_REC_NBR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_API_SUFFIX), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_API_SUFFIX
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_ACTIVE_INACTIVE_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_ACTIVE_INACTIVE_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DWN_HOLE_COMMINGLE_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_DWN_HOLE_COMMINGLE_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CREATED_FROM_PI_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_CREATED_FROM_PI_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_RULE_37_NBR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_RULE_37_NBR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_DATE_POINTER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLUG_DATE_POINTER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBoreTechnicalDataFormsFileDate>? WellBoreTechnicalDataFormsFileDate
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBoreTechnicalDataFormsFileDate>();

        public WellBoreCompletionInformation()
        {
        }

        public WellBoreCompletionInformation(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-COMPLETION-SEG.
            //03 WB-OIL-KEY.
            WB_OIL_CODE     = StringParser.ReadAsChar(source, 3    - 1, 1);
            WB_OIL_DIST     = StringParser.ReadAsInt16(source, 4   - 1, 2);
            WB_OIL_LSE_NBR  = StringParser.ReadAsInt64(source, 6   - 1, 5);
            WB_OIL_WELL_NBR = StringParser.ReadAsString(source, 11 - 1, 6);
            //03 WB-GAS-KEY REDEFINES WB-OIL-KEY.
            //05 WB-GAS-CODE PIC X(01).
            //05 WB-GAS-RRC-ID PIC 9(06).
            WB_GAS_DIST                = StringParser.ReadAsInt16(source, 17  - 1, 2);
            WB_GAS_WELL_NO             = StringParser.ReadAsString(source, 19 - 1, 6);
            WB_MULTI_WELL_REC_NBR      = StringParser.ReadAsChar(source, 25   - 1, 1);
            WB_API_SUFFIX              = StringParser.ReadAsInt16(source, 26  - 1, 2);
            WB_ACTIVE_INACTIVE_CODE    = StringParser.ReadAsChar(source, 46   - 1, 1);
            WB_DWN_HOLE_COMMINGLE_CODE = StringParser.ReadAsChar(source, 87   - 1, 1);
            WB_CREATED_FROM_PI_FLAG    = StringParser.ReadAsChar(source, 122  - 1, 1);
            WB_RULE_37_NBR             = StringParser.ReadAsInt64(source, 123 - 1, 7);
            WB_PLUG_DATE_POINTER       = StringParser.ReadAsInt64(source, 158 - 1, 8);
        }
    }
}
