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
    [XmlRoot(nameof(WellBoreTechnicalDataFormsFileDate))]
    [Table(nameof(WellBoreTechnicalDataFormsFileDate))]
    public class WellBoreTechnicalDataFormsFileDate
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
        //02 WELL-BORE-FILE-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_FILE_KEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_FILE_KEY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_FILE_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_FILE_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_EXCEPT_RULE_11), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_EXCEPT_RULE_11
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_CEMENT_AFFIDAVIT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_CEMENT_AFFIDAVIT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_G_5), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_G_5
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W_12), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_W_12
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DIR_SURVEY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_DIR_SURVEY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_W2_G1_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_W2_G1_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //03 WB-COMPL-DATE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_COMPL_CENTURY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_COMPL_CENTURY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_COMPL_YEAR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_COMPL_YEAR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_COMPL_MONTH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_COMPL_MONTH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_COMPL_DAY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_COMPL_DAY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DRL_COMPL_DATE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_DRL_COMPL_DATE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUGB_DEPTH1), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLUGB_DEPTH1
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUGB_DEPTH2), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLUGB_DEPTH2
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_WATER_INJECTION_NBR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_WATER_INJECTION_NBR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_SALT_WTR_NBR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_SALT_WTR_NBR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_REMARKS_IND), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_REMARKS_IND
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_ELEVATION), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_ELEVATION
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_ELEVATION_CODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_ELEVATION_CODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_LOG_FILE_RBA), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_LOG_FILE_RBA
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_DOCKET_NBR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WB_DOCKET_NBR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PSA_WELL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_PSA_WELL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_ALLOCATION_WELL_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WB_ALLOCATION_WELL_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBoreRemarks>? WellBoreRemarks
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBoreRemarks>();

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBoreTubing>? WellBoreTubing
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBoreTubing>();

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBoreCasing>? WellBoreCasing
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBoreCasing>();

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBorePerforations>? WellBorePerforations
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBorePerforations>();

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBoreLiner>? WellBoreLiner
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBoreLiner>();

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBoreFormation>? WellBoreFormation
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBoreFormation>();

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBoreSqueeze>? WellBoreSqueeze
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBoreSqueeze>();

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        public virtual List<WellBoreUsableQualityWaterProtection>? WellBoreUsableQualityWaterProtection
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        } = new List<WellBoreUsableQualityWaterProtection>();

        public WellBoreTechnicalDataFormsFileDate()
        {
        }

        public WellBoreTechnicalDataFormsFileDate(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-FILE-DATE.
            WB_FILE_KEY         = StringParser.ReadAsInt64(source, 3  - 1, 8);
            WB_FILE_DATE        = StringParser.ReadAsInt64(source, 11 - 1, 8);
            WB_EXCEPT_RULE_11   = StringParser.ReadAsChar(source, 27  - 1, 1);
            WB_CEMENT_AFFIDAVIT = StringParser.ReadAsChar(source, 28  - 1, 1);
            WB_G_5              = StringParser.ReadAsChar(source, 29  - 1, 1);
            WB_W_12             = StringParser.ReadAsChar(source, 30  - 1, 1);
            WB_DIR_SURVEY       = StringParser.ReadAsChar(source, 31  - 1, 1);
            WB_W2_G1_DATE       = StringParser.ReadAsInt64(source, 32 - 1, 8);
            //03 WB-COMPL-DATE.
            WB_COMPL_CENTURY        = StringParser.ReadAsInt16(source, 40   - 1, 2);
            WB_COMPL_YEAR           = StringParser.ReadAsInt16(source, 42   - 1, 2);
            WB_COMPL_MONTH          = StringParser.ReadAsInt16(source, 44   - 1, 2);
            WB_COMPL_DAY            = StringParser.ReadAsInt16(source, 46   - 1, 2);
            WB_DRL_COMPL_DATE       = StringParser.ReadAsInt64(source, 48   - 1, 8);
            WB_PLUGB_DEPTH1         = StringParser.ReadAsInt64(source, 56   - 1, 5);
            WB_PLUGB_DEPTH2         = StringParser.ReadAsInt64(source, 61   - 1, 5);
            WB_WATER_INJECTION_NBR  = StringParser.ReadAsString(source, 66  - 1, 6);
            WB_SALT_WTR_NBR         = StringParser.ReadAsInt64(source, 72   - 1, 5);
            WB_REMARKS_IND          = StringParser.ReadAsChar(source, 85    - 1, 1);
            WB_ELEVATION            = StringParser.ReadAsInt32(source, 86   - 1, 4);
            WB_ELEVATION_CODE       = StringParser.ReadAsString(source, 90  - 1, 2);
            WB_LOG_FILE_RBA         = StringParser.ReadAsInt64(source, 92   - 1, 8);
            WB_DOCKET_NBR           = StringParser.ReadAsString(source, 100 - 1, 10);
            WB_PSA_WELL_FLAG        = StringParser.ReadAsChar(source, 110   - 1, 1);
            WB_ALLOCATION_WELL_FLAG = StringParser.ReadAsChar(source, 111   - 1, 1);
        }

        public static string? ElevationCodeToString(string? elevationCode)
        {
            if(elevationCode is null)
            {
                return null;
            }

            switch(elevationCode)
            {
                case "GL":
                {
                    return "Ground Level";
                }
                case "DF":
                {
                    return "Derrick Floor";
                }
                case "KB":
                {
                    return "Kelly Bushing";
                }
                case "RT":
                {
                    return "Rotary Table";
                }
                case "GR":
                {
                    return "Ground";
                }
                default:
                    return null;
            }
        }
    }
}
