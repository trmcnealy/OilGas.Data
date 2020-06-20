#nullable enable
using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Xml.Serialization;

using Engineering.DataSource;

using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace OilGas.Data.RRC.Texas
{
    public enum SymNum
    {
        PermittedLocation                       = 2,
        DryHole                                 = 3,
        OilWell                                 = 4,
        GasWell                                 = 5,
        OilGasWell                              = 6,
        PluggedOilWell                          = 7,
        PluggedGasWell                          = 8,
        CanceledLocation                        = 9,
        PluggedOilGasWell                       = 10,
        InjectionDisposalWell                   = 11,
        CoreTest                                = 12,
        DirectionalSurfaceLocation              = 13,
        RadioactiveWell                         = 15,
        SulfurCoreTest                          = 16,
        StorageFromOil                          = 17,
        StorageFromGas                          = 18,
        ShutInWellOil                           = 19,
        ShutInWellGas                           = 20,
        InjectionDisposalFromOil                = 21,
        InjectionDisposalFromGas                = 22,
        InjectionDisposalFromOilGas             = 23,
        OffshorePlatform                        = 24,
        GeothermalWell                          = 36,
        BrineMiningWell                         = 73,
        WaterSupplyWell                         = 74,
        WaterSupplyFromOil                      = 75,
        WaterSupplyFromGas                      = 76,
        WaterSupplyFromOilGas                   = 77,
        ObservationWell                         = 78,
        ObservationFromOil                      = 79,
        ObservationFromGas                      = 80,
        ObservationFromOilGas                   = 81,
        HorizontalDrainhole                     = 86,
        SidetrackWellSurfaceLocation            = 87,
        StorageWell                             = 88,
        ServiceWell                             = 89,
        ServiceFromOil                          = 90,
        ServiceFromGas                          = 91,
        ServiceFromOilGas                       = 92,
        StorageFromOilGas                       = 103,
        InjectionDisposalFromStorage            = 104,
        InjectionDisposalFromStorageOil         = 105,
        InjectionDisposalFromStorageGas         = 106,
        InjectionDisposalFromStorageOilGas      = 107,
        ObservationFromStorage                  = 108,
        ObservationFromStorageOil               = 109,
        ObservationFromStorageGas               = 110,
        ObservationFromStorageOilGas            = 111,
        ServiceFromStorage                      = 112,
        ServiceFromStorageOil                   = 113,
        ServiceFromStorageGas                   = 114,
        ServiceFromStorageOilGas                = 115,
        PluggedStorage                          = 116,
        PluggedStorageOil                       = 117,
        PluggedStorageGas                       = 118,
        PluggedStorageOilGas                    = 119,
        BrineMiningFromOil                      = 121,
        BrineMiningFromGas                      = 122,
        BrineMiningFromOilGas                   = 123,
        InjectionDisposalFromBrineMining        = 124,
        InjectionDisposalFromBrineMiningOil     = 125,
        InjectionDisposalFromBrineMiningGas     = 126,
        InjectionDisposalFromBrineMiningOilGas  = 127,
        ObservationFromBrineMining              = 128,
        ObservationFromBrineMiningOil           = 129,
        ObservationFromBrineMiningGas           = 130,
        ObservationFromBrineMiningOilGas        = 131,
        ServiceFromBrineMining                  = 132,
        ServiceFromBrineMiningOil               = 133,
        ServiceFromBrineMiningGas               = 134,
        ServiceFromBrineMiningOilGas            = 135,
        PluggedBrineMining                      = 136,
        PluggedBrineMiningOil                   = 137,
        PluggedBrineMiningGas                   = 138,
        PluggedBrineMiningOilGas                = 139,
        StorageBrineMining                      = 140,
        StorageBrineMiningOil                   = 141,
        StorageBrineMiningGas                   = 142,
        StorageBrineMiningOilGas                = 143,
        InjDisposalFromStorageBrineMining       = 144,
        InjDisposalFromStorageBrineMiningOil    = 145,
        InjDisposalFromStorageBrineMiningGas    = 146,
        InjDisposalFromStorageBrineMiningOilGas = 147,
        ObservationFromStorageBrineMining       = 148,
        ObservationFromStorageBrineMiningOil    = 149,
        ObservationFromStorageBrineMiningGas    = 150,
        ObservationFromStorageBrineMiningOilGas = 151,
        PluggedStorageBrineMining               = 152,
        PluggedStorageBrineMiningOil            = 153,
        PluggedStorageBrineMiningGas            = 154,
        PluggedStorageBrineMiningOilGas         = 155
    }

    /// <summary>DATA ITEMS IN THESURV<FIPS>P:< = summary>
    [Serializable]
    [DataContract]
    [XmlRoot(nameof(SurveyP))]
    [Table(nameof(SurveyP))]
    public sealed class SurveyP
    {
        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        [Key]
        public string? ABSTRACT_N
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        [Key]
        public string? LEVEL1_SUR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        [Key]
        public string? LEVLE2_BLO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        [Key]
        public string? LEVEL3_SUR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        [Key]
        public string? LEVEL4_SUR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        [Key]
        public string? ABSTRACT_L
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [IgnoreDataMember]
        [XmlIgnore]
        [JsonIgnore]
        [Key]
        public string? SCRAP_FILE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
    }

    public sealed class SurveyB
    {
        public string? BAYNUM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? BAYID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? TRACTNUM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? TYPE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? CREATED_US
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? CREATED_DA
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? LAST_EDITE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? LAST_EDI_1
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
    }

    public sealed class SurveyL
    {
        public short LTYPE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? QUAD15M
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
    }

    public sealed class SurveyAbspt
    {
        public double SYMBOLID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? TEXTSTRING
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FONTNAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public double FONTSIZE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public double ANGLE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? JUST
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public double ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public double SYMBOL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
    }

    public sealed class SurveyLabpt
    {
        public double TEXTSTRING
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? FONTNAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public double FONTSIZE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public double ANGLE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? JUST
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public double ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public double SYMBOL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
    }
    
    [Serializable]
    [DataContract]
    [XmlRoot(nameof(WellB))]
    [Table(nameof(WellB))]
    public sealed class WellB
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
        [JsonProperty(nameof(BOTTOM_ID), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? BOTTOM_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(SURFACE_ID), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? SURFACE_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(SYMNUM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? SYMNUM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(APINUM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? APINUM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(RELIAB), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? RELIAB
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(API10), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? API10
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(API), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? API
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LONG27), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? LONG27
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LAT27), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? LAT27
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LONG83), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? LONG83
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LAT83), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? LAT83
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(OUT_FIPS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? OUT_FIPS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(CWELLNUM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? CWELLNUM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(RADIOACT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? RADIOACT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WELLID), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WELLID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(STCODE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? STCODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellB()
        {
        }

        public WellB(string[] values)
        {
            int index = 0;

            BOTTOM_ID = long.Parse(values[index++].TrimEnd());
            SURFACE_ID = long.Parse(values[index++].TrimEnd());
            SYMNUM = long.Parse(values[index++].TrimEnd());
            APINUM = values[index++].TrimEnd();
            RELIAB = values[index++].TrimEnd();
            API10 = values[index++].TrimEnd();
            API = "42" + values[index++].TrimEnd();
            LONG27 = double.Parse(values[index++].TrimEnd());
            LAT27 = double.Parse(values[index++].TrimEnd());
            LONG83 = double.Parse(values[index++].TrimEnd());
            LAT83 = double.Parse(values[index++].TrimEnd());
            OUT_FIPS = values[index++].TrimEnd();
            CWELLNUM = values[index++].TrimEnd();
            RADIOACT = values[index++].TrimEnd();
            WELLID = values[index++].TrimEnd();
            STCODE = values[index].TrimEnd();
        }
    }
    
    [Serializable]
    [DataContract]
    [XmlRoot(nameof(WellS))]
    [Table(nameof(WellS))]
    public sealed class WellS
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
        [JsonProperty(nameof(SURFACE_ID), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long SURFACE_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(SYMNUM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long SYMNUM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(API), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? API
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(RELIAB), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? RELIAB
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LONG27), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double LONG27
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LAT27), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double LAT27
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LONG83), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double LONG83
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LAT83), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double LAT83
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WELLID), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WELLID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellS()
        {
        }

        public WellS(string[] values)
        {
            int index = 0;

            SURFACE_ID = long.Parse(values[index++].TrimEnd());
            SYMNUM = long.Parse(values[index++].TrimEnd());
            API = "42" + values[index++].TrimEnd();
            RELIAB = values[index++].TrimEnd();
            LONG27 = double.Parse(values[index++].TrimEnd());
            LAT27 = double.Parse(values[index++].TrimEnd());
            LONG83 = double.Parse(values[index++].TrimEnd());
            LAT83 = double.Parse(values[index++].TrimEnd());
            WELLID = values[index].TrimEnd();
        }
    }

    public sealed class WellL
    {
        public long BOTTOM_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public long SURFACE_ID
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? API10
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public ApiNumber API
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public string? STCODE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellL(string[] values)
        {
            int index = 0;

            BOTTOM_ID  = long.Parse(values[index++].TrimEnd());
            SURFACE_ID = long.Parse(values[index++].TrimEnd());
            API10      = values[index++].TrimEnd();
            API        = "42" + values[index++].TrimEnd();
            STCODE     = values[index].TrimEnd();
        }
    }
}
