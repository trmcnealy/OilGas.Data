#nullable enable

using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Diagnostics;
using System.Globalization;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Xml.Serialization;

using Engineering.DataSource;

using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

using Npgsql;

namespace OilGas.Data.RRC.Texas
{
    [Serializable]
    [DataContract]
    [XmlRoot(nameof(ProductionAggr))]
    [Table(nameof(ProductionAggr))]
    public sealed class ProductionAggr : IEquatable<ProductionAggr>
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
        [JsonProperty("API", NamingStrategyType = typeof(DefaultNamingStrategy))]
        [Key]
        public ApiNumber Api
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
        [JsonProperty(nameof(FIELD_CLASS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char FIELD_CLASS
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

        public ProductionAggr()
        {
        }

        public ProductionAggr(NpgsqlDataReader reader)
        {
            FIELD_CLASS = reader[0].CharValue()!.Value;
            //OIL_GAS_CODE = (char)reader[1];
            DISTRICT_NO   = reader[2].IntValue()!.Value;
            DISTRICT_NAME = reader[3].StringValue();
            LEASE_NO      = reader[4].LongValue()!.Value;
            LEASE_NAME    = reader[5].StringValue();
            WELL_NO       = reader[6].StringValue();
            FIELD_NO      = reader[7].LongValue()!.Value;
            FIELD_NAME    = reader[8].StringValue();
            OPERATOR_NO   = reader[9].LongValue()!.Value;
            OPERATOR_NAME = reader[10].StringValue();
            Api           = "42" + reader[11].StringValue() + reader[12].StringValue();
        }

        public ProductionAggr(string  apiCountyCode,
                              string  apiUniqueNo,
                              long    leaseNo,
                              string? leaseName,
                              string? wellNo,
                              char    fieldClass,
                              long    fieldNo,
                              string? fieldName,
                              int     districtNo,
                              string? districtName,
                              long    operatorNo,
                              string? operatorName)
        {
            Api           = "42" + apiCountyCode + apiUniqueNo;
            LEASE_NO      = leaseNo;
            LEASE_NAME    = leaseName;
            WELL_NO       = wellNo;
            FIELD_CLASS   = fieldClass;
            FIELD_NO      = fieldNo;
            FIELD_NAME    = fieldName;
            DISTRICT_NO   = districtNo;
            DISTRICT_NAME = districtName;
            OPERATOR_NO   = operatorNo;
            OPERATOR_NAME = operatorName;
        }

        public bool Equals(ProductionAggr? other)
        {
            if(ReferenceEquals(null, other))
            {
                return false;
            }

            if(ReferenceEquals(this, other))
            {
                return true;
            }

            return Api.Equals(other.Api);
        }

        public override bool Equals(object? obj)
        {
            return ReferenceEquals(this, obj) || obj is ProductionAggr other && Equals(other);
        }

        public override int GetHashCode()
        {
            return Api.GetHashCode();
        }

        public static bool operator ==(ProductionAggr? left,
                                       ProductionAggr? right)
        {
            return Equals(left, right);
        }

        public static bool operator !=(ProductionAggr? left,
                                       ProductionAggr? right)
        {
            return !Equals(left, right);
        }
    }

    [Serializable]
    [DataContract]
    [XmlRoot(nameof(WellProductionAggr))]
    [Table(nameof(WellProductionAggr))]
    public sealed class WellProductionAggr
    {
        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(Api), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public ApiNumber Api
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(Date), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public DateTime Date
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(GAS_VOL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? GAS_VOL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(COND_VOL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? COND_VOL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(OIL_VOL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? OIL_VOL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellProductionAggr()
        {
        }

        public WellProductionAggr(ApiNumber        api,
                                  NpgsqlDataReader reader)
        {
            Api      = api;
            Date     = new DateTime(reader[0].IntValue()!.Value, reader[1].IntValue()!.Value, 1);
            GAS_VOL  = reader[2].FloatValue();
            COND_VOL = reader[3].FloatValue();
            OIL_VOL  = reader[4].FloatValue();
        }
    }

    [Serializable]
    [DataContract]
    [XmlRoot(nameof(WellLocationAggr))]
    [Table(nameof(WellLocationAggr))]
    public sealed class WellLocationAggr
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
        [JsonProperty("API", NamingStrategyType = typeof(DefaultNamingStrategy))]
        [Key]
        public ApiNumber Api
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(SURFACE_LONG27), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double SURFACE_LONG27
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(SURFACE_LAT27), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double SURFACE_LAT27
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(SURFACE_LONG83), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double SURFACE_LONG83
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(SURFACE_LAT83), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double SURFACE_LAT83
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(BOTTOM_LONG27), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? BOTTOM_LONG27
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(BOTTOM_LAT27), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? BOTTOM_LAT27
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(BOTTOM_LONG83), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? BOTTOM_LONG83
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(BOTTOM_LAT83), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public double? BOTTOM_LAT83
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellLocationAggr()
        {
        }

        public WellLocationAggr(NpgsqlDataReader reader)
        {
            Api            = reader[0].StringValue();
            SURFACE_LONG27 = reader[1].DoubleValue()!.Value;
            SURFACE_LAT27  = reader[2].DoubleValue()!.Value;
            SURFACE_LONG83 = reader[3].DoubleValue()!.Value;
            SURFACE_LAT83  = reader[4].DoubleValue()!.Value;
            BOTTOM_LONG27  = reader[5].DoubleValue();
            BOTTOM_LAT27   = reader[6].DoubleValue();
            BOTTOM_LONG83  = reader[7].DoubleValue();
            BOTTOM_LAT83   = reader[8].DoubleValue();
        }
    }

    [Serializable]
    [DataContract]
    [XmlRoot(nameof(LeaseTestAggr))]
    [Table(nameof(LeaseTestAggr))]
    public sealed class LeaseTestAggr
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
        [JsonProperty(nameof(Api), NamingStrategyType = typeof(DefaultNamingStrategy))]
        [Key]
        public ApiNumber Api
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(DISTRICT_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int DISTRICT_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(LEASE_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long LEASE_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WELL_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? WELL_NUMBER
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
        [JsonProperty(nameof(RESERVOIR_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? RESERVOIR_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(RESERVOIR_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? RESERVOIR_NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(SIWH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? SIWH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(AVG_RESERVOIR_BHP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? AVG_RESERVOIR_BHP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(AVG_RESERVOIR_BH_TEMP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? AVG_RESERVOIR_BH_TEMP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FORMATION_VOLUME_FACTOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FORMATION_VOLUME_FACTOR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(GAS_GRAVITY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? GAS_GRAVITY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(OIL_GRAVITY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? OIL_GRAVITY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(SOLUTION_GAS_OIL_RATIO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? SOLUTION_GAS_OIL_RATIO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }




        public LeaseTestAggr()
        {
        }

        private enum ColumnIndices
        {
            Api = 0,
            DISTRICT_NUMBER,
            LEASE_NUMBER,
            WELL_NUMBER,    
            FIELD_CLASS,
            RESERVOIR_NUMBER,
            RESERVOIR_NAME,
            SIWH,
            AVG_RESERVOIR_BHP,
            AVG_RESERVOIR_BH_TEMP,
            FORMATION_VOLUME_FACTOR,
            GAS_GRAVITY,
            OIL_GRAVITY,
            SOLUTION_GAS_OIL_RATIO
        }

        private const int idxApi                     = (int)ColumnIndices.Api;
        private const int idxDISTRICT_NUMBER = (int)ColumnIndices.DISTRICT_NUMBER;
        private const int idxLEASE_NUMBER = (int)ColumnIndices.LEASE_NUMBER;
        private const int idxWELL_NUMBER = (int)ColumnIndices.WELL_NUMBER;
        private const int idxFIELD_CLASS             = (int)ColumnIndices.FIELD_CLASS;
        private const int idxRESERVOIR_NUMBER = (int)ColumnIndices.RESERVOIR_NUMBER;
        private const int idxRESERVOIR_NAME = (int)ColumnIndices.RESERVOIR_NAME; 
        private const int idxSIWH = (int)ColumnIndices.SIWH;
        private const int idxAVG_RESERVOIR_BHP = (int)ColumnIndices.AVG_RESERVOIR_BHP;
        private const int idxAVG_RESERVOIR_BH_TEMP = (int)ColumnIndices.AVG_RESERVOIR_BH_TEMP;
        private const int idxFORMATION_VOLUME_FACTOR = (int)ColumnIndices.FORMATION_VOLUME_FACTOR;
        private const int idxGAS_GRAVITY = (int)ColumnIndices.GAS_GRAVITY;
        private const int idxOIL_GRAVITY = (int)ColumnIndices.OIL_GRAVITY;
        private const int idxSOLUTION_GAS_OIL_RATIO = (int)ColumnIndices.SOLUTION_GAS_OIL_RATIO;
        
        public LeaseTestAggr(NpgsqlDataReader reader)
        {
            Api                     = reader[idxApi].StringValue();
            DISTRICT_NUMBER         = reader[idxDISTRICT_NUMBER].IntValue()!.Value;
            LEASE_NUMBER            = reader[idxLEASE_NUMBER].LongValue()!.Value;
            WELL_NUMBER             = reader[idxWELL_NUMBER].StringValue();
            FIELD_CLASS             = reader[idxFIELD_CLASS].CharValue();
            RESERVOIR_NUMBER        = reader[idxRESERVOIR_NUMBER].IntValue();
            RESERVOIR_NAME          = reader[idxRESERVOIR_NAME].StringValue();
            SIWH                    = (float?)reader[idxSIWH].LongValue();
            AVG_RESERVOIR_BHP       = reader[idxAVG_RESERVOIR_BHP].FloatValue();
            AVG_RESERVOIR_BH_TEMP   = reader[idxAVG_RESERVOIR_BH_TEMP].FloatValue();
            FORMATION_VOLUME_FACTOR = reader[idxFORMATION_VOLUME_FACTOR].FloatValue();
            GAS_GRAVITY             = reader[idxGAS_GRAVITY].FloatValue();
            OIL_GRAVITY             = reader[idxOIL_GRAVITY].FloatValue();
            SOLUTION_GAS_OIL_RATIO  = reader[idxSOLUTION_GAS_OIL_RATIO].FloatValue();
        }

        public LeaseTestAggr(ApiNumber api,
                             int districtNumber,
                             long leaseNumber,
                             string? wellNumber,
                             char? fieldClass,
                             int? reservoirNumber,
                             string? reservoirName,
                             float? siwh,
                             float? avgReservoirBhp,
                             float? avgReservoirBhTemp,
                             float? formationVolumeFactor,
                             float? gasGravity,
                             float? oilGravity,
                             float? solutionGasOilRatio)
        {
            Api = api;
            DISTRICT_NUMBER = districtNumber;
            LEASE_NUMBER = leaseNumber;
            WELL_NUMBER = wellNumber;
            FIELD_CLASS = fieldClass;
            RESERVOIR_NUMBER = reservoirNumber;
            RESERVOIR_NAME = reservoirName;
            SIWH = siwh;
            AVG_RESERVOIR_BHP = avgReservoirBhp;
            AVG_RESERVOIR_BH_TEMP = avgReservoirBhTemp;
            FORMATION_VOLUME_FACTOR = formationVolumeFactor;
            GAS_GRAVITY = gasGravity;
            OIL_GRAVITY = oilGravity;
            SOLUTION_GAS_OIL_RATIO = solutionGasOilRatio;
        }
    }

    [Serializable]
    [DataContract]
    [XmlRoot(nameof(WellTestAggr))]
    [Table(nameof(WellTestAggr))]
    public sealed class WellTestAggr
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
        [JsonProperty(nameof(FL_DISTRICT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short FL_DISTRICT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_FIELD_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long FL_FIELD_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_RESERVOIR_NUMBER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int FL_RESERVOIR_NUMBER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? FL_NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_FIELD_CLASS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char FL_FIELD_CLASS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_RESERVOIR_NAME), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? FL_RESERVOIR_NAME
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_FORMATION_COMPOSITION), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public string? FL_FORMATION_COMPOSITION
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_OIL_DISC_WELL_GRAVITY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float FL_OIL_DISC_WELL_GRAVITY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_RRCID_DETERMINING_WELL), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? FL_RRCID_DETERMINING_WELL
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_G_1_GAS_GRAVITY), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FL_G_1_GAS_GRAVITY
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_AVG_RESERVOIR_BHP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FL_AVG_RESERVOIR_BHP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_AVG_RESERVOIR_BH_TEMP), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FL_AVG_RESERVOIR_BH_TEMP
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_FORMATION_VOLUME_FACTOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FL_FORMATION_VOLUME_FACTOR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_SOLUTION_GAS_OIL_RATIO), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FL_SOLUTION_GAS_OIL_RATIO
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(FL_DEVIATION_FACTOR), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public float? FL_DEVIATION_FACTOR
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellTestAggr()
        {
        }

        public WellTestAggr(NpgsqlDataReader reader)
        {
            FL_DISTRICT                = reader[0].ShortValue()!.Value;
            FL_FIELD_NUMBER            = reader[1].LongValue()!.Value;
            FL_RESERVOIR_NUMBER        = reader[2].IntValue()!.Value;
            FL_NAME                    = reader[3].StringValue();
            FL_FIELD_CLASS             = reader[4].CharValue()!.Value;
            FL_RESERVOIR_NAME          = reader[5].StringValue();
            FL_FORMATION_COMPOSITION   = reader[6].StringValue();
            FL_OIL_DISC_WELL_GRAVITY   = reader[7].FloatValue()!.Value;
            FL_RRCID_DETERMINING_WELL  = reader[8].LongValue();
            FL_G_1_GAS_GRAVITY         = reader[9].FloatValue();
            FL_AVG_RESERVOIR_BHP       = reader[10].FloatValue();
            FL_AVG_RESERVOIR_BH_TEMP   = reader[11].FloatValue();
            FL_FORMATION_VOLUME_FACTOR = reader[12].FloatValue();
            FL_SOLUTION_GAS_OIL_RATIO  = reader[13].FloatValue();
            FL_DEVIATION_FACTOR        = reader[14].FloatValue();
        }
    }
}
