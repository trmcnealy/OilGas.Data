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
    [XmlRoot(nameof(WellBorePluggingDataCasingTubing))]
    [Table(nameof(WellBorePluggingDataCasingTubing))]
    public class WellBorePluggingDataCasingTubing
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
        //02 WELL-BORE-PLUG-CAS-SEG.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLG_CAS_COUNTER), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLG_CAS_COUNTER
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //05 WB-PLUG-CT-RECORD.

        //07 WB-PLUG-CAS-SIZE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_CAS_INCH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_PLUG_CAS_INCH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_CAS_FRAC_NUM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_PLUG_CAS_FRAC_NUM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_CAS_FRAC_DENOM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_PLUG_CAS_FRAC_DENOM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //07 WB-PLUG-CAS-LBS-FT.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_WGT_WHOLE), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public int? WB_PLUG_WGT_WHOLE
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_WGT_TENTHS), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public sbyte? WB_PLUG_WGT_TENTHS
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_AMT_PUT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLUG_AMT_PUT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_AMT_LEFT), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WB_PLUG_AMT_LEFT
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }
        //07 WB-PLUG-HOLE-SIZE.

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_HOLE_INCH), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_PLUG_HOLE_INCH
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_HOLE_FRAC_NUM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_PLUG_HOLE_FRAC_NUM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WB_PLUG_HOLE_FRAC_DENOM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WB_PLUG_HOLE_FRAC_DENOM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBorePluggingDataCasingTubing()
        {
        }

        public WellBorePluggingDataCasingTubing(ReadOnlySpan<byte> source)
        {
            //02 WELL-BORE-PLUG-CAS-SEG.
            WB_PLG_CAS_COUNTER = StringParser.ReadAsInt64(source, 3 - 1, 6);
            //05 WB-PLUG-CT-RECORD.
            //07 WB-PLUG-CAS-SIZE.
            WB_PLUG_CAS_INCH       = StringParser.ReadAsInt16(source, 9  - 1, 2);
            WB_PLUG_CAS_FRAC_NUM   = StringParser.ReadAsInt16(source, 11 - 1, 2);
            WB_PLUG_CAS_FRAC_DENOM = StringParser.ReadAsInt16(source, 13 - 1, 2);
            //07 WB-PLUG-CAS-LBS-FT.
            WB_PLUG_WGT_WHOLE  = StringParser.ReadAsInt32(source, 15 - 1, 3);
            WB_PLUG_WGT_TENTHS = StringParser.ReadAsSByte(source, 18 - 1, 1);
            WB_PLUG_AMT_PUT    = StringParser.ReadAsInt64(source, 19 - 1, 5);
            WB_PLUG_AMT_LEFT   = StringParser.ReadAsInt64(source, 24 - 1, 5);
            //07 WB-PLUG-HOLE-SIZE.
            WB_PLUG_HOLE_INCH       = StringParser.ReadAsInt16(source, 29 - 1, 2);
            WB_PLUG_HOLE_FRAC_NUM   = StringParser.ReadAsInt16(source, 31 - 1, 2);
            WB_PLUG_HOLE_FRAC_DENOM = StringParser.ReadAsInt16(source, 33 - 1, 2);
        }
    }
}
