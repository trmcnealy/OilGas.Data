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
    [XmlRoot(nameof(WellBoreDrillingPermitStatus))]
    [Table(nameof(WellBoreDrillingPermitStatus))]
    public class WellBoreDrillingPermitStatus
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
        //02 WBDASTAT-SEGMENT.

        //03 WBDASTAT-NUMBER. 3

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WBDASTAT_STAT_NUM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public long? WBDASTAT_STAT_NUM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WBDASTAT_UNIQ_NUM), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public short? WBDASTAT_UNIQ_NUM
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        [DataMember]
        [XmlElement]
        [JsonProperty(nameof(WBDASTAT_DELETED_FLAG), NamingStrategyType = typeof(DefaultNamingStrategy))]
        public char? WBDASTAT_DELETED_FLAG
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        public WellBoreDrillingPermitStatus()
        {
        }

        public WellBoreDrillingPermitStatus(ReadOnlySpan<byte> source)
        {
            //02 WBDASTAT-SEGMENT.
            //03 WBDASTAT-NUMBER. 3
            WBDASTAT_STAT_NUM     = StringParser.ReadAsInt64(source, 3  - 1, 7);
            WBDASTAT_UNIQ_NUM     = StringParser.ReadAsInt16(source, 10 - 1, 2);
            WBDASTAT_DELETED_FLAG = StringParser.ReadAsChar(source, 12  - 1, 1);
        }
    }
}
